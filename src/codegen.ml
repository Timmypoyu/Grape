(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Grape" in

  (* Get types from the LLVM module context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and str_t      = L.pointer_type (L.i8_type context)
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context
  and obj_ptr_t  = L.pointer_type (L.i8_type context)
  and void_ptr_t = L.pointer_type (L.i8_type context)
  in

  (* TODO: Make pointers type-dependent? *)
  (* Return the LLVM type for AST node of Grape type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Str   -> str_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t
    | A.Node(_)  -> obj_ptr_t
    | A.List(_)  -> obj_ptr_t
    | A.Edge(_,_)  -> obj_ptr_t
    | A.Graph(_,_) -> obj_ptr_t 
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* STD IO *)

  (* Function signature / type *)
  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf : L.llvalue = 
      L.declare_function "printf" printf_t the_module in
  let printbig_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
  let printbig : L.llvalue =
      L.declare_function "printbig" printbig_t the_module in

  (* NODE FUNCTIONS *)

  let init_t : L.lltype = 
      L.var_arg_function_type obj_ptr_t [| void_ptr_t |] in

  let init_node : L.llvalue = 
      L.declare_function "init_node" init_t the_module in
  let init_edge : L.llvalue = 
      L.declare_function "init_edge" init_t the_module in

  let init_graph_t : L.lltype = 
      L.var_arg_function_type obj_ptr_t [||] in
  let init_graph : L.llvalue = 
      L.declare_function "init_graph" init_graph_t the_module in

  let add_node_t : L.lltype = 
      L.var_arg_function_type obj_ptr_t [| obj_ptr_t |] in
  let add_node : L.llvalue = 
      L.declare_function "add_node" add_node_t the_module in

  let add_edge_t : L.lltype = 
      L.var_arg_function_type obj_ptr_t [| obj_ptr_t |] in
  let add_edge : L.llvalue = 
      L.declare_function "add_edge" add_edge_t the_module in

  let link_edge_t : L.lltype = 
    L.var_arg_function_type void_t [|obj_ptr_t; obj_ptr_t|] in
  let link_edge_to : L.llvalue = 
      L.declare_function "link_edge_to" link_edge_t the_module in

  let link_edge_from : L.llvalue = 
      L.declare_function "link_edge_from" link_edge_t the_module in
  (* This must match the C library function name *)

  (* list functions*)

  let init_list_t : L.lltype = 
      L.var_arg_function_type obj_ptr_t [||] in
  let init_list : L.llvalue = 
      L.declare_function "init_list" init_list_t the_module in

  let push_list_t : L.lltype = 
      L.var_arg_function_type void_t [|obj_ptr_t; void_ptr_t |] in
  let push_list : L.llvalue = 
      L.declare_function "push_list" push_list_t the_module in

  let push_front_list_t : L.lltype = 
      L.var_arg_function_type void_t [|obj_ptr_t; void_ptr_t|] in
  let push_front_list : L.llvalue = 
      L.declare_function "push_front_list" push_front_list_t the_module in

  let pop_front_list_t : L.lltype = 
      L.var_arg_function_type void_t [|obj_ptr_t|] in
  let pop_front_list : L.llvalue = 
      L.declare_function "pop_front_list" pop_front_list_t the_module in

  let pop_list_t : L.lltype = 
      L.var_arg_function_type void_t [|obj_ptr_t|] in
  let pop_list : L.llvalue = 
      L.declare_function "pop_list" pop_list_t the_module in

  let list_get_t : L.lltype = 
      L.var_arg_function_type void_ptr_t [|i32_t ; obj_ptr_t|] in
  let list_get : L.llvalue = 
      L.declare_function "list_get" list_get_t the_module in

  let size_t : L.lltype = 
      L.var_arg_function_type i32_t [|obj_ptr_t|] in
  let size : L.llvalue = 
      L.declare_function "size" size_t the_module in
	
  (* string functions*)

  let get_char_t: L.lltype = 
      L.var_arg_function_type str_t [|i32_t; str_t|] in
  let get_char : L.llvalue = 
      L.declare_function "get_char" get_char_t the_module in

  let str_size_t : L.lltype = 
      L.var_arg_function_type i32_t [|str_t|] in
  let str_size : L.llvalue = 
      L.declare_function "str_size" str_size_t the_module in

  let str_equal_t : L.lltype = 
      L.var_arg_function_type i1_t [|str_t; str_t|] in
  let str_equal : L.llvalue = 
      L.declare_function "str_equal" str_equal_t the_module in


  (* graph functions*)

 
  let get_outgoing_t : L.lltype = 
      L.var_arg_function_type obj_ptr_t [|obj_ptr_t|] in
  let get_outgoing : L.llvalue = 
      L.declare_function "get_outgoing" get_outgoing_t the_module in

  let get_outgoing2_t : L.lltype = 
      L.var_arg_function_type obj_ptr_t [|obj_ptr_t; obj_ptr_t|] in
  let get_outgoing2 : L.llvalue = 
      L.declare_function "get_outgoing" get_outgoing2_t the_module in

  let get_t : L.lltype = 
      L.var_arg_function_type void_ptr_t [|obj_ptr_t|] in
  let get_val : L.llvalue = 
      L.declare_function "get_val" get_t the_module in

  let get_to_t : L.lltype = 
      L.var_arg_function_type obj_ptr_t [|obj_ptr_t|] in
  let get_to : L.llvalue = 
      L.declare_function "get_to" get_to_t the_module in

  let get_from_t : L.lltype = 
      L.var_arg_function_type obj_ptr_t [|obj_ptr_t|] in
  let get_from : L.llvalue = 
      L.declare_function "get_from" get_from_t the_module in

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
 
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and str_format_str = L.build_global_stringptr "%s\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
      let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
      StringMap.add n local m 

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in

      let rec add_declaration locals = function
          [] -> locals
        | hd::tl -> add_declaration (match hd with
             SDeclare (t, id, _) -> (t, id) :: locals
           | _ -> locals) tl
      in
          (* slocals: typ * string list *)
      let declarations = add_declaration [] fdecl.sbody in
      List.fold_left add_local formals declarations
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
        with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((typ, e) : sexpr) = match e with
      SIntLit i   -> L.const_int i32_t i
    | SStrLit s   -> L.build_global_stringptr s "string" builder
    | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
    | SFloatLit l -> L.const_float_of_string float_t l
    | SNoexpr     -> L.const_int i32_t 0
    | SId s       -> L.build_load (lookup s) s builder
    | SProp (o, p) ->
      let tobj = fst o in
      let o' = expr builder o in
      (match (tobj, p) with
        (A.Edge (t, _), "val") ->
          let dest_ptr = L.pointer_type (ltype_of_typ t) in
          let data_ptr = L.build_call get_val [|o'|] "val" builder in  
          let data_ptr = L.build_bitcast data_ptr dest_ptr "data" builder in
          L.build_load data_ptr "data" builder
      | (A.Node t, "val") ->
          let dest_ptr = L.pointer_type (ltype_of_typ t) in
          let data_ptr = L.build_call get_val [|o'|] "val" builder in  
          let data_ptr = L.build_bitcast data_ptr dest_ptr "data" builder in
          L.build_load data_ptr "data" builder
      | (A.Edge (_, t), "to") ->
          let dest_ptr = L.pointer_type (ltype_of_typ t) in
          let data_ptr = L.build_call get_to [|o'|] "to" builder in  
          L.build_bitcast data_ptr dest_ptr "data" builder
      | (A.Edge (_, t), "from") ->
          let dest_ptr = L.pointer_type (ltype_of_typ t) in
          let data_ptr = L.build_call get_from [|o'|] "from" builder in  
          L.build_bitcast data_ptr dest_ptr "data" builder
      | (_, _) -> raise (Failure "no such property"))
    | SAssign (s, e) -> let e' = expr builder e in
      ignore(L.build_store e' (lookup s) builder); e'
    | SNodeLit (t, v) -> (* Cast data type into void pointer to init node *)
      let data_value = expr builder (t, v) in 
      let data = L.build_malloc (ltype_of_typ t) "data_malloc" builder in
        ignore ( L.build_store data_value data builder);
      let data = L.build_bitcast data void_ptr_t "data_bitcast" builder in
      let node = L.build_call init_node [|data|] "init_node" builder in node
    | SEdgeLit (t, v) -> 
      let data_value = expr builder (t, v) in 
      let data = L.build_malloc (ltype_of_typ t) "data_malloc" builder in
        ignore ( L.build_store data_value data builder);
      let data = L.build_bitcast data void_ptr_t "data_bitcast" builder in
      let edge = L.build_call init_edge [|data|] "init_edge" builder in edge
(* TODO: Initialize with empty lists *)
    | SDirEdgeLit _ -> raise (Failure "Unimplemented")
    | SGraphLit l -> 
      let graph = L.build_call init_graph [||] "init_graph" builder in 
      let rec init_path lastEdge isLast = function
        | [] -> graph
        | [hd] when isLast = 1 ->
          let node = expr builder (fst hd) in
          ignore(L.build_call add_node [|graph; node|] "" builder);
          ignore(L.build_call link_edge_to [|lastEdge; node|] "" builder);
          graph
        | hd::tl when isLast = 0 -> 
          let edge = expr builder (snd hd) in 
          let node = expr builder (fst hd) in
          ignore(L.build_call add_node [|graph; node|] "" builder);
          ignore(L.build_call add_edge [|graph; edge|] "" builder);
          ignore(L.build_call link_edge_from [|edge; node|] "" builder);
          init_path edge 1 tl
        | hd::tl  -> 
          let edge = expr builder (snd hd) in 
          let node = expr builder (fst hd) in
          ignore(L.build_call add_node [|graph; node|] "" builder);
          ignore(L.build_call add_edge [|graph; edge|] "" builder);
          ignore(L.build_call link_edge_from [|edge; node|] "" builder); 
          ignore(L.build_call link_edge_to [|lastEdge; node|] "" builder); 
          init_path edge 1 tl
      in 
      ignore(List.map (init_path (L.const_int i8_t 0) 0) l); graph
    | SIndex (e, i) ->
      let e' = expr builder e in
      let i' = expr builder i in 
      (match (fst e) with 
          A.Str -> L.build_call get_char [|i'; e'|] "get_char" builder 
        | A.List t -> 
          let data_ptr = L.build_call list_get [|i'; e'|] "list_get" builder in  
          match t with 
              A.List _ | A.Node _ | A.Edge (_,_) | A.Graph _ -> data_ptr
            | _ -> 
              let dest_ptr = L.pointer_type (ltype_of_typ t) in
              let data_ptr = L.build_bitcast data_ptr dest_ptr "data" builder in
              L.build_load data_ptr "data" builder 
        | _ -> raise (Failure "Cannot index type"))
    | SListLit i ->
      let rec fill_list lst = (function 
      [] -> lst
      |sx :: tail ->
      let (atyp,_) = sx in
      let data_ptr = (match atyp with
        A.List _ | A.Graph (_,_) | A.Edge _ | A.Node _  -> expr builder sx 
        | _  -> let data = L.build_malloc (ltype_of_typ atyp) "data" builder in
          let data_value = expr builder sx in  
          ignore (L.build_store data_value data builder); data) in 
      let data = L.build_bitcast data_ptr void_ptr_t "data" builder in 
      ignore(L.build_call push_list [|lst; data|] "" builder); fill_list lst tail) in
      let lst = L.build_call init_list [||] "init_list" builder in 
      fill_list lst i 
    | SBinop ((A.Float,_ ) as e1, op, e2) ->
      let e1' = expr builder e1
      and e2' = expr builder e2 in
      (match op with 
          A.Add     -> L.build_fadd
        | A.Sub     -> L.build_fsub
        | A.Mult    -> L.build_fmul
        | A.Div     -> L.build_fdiv 
        | A.Exp     -> raise (Failure "Unimplemented")
        | A.Mod     -> raise (Failure "Unimplemented")
        | A.Amp     -> raise (Failure "Unimplemented")
        | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
        | A.Neq     -> L.build_fcmp L.Fcmp.One
        | A.Less    -> L.build_fcmp L.Fcmp.Olt
        | A.Leq     -> L.build_fcmp L.Fcmp.Ole
        | A.Greater -> L.build_fcmp L.Fcmp.Ogt
        | A.Geq     -> L.build_fcmp L.Fcmp.Oge
        | A.And | A.Or ->
          raise (Failure "internal error: semant should have rejected and/or on float")
      ) e1' e2' "tmp" builder
    | SBinop ((A.Str, _) as e1, A.Equal, e2) ->
      let e1' = expr builder e1
      and e2' = expr builder e2 in
      L.build_call str_equal [|e1'; e2'|] "str_equal" builder
    | SBinop (e1, op, e2) ->
      let e1' = expr builder e1
      and e2' = expr builder e2 in
      (match op with
          A.Add     -> L.build_add
        | A.Sub     -> L.build_sub
        | A.Exp     -> raise(Failure "Unimplemented")
        | A.Mod     -> raise(Failure "Unimplemented")
        | A.Amp     -> raise(Failure "Unimplemented")
        | A.Mult    -> L.build_mul
        | A.Div     -> L.build_sdiv
        | A.And     -> L.build_and
        | A.Or      -> L.build_or
        | A.Equal   -> L.build_icmp L.Icmp.Eq
        | A.Neq     -> L.build_icmp L.Icmp.Ne
        | A.Less    -> L.build_icmp L.Icmp.Slt
        | A.Leq     -> L.build_icmp L.Icmp.Sle
        | A.Greater -> L.build_icmp L.Icmp.Sgt
        | A.Geq     -> L.build_icmp L.Icmp.Sge
      ) e1' e2' "tmp" builder
    | SUnop(op, ((t, _) as e)) ->
      let e' = expr builder e in
      (match op with
          A.Neg when t = A.Float -> L.build_fneg 
        | A.Neg                  -> L.build_neg
        | A.Not                  -> L.build_not
      ) e' "tmp" builder
    | SCall ("print", [e]) | SCall ("printb", [e]) ->
        L.build_call printf [| int_format_str ; (expr builder e) |] "printf" builder
    | SCall ("printbig", [e]) ->
        L.build_call printbig [| (expr builder e) |] "printbig" builder
    | SCall ("printf", [e]) -> 
        L.build_call printf [| float_format_str ; (expr builder e) |] "printf" builder
    | SCall ("prints", [e]) ->
        L.build_call printf [| str_format_str ; (expr builder e) |] "prints" builder
    | SCall ("size", [e]) -> 
        L.build_call size [|expr builder e|] "size" builder  
    | SCall ("str_size", [e]) -> 
        L.build_call str_size [|expr builder e|] "str_size" builder  
    | SCall ("get_char", [e;f]) -> 
        L.build_call get_char [|expr builder e; expr builder f|] "get_char" builder  
    | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in     
        let llargs = List.rev (List.map (expr builder) (List.rev args)) in
        let result = (match fdecl.styp with 
            A.Void -> ""
          | _ -> f ^ "_result") in
        L.build_call fdef (Array.of_list llargs) result builder
    | SMethod (o, m, args) ->
      let tobj = fst o in
      let o' = expr builder o in
      (match (tobj, m) with
        (A.Graph (n, e), "outgoing") ->
          let a' = expr builder (List.hd args) in
          let ltype = ltype_of_typ (A.List (A.Edge (e, n))) in
          let data_ptr = L.build_call get_outgoing2 [|o'; a'|] "outgoing" builder in  
          L.build_bitcast data_ptr ltype "data" builder
        | (_, _) -> raise (Failure "no such method"))
  in
    
    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
    Some _ -> ()
  | None -> ignore (instr builder) in

(* Build the code for the given statement; return the builder for
   the statement's successor (i.e., the next instruction will be built
   after the one generated by this call) *)

let rec stmt builder = function
    SBlock sl -> List.fold_left stmt builder sl
  | SExpr e -> ignore(expr builder e); builder
  | SDeclare (_, _, a) -> ignore(expr builder a); builder 
  | SReturn e -> ignore(match fdecl.styp with 
        A.Void -> L.build_ret_void builder          (* return void instr *)
      | _ -> L.build_ret (expr builder e) builder );  (* Build return statement *)
        builder
  | SIf (predicate, then_stmt, else_stmt) ->
    let bool_val = expr builder predicate in
    let merge_bb = L.append_block context "merge" the_function in
    let build_br_merge = L.build_br merge_bb in (* partial function *)

    let then_bb = L.append_block context "then" the_function in
    add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
    build_br_merge;

    let else_bb = L.append_block context "else" the_function in
    add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
    build_br_merge;

    ignore(L.build_cond_br bool_val then_bb else_bb builder);
    L.builder_at_end context merge_bb

  | SWhile (predicate, body) ->
    let pred_bb = L.append_block context "while" the_function in
    ignore(L.build_br pred_bb builder);

    let body_bb = L.append_block context "while_body" the_function in
    add_terminal (stmt (L.builder_at_end context body_bb) body)
    (L.build_br pred_bb);

    let pred_builder = L.builder_at_end context pred_bb in
    let bool_val = expr pred_builder predicate in

    let merge_bb = L.append_block context "merge" the_function in
    ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
    L.builder_at_end context merge_bb

    | SEach (_, _) -> raise(Failure "not implemented!") 
	in

    (* Build the code for each statement in the function *)
  let builder = stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
