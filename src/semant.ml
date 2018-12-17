(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Verify a list of bindings has no void types or duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
     List.iter (function
	(Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
        let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
	      raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (**** Check global variables ****)
  check_binds "global" globals;

  (* Methods are treated as functions where their object is the first arg *)
  (**** Get types of overloaded functions ****)
  let overload_functions = [ 
    ("print", Void, function 
        [_] -> Void 
      | _ -> raise (Failure "invalid args"));
    ("size", Int, function 
        [List _] | [Str] -> Int 
      | _ -> raise (Failure "invalid args")); 
    ("outgoing", Any, function 
        [Graph (a, b); Node c] when a = c -> 
          List (Edge (b, a))
      | _ -> raise (Failure "invalid args")); 
  ] in

  (* Collect function declarations for built-in functions: no bodies or formals *)
  let grape_func_decls = 
    let add_bind func_map (n, t,  o) = 
      StringMap.add n {
        name = n; 
        otyp = o;
        typ = t ;
        body = [];
        formals = [];
      } func_map 
    in List.fold_left add_bind StringMap.empty overload_functions
  in

  (* Add function name to symbol table *)
  let function_decls =
    let add_func map fd = 
      let key = fd.name 
      and built_in_err = "function " ^ fd.name ^ " may not be defined"
      and dup_err = "duplicate function " ^ fd.name
      and make_err er = raise (Failure er)
      in match fd with (* No duplicate functions or redefinitions of built-ins *)
          _ when StringMap.mem key built_in_functions -> make_err built_in_err
        | _ when StringMap.mem key map -> make_err dup_err  
        | _ -> StringMap.add key {
            typ = fd.typ;
            otyp = (fun a -> fd.typ)
            name = fd.name;
            body = fd.body;
            formals = fd.formals;
          } map 
    in List.fold_left add_func StringMap.empty functions
  in

  (* Return a function from our symbol table *)
  (* Make sure main exists, and get the return type of a function *)
  let return_typ name args = 
    let args' = List.map (fun a -> fst a) args in
    try (StringMap.find name function_decls).typ args'
  in

  let _ = return_typ "main" [] in (* Ensure "main" is defined *)

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let rec check_assign lvaluet rvaluet err =
      match (lvaluet, rvaluet) with
          (Any, a) -> a
        | (a, Any) -> a
        | (Edge (a,_), Edge (b, c)) -> Edge (check_assign a b err, c)
        | (Node a, Node b) -> Node (check_assign a b err)
        | (List a, List b) -> List (check_assign a b err)
        | (List a, List b) -> List (check_assign a b err)
        | (Graph (a, b), Graph (c, d)) -> 
            Graph (check_assign a c err, check_assign b d err)
        | (a, b) -> if a = b then a else raise (Failure err)
    in   

    let rec concat_statements locals = function
        [] -> locals
      | hd::tl -> concat_statements (match hd with
           Declare (t, id, a) -> (t, id) :: locals 
                    (* TODO: Check expr type of a *)
         | _ -> locals) tl
    in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
      StringMap.empty (globals @ func.formals @ (concat_statements [] func.body))
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier locals s =
      try StringMap.find s locals
      with Not_found -> raise (Failure ("Undeclared identifier " ^ s))
    in

    let expr_list lst expr =
        let rec helper typ tlist = function
            [] -> (typ, tlist)
          | hd :: _ when (match (typ, fst (expr hd)) with 
                (Node a , Node b) -> a <> b
              | (Edge (a, b), Edge (c, d)) -> a <> c && b <> d
              | (a, b) -> a <> b) ->
          	raise (Failure ("Type inconsistency with list "))
          | hd :: tl -> helper typ (expr hd :: tlist) tl
        in
      let typ = match (List.length lst) with  
          0 -> Any
        | _ -> (fst (expr (List.hd lst))) in
      helper typ [] lst 
    in

    let expr_graph graph expr = 
      let rec type_of_path ntyp etyp plist = function
          [] -> ((ntyp, etyp), List.rev plist)
        | hd :: _ when fst (expr (fst hd)) <> ntyp ->
          raise (Failure (
            "node type inconsistency with path " ^ 
            string_of_typ (fst (expr (fst hd))) ^ 
            string_of_typ ntyp ))
        | hd :: tl when (List.length tl) = 0 && fst (expr (snd hd)) <> Void ->  
          raise (Failure ("edge type inconsistency with path "))
        | hd :: tl when (List.length tl) <> 0 && fst (expr (snd hd)) <> etyp ->
          raise (Failure ("edge type inconsistency with path "))
        | hd :: tl -> type_of_path ntyp etyp (((expr (fst hd)), (expr (snd hd)))::plist) tl 
      in

      let rec expr_graph ntyp etyp type_of_path glist = function
          [] -> ((ntyp, etyp), List.rev glist)
        | hd :: _ when fst (type_of_path ntyp etyp [] hd) <> (ntyp, etyp) ->
          raise (Failure ("path type inconsistency"))
        | hd :: tl -> expr_graph ntyp etyp type_of_path ((snd (type_of_path ntyp etyp [] hd))::glist) tl
      in
             
      let edgeType = fst (expr (snd (List.hd (List.hd graph)))) in
      let nodeType = fst (expr (fst (List.hd (List.hd graph)))) in
      expr_graph nodeType edgeType type_of_path [] graph in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr locals = function
        IntLit l  -> (Int, SIntLit l)
      | FloatLit l -> (Float, SFloatLit l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | StrLit s   -> (Str, SStrLit s)
      | NodeLit n ->  let (t, d) = expr locals n in (Node t, SNodeLit (t, d))
      | ListLit l -> let (t, l) = expr_list l (expr locals) in (List t, SListLit l) 
      | Index (e, i) -> 
          let (te, _) as e' = expr locals e in 
          let (ti, _) as i' = expr locals i in 
          if ti != Int then raise (Failure ("list index not integer"))
          else (match te with 
              List x -> (x, SIndex (e', i')) 
            | Str -> (Str, SIndex (e', i')) 
            | _ -> raise (Failure ("not iterable")))
      | GraphLit g -> let ((n, e), l) = expr_graph g (expr locals) in 
        (Graph (n, e), SGraphLit l)       
      | EdgeLit s -> let t = expr locals s in (Edge ((fst t), Any), SEdgeLit t)  
      | DirEdgeLit s -> let t = expr locals s in (Edge ((fst t), Any), SDirEdgeLit t)
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier locals s, SId s)
      | Prop (e, p) -> 
        let (et, _) as e' = expr locals e in
        let pt = (match (et, p) with
            (Node t,      "val") -> t
          | (Edge (t, _), "val") -> t
          | (Edge (_, t), "to") -> Node t
          | (Edge (_, t), "from") -> Node t
          | (_, _) -> raise (Failure ("no such property"))) in
        (pt, SProp (e', p))
      | Assign (var, e) as ex -> 
          let lt = type_of_identifier locals var
          and (rt, e') = expr locals e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex in 
          (check_assign lt rt err, SAssign(var, (rt, e')))
      | Unop(op, e) as ex -> 
          let (t, e') = expr locals e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr locals e1 
          and (t2, e2') = expr locals e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div when same && t1 = Float -> Float
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	          Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call(name, args) -> 
          let args' = List.map (expr locals) args in
          let typ = return_typ name args' in
          (typ, SCall(name, args'))
      | Method(obj, name, args) ->
          let (t, _) as o' = expr locals obj in
          let args' = o' :: List.map (expr locals) args in
          let typ = return_typ name args' in
          (typ , SCall(name, args'))
    in

    let check_bool_expr locals e = 
      let (t', e') = expr locals e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt locals = function
        Expr e -> SExpr (expr locals e)
      | If(p, b1, b2) -> 
          SIf(check_bool_expr locals p, check_stmt locals b1, check_stmt locals b2)
      | Each(p, s) -> 
          let p' = expr locals p in
          let this_locals = StringMap.add "this" (match fst p' with
              List s -> s | Str -> Str 
            | _ -> raise (Failure "type not iterable")
          ) locals in
          SEach(p', check_stmt this_locals s)
      | While(p, s) -> let newLocals = StringMap.map (fun a -> a) locals in
          SWhile(check_bool_expr locals p, check_stmt locals s)
      | Declare (typ, id, asn)  -> 
          let a = expr locals asn in SDeclare(typ, id, a)
      | Return e -> let (t, e') = expr locals e in
        let typ = return_typ func.name func.formals in
        if t = typ then SReturn (t, e') 
        else raise ( Failure ("return gives " ^ string_of_typ t ^
          " expected " ^ string_of_typ typ ^ " in " ^ string_of_expr e))
	    
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          (* Copy locals into new block *)
          let newLocals = StringMap.map (fun a -> a) locals in
          let rec check_stmt_list locals = function
              [Return _ as s] -> [check_stmt locals s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list locals (sl @ ss) (* Flatten blocks *)
            | Declare (t, i, a) :: ss -> 
                let a' = expr locals a in
                let new_locals = StringMap.add i t locals in
                (* if StringMap.mem ? *)
                check_stmt_list newLocals ss

            | s :: ss         -> check_stmt locals s :: check_stmt_list locals ss
            | []              -> []
          in SBlock(check_stmt_list newLocals sl)

    in (* body of check_function *)
    { styp = return_typ func.name func.formals;
      sname = func.name;
      sformals = func.formals;
      sbody = match check_stmt StringMap.empty (Block func.body) with
	      SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in (globals, List.map check_function functions)
