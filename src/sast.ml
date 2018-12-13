(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SIntLit of int
  | SFloatLit of string
  | SBoolLit of bool
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SNodeLit of sexpr 
  | SEdgeLit of sexpr
  | SDirEdgeLit of sexpr 
  | SGraphLit of ((sexpr * sexpr) list) list 
  | SListLit of sexpr list 
  | SStrLit of string
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SDeclare of typ * string * sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SEach of sexpr * sstmt
  | SWhile of sexpr * sstmt

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    sbody : sstmt list;
  }

type sprogram = bind list * sfunc_decl list

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SIntLit(l) -> string_of_int l
  | SFloatLit(l) -> l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SNodeLit(e) -> "'" ^ string_of_sexpr e ^ "'" 
  | SListLit(e) -> "[" ^ String.concat "," (List.map string_of_sexpr e) ^ "]"
  | SDirEdgeLit(e) -> "_" ^ string_of_sexpr e ^ "_>"
  | SEdgeLit(e) -> "_" ^ string_of_sexpr e ^ "_<"
  | SGraphLit(e) -> "<" ^ String.concat ", " (List.map (function lst -> String.concat " " (List.map (function (k, v) ->  string_of_sexpr k ^ " " ^ string_of_sexpr v) lst ))e) ^ ">"  
  | SStrLit(e) -> e
  | SId(e) -> e
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SNoexpr -> ""
				  ) ^ ")"				     

let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SDeclare(t, id, a) -> string_of_typ t ^ " " ^ (match (snd a) with SNoexpr -> id | _ -> string_of_sexpr a) ^ ";\n"
  | SEach(e, s) -> "each (" ^ string_of_sexpr e ^ ")" ^ string_of_sstmt s

let string_of_sfdecl fdecl =
  "fun " ^ string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ") {\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
