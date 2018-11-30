(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or | Exp | Mod | Amp 

type uop = Neg | Not

type typ = Int | Float | Bool | Void | Str 
        | Graph of typ * typ
	| Node of typ
	| Edge of typ
	| List of typ 

(* variable type declaration *)
type bind = typ * string

type expr =
    IntLit of int
  | FloatLit of string  
  | BoolLit of bool
  | NodeLit of expr
  | EdgeLit of expr
  | DirEdgeLit of expr 
  | GraphLit of ((expr * expr) list) list 
  | ListLit of expr list 
  | StrLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list 
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | Each of expr * stmt
  | While of expr * stmt
  | DecAsn of typ * string * expr

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type program = bind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Mod -> "%"
  | Exp -> "**"
  | Amp -> "&"
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | FloatLit(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | NodeLit(e) -> "'" ^ string_of_expr e ^ "'"
  | EdgeLit(e) -> "_" ^ string_of_expr e ^ "_<"
  | DirEdgeLit(e) -> "_" ^ string_of_expr e ^ "_>"
  | GraphLit(e) -> "<" ^ String.concat ", " (List.map (function lst -> String.concat " " (List.map (function (k, v) -> string_of_expr k ^ " " ^ string_of_expr v) lst ))e) ^ ">" 
  | ListLit(e) -> "[" ^ String.concat ", " (List.map string_of_expr e) ^ "]" 
  | StrLit(e) -> e
  | Call(f, el) -> 
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Each(e, s) -> "each (" ^ string_of_expr e ^ ")" ^ string_of_stmt s

let rec string_of_typ = function
    Int -> "Int"
  | Bool -> "Bool"
  | Void -> "Void"
  | Float -> "Float"
  | Str -> "String"
  | Node(t) -> "Node<" ^ string_of_typ t ^ ">" 
  | Edge(t) -> "Edge<" ^ string_of_typ t ^ ">"
  | Graph(s,t) -> "Graph<" ^ string_of_typ s ^ string_of_typ t ^ ">"
  | List(t) -> "List<" ^ string_of_typ t ^ ">"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
