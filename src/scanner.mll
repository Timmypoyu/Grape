(* Ocamllex scanner for MicroC *)

{ open Parser }

let digit = ['0'-'9']
let letter = ['A'-'Z' 'a'-'z']
let quote = '\"'
(* TODO: Floats *)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "///"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "Node"   { NODE }
| "Graph"  { GRAPH }
| "bool"   { BOOL }
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
| digit+ as lxm { INT_LIT(int_of_string lxm) }
| letter (letter | digit | '_')* as lxm { ID(lxm) }
| '\"' [^'\"']* as lxm '\"' { STR_LIT(lxm) }
| []
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "///" { token lexbuf }
| _    { comment lexbuf }
