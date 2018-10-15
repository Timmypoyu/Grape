(* Ocamllex scanner for MicroC *)

{ open Parser }

let digit = ['0'-'9']
let letter = ['A'-'Z' 'a'-'z']
let lowerLetter = ['a'-'z']
let quote = '\"'
(* TODO: Floats *)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "///"    { comment lexbuf }           (* Comments *)
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
| "and"    { AND }
| "or"     { OR }
| "not"    { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "each"   { EACH }
| "while"  { WHILE }
| "return" { RETURN }
| "Int"    { INT }
| "Node"   { NODE }
| "Graph"  { GRAPH }
| "String" { STR }
| "Bool"   { BOOL }
| "True"   { TRUE }
| "False"  { FALSE }
| digit+ as lxm { INT_LIT(int_of_string lxm) }
| lowerLetter (letter | digit | '_')* as lxm { ID(lxm) }
| '\"' [^'\"']* as lxm '\"' { STR_LIT(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "///" { token lexbuf }
| _    { comment lexbuf }
