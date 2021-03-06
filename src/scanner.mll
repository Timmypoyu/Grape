(* Ocamllex scanner for MicroC *)

{ open Parser }

(*Hello*)
let digit = ['0'-'9']
let decimal = (digit+ '.' digit*)
let letter = ['A'-'Z' 'a'-'z']
let lowerLetter = ['a'-'z']
(* TODO: Floats *)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "///"    { mComment lexbuf }           (* Comments *)
| "//"     { sComment lexbuf }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['	     { LBRACK }
| ']'      { RBRACK }
| ';'      { SEMI }
| "'"      { SQUOT }
| '"'      { DQUOT }
| ','      { COMMA }
| '.'      { DOT }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| "**" 	   { EXP }	
| '/'      { DIVIDE }
| '%' 	   { MOD }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '&'	     { AMP } 
| '<'      { LT }
| "<<"     { GRAPS }
| ">>"     { GRAPE }
| "<="     { LEQ }
| ">"      { GT }
| "_"      { UNDS }
| ">="     { GEQ }
| "fun"	   { FUN }	
| "and"    { AND }
| "or"     { OR }
| "not"    { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "each"   { EACH }
| "for"	   { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "Int"    { INT }
| "Edge"   { EDGE }
| "Node"   { NODE }
| "Dict"   { DICT }
| "Graph"  { GRAPH }
| "List"   { LIST }
| "String" { STR }
| "Bool"   { BOOL }
| "Yes"   { TRUE }
| "No"  { FALSE }
| decimal as lxm            { FLOAT_LIT(lxm) }
| digit+ as lxm             { INT_LIT(int_of_string lxm) }
| '\"' ([^'\"']* as lxm) '\"' { STR_LIT(lxm) }
| lowerLetter (letter | digit | '_')* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and mComment = parse
| "///" { token lexbuf }
| _ { mComment lexbuf }

and sComment = parse
  '\n' { token lexbuf }
| _    { sComment lexbuf }
