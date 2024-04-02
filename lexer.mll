{
  (*open Lexing*)
  open Parser

  let string_buf = Buffer.create 256

  exception Error of char
}

let num = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | num | '-' | '_')*

rule token = parse
| eof             { Lend }
| [ ' ' '\t' ]    { token lexbuf }
| '\n'            { Lexing.new_line lexbuf; token lexbuf }
| "true"          { Lbool (true) }
| "false"         { Lbool (false) }
| '"'             { Buffer.clear string_buf; string lexbuf; Lstr (Buffer.contents string_buf) }
| "int"           { Ltype (Int_t) }
| "bool"          { Ltype (Bool_t) }
| "void"          { Ltype (Void_t) }
| "string"        { Ltype (Str_t) }
| "return"        { Lreturn }
| '('             { Lparen }
| ')'             { Rparen }
| ';'             { Lsc }
| "="             { Lassign }
| "+"             { Ladd }
| "-"             { Lsub }
| "*"             { Lmult }
| "/"             { Ldiv }
| "%"             { Lmod }
| "&&"            { Land }
| "||"            { Lor }
| ","             { Lvirgule }
| "=="            { Liseq }
| "!="            { Lnoteq }
| "!"             { Lnot }
| "^"             { Lxor }
| "{"             { Laccolad }
| "}"	            { Raccolad }
| ">"             { Lgt }
| ">>"            { Lshr }
| "<<"            { Lshl }
| "<"             { Llt }
| ">="            { Lge }
| "<="            { Lle }
| "if"            { Lif }
| "else"          { Lelse }
| "print"         { Lprint }
| "input"         { Linput }
| "while"         { Lwhile }
| num+ as n       { Lint (int_of_string n) }
| ident as i      { Lvar i }
| _ as c          { raise (Error c) }

and comment = parse
  | eof   { Lend }
  | '\n'  { Lexing.new_line lexbuf; token lexbuf }
  | _     { comment lexbuf }

and string = parse
| '"'     { () }
| _ as c  { Buffer.add_char string_buf c;
            string lexbuf }