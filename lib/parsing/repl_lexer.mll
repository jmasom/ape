{
open Repl_parser
exception SyntaxError of string
}

let white = [' ' '\t']+

let digit = ['0'-'9']
let int = digit+

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white   { read lexbuf }
  | int     { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | id      { ID (Lexing.lexeme lexbuf) }
  | '"'     { read_string (Buffer.create 16) lexbuf }
  | "->"    { ARROW }
  | '['     { OPEN_BRACK }
  | ']'     { CLOSE_BRACK }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '('     { OPEN_PAREN }
  | ')'     { CLOSE_PAREN }
  | '{'     { OPEN_BRACE }
  | '}'     { CLOSE_BRACE }
  | _       { raise (SyntaxError ("Illegal character: " ^ Lexing.lexeme lexbuf)) }
  | eof     { EOF }

and read_string buf =
  parse
  | '"'       { LITERAL (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
