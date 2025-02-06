{
module Parser = Expr_parser
exception SyntaxError of string
}

let white = [' ' '\t']+

let digit = ['0'-'9']
let int = digit+
let float = digit+ '.' digit* | digit* '.' digit+

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white   { read lexbuf }
  | int     { Parser.INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float   { Parser.FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | id      { Parser.ID (Lexing.lexeme lexbuf) }
  | '"'     { read_string (Buffer.create 16) lexbuf }
  | '['     { Parser.OPEN_BRACK }
  | ']'     { Parser.CLOSE_BRACK }
  | '+'     { Parser.PLUS }
  | '-'     { Parser.MINUS }
  | '('     { Parser.OPEN_PAREN }
  | ')'     { Parser.CLOSE_PAREN }
  | ':'     { Parser.COLON }
  | '?'     { Parser.Q_MARK }
  | '{'     { Parser.OPEN_BRACE }
  | '}'     { Parser.CLOSE_BRACE }
  | ','     { Parser.COMMA }
  | '#'     { Parser.HASH }
  | '|'     { Parser.PIPE }
  | _       { Parser.UNKNOWN_LEXEME (Lexing.lexeme lexbuf) }
  | eof     { Parser.EOF }

and read_string buf =
  parse
  | '"'       { Parser.LITERAL (Buffer.contents buf) }
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
