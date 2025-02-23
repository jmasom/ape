module type ParseableType = sig
  module Ast : sig
    type t
  end

  module Parser : sig
    type token

    exception Error

    val main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Ast.t
  end

  module Lexer : sig
    exception SyntaxError of string

    val read : Lexing.lexbuf -> Parser.token
  end
end

module Make (Input : ParseableType) = struct
  type ast = Input.Ast.t

  let parse s =
    try Ok (Input.Parser.main Input.Lexer.read (Lexing.from_string s)) with
    | Input.Lexer.SyntaxError s -> Error (`Lexer_error s)
    | Input.Parser.Error -> Error `Parser_error
end

module Expr = struct
  include Make (struct
    module Ast = struct
      type t = Types.expr_ast
    end

    module Lexer = Expr_lexer
    module Parser = Expr_parser
  end)
end

module Repl = struct
  include Make (struct
    module Ast = struct
      type t = Types.repl_ast
    end

    module Lexer = Repl_lexer
    module Parser = Repl_parser
  end)
end
