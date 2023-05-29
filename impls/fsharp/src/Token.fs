module Token

type TokenType =
    | TT_tint
    | TT_tbool
    | TT_colon
    | TT_int
    | TT_true
    | TT_false
    | TT_fun
    | TT_is
    | TT_if
    | TT_then
    | TT_else
    | TT_lparen
    | TT_rparen
    | TT_less
    | TT_equal
    | TT_plus
    | TT_minus
    | TT_times
    | TT_ident
    | TT_let
    | TT_semisemi
    | TT_eof

type Token =
    { Type: TokenType
      Lexeme: string
      Line: int }
