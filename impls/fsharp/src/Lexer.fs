module Lexer
    open System
    open Token

    let keywords = 
      Map.ofList [ 
        ("int", TT_tint)
        ("bool", TT_tbool)
        ("if", TT_if)
        ("then", TT_then)
        ("else", TT_else)
        ("true", TT_true)
        ("false", TT_false)
        ("fun", TT_fun)
        ("let", TT_let)
        ("is", TT_is) ]

    module Option =
      let toBoolean predicate = function
        | Some x -> predicate x
        | None   -> false

    exception InvalidCharacter of char
    exception ExpectedSemicolon 

    type Lexer = 
        { mutable Start   : int
          mutable Current : int
          mutable Line    : int
          Source  : string }

    let init source = 
        { Start   = 0
          Current = 0
          Line    = 1
          Source  = source }

    let advance lexer = 
      if lexer.Current < String.length lexer.Source then
        lexer.Current <- lexer.Current + 1;
        Some <| lexer.Source.[lexer.Current - 1]
      else
        None

    let peek lexer = 
      if lexer.Current < String.length lexer.Source then
        Some <| lexer.Source.[lexer.Current]
      else
        None

    let isAtEnd lexer =
      lexer.Current >= String.length lexer.Source

    module Char = 
      let isValidIdentifier = function
        | ')' | '(' | ':' | ';' | ' ' -> false
        | _                           -> true

    let mkToken lexer typ =
      { Type = typ
        Lexeme = lexer.Source.[lexer.Start .. lexer.Current - 1]
        Line = lexer.Line }

    let lexSemiSemi lexer = 
      if advance lexer = Some ';' then
        mkToken lexer TT_semisemi
      else
        raise ExpectedSemicolon

    let lexInteger lexer =
      while Option.toBoolean Char.IsDigit (peek lexer) do
        ignore <| advance lexer
      mkToken lexer TT_int

    let currentLexeme lexer =
      lexer.Source.[lexer.Start .. lexer.Current - 1]

    let maybe def mapper = function
      | Some x -> mapper x
      | _      -> def

    let lexIdentifierOrKeyword lexer =
      while Option.toBoolean Char.isValidIdentifier (peek lexer) do
        ignore <| advance lexer
      Map.tryFind (currentLexeme lexer) keywords
      |> maybe (mkToken lexer TT_ident) (fun typ -> mkToken lexer typ)

    let rec lex lexer =
      seq {
        if not (isAtEnd lexer) then
          lexer.Start <- lexer.Current
          match advance lexer with
          | Some '*' -> yield (mkToken lexer TT_times)
          | Some '+' -> yield (mkToken lexer TT_plus)
          | Some '-' -> yield (mkToken lexer TT_minus)
          | Some '<' -> yield (mkToken lexer TT_less)
          | Some '=' -> yield (mkToken lexer TT_equal)
          | Some ':' -> yield (mkToken lexer TT_colon)
          | Some '(' -> yield (mkToken lexer TT_lparen)
          | Some ')' -> yield (mkToken lexer TT_rparen)
          | Some ';' -> yield (lexSemiSemi lexer)
          | Some ' ' | Some '\t' -> yield! (lex lexer)
          | Some '\n' -> 
              lexer.Line <- lexer.Line + 1
              yield! (lex lexer)
          | Some c when Char.IsDigit c -> yield (lexInteger lexer)
          | Some _ -> yield (lexIdentifierOrKeyword lexer)
          | None -> yield (mkToken lexer TT_eof)
          yield! (lex lexer)
      }

