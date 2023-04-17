open Lexer
open Syntax

type parser = { mutable current : int; mutable tokens : token list }

let parse_expr parser = [ SExpr (EInt 42) ]

let parse source =
  let l = new lexer source in
  let tokens = l#lex in
  let parser = { current = 0; tokens } in
  parse_expr parser
