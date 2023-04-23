open Printf
open Lexer
open Parser
open Syntax

let exec program =
  try
    let stmts = parse program in
    List.iter (fun stmt -> print_endline @@ string_of_stmt stmt) stmts
  with
  | Invalid_Token c -> eprintf "Invalid character in input: %c" c
  | Expected_Character c -> eprintf "Expected character: %c" c
  | Expected_Token typ ->
      eprintf "Expected a token of type: %s" (show_token_type typ)
  | Parser_Exception err -> eprintf "Parser error: %s" err

let _ = exec "let max = if 2 < 7 then 2 + 3 else 7 * 3"
