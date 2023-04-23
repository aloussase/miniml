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

let _ = exec "let max = if 2 < 7 then 2 + 3 else 7 * 3;;"
