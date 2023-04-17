open Printf
open Lexer

let exec program =
  try
    begin
      let l = new lexer program in
      let tokens = l#lex in
      List.iter (fun tok -> print_endline tok.lexeme) tokens
    end
  with
  | Invalid_Token c -> printf "Invalid character in input: %c" c
  | Expected_Character c -> printf "Expected character: %c" c

let _ = exec "fun id(x: int): int is x;;"
