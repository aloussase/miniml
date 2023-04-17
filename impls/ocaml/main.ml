open Printf
open Lexer

let exec program =
  try
    begin
      let l = new lexer program in
      let tokens = l#lex in
      List.iter print_token tokens
    end
  with
  | Invalid_Token c -> printf "Invalid character in input: %c" c
  | Expected_Character c -> printf "Expected character: %c" c

let _ = exec "let inc = fun inc(x: int): int is x + 1;;"
