exception Invalid_Token of char
exception Expected_Character of char

type token_type =
  | TT_tint
  | TT_tbool
  | TT_colon (* types *)
  | TT_int
  | TT_true
  | TT_false (* literals *)
  | TT_fun
  | TT_is (* functions *)
  | TT_if
  | TT_then
  | TT_else (* if expressions *)
  | TT_lparen
  | TT_rparen (* parens *)
  | TT_less
  | TT_equal (* logic operators *)
  | TT_plus
  | TT_minus
  | TT_times (* arith operators *)
  | TT_ident
  | TT_semisemi
  | TT_eof

type token = { typ : token_type; lexeme : string; line : int; col : int }

class lexer (source : string) =
  object (self)
    val source = source

    val keywords =
      [
        ("int", TT_tint);
        ("bool", TT_tbool);
        ("true", TT_true);
        ("false", TT_false);
        ("fun", TT_fun);
        ("is", TT_is);
        ("if", TT_if);
        ("then", TT_then);
        ("else", TT_else);
      ]

    val mutable line = 1
    val mutable start = 0
    val mutable current = 0

    method lex =
      let rec f tokens =
        start <- current;
        match self#advance with
        | Some ':' -> f (self#token_at_current TT_colon :: tokens)
        | Some '(' -> f (self#token_at_current TT_lparen :: tokens)
        | Some ')' -> f (self#token_at_current TT_rparen :: tokens)
        | Some ';' -> begin
            match self#advance with
            | Some ';' -> self#token_at_current TT_semisemi :: tokens
            | _ -> raise (Expected_Character ';')
          end
        | Some '\n' ->
            line <- line + 1;
            f tokens
        | Some ' ' | Some '\t' -> f tokens
        | Some '0'
        | Some '1'
        | Some '2'
        | Some '3'
        | Some '4'
        | Some '5'
        | Some '6'
        | Some '7'
        | Some '8'
        | Some '9' -> begin
            failwith "TODO"
          end
        | Some c -> begin
            while
              Option.is_some self#peek
              && self#is_valid_identifier_char (Option.get self#peek)
            do
              ignore self#advance
            done;
            let lexeme = String.sub source start (current - start) in
            match List.assoc_opt lexeme keywords with
            | Some typ -> f (self#token_at_current typ :: tokens)
            | None -> f (self#token_at_current TT_ident :: tokens)
          end
        | None -> tokens
      in
      List.rev (f [])

    method is_valid_identifier_char c =
      match c with
      | ')' -> false
      | '(' -> false
      | ':' -> false
      | ';' -> false
      | ' ' -> false
      | _ -> true

    method token_at_current typ =
      let lexeme_len = current - start in
      let lexeme = String.sub source start lexeme_len in
      { typ; line; col = start; lexeme }

    method peek : char option =
      if current < String.length source then
        Some source.[current]
      else
        None

    method advance : char option =
      match self#peek with
      | Some c ->
          current <- current + 1;
          Some c
      | None -> None
  end
