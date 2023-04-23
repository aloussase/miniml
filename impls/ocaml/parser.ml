open Lexer
open Syntax

type parser = { mutable tokens : token list; mutable previous : token }

exception Expected_Token of token_type
exception Parser_Exception of string

let advance parser =
  match parser.tokens with
  | x :: xs ->
      parser.tokens <- xs;
      parser.previous <- x;
      Some x
  | _ -> None

let peek parser =
  match parser.tokens with
  | x :: _ -> Some x
  | _ -> None

let expect parser typ =
  match advance parser with
  | Some token when token.typ == typ -> token
  | _ -> raise @@ Expected_Token typ

let match_ parser typ =
  match parser.tokens with
  | x :: _ when x.typ == typ ->
      ignore @@ advance parser;
      true
  | _ -> false

let rec parse_factor parser =
  match advance parser with
  | Some { typ = TT_int; lexeme } -> EInt (int_of_string lexeme)
  | Some { typ = TT_ident; lexeme } -> EIdent lexeme
  | Some { typ = TT_lparen } ->
      let result = parse_expr parser in
      ignore @@ expect parser TT_rparen;
      result
  | _ ->
      raise
      @@ Parser_Exception
           "expected an int, and identifier or a parenthesized expression"

and parse_mult parser =
  let lhs = ref @@ parse_factor parser in
  while match_ parser TT_times do
    let rhs = parse_mult parser in
    lhs := ETimes (!lhs, rhs)
  done;
  !lhs

and parse_sum parser =
  let lhs = ref @@ parse_mult parser in
  while match_ parser TT_plus || match_ parser TT_minus do
    let previous = parser.previous in
    let rhs = parse_sum parser in
    lhs :=
      match previous.typ with
      | TT_minus -> EMinus (!lhs, rhs)
      | TT_plus -> EPlus (!lhs, rhs)
      | _ -> failwith "unexpected token in parse_sum"
  done;
  !lhs

and parse_less parser =
  let lhs = ref @@ parse_sum parser in
  while match_ parser TT_less do
    let rhs = parse_less parser in
    lhs := ELess (!lhs, rhs)
  done;
  !lhs

and parse_equal parser =
  let lhs = ref @@ parse_less parser in
  while match_ parser TT_equal do
    let rhs = parse_equal parser in
    lhs := EEqual (!lhs, rhs)
  done;
  !lhs

and parse_if parser =
  ignore @@ expect parser TT_if;
  let condition = parse_expr parser in
  ignore @@ expect parser TT_then;
  let then_branch = parse_expr parser in
  ignore @@ expect parser TT_else;
  let else_branch = parse_expr parser in
  EIf (condition, then_branch, else_branch)

and parse_expr parser =
  match Option.map (fun tok -> tok.typ) (peek parser) with
  | Some TT_if -> parse_if parser
  | _ -> parse_equal parser

and parse_let parser =
  ignore @@ expect parser TT_let;
  let ident = expect parser TT_ident in
  ignore @@ expect parser TT_equal;
  let expr = parse_expr parser in
  ignore @@ expect parser TT_semisemi;
  SLet (ident.lexeme, expr)

and parse_expr_stmt parser =
  let expr = parse_expr parser in
  ignore @@ expect parser TT_semisemi;
  SExpr expr

and parse_stmt parser =
  match Option.map (fun tok -> tok.typ) (peek parser) with
  | Some TT_let -> parse_let parser
  | _ -> parse_expr_stmt parser

let parse source =
  let l = new lexer source in
  let tokens = l#lex in
  let parser = { tokens; previous = List.hd tokens } in
  [ parse_stmt parser ]
