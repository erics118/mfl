open Token
open Ast

type state = {
  mutable cur_tok : token;
  lex : Lexer.state;
}

(** raised on parse errors *)
exception Parse_error of pos * string

let create lex_st = { cur_tok = TokEof; lex = lex_st }
let advance st = st.cur_tok <- Lexer.next_token st.lex

(* position of the current token *)
let cur_pos st = Lexer.tok_pos st.lex

let consume st tok =
  if st.cur_tok = tok then advance st
  else
    raise
      (Parse_error
         (cur_pos st, Printf.sprintf "expected '%s'" (string_of_token tok)))

let consume_identifier st =
  match st.cur_tok with
  | TokIdent name ->
      advance st;
      name
  | _ -> raise (Parse_error (cur_pos st, "expected identifier"))

let op_of_tok = function
  | TokPlus -> Some Add
  | TokMinus -> Some Sub
  | TokStar -> Some Mul
  | TokSlash -> Some Div
  | TokPercent -> Some Mod
  | TokEqEq -> Some Equal
  | TokBangEq -> Some Neq
  | TokLt -> Some Less
  | TokLtEq -> Some Leq
  | TokGt -> Some Greater
  | TokGtEq -> Some Geq
  | TokAmpAmp -> Some And
  | TokPipePipe -> Some Or
  | TokAmp -> Some BitAnd
  | TokPipe -> Some BitOr
  | TokCaret -> Some BitXor
  | TokLtLt -> Some LShift
  | TokGtGt -> Some RShift
  | _ -> None

let cur_precedence st =
  match op_of_tok st.cur_tok with
  | Some op -> precedence op
  | None -> -1

(* Parse a comma-separated list of items terminated by ')' *)
let parse_rparen_list st parse_item =
  match st.cur_tok with
  | TokRParen -> []
  | _ ->
      let rec loop rev_items =
        let item = parse_item st in
        match st.cur_tok with
        | TokComma ->
            consume st TokComma;
            loop (item :: rev_items)
        | TokRParen -> List.rev (item :: rev_items)
        | _ -> raise (Parse_error (cur_pos st, "expected ',' or ')'"))
      in
      loop []
