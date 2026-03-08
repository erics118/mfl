type op =
  | Add
  | Sub
  | Mul
  | Div

type expr =
  | Number of float
  | Binary of op * expr * expr

let precedence = function
  | Add | Sub -> 10
  | Mul | Div -> 20

let char_of_op = function
  | Add -> '+'
  | Sub -> '-'
  | Mul -> '*'
  | Div -> '/'

let pp_number n = Printf.sprintf "%g" n

let rec pp_expr ?(parent_prec = 0) = function
  | Number n -> pp_number n
  | Binary (op, lhs, rhs) ->
      let current_prec = precedence op in
      let lhs_str = pp_expr ~parent_prec:current_prec lhs in
      let rhs_parent_prec =
        match op with
        | Sub | Div -> current_prec + 1
        | _ -> current_prec
      in
      let rhs_str = pp_expr ~parent_prec:rhs_parent_prec rhs in
      let rendered =
        Printf.sprintf "%s %c %s" lhs_str (char_of_op op) rhs_str
      in
      if current_prec < parent_prec then Printf.sprintf "(%s)" rendered
      else rendered
