let rec interpret : Ast.expr -> Ast.expr = function
  | Number _ as n -> n
  | Binary (op, l, r) -> interpret_infix op l r

and interpret_infix op l r =
  match (l, r) with
  | Number ll, Number rr -> begin
      match op with
      | Ast.Add -> Number (ll +. rr)
      | Ast.Sub -> Number (ll -. rr)
      | Ast.Mul -> Number (ll *. rr)
      | Ast.Div -> Number (ll /. rr)
    end
  | _, _ ->
      (* one of the two is not a Number, so continue interpreting it *)
      interpret_infix op (interpret l) (interpret r)
