let rec interpret : Ast.expr -> Ast.expr = function
  | Integer _ as n -> n
  | Binary (op, l, r) -> interpret_infix op l r

and interpret_infix op l r =
  match (l, r) with
  | Integer ll, Integer rr -> begin
      match op with
      | Ast.Add -> Integer (ll + rr)
      | Ast.Sub -> Integer (ll - rr)
      | Ast.Mul -> Integer (ll * rr)
      | Ast.Div -> Integer (ll / rr)
    end
  | _, _ ->
      (* one of the two is not a Integer, so continue interpreting it *)
      interpret_infix op (interpret l) (interpret r)
