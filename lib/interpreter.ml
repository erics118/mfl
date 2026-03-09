let rec interpret : Ast.expr -> Ast.expr = function
  | IntLiteral _ as n -> n
  | BinaryOp (op, l, r) -> interpret_infix op l r
  | _ -> failwith "unimplemented"

and interpret_infix op l r =
  match (l, r) with
  | IntLiteral ll, IntLiteral rr -> begin
      match op with
      | Ast.Add -> IntLiteral (ll + rr)
      | Ast.Sub -> IntLiteral (ll - rr)
      | Ast.Mul -> IntLiteral (ll * rr)
      | Ast.Div -> IntLiteral (ll / rr)
      | _ -> failwith "unimplemented"
    end
  | _, _ ->
      (* one of the two is not a Integer, so continue interpreting it *)
      interpret_infix op (interpret l) (interpret r)
