let rec interpret : Ast.expr -> Ast.expr = function
  | IntLiteral _ as n -> n
  | BoolLiteral _ as b -> b
  | UnaryOp (Neg, e) -> (
      match interpret e with
      | IntLiteral n -> IntLiteral (-n)
      | _ -> failwith "type error: Neg expects int")
  | UnaryOp (Not, e) -> (
      match interpret e with
      | BoolLiteral b -> BoolLiteral (not b)
      | _ -> failwith "type error: Not expects bool")
  | BinaryOp (op, l, r) -> interpret_binop op (interpret l) (interpret r)

and interpret_binop op l r =
  let int_op f a b =
    match f a b with
    | exception Division_by_zero -> failwith "div by zero"
    | result -> Ast.IntLiteral result
  in
  let cmp_op f a b = Ast.BoolLiteral (f a b) in
  match (op, l, r) with
  | Ast.Add, IntLiteral a, IntLiteral b -> int_op ( + ) a b
  | Ast.Sub, IntLiteral a, IntLiteral b -> int_op ( - ) a b
  | Ast.Mul, IntLiteral a, IntLiteral b -> int_op ( * ) a b
  | Ast.Div, IntLiteral a, IntLiteral b -> int_op ( / ) a b
  | Ast.Mod, IntLiteral a, IntLiteral b -> int_op ( mod ) a b
  | Ast.BitAnd, IntLiteral a, IntLiteral b -> int_op ( land ) a b
  | Ast.BitOr, IntLiteral a, IntLiteral b -> int_op ( lor ) a b
  | Ast.BitXor, IntLiteral a, IntLiteral b -> int_op ( lxor ) a b
  | Ast.Equal, IntLiteral a, IntLiteral b -> cmp_op ( = ) a b
  | Ast.Neq, IntLiteral a, IntLiteral b -> cmp_op ( <> ) a b
  | Ast.Less, IntLiteral a, IntLiteral b -> cmp_op ( < ) a b
  | Ast.Leq, IntLiteral a, IntLiteral b -> cmp_op ( <= ) a b
  | Ast.Greater, IntLiteral a, IntLiteral b -> cmp_op ( > ) a b
  | Ast.Geq, IntLiteral a, IntLiteral b -> cmp_op ( >= ) a b
  | Ast.And, BoolLiteral a, BoolLiteral b -> BoolLiteral (a && b)
  | Ast.Or, BoolLiteral a, BoolLiteral b -> BoolLiteral (a || b)
  | _ -> failwith "type error"
