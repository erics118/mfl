exception Type_error of string
exception Div_by_zero

let type_of = function
  | Ast.IntLiteral _ -> "int"
  | Ast.BoolLiteral _ -> "bool"
  | _ -> assert false

let type_error op operands =
  let plural = if List.length operands = 1 then "" else "s" in
  let typs = String.concat ", " operands in
  raise
    (Type_error
       (Printf.sprintf "%s: invalid operand type%s (%s)" op plural typs))

let rec interpret : Ast.expr -> Ast.expr = function
  | IntLiteral _ as n -> n
  | BoolLiteral _ as b -> b
  | UnaryOp (Neg, e) -> (
      match interpret e with
      | IntLiteral n -> IntLiteral (-n)
      | v -> type_error (Ast.string_of_uop Neg) [ type_of v ])
  | UnaryOp (Not, e) -> (
      match interpret e with
      | BoolLiteral b -> BoolLiteral (not b)
      | v -> type_error (Ast.string_of_uop Not) [ type_of v ])
  | BinaryOp (op, l, r) -> interpret_binary op l r
  | Statement e -> interpret e
  | EmptyStmt -> EmptyStmt
  | CompoundStmt statements ->
      let rec interpret_seq last_non_empty = function
        | [] -> last_non_empty
        | stmt :: rest ->
            let value = interpret stmt in
            let last_non_empty =
              match value with
              | EmptyStmt -> last_non_empty
              | _ -> value
            in
            interpret_seq last_non_empty rest
      in
      interpret_seq EmptyStmt statements
  | _ -> failwith "todo"

(* short circuiting *)
and interpret_binary op l r =
  match op with
  | Ast.And -> (
      match interpret l with
      | BoolLiteral false -> BoolLiteral false
      | l_val -> interpret_binop Ast.And l_val (interpret r))
  | Ast.Or -> (
      match interpret l with
      | BoolLiteral true -> BoolLiteral true
      | l_val -> interpret_binop Ast.Or l_val (interpret r))
  | _ -> interpret_binop op (interpret l) (interpret r)

(* actual evaluates the binary operators *)
and interpret_binop op l r =
  let te () = type_error (Ast.string_of_op op) [ type_of l; type_of r ] in
  match (l, r) with
  | IntLiteral a, IntLiteral b -> (
      let int_op f =
        match f a b with
        | exception Division_by_zero -> raise Div_by_zero
        | n -> Ast.IntLiteral n
      in
      let cmp_op f = Ast.BoolLiteral (f a b) in
      match op with
      | Ast.Add -> int_op ( + )
      | Ast.Sub -> int_op ( - )
      | Ast.Mul -> int_op ( * )
      | Ast.Div -> int_op ( / )
      | Ast.Mod -> int_op ( mod )
      | Ast.BitAnd -> int_op ( land )
      | Ast.BitOr -> int_op ( lor )
      | Ast.BitXor -> int_op ( lxor )
      | Ast.Equal -> cmp_op ( = )
      | Ast.Neq -> cmp_op ( <> )
      | Ast.Less -> cmp_op ( < )
      | Ast.Leq -> cmp_op ( <= )
      | Ast.Greater -> cmp_op ( > )
      | Ast.Geq -> cmp_op ( >= )
      | _ -> te ())
  | BoolLiteral a, BoolLiteral b -> (
      match op with
      | Ast.And -> BoolLiteral (a && b)
      | Ast.Or -> BoolLiteral (a || b)
      | Ast.Equal -> BoolLiteral (a == b)
      | Ast.Neq -> BoolLiteral (a <> b)
      | _ -> te ())
  | _ -> te ()
