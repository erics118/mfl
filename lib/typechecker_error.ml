open Ast
open Typechecker_types

type type_error =
  | UnknownType of string
  | UnboundVariable of string
  | UnboundFunction of string
  | MissingReturn of string
  | ArityMismatch of string * int * int (* name, expected, got *)
  | BinaryTypeMismatch of op * typ * typ
  | UnaryTypeMismatch of uop * typ
  | TypeMismatch of typ * typ (* expected, got *)
  | CondNotBool of typ
  | ReturnOutsideFunction
  | BreakOutsideLoop
  | ContinueOutsideLoop
  | NotLvalue
  | IncDecTypeMismatch of [ `Pre | `Post ] * [ `Inc | `Dec ] * typ
  | InvalidCast of typ * typ

exception Type_error of pos * type_error

let string_of_type_error = function
  | UnknownType name -> Printf.sprintf "unknown type '%s'" name
  | UnboundVariable x -> Printf.sprintf "unbound variable '%s'" x
  | UnboundFunction f -> Printf.sprintf "unbound function '%s'" f
  | MissingReturn f ->
      Printf.sprintf "control reaches end of non-void function '%s'" f
  | ArityMismatch (f, expected, got) ->
      Printf.sprintf "'%s' expects %d argument(s) but got %d" f expected got
  | BinaryTypeMismatch (op, lt, rt) ->
      Printf.sprintf "operator '%s': type mismatch between '%s' and '%s'"
        (string_of_op op) (string_of_typ lt) (string_of_typ rt)
  | UnaryTypeMismatch (op, t) ->
      Printf.sprintf "operator '%s': invalid operand type '%s'"
        (string_of_uop op) (string_of_typ t)
  | TypeMismatch (expected, got) ->
      Printf.sprintf "expected type '%s' but got '%s'" (string_of_typ expected)
        (string_of_typ got)
  | CondNotBool t ->
      Printf.sprintf "condition must be 'bool' but got '%s'" (string_of_typ t)
  | ReturnOutsideFunction -> "return statement outside of a function"
  | BreakOutsideLoop -> "break statement outside of a loop"
  | ContinueOutsideLoop -> "continue statement outside of a loop"
  | NotLvalue -> "expression is not an lvalue"
  | IncDecTypeMismatch (fix, dir, t) ->
      let fix_s =
        match fix with
        | `Pre -> "prefix"
        | `Post -> "postfix"
      in
      let dir_s =
        match dir with
        | `Inc -> "++"
        | `Dec -> "--"
      in
      Printf.sprintf "operator '%s %s': invalid operand type '%s'" fix_s dir_s
        (string_of_typ t)
  | InvalidCast (from_t, to_t) ->
      Printf.sprintf "cannot cast from '%s' to '%s'" (string_of_typ from_t)
        (string_of_typ to_t)
