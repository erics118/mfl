open Ast

let rec string_of_typ = function
  | Bool -> "bool"
  | Void -> "void"
  | Char -> "char"
  | SChar -> "signed char"
  | UChar -> "unsigned char"
  | Short -> "short"
  | UShort -> "unsigned short"
  | Int -> "int"
  | UInt -> "unsigned int"
  | Long -> "long"
  | ULong -> "unsigned long"
  | LongLong -> "long long"
  | ULongLong -> "unsigned long long"
  | Ptr t -> string_of_typ t ^ "*"
  | Array (t, sz) -> string_of_typ t ^ "[" ^ string_of_int sz ^ "]"

(** true for any integer type, including Bool, excluding Void *)
let is_integer_type = function
  | Bool
  | Char
  | SChar
  | UChar
  | Short
  | UShort
  | Int
  | UInt
  | Long
  | ULong
  | LongLong
  | ULongLong -> true
  | Ptr _ -> false
  | Array (_, _) -> false
  | Void -> false

let is_pointer_type = function
  | Ptr _ -> true
  | _ -> false

(** is a pointer, integer, float, not array, struct, union *)
let is_scalar_type = function
  | Void -> false
  | t -> is_integer_type t || is_pointer_type t

(** width in bits of an integer type *)
let integer_width = function
  | Bool -> 1
  | Char | SChar | UChar -> 8
  | Short | UShort -> 16
  | Int | UInt -> 32
  | Long | ULong | LongLong | ULongLong -> 64
  | Ptr _ -> 64
  | Array (_, _) | Void -> 0

(** rank of integers, in order of priority when casting implicitly *)
let integer_rank = function
  | Bool -> 0
  | Char | SChar | UChar -> 1
  | Short | UShort -> 2
  | Int | UInt -> 3
  | Long | ULong -> 4
  | LongLong | ULongLong -> 5
  | Ptr _ | Array (_, _) | Void -> assert false

(** true for signed integer types *)
let is_signed_type = function
  | Char | SChar | Short | Int | Long | LongLong -> true
  | UChar | UShort | UInt | ULong | ULongLong | Bool | Ptr _
  | Array (_, _)
  | Void -> false

(** gets the unsigned version of a signed type *)
let unsigned_counterpart = function
  | Char -> UChar
  | SChar -> UChar
  | Short -> UShort
  | Int -> UInt
  | Long -> ULong
  | LongLong -> ULongLong
  (* these types don't change *)
  | (UChar | UShort | UInt | ULong | ULongLong) as t -> t
  (* these types don't have an unsigned counterpart *)
  | Bool | Ptr _ | Array (_, _) | Void -> invalid_arg "unsigned counterpart"

let expr_typ : checked expr -> typ = function
  | IntLiteral (ann, _)
  | BoolLiteral (ann, _)
  | CharLiteral (ann, _)
  | VarRef (ann, _)
  | BinaryOp (ann, _, _, _)
  | UnaryOp (ann, _, _)
  | Ternary (ann, _, _, _)
  | FuncCall (ann, _, _)
  | Assign (ann, _, _)
  | PreInc (ann, _)
  | PreDec (ann, _)
  | PostInc (ann, _)
  | PostDec (ann, _)
  | Cast (ann, _, _)
  | Subscript (ann, _, _)
  | ImplicitCast (ann, _, _) -> typ_of ann
