open Ast

let context = Llvm.global_context ()
let the_module = Llvm.create_module context "mfl"
let builder = Llvm.builder context

(* builtin types *)
let void_type = Llvm.void_type context
let bool_type = Llvm.i1_type context

(* bool should be an i8 in memory, but for computation it is a i1 *)
let bool_mem_type = Llvm.i8_type context
let char_type = Llvm.i8_type context
let short_type = Llvm.i16_type context
let int_type = Llvm.i32_type context
let long_type = Llvm.i64_type context
let float_type = Llvm.float_type context
let double_type = Llvm.double_type context
let pointer_type = Llvm.pointer_type context

(* maps struct tag to (llvm struct type, resolved field list) *)
let struct_defs : (string, Llvm.lltype * (string * typ) list) Hashtbl.t =
  Hashtbl.create 8

(** gets the size of a type in bytes *)
let rec sizeof_typ = function
  | Bool -> 1
  | Char | SChar | UChar -> 1
  | Short | UShort -> 2
  | Int | UInt -> 4
  | Long | ULong | LongLong | ULongLong -> 8
  | Float -> 4
  | Double -> 8
  | LongDouble -> 8
  | Ptr _ -> 8
  | Array (t, sz) -> sizeof_typ t * sz
  | Void -> 0
  | Struct tag -> begin
      (* sum of field sizes; does not account for alignment padding *)
      match Hashtbl.find_opt struct_defs tag with
      | None -> 0
      | Some (_, fields) ->
          List.fold_left (fun acc (_, ft) -> acc + sizeof_typ ft) 0 fields
    end

(* maps variable names to their alloca ptr within the current function *)
let locals : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 16

(* stack of (continue_bb, break_bb) for the each of the enclosing loops *)
let loop_stack : (Llvm.llbasicblock * Llvm.llbasicblock) Stack.t =
  Stack.create ()

(* true if the current block already ends with a terminator (ret, br, etc.) *)
let block_terminated () =
  Llvm.block_terminator (Llvm.insertion_block builder) <> None

(* emit an unconditional branch to bb only if the current block isn't already
   terminated *)
let br_if_open bb =
  if not (block_terminated ()) then ignore (Llvm.build_br bb builder)

(** zext i1 to i8 for storing a bool to memory *)
let bool_to_mem v name = Llvm.build_zext v bool_mem_type name builder

(** trunc i8 to i1 for loading a bool from memory *)
let bool_from_mem v name = Llvm.build_trunc v bool_type name builder

(* zext attribute for bool function params and ret values. zext i1 as necessary,
   matching the C ABI *)
let zext_attr = Llvm.create_enum_attr context "zeroext" 0L

let ensure_builtin_decl name =
  match Llvm.lookup_function name the_module with
  | Some fn -> fn
  | None -> begin
      match name with
      | "printint" ->
          let ty = Llvm.function_type void_type [| int_type |] in
          Llvm.declare_function name ty the_module
      | "printbool" ->
          let ty = Llvm.function_type void_type [| bool_type |] in
          let fn = Llvm.declare_function name ty the_module in
          Llvm.add_function_attr fn zext_attr (Llvm.AttrIndex.Param 0);
          fn
      | "malloc" ->
          let ty = Llvm.function_type pointer_type [| long_type |] in
          Llvm.declare_function name ty the_module
      | _ -> failwith ("undefined function: " ^ name)
    end

(* convert a typechecker typ to an llvm type *)
let rec llvm_of_typ = function
  | Bool -> bool_mem_type
  | Void -> void_type
  | Char | SChar | UChar -> char_type
  | Short | UShort -> short_type
  | Int | UInt -> int_type
  | Long | ULong | LongLong | ULongLong -> long_type
  | Float -> float_type
  | Double -> double_type
  | LongDouble -> double_type
  | Array (t, sz) -> Llvm.array_type (llvm_of_typ t) sz
  | Ptr _ -> pointer_type
  | Struct tag -> begin
      match Hashtbl.find_opt struct_defs tag with
      | Some (llty, _) -> llty
      | None -> failwith ("struct not defined: " ^ tag)
    end

(** load a value of type [t] from [ptr], returning the computation-ready value.
    handles any type-specific conversions, ie bool i8->i1 *)
let emit_load t ptr name =
  let v = Llvm.build_load (llvm_of_typ t) ptr name builder in
  if t = Bool then bool_from_mem v (name ^ "_b") else v

(** store a computation value of type [t] to [ptr]. handles any type-specific
    conversions, ie bool i1->i8 *)
let emit_store t v ptr =
  let v' = if t = Bool then bool_to_mem v "storeb" else v in
  ignore (Llvm.build_store v' ptr builder)

(* true for signed integer types *)
let is_signed = function
  | Char | SChar | Short | Int | Long | LongLong -> true
  | UChar | UShort | UInt | ULong | ULongLong -> false
  | Float | Double | LongDouble -> false
  | Bool | Ptr _ | Array (_, _) | Struct _ | Void -> false

(* extract the resolved type from a checked expression annotation *)
let expr_type : checked expr -> typ = function
  | IntLiteral (Checked (_, t), _, _)
  | FloatLiteral (Checked (_, t), _)
  | DoubleLiteral (Checked (_, t), _)
  | LongDoubleLiteral (Checked (_, t), _)
  | BoolLiteral (Checked (_, t), _)
  | CharLiteral (Checked (_, t), _)
  | StringLiteral (Checked (_, t), _)
  | VarRef (Checked (_, t), _)
  | BinaryOp (Checked (_, t), _, _, _)
  | UnaryOp (Checked (_, t), _, _)
  | Ternary (Checked (_, t), _, _, _)
  | FuncCall (Checked (_, t), _, _)
  | Assign (Checked (_, t), _, _)
  | PreInc (Checked (_, t), _)
  | PreDec (Checked (_, t), _)
  | PostInc (Checked (_, t), _)
  | PostDec (Checked (_, t), _)
  | Cast (Checked (_, t), _, _)
  | ImplicitCast (Checked (_, t), _, _)
  | SizeofExpr (Checked (_, t), _)
  | SizeofType (Checked (_, t), _)
  | Subscript (Checked (_, t), _, _)
  | MemberAccess (Checked (_, t), _, _) -> t
  | _ -> assert false [@coverage off]

let codegen_int n = Llvm.const_int int_type n
