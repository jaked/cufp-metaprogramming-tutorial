open Camlp4.PreCast

type t =
  | Jq_null
  | Jq_bool   of bool
  | Jq_number of float
  | Jq_string of string
  | Jq_array  of t
  | Jq_object of t

  | Jq_colon  of t * t
  | Jq_comma  of t * t
  | Jq_nil

  | Jq_Ant    of Loc.t * string

module MetaExpr :
sig
  val meta_t : Ast.loc -> t -> Ast.expr
end

module MetaPatt :
sig
  val meta_t : Ast.loc -> t -> Ast.patt
end

val t_of_list : t list -> t
val list_of_t : t -> t list -> t list
