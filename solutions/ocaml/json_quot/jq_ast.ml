open Camlp4.PreCast (* for Ast refs in generated code *)

module Jq_ast =
struct
  type float' = float

  type t =
    | Jq_null
    | Jq_bool   of bool
    | Jq_number of float'
    | Jq_string of string
    | Jq_array  of t
    | Jq_object of t

    | Jq_colon  of t * t
    | Jq_comma  of t * t
    | Jq_nil

    | Jq_Ant    of Loc.t * string
end

include Jq_ast

module MetaExpr =
struct
  let meta_float' _loc f = <:expr< $`flo:f$ >>
  include Camlp4Filters.MetaGeneratorExpr(Jq_ast)
end

module MetaPatt =
struct
  let meta_float' _loc f = <:patt< $`flo:f$ >>
  include Camlp4Filters.MetaGeneratorPatt(Jq_ast)
end

let rec t_of_list = function
  | [] -> Jq_nil
  | [e] -> e
  | e::es -> Jq_comma (e, t_of_list es)

let rec list_of_t x acc =
  match x with
    | Jq_nil -> acc
    | Jq_comma (e1, e2) -> list_of_t e1 (list_of_t e2 acc)
    | e -> e :: acc
