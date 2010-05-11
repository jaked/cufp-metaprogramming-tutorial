open Camlp4.PreCast

type t =
  | Jq_null   of Loc.t
  | Jq_bool   of Loc.t * string
  | Jq_number of Loc.t * string
  | Jq_string of Loc.t * string
  | Jq_array  of Loc.t * t
  | Jq_object of Loc.t * t

  | Jq_colon  of Loc.t * t * t
  | Jq_comma  of Loc.t * t * t
  | Jq_nil    of Loc.t
  | Jq_Ant    of Loc.t * string

val t_of_list : t list -> t

module Meta :
  sig
    module type META_LOC =
      sig
        val meta_loc_patt : Loc.t -> Loc.t -> Ast.patt
        val meta_loc_expr : Loc.t -> Loc.t -> Ast.expr
      end
    module MetaLoc : META_LOC
    module Make :
      functor (MetaLoc : META_LOC) ->
        sig
          module Expr :
            sig
              val meta_t : Ast.loc -> t -> Ast.expr
            end
          module Patt :
            sig
              val meta_t : Ast.loc -> t -> Ast.patt
            end
        end
  end
