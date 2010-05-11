open Camlp4.PreCast

module Jq_ast =
struct
  type loc = Loc.t

  type t =
    | Jq_null   of loc
    | Jq_bool   of loc * string
    | Jq_number of loc * string
    | Jq_string of loc * string
    | Jq_array  of loc * t
    | Jq_object of loc * t

    | Jq_colon  of loc * t * t
    | Jq_comma  of loc * t * t
    | Jq_nil    of loc
    | Jq_Ant    of loc * string
end

include Jq_ast

external loc_of_t : t -> Loc.t = "%field0"

let rec t_of_list = function
  | [] -> Jq_nil Loc.ghost
  | [e] -> e
  | e::es -> Jq_comma (loc_of_t e, e, t_of_list es)

let rec list_of_t x acc =
  match x with
    | Jq_nil _ -> acc
    | Jq_comma (_, e1, e2) -> list_of_t e1 (list_of_t e2 acc)
    | e -> e :: acc

module Meta =
struct

  (* I don't really understand what's going on here but this is how
     Camlp4Ast.mlast does it. *)

  module type META_LOC =
  sig
    val meta_loc_patt : Loc.t -> Loc.t -> Ast.patt
    val meta_loc_expr : Loc.t -> Loc.t -> Ast.expr
  end

  module MetaLoc =
  struct
    let meta_loc_patt _loc location =
      let (a, b, c, d, e, f, g, h) = Loc.to_tuple location in
      <:patt< Loc.of_tuple
        ($`str:a$, $`int:b$, $`int:c$, $`int:d$,
        $`int:e$, $`int:f$, $`int:g$,
        $if h then <:patt< True >> else <:patt< False >> $) >>
    let meta_loc_expr _loc location =
      let (a, b, c, d, e, f, g, h) = Loc.to_tuple location in
      <:expr< Loc.of_tuple
        ($`str:a$, $`int:b$, $`int:c$, $`int:d$,
        $`int:e$, $`int:f$, $`int:g$,
        $if h then <:expr< True >> else <:expr< False >> $) >>
  end

  module Make (MetaLoc : META_LOC) =
  struct
    module Expr =
    struct
      let meta_loc = MetaLoc.meta_loc_expr
      include Camlp4Filters.MetaGeneratorExpr(Jq_ast)
    end

    module Patt =
    struct
      let meta_loc = MetaLoc.meta_loc_patt
      include Camlp4Filters.MetaGeneratorPatt(Jq_ast)
    end
  end
end
