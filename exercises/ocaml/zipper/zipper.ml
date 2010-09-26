open Camlp4

module Id : Sig.Id =
struct
  let name = "zipper"
  let version = "0.1"
end

let (|>) x f = f x

module Make (AstFilters : Sig.AstFilters) =
struct
  open AstFilters

  let _loc = Ast.Loc.ghost

  let filter =
  object
    inherit Ast.map as super

    method str_item = function
      | <:str_item< type $Ast.TyDcl (_, tid, tvars, <:ctyp< [ $arms$ ] >>, [])$ >> as si ->
          let si' = <:str_item< >> in
          (*
            TODO
            fill in si' with zipper type decl
          *)
          <:str_item<
            $si$;
            $si'$
          >>
      | si -> super#str_item si
  end

  let _ =
    AstFilters.register_str_item_filter filter#str_item
end

module M = Camlp4.Register.AstFilter(Id)(Make)
