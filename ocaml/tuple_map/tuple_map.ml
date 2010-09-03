module Make (AstFilters : Camlp4.Sig.AstFilters) =
struct
  open AstFilters (* for Ast module *)

  let filter =
    let map =
      object
        inherit Ast.map as super

        method expr e =
          match super#expr e with
            | <:expr@_loc< Tuple.map $f$ $tup:t$ >> ->
                let t = 
                  List.map
                    (fun e ->
                       let _loc = Ast.loc_of_expr e in
                       <:expr< $f$ $super#expr e$ >>)
                    (Ast.list_of_expr t []) in
                <:expr< $tup:Ast.exCom_of_list t$ >>
            | e -> e
      end in
    map#str_item

  let _ = AstFilters.register_str_item_filter filter
end

module Id = 
struct 
  let name = "tuple_map" 
  let version = "0.1" 
end 

module M = Camlp4.Register.AstFilter(Id)(Make)
