module Make (AstFilters : Camlp4.Sig.AstFilters) =
struct
  open AstFilters (* for Ast module *)

  (* like Array.init *)
  let init n f =
    let rec loop k a =
      if k < 0
      then a
      else loop (k - 1) (f k :: a) in
    loop (n - 1) []

  let filter =
    let map =
      object
        inherit Ast.map as super

        method expr e =
          match super#expr e with

            | <:expr@_loc< Tuple.map $f$ $tup:t$ >> ->
                let t' = 
                  List.map
                    (fun e -> <:expr< $f$ $e$ >>)
                    (Ast.list_of_expr t []) in
                <:expr< $tup:Ast.exCom_of_list t'$ >>

            | <:expr@_loc< Tuple.map $int:n$ $f$ $t$ >> ->
                let n = int_of_string n in
                let ps = init n (fun i -> <:patt< $lid:"x"^string_of_int i$ >>) in
                let es = init n (fun i -> <:expr< $lid:"x"^string_of_int i$ >>) in
                let t' =
                  List.map
                    (fun e -> <:expr< $f$ $e$ >>)
                    es in
                <:expr<
                  let $tup:Ast.paCom_of_list ps$ = $t$ in
                  $tup:Ast.exCom_of_list t'$
                >>

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
