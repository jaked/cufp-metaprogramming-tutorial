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

  let tapps t ts =
    List.fold_left
      (fun t t' -> <:ctyp< $t$ $t'$ >>)
      t
      ts

  let ctyp_eq t1 t2 =
    let strip_locs t = (Ast.map_loc (fun _ -> Ast.Loc.ghost))#ctyp t in
    strip_locs t1 = strip_locs t2

  let arm tid tvars a l =
    match a with
      | <:ctyp< $uid:_$ >> -> l
      | <:ctyp< $uid:cid$ of $parts$ >> ->
          let tdef = tapps <:ctyp< $lid:tid$ >> tvars in
          let tdef' = tapps <:ctyp< $lid:tid^"'"$ >> tvars in
          let parts = Ast.list_of_ctyp parts [] in
          let tdefs = List.length (List.filter (fun t -> ctyp_eq t tdef) parts) in
          let rec loop i =
            if i >= tdefs then l
            else
              let rec loop2 j = function
                | [] -> []
                | h::t when ctyp_eq h tdef ->
                    let h = if j = i then tdef' else h in
                    h :: loop2 (j+1) t
                | h::t -> h :: loop2 j t in
              <:ctyp< $uid:cid^string_of_int i$ of $list:loop2 0 parts$ >> :: loop (i+1) in
          loop 0
      | _ -> assert false

  let filter =
  object
    inherit Ast.map as super

    method str_item = function
      | <:str_item< type $Ast.TyDcl (_, tid, tvars, <:ctyp< [ $arms$ ] >>, [])$ >> as si ->
          let arms = List.fold_right (arm tid tvars) (Ast.list_of_ctyp arms []) [] in
          <:str_item<
            $si$;
            type $Ast.TyDcl (_loc, tid^"'", tvars, <:ctyp< [ Top | $list:arms$ ] >>, [])$
          >>
      | si -> super#str_item si
  end

  let _ =
    AstFilters.register_str_item_filter filter#str_item
end

module M = Camlp4.Register.AstFilter(Id)(Make)
