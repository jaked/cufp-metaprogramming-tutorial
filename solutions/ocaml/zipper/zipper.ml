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

  let arm tid a l =
    match a with
      | <:ctyp< $uid:_$ >> -> l
      | <:ctyp@_loc< $uid:cid$ of $parts$ >> ->
          let parts = Ast.list_of_ctyp parts [] in
          let tids =
            List.length
              (List.filter
                 (function <:ctyp< $lid:tid'$ >> when tid' = tid -> true | _ -> false)
                 parts) in
          let rec loop i =
            if i >= tids then l
            else
              let rec loop2 j = function
                | [] -> []
                | <:ctyp@_loc< $lid:tid'$ >> as h :: t when tid' = tid ->
                    let h = if j = i then <:ctyp< $lid:tid^"'"$ >> else h in
                    h :: loop2 (j+1) t
                | h::t -> h :: loop2 j t in
              <:ctyp< $uid:cid^string_of_int i$ of $list:loop2 0 parts$ >> :: loop (i+1) in
          loop 0
      | _ -> assert false

  let filter =
  object
    inherit Ast.map as super

    method str_item = function
      | <:str_item@_loc< type $lid:tid$ = [ $arms$ ] >> as si ->
          let arms = List.fold_right (arm tid) (Ast.list_of_ctyp arms []) [] in
          <:str_item<
            $si$;
            type $lid:tid^"'"$ = [ Top | $list:arms$ ]
          >>
      | si -> super#str_item si
  end

  let _ =
    AstFilters.register_str_item_filter filter#str_item
end

module M = Camlp4.Register.AstFilter(Id)(Make)
