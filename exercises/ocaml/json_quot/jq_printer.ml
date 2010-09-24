open Format
open Jq_ast

let rec t_ ppf = function
  | Jq_null  -> fprintf ppf "null"
  | Jq_bool b -> fprintf ppf "%B" b
  | Jq_number n -> fprintf ppf "%g" n
  | Jq_string s -> fprintf ppf "%S" s

  | Jq_colon (t1, t2) -> fprintf ppf "@[<h>%a@ :@ %a@]" t t1 t t2

  | Jq_array t' -> fprintf ppf "@[<hv>[@;<1 2>%a@ ]@]" t t'
  | Jq_object t' -> fprintf ppf "@[<hv>{@;<1 2>%a@ }@]" t t'

  | Jq_Ant (_, s) -> fprintf ppf "$%s$" s

  | Jq_nil -> ()

  | Jq_comma (t', Jq_nil) -> t ppf t'
  | Jq_comma (t1, t2) -> fprintf ppf "%a,@;<1 2>%a" t t1 t t2

and t ppf t = t_ ppf (t_of_list (list_of_t t []))
