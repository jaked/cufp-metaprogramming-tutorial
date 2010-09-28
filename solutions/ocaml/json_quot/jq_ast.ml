open Camlp4.PreCast (* for Ast refs in generated code *)

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

let rec meta_t _loc = function
  | Jq_null -> <:expr< Jq_ast.Jq_null >>
  | Jq_bool b -> <:expr< Jq_ast.Jq_bool $`bool:b$ >>
  | Jq_number f -> <:expr< Jq_ast.Jq_number $`flo:f$ >>
  | Jq_string s -> <:expr< Jq_ast.Jq_string $str:s$ >>
  | Jq_array t -> <:expr< Jq_ast.Jq_array $meta_t _loc t$ >>
  | Jq_object t -> <:expr< Jq_ast.Jq_object $meta_t _loc t$ >>

  | Jq_colon (t1, t2) -> <:expr< Jq_ast.Jq_colon ($meta_t _loc t1$, $meta_t _loc t2$) >>
  | Jq_comma (t1, t2) -> <:expr< Jq_ast.Jq_comma ($meta_t _loc t1$, $meta_t _loc t2$) >>
  | Jq_nil -> <:expr< Jq_nil >>

  | Jq_Ant (_loc, s) -> Ast.ExAnt (_loc, s)

let rec t_of_list = function
  | [] -> Jq_nil
  | [e] -> e
  | e::es -> Jq_comma (e, t_of_list es)

let rec list_of_t x acc =
  match x with
    | Jq_nil -> acc
    | Jq_comma (e1, e2) -> list_of_t e1 (list_of_t e2 acc)
    | e -> e :: acc
