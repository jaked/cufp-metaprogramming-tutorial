open Camlp4.PreCast

module Q = Syntax.Quotation
module AQ = Syntax.AntiquotSyntax

let destruct_aq s =
  let pos = String.index s ':' in
  let len = String.length s in
  let name = String.sub s 0 pos
  and code = String.sub s (pos + 1) (len - pos - 1) in
  name, code

let aq_expander =
object
  inherit Ast.map as super
  method expr =
    function
      | Ast.ExAnt (_loc, s) ->
          let n, c = destruct_aq s in
          let e = AQ.parse_expr _loc c in
          begin match n with
            | "bool" -> <:expr< Jq_ast.Jq_bool $e$ >>
            | "int" -> <:expr< Jq_ast.Jq_number (float_of_int $e$) >>
            | "flo" -> <:expr< Jq_ast.Jq_number $e$ >>
            | "str" -> <:expr< Jq_ast.Jq_string $e$ >>
            | "list" -> <:expr< Jq_ast.t_of_list $e$ >>
            | "alist" -> <:expr< Jq_ast.t_of_list (List.map (fun (k, v) -> Jq_ast.Jq_colon (Jq_ast.Jq_string k, v)) $e$) >>
            | _ -> e
          end
      | e -> super#expr e
end

let parse_quot_string loc s =
  let q = !Camlp4_config.antiquotations in
  Camlp4_config.antiquotations := true;
  let res = Jq_parser.parse_json_eoi loc s in
  Camlp4_config.antiquotations := q;
  res

let expand_expr loc _ s =
  let ast = parse_quot_string loc s in
  let meta_ast = Jq_ast.meta_t loc ast in
  aq_expander#expr meta_ast

let expand_str_item loc _ s =
  let exp_ast = expand_expr loc None s in
  <:str_item@loc< $exp:exp_ast$ >>

;;

Q.add "json" Q.DynAst.expr_tag expand_expr;
Q.add "json" Q.DynAst.str_item_tag expand_str_item;
Q.default := "json"
