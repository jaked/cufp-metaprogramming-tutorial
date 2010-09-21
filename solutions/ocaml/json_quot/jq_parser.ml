open Camlp4.PreCast
open Jq_ast

module Gram = MakeGram(Lexer)

let json_eoi = Gram.Entry.mk "json_eoi"

let parse_json_eoi loc s = Gram.parse_string json_eoi loc s

;;

EXTEND Gram
  GLOBAL: json_eoi;

  json: [[
      "null" -> Jq_null
    | "true" -> Jq_bool true
    | "false" -> Jq_bool false
    | i = INT -> Jq_number (float_of_string i)
    | f = FLOAT -> Jq_number (float_of_string f)
    | s = STRING -> Jq_string s

    | `ANTIQUOT (""|"bool"|"int"|"flo"|"str"|"list"|"alist" as n, s) ->
        Jq_Ant (_loc, n ^ ":" ^ s)

    | "["; es = SELF; "]" -> Jq_array es
    | "{"; kvs = SELF; "}" -> Jq_object kvs

    | e1 = SELF; ","; e2 = SELF -> Jq_comma (e1, e2)
    | -> Jq_nil

    | e1 = SELF; ":"; e2 = SELF -> Jq_colon (e1, e2)
  ]];

  json_eoi: [[ x = json; EOI -> x ]];
END
