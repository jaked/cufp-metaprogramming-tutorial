open Camlp4.PreCast
open Jq_ast

module Gram = MakeGram(Lexer)

let json = Gram.Entry.mk "json"

;;

EXTEND Gram
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
END
