open Jq_ast
open Jq_lexer

module Gram = Camlp4.PreCast.MakeGram(Jq_lexer)

let json = Gram.Entry.mk "json"

;;

EXTEND Gram
  json: [[
      "null" -> Jq_null
    | "true" -> Jq_bool true
    | "false" -> Jq_bool false
    | n = NUMBER -> Jq_number (float_of_string n)
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
