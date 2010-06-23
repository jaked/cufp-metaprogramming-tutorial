module Gram = Camlp4.Struct.Grammar.Static.Make(Jq_lexer)

open Jq_lexer
open Jq_ast

let mk_anti n s = "\\$"^n^":"^s

let json = Gram.Entry.mk "json"

;;

EXTEND Gram
  GLOBAL: json;

comma_list: [[
  `ANTIQUOT ("list" as n, s) -> Jq_Ant (_loc, mk_anti n s)
| e1 = SELF; ","; e2 = SELF -> Jq_comma (e1, e2)
| e = json -> e
| -> Jq_nil
]];

kv_comma_list: [[
  `ANTIQUOT ("list" as n, s) -> Jq_Ant (_loc, mk_anti n s)
| e1 = SELF; ","; e2 = SELF -> Jq_comma (e1, e2)
| e1 = json; ":"; e2 = json -> Jq_colon (e1, e2)
| -> Jq_nil
]];

json: [[
  `ANTIQUOT (""|"bool"|"int"|"flo"|"str" as n, s) -> Jq_Ant (_loc, mk_anti n s)
| n = NUMBER -> Jq_number n
| s = STRING -> Jq_string s
| "null" -> Jq_null
| "true" -> Jq_bool "true"
| "false" -> Jq_bool "false"
| "["; es = comma_list; "]" -> Jq_array es
| "{"; kvs = kv_comma_list; "}" -> Jq_object kvs
]];

END
