module Gram = Camlp4.Struct.Grammar.Static.Make(Jq_lexer)

open Jq_lexer
open Jq_ast

let mk_anti ?(c = "") n s = "\\$"^n^c^":"^s

let a_STRING = Gram.Entry.mk "a_STRING"
let a_NUM = Gram.Entry.mk "a_NUM"

let comma_list = Gram.Entry.mk "comma_list"
let kv_comma_list = Gram.Entry.mk "kv_comma_list"
let json = Gram.Entry.mk "json"

;;

EXTEND Gram

a_STRING: [[
  `ANTIQUOT (""|"str"|"`str" as n, s) -> mk_anti n s
| s = STRING -> s
]];

a_NUM: [[
  `ANTIQUOT (""|"int"|"`int" as n, s) -> mk_anti n s
| `ANTIQUOT (""|"flo"|"`flo" as n, s) -> mk_anti n s
| s = NUMBER -> s
]];

comma_list: [[
  `ANTIQUOT ("list" as n, s) -> Jq_Ant (_loc, mk_anti n s)
| e1 = SELF; ","; e2 = SELF -> Jq_comma (_loc, e1, e2)
| e = json -> e
| -> Jq_nil _loc
]];

kv_comma_list: [[
  `ANTIQUOT ("list" as n, s) -> Jq_Ant (_loc, mk_anti n s)
| e1 = SELF; ","; e2 = SELF -> Jq_comma (_loc, e1, e2)
| e1 = json; ":"; e2 = json -> Jq_colon (_loc, e1, e2)
| -> Jq_nil _loc
]];

json: [[
  `ANTIQUOT (""|"anti" as n, s) -> Jq_Ant (_loc, mk_anti n s)
| i = a_NUM -> Jq_number (_loc, i)
| s = a_STRING -> Jq_string (_loc, s)
| "null" -> Jq_null (_loc)
| "true" -> Jq_bool (_loc, "true")
| "false" -> Jq_bool (_loc, "false")
| "["; es = comma_list; "]" -> Jq_array (_loc, es)
| "{"; kvs = kv_comma_list; "}" -> Jq_object (_loc, kvs)
]];

END

let parse_file fn =
  let ch = open_in fn in
  Gram.parse json (Loc.mk fn) (Stream.of_channel ch)

let parse_stdin () =
  Gram.parse json (Loc.mk "<stdin>") (Stream.of_channel stdin)

let parse_string s =
  Gram.parse_string json (Loc.mk "<string>") s
