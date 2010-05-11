module Gram : Camlp4.Sig.Grammar.Static
  with module Loc = Jq_lexer.Loc
  and module Token = Jq_lexer.Token

val json : Jq_ast.t Gram.Entry.t
val parse_file : string -> Jq_ast.t
val parse_stdin : unit -> Jq_ast.t
val parse_string : string -> Jq_ast.t
