module Gram : Camlp4.Sig.Grammar.Static
  with module Loc = Camlp4.PreCast.Loc
  and module Token = Jq_lexer.Token

val json : Jq_ast.t Gram.Entry.t
