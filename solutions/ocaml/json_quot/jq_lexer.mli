(* adapted from CDuce parser/ulexer.mli *)

open Camlp4.Sig

type token =
    | KEYWORD  of string
    | NUMBER   of string
    | STRING   of string
    | ANTIQUOT of string * string
    | EOI

module Loc   : Loc with type t = Camlp4.PreCast.Loc.t
module Token : Token with module Loc = Loc and type t = token
module Error : Error

val mk : unit -> (Loc.t -> char Stream.t -> (Token.t * Loc.t) Stream.t)
