(* adapted from CDuce parser/ulexer.ml and camlp4/Camlp4/Struct/Lexer.mll *)

module Loc = Camlp4.PreCast.Loc

module Error =
struct
  type t = string
  exception E of string
  let print = Format.pp_print_string
  let to_string x = x
end
let _ = let module M = Camlp4.ErrorHandler.Register(Error) in ()

type token =
  | KEYWORD  of string
  | NUMBER   of string
  | STRING   of string
  | ANTIQUOT of string * string
  | EOI

module Token =
struct
  module Loc = Loc
  module Error = Error

  type t = token

  let to_string t =
    let sf = Printf.sprintf in
    match t with
      | KEYWORD s       -> sf "KEYWORD %S" s
      | NUMBER s        -> sf "NUMBER %s" s
      | STRING s        -> sf "STRING \"%s\"" s
      | ANTIQUOT (n, s) -> sf "ANTIQUOT %s: %S" n s
      | EOI             -> sf "EOI"

  let print ppf x = Format.pp_print_string ppf (to_string x)

  let match_keyword kwd =
    function
      | KEYWORD kwd' when kwd = kwd' -> true
      | _ -> false

  let extract_string =
    function
      | KEYWORD s | NUMBER s | STRING s -> s
      | tok ->
          invalid_arg
            ("Cannot extract a string from this token: " ^
               to_string tok)

  module Filter =
  struct
    type token_filter = (t, Loc.t) Camlp4.Sig.stream_filter
    type t = unit
    let mk _ = ()
    let filter _ strm = strm
    let define_filter _ _ = ()
    let keyword_added _ _ _ = ()
    let keyword_removed _ _ = ()
  end

end

module L = Ulexing

type context = {
  mutable loc : Loc.t;
  mutable start_loc : Loc.t option; (* if set, start lexeme here *)
  antiquots   : bool;
  lexbuf      : Ulexing.lexbuf;
  enc         : Ulexing.enc ref;
  buffer      : Buffer.t;
}

let current_loc c =
  let (fn, bl, bb, bo, el, eb, _, g) = Loc.to_tuple c.loc in
  let bl, bb, bo =
    match c.start_loc with
      | Some loc ->
          let (_, bl, bb, bo, _, _, _, _) = Loc.to_tuple loc in
          bl, bb, bo
      | None -> bl, bb, Ulexing.lexeme_start c.lexbuf in
  let eo = Ulexing.lexeme_end c.lexbuf in
  c.loc <- Loc.of_tuple (fn, bl, bb, bo, el, eb, eo, g);
  c.start_loc <- None;
  c.loc

let set_start_loc c =
  let (fn, bl, bb, bo, el, eb, eo, g) = Loc.to_tuple c.loc in
  let bo = Ulexing.lexeme_start c.lexbuf in
  let eo = Ulexing.lexeme_end c.lexbuf in
  c.start_loc <- Some (Loc.of_tuple (fn, bl, bb, bo, el, eb, eo, g))

let next_line c =
  let (fn, bl, bb, bo, el, eb, eo, g) = Loc.to_tuple c.loc in
  let bl = bl + 1 in
  let el = el + 1 in
  let bb = Ulexing.lexeme_end c.lexbuf in
  let eb = bb in
  c.loc <- Loc.of_tuple (fn, bl, bb, bo, el, eb, eo, g)

let error c s = Loc.raise (current_loc c) (Error.E s)

let regexp identinit =
  ['A'-'Z' 'a'-'z' '\192'-'\214' '\216'-'\246' '\248'-'\255' ]
let regexp identchar = (identinit | ['_' '\'' '0'-'9' ])
let regexp ident = identinit identchar*
let regexp hex = ['0'-'9''a'-'f''A'-'F']

let regexp newline = ('\010' | '\013' | "\013\010")
let regexp blank = [' ' '\009']

(* Buffer for string literals *)

let store_lexeme c =
  Buffer.add_string c.buffer (Ulexing.utf8_lexeme c.lexbuf)

let store_code c = Utf8.store c.buffer
let store_ascii c = Buffer.add_char c.buffer

let get_stored_string c =
  let s = Buffer.contents c.buffer in
  Buffer.reset c.buffer;
  s

(* Parse hex escapes *)

let hex_digit = function
  | '0'..'9' as c -> (Char.code c) - (Char.code '0')
  | 'a'..'f' as c -> (Char.code c) - (Char.code 'a') + 10
  | 'A'..'F' as c -> (Char.code c) - (Char.code 'A') + 10
  | _ -> -1

let parse_hex cx i =
  let s = L.latin1_sub_lexeme cx.lexbuf i (L.lexeme_length cx.lexbuf - i - 1) in
  let r = ref 0 in
  String.iter
    (fun c ->
       let c = hex_digit c in
       if c >= 16 || c < 0 then error cx "Invalid hex digit";
       r := !r * 16 + c)
    s;
  !r

let illegal c = error c "Illegal character"

let rec token c = lexer
  | eof -> EOI

  | newline -> next_line c; token c c.lexbuf
  | blank+ -> token c c.lexbuf

  | '-'? ['0'-'9']+ ('.' ['0'-'9']* )?
      (('e'|'E')('+'|'-')?(['0'-'9']+))? ->
        NUMBER (L.utf8_lexeme c.lexbuf)

  | [ "{}[]:," ] | "null" | "true" | "false" ->
      KEYWORD (L.utf8_lexeme c.lexbuf)

  | '"' ->
      set_start_loc c;
      string c c.lexbuf;
      STRING (get_stored_string c)

  | "$" ->
      (* XXX check antiquots *)
      set_start_loc c;
      c.enc := Ulexing.Latin1;
      let aq = antiquot c lexbuf in
      c.enc := Ulexing.Utf8;
      aq

  | _ -> illegal c

and string c = lexer
  | eof | newline -> error c "Unterminated string"

  | '"' -> ()

  | '\\' ['"' '\\' '/'] ->
      store_ascii c (Ulexing.latin1_lexeme_char c.lexbuf 1);
      string c c.lexbuf

  | "\\b" -> store_ascii c '\n'; string c c.lexbuf
  | "\\f" -> store_ascii c '\012'; string c c.lexbuf
  | "\\n" -> store_ascii c '\n'; string c c.lexbuf
  | "\\r" -> store_ascii c '\r'; string c c.lexbuf
  | "\\t" -> store_ascii c '\t'; string c c.lexbuf

  | '\\' 'u' hex hex hex hex ->
      store_code c (parse_hex c 2);
      string c c.lexbuf

  | '\\' -> illegal c

  | _ -> store_lexeme c; string c c.lexbuf

and antiquot c = lexer
  | eof -> error c "Unterminated antiquotation"

  | '$' -> ANTIQUOT ("", "")

  | ident ':' ->
      let name = Ulexing.latin1_sub_lexeme c.lexbuf 0 (Ulexing.lexeme_length c.lexbuf - 1) in
      antiquot_loop c c.lexbuf;
      ANTIQUOT (name, get_stored_string c)

  | newline ->
      next_line c;
      store_lexeme c;
      antiquot_loop c c.lexbuf;
      ANTIQUOT ("", get_stored_string c)

  | _ ->
      store_lexeme c;
      antiquot_loop c c.lexbuf;
      ANTIQUOT ("", get_stored_string c)

and antiquot_loop c = lexer
| eof -> error c "Unterminated antiquotation"

| '$' -> ()

| newline ->
    next_line c;
    store_lexeme c;
    antiquot_loop c c.lexbuf

| '<' (':' ident)? ('@' ident)? '<' ->
    store_lexeme c;
    (*
      XXX
      if the quotation is JSON we should really switch back to UTF8.
      since we just copy chars here I think the only problem is that
      the line numbers could get screwed up if \n appears as part of a
      code point.
    *)
    quotation c c.lexbuf;
    antiquot_loop c c.lexbuf

| _ ->
    store_lexeme c;
    antiquot_loop c c.lexbuf

and quotation c = lexer
| eof -> error c "Unterminated quotation"

| ">>" -> store_lexeme c

| newline ->
    c.loc <- Loc.move_line 1 c.loc;
    quotation c c.lexbuf

| _ ->
    store_lexeme c;
    quotation c c.lexbuf

let mk () start_loc cs =
  let enc = ref Ulexing.Utf8 in
  let lb = L.from_var_enc_stream enc cs in
  let c = {
    loc        = start_loc;
    start_loc  = None;
    antiquots  = !Camlp4_config.antiquotations;
    lexbuf     = lb;
    enc        = enc;
    buffer     = Buffer.create 256;
  } in
  let next _ =
    let tok =
      try token c c.lexbuf
      with
        | Ulexing.Error -> error c "Unexpected character"
        | Ulexing.InvalidCodepoint i -> error c "Code point invalid for the current encoding"
    in
    Some (tok, current_loc c)
  in
  Stream.from next
