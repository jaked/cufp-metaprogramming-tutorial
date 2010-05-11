open Format
open Jq_ast

(* XXX figure out how Format works *)

module JSString =
struct

  (* UChar and UTF8 taken from camomile 0.7.2 *)
  module UChar : sig
    type t
    external uint_code : t -> int = "%identity"
    type uchar = t
    val of_int : int -> uchar
  end =
  struct
    (* $Id: uChar.ml,v 1.4 2004/09/04 16:07:38 yori Exp $ *)
    (* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
    
    type t = int

    external uint_code : t -> int = "%identity"

    let chr_of_uint n = 
      if n lsr 31 = 0 then n else 
      invalid_arg "UChar.char_of_uint"
      
    type uchar = t

    let of_int n = chr_of_uint n
  end

  module UTF8 : sig
    type t = string
    val iter : (UChar.t -> unit) -> t -> unit
  end =
  struct
    (* $Id: uTF8.ml,v 1.11 2004/09/04 16:07:38 yori Exp $ *)
    (* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

    type t = string

    let look s i =
      let n' =
        let n = Char.code s.[i] in
        if n < 0x80 then n else
        if n <= 0xdf then
          (n - 0xc0) lsl 6 lor (0x7f land (Char.code s.[i + 1]))
        else if n <= 0xef then
          let n' = n - 0xe0 in
          let m0 = Char.code s.[i + 2] in
          let m = Char.code (String.unsafe_get s (i + 1)) in
          let n' = n' lsl 6 lor (0x7f land m) in
          n' lsl 6 lor (0x7f land m0)
        else if n <= 0xf7 then
          let n' = n - 0xf0 in
          let m0 = Char.code s.[i + 3] in
          let m = Char.code (String.unsafe_get s (i + 1)) in
          let n' = n' lsl 6 lor (0x7f land m) in
          let m = Char.code (String.unsafe_get s (i + 2)) in
          let n' = n' lsl 6 lor (0x7f land m) in
          n' lsl 6 lor (0x7f land m0)     
        else if n <= 0xfb then
          let n' = n - 0xf8 in
          let m0 = Char.code s.[i + 4] in
          let m = Char.code (String.unsafe_get s (i + 1)) in
          let n' = n' lsl 6 lor (0x7f land m) in
          let m = Char.code (String.unsafe_get s (i + 2)) in
          let n' = n' lsl 6 lor (0x7f land m) in
          let m = Char.code (String.unsafe_get s (i + 3)) in
          let n' = n' lsl 6 lor (0x7f land m) in
          n' lsl 6 lor (0x7f land m0)     
        else if n <= 0xfd then
          let n' = n - 0xfc in
          let m0 = Char.code s.[i + 5] in
          let m = Char.code (String.unsafe_get s (i + 1)) in
          let n' = n' lsl 6 lor (0x7f land m) in
          let m = Char.code (String.unsafe_get s (i + 2)) in
          let n' = n' lsl 6 lor (0x7f land m) in
          let m = Char.code (String.unsafe_get s (i + 3)) in
          let n' = n' lsl 6 lor (0x7f land m) in
          let m = Char.code (String.unsafe_get s (i + 4)) in
          let n' = n' lsl 6 lor (0x7f land m) in
          n' lsl 6 lor (0x7f land m0)
        else invalid_arg "UTF8.look"
      in
      UChar.of_int n'
    
    let rec search_head s i =
      if i >= String.length s then i else
      let n = Char.code (String.unsafe_get s i) in
      if n < 0x80 || n >= 0xc2 then i else
      search_head s (i + 1)

    let next s i = 
      let n = Char.code s.[i] in
      if n < 0x80 then i + 1 else
      if n < 0xc0 then search_head s (i + 1) else
      if n <= 0xdf then i + 2
      else if n <= 0xef then i + 3
      else if n <= 0xf7 then i + 4
      else if n <= 0xfb then i + 5
      else if n <= 0xfd then i + 6
      else invalid_arg "UTF8.next"
    
    let rec iter_aux proc s i =
      if i >= String.length s then () else
      let u = look s i in
      proc u;
      iter_aux proc s (next s i)
    
    let iter proc s = iter_aux proc s 0
  end

  let sprint_uchar u =
    let n = UChar.uint_code u in
    let n2 = n land 0xffff in
    let n1 = n lsr 16 in
    if n1 = 0
    then Printf.sprintf "\\u%04X" n2
    else Printf.sprintf "\\U%04X%04X" n1 n2

  let escaped s =
    let buf = Buffer.create 0 in
    let proc u =
      let n = UChar.uint_code u in
      if n > 0x7f || n < 0
      then Buffer.add_string buf (sprint_uchar u)
      else if n = 39
      then Buffer.add_string buf "\\'"
      else Buffer.add_string buf (String.escaped (String.make 1 (Char.chr n))) in

    UTF8.iter proc s;
    Buffer.contents buf
end

let rec t ppf = function
  | Jq_null _  -> fprintf ppf "null"
  | Jq_bool (_, b) -> fprintf ppf "%s" b
  | Jq_number (_, n) -> fprintf ppf "%s" n
  | Jq_string (_, s) -> fprintf ppf "\"%s\"" (JSString.escaped s)

  | Jq_colon (_, t1, t2) -> fprintf ppf "@[<h>%a@ :@ %a@]" t t1 t t2

  | Jq_array (_, ts) -> fprintf ppf "@[<hv>[@;<1 2>%a@ ]@]" commas ts

  | Jq_object (_, ts) -> fprintf ppf "@[<hv>{@;<1 2>%a@ }@]" commas ts

  | Jq_Ant (_, s) -> fprintf ppf "$%s$" s

  | Jq_nil _ -> assert false
  | Jq_comma _ -> assert false

and commas ppf e =
  match e with
    | Jq_nil _ -> ()
    | Jq_comma (_, t1, t2) ->
        commas ppf t1;
        fprintf ppf ",@;<1 2>";
        commas ppf t2;
    | _ ->
        t ppf e

let escaped = JSString.escaped
