open Camlp4.PreCast

let is_true _loc j =
  <:expr<
    match $j$ with
      | Json_type.Null
      | Json_type.Bool false
      | Json_type.Int 0
      | Json_type.Float 0. -> false
      | _ -> true
  >>

let is_equal _loc j1 j2 =
  <:expr<
    match $j1$, $j2$ with
      | Json_type.Null, Json_type.Null -> true
      | Json_type.Bool b1, Json_type.Bool b2 -> b1 = b2
      | Json_type.Int i1, Json_type.Int i2 -> i1 = i2
      | Json_type.Float f1, Json_type.Float f2 -> f1 = f2
      | Json_type.String s1, Json_type.String s2 -> s1 = s2

      | Json_type.Float f1, Json_type.Int i2 -> f1 = float_of_int i2
      | Json_type.Int i1, Json_type.Float f2 -> float_of_int i1 = f2

      | _ -> false
  >>

let is_not_equal _loc j1 j2 = <:expr< not $is_equal _loc j1 j2$ >>

let rec list _loc = function
  | [] -> <:expr< [] >>
  | h :: t -> <:expr< $h$ :: $list _loc t$ >>

let expand_compr _loc e l =
  let rec ec = function
    | [] -> <:expr< let e = $e$ in fun a -> e :: a >>
    | `gen (p, l) :: t ->
        <:expr< List.fold_right (fun $p$ -> $ec t$) (Json_type.Browse.array $l$) >>
    | `cond b :: t ->
        <:expr< if $is_true _loc b$ then $ec t$ else fun a -> a >>
    | `bind (p, e') :: t ->
        <:expr< let $p$ = $e'$ in $ec t$ >> in
  <:expr< Json_type.Array ($ec l$ []) >>

module Gram = MakeGram(Lexer)

let compr = Gram.Entry.mk "compr"

EXTEND Gram
  GLOBAL: compr;

  expr: [
    LEFTA
    [ e1 = expr; "==="; e2 = expr -> <:expr< Json_type.Bool $is_equal _loc e1 e2$ >>
    | e1 = expr; "!=="; e2 = expr -> <:expr< Json_type.Bool $is_not_equal _loc e1 e2$ >>
    ]
  | [ "!"; e = expr -> <:expr< Json_type.Bool (not $is_true _loc e$) >> ]
  | LEFTA
    [ a = expr; "["; i = expr; "]" ->
        <:expr< List.nth (Json_type.Browse.array $a$) (Json_type.Browse.int $i$) >>
    | o = expr; "."; f = LIDENT ->
        <:expr< List.assoc $str:f$ (Json_type.Browse.objekt $o$) >>
    ]
  |
    [ i = INT -> <:expr< Json_type.Int $int: i$ >>
    | f = FLOAT -> <:expr< Json_type.Float $flo: f$ >>
    | s = STRING -> <:expr< Json_type.String $str: s$ >>
    | "null" -> <:expr< Json_type.Null >>
    | "false" -> <:expr< Json_type.Bool false >>
    | "true" -> <:expr< Json_type.Bool true >>
    | "<"; c = compr; ">" -> c
    | "["; es = LIST0 expr SEP ","; "]" ->
        <:expr< Json_type.Array $list _loc es$ >>
    | "{"; kvs = LIST0 [ k = STRING; ":"; v = expr -> <:expr< ($str: k$, $v$) >> ] SEP ","; "}" ->
        <:expr< Json_type.Object $list _loc kvs$ >>
    | id = LIDENT -> <:expr< $lid: id$ >>
    | `ANTIQUOT (n, aq) ->
        let aq = Syntax.AntiquotSyntax.parse_expr _loc aq in
        match n with
          | "" -> aq
          | "bool" -> <:expr< Json_type.Bool $aq$ >>
          | "int" -> <:expr< Json_type.Int $aq$ >>
          | "float" -> <:expr< Json_type.Float $aq$ >>
          | "string" -> <:expr< Json_type.String $aq$ >>
          | "array" -> <:expr< Json_type.Array $aq$ >>
          | "object" -> <:expr< Json_type.Object $aq$ >>
          | _ -> raise Stream.Failure
    ]
  ];

  item: [
    [ id = LIDENT; "<-"; e = expr -> `gen (<:patt< $lid: id$ >>, e)
    | "let"; id = LIDENT; "="; e = expr -> `bind (<:patt< $lid: id$>>, e)
    | e = expr -> `cond e
    ]
  ];

  compr: [
    [ e = expr; "|"; l = LIST1 item SEP "," -> expand_compr _loc e l ]
  ];
END

;;

Quotation.add "json_compr" Quotation.DynAst.expr_tag
  begin fun loc loc_name_opt s ->
    let q = !Camlp4_config.antiquotations in
    Camlp4_config.antiquotations := true;
    let res = Gram.parse_string compr loc s in
    Camlp4_config.antiquotations := q;
    res
  end
