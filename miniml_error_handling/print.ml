let ty t ppf =
  let rec ty ~max_level t ppf =
    if not (Format.over_max_boxes ()) then
      match t with
        | Syntax.TInt -> Zoo.print_parens ppf ~max_level ~at_level:0 "int"
        | Syntax.TBool -> Zoo.print_parens ppf ~max_level ~at_level:0 "bool"
        | Syntax.TArrow (t1, t2) ->
          Zoo.print_parens ppf ~max_level ~at_level:1 "%t ->@ %t" (ty ~max_level:0 t1) (ty ~max_level:1 t2)
  in
    ty ~max_level:1 t ppf

let mvalue m ppf =
  match m with
    | Machine.MInt k -> Zoo.print_parens ppf "%d" k
    | Machine.MBool b -> Zoo.print_parens ppf "%b" b
    | Machine.MClosure _ -> Zoo.print_parens ppf "<fun>"

let rec expr e ppf =
  match e.Zoo.data with
    | Syntax.Var x -> Format.fprintf ppf "%s" x
    | Syntax.Int k -> Format.fprintf ppf "%d" k
    | Syntax.Bool b -> Format.fprintf ppf "%b" b
    | Syntax.Times (e1, e2) -> Format.fprintf ppf "(%a * %a)" expr e1 expr e2
    | Syntax.Plus (e1, e2) -> Format.fprintf ppf "(%a + %a)" expr e1 expr e2
    | Syntax.Minus (e1, e2) -> Format.fprintf ppf "(%a - %a)" expr e1 expr e2
    | Syntax.Divide (e1, e2) -> Format.fprintf ppf "(%a / %a)" expr e1 expr e2
    | Syntax.Equal (e1, e2) -> Format.fprintf ppf "(%a = %a)" expr e1 expr e2
    | Syntax.Less (e1, e2) -> Format.fprintf ppf "(%a < %a)" expr e1 expr e2
    | Syntax.If (e1, e2, e3) -> Format.fprintf ppf "if %a then %a else %a" expr e1 expr e2 expr e3
    | Syntax.Fun (f, x, t1, t2, e) -> Format.fprintf ppf "fun %s(%s:%a):%a is %a" f x ty t1 ty t2 expr e
    | Syntax.Apply (e1, e2) -> Format.fprintf ppf "(%a %a)" expr e1 expr e2
    | Syntax.Raise e -> Format.fprintf ppf "raise %a" expr e
    | Syntax.TryWith (e1, e2) -> Format.fprintf ppf "try %a with %a" expr e1 expr e2
