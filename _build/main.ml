open Ast

let is_value : expr -> bool = function
  | Int _ -> true
  | Add _ | Sub _ | Mult _ | Div _ | Lvalue _
  | Let _ | String _ | ID _ -> false

let rec subst e v x = match e with
  | Lvalue y      -> if x=y then v else e
  | Int n      -> Int n
  | Add(el,er) -> Add(subst el v x, subst er v x)
  | Sub(el,er) -> Sub(subst el v x, subst er v x)
  | Let(y,ebind,ebody) ->
      let ebind' = subst ebind v x in
      if x=y
      then Let(y, ebind', ebody)
      else Let(y, ebind', subst ebody v x)

let rec step : expr -> expr = function
  | Int n               -> failwith "No step"
  | Lvalue _               -> failwith "Unbound variable"
  | Add(Int n1, Int n2) -> Int (n1 + n2)
  | Add(Int n1, e2)     -> Add (Int n1, step e2)
  | Add(e1, e2)         -> Add (step e1, e2)
  | Sub(Int n1, Int n2) -> Int (n1 - n2)
  | Sub(Int n1, e2)     -> Sub (Int n1, step e2)
  | Sub(e1, e2)         -> Sub (step e1, e2)
  | Mult(Int n1, Int n2)-> Int (n1 * n2)
  | Mult(Int n1, e2)    -> Mult (Int n1, step e2)
  | Mult(e1, e2)        -> Mult (step e1, e2)
  | Div(Int n1, Int n2) -> Int (n1 * n2)
  | Div(Int n1, e2)     -> Div (Int n1, step e2)
  | Div(e1, e2)         -> Div (step e1, e2)
  | Let(x, Int n, e2)   -> subst e2 (Int n) x
  | Let(x, e1, e2)      -> Let (x, step e1, e2)

let rec eval : expr -> expr = fun e ->
    if is_value e then e
    else eval (step e)

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Tiger_parser.prog Tiger_lex.read lexbuf in
  ast

let get_value = function
  | Int i -> i
  | _ -> failwith "No value"

let interp (e:string) : int =
    e |> parse |> eval |> get_value
