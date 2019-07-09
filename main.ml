open Ast

type t =
  |TInt
  |TBool

module type Context = sig
  type tp
  val empty: tp
  val checkfor : tp -> string -> t
  val extend : tp -> string -> t -> tp
end

module Context : Context = struct
  type tp = (string * t) list
  let empty = []
  let checkfor cx x =
    try List.assoc x cx
    with Not_found -> failwith "Unbound var"
  let extend cx x z =
    (x, z) :: cx
end

open Context

let rec typeOf cx e =
  match e with
  | Int _ -> TInt
  | Bool _ -> TBool
  | Lvalue x -> checkfor cx x
  | Let (x, a, b ) -> typeOfLet cx x a b
  | Binop (bop, a, b ) -> typeOfBop cx bop a b

and typeOfLet cx x a b =
    let t1 = typeOf cx a in
    let cx' = extend cx x t1 in
    typeOf cx' b

and typeOfBop cx bop e1 e2 =
      let t1, t2 = typeOf cx e1, typeOf cx e2 in
      match bop, t1, t2 with
      | Add, TInt, TInt
      | Mult, TInt, TInt -> TInt
      | _ -> failwith "Type mismatch"

let typeCheck e =
  ignore (typeOf empty e)

let is_value : expr -> bool = function
  | Int _ | Bool _ | String _ -> true
  | Lvalue _ | Let _ | ID _ | Binop _  -> false

let rec subst e v x =
  match e with
  | Lvalue y      -> if x=y then v else e
  | Int _         -> e
  | Bool _        -> e
  | Binop (bop, e1, e2) ->
      Binop (bop, subst e1 v x, subst e2 v x)
  | Let(y,ebind,ebody) ->
      let ebind' = subst ebind v x in
      if x=y
      then Let(y, ebind', ebody)
      else Let(y, ebind', subst ebody v x)

let rec step : expr -> expr = function
  | Int n               -> failwith "No step"
  | Lvalue _            -> failwith "Unbound variable"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 ->
      step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 ->
      Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop (bop, step e1, e2)
  | Let(x, Int n, e2)   -> subst e2 (Int n) x
  | Let(x, e1, e2)      -> Let (x, step e1, e2)

  and step_bop bop e1 e2 = match bop, e1, e2 with
    | Add, Int a, Int b -> Int (a + b)
    | Mult, Int a, Int b -> Int (a * b)
    | _ -> failwith "Type mismatch"

let rec eval (e : expr) : expr = match e with
  | Int _ | Bool _ -> e
  | Lvalue _ -> failwith "Unbound variable"
  | Binop (bop, e1, e2) -> eval_bop bop e1 e2
  | Let (x, e1, e2) -> subst e2 (eval e1) x |> eval

and eval_bop bop e1 e2 = match bop, eval e1, eval e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | _ -> failwith "Type mismatch"

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let get_value = function
  | Int i -> i
  | _ -> failwith "No value"

let interp (s : string) : expr =
  let e = parse s in
  typeCheck e;
  eval e
