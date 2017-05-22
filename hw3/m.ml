(**************************)
(*    syntax              *)
(**************************)
type inst = 
  | Push of int 
  | Add
  | Mul
  | Sub
  | True
  | False
  | Eq
  | Le
  | And
  | Neg
  | Fetch of var
  | Store of var 
  | Noop
  | Branch of cmd * cmd
  | Loop of cmd * cmd
  | Read
  | Print

and cmd = inst list
and var = string

(**************************)
(*    pretty printer      *)
(**************************)
let rec i2s inst =
  match inst with
  | Push n -> "push("^string_of_int n^")"
  | Add -> "add"
  | Mul -> "mul"
  | Sub -> "sub"
  | True -> "true"
  | False -> "false"
  | Eq -> "eq"
  | Le -> "le"
  | And -> "and"
  | Neg -> "neg"
  | Fetch x -> "fetch("^x^")"
  | Store x -> "store("^x^")"
  | Noop -> "noop"
  | Branch (c1,c2) -> "branch("^c2s c1^","^c2s c2^")"
  | Loop (c1,c2) -> "loop("^c2s c1^","^c2s c2^")"
  | Read -> "read"
  | Print -> "print"
and c2s cmd = List.fold_left (fun s i -> s ^ " " ^ i2s i) "" cmd
let pp cmd = print_endline (c2s cmd)

(**************************)
(*    semantics           *)
(**************************)
type value = Z of int | T of bool
type stack = value list
type state = (var, int) PMap.t

let state_empty = PMap.empty
let state_lookup x s = PMap.find x s
let state_bind x v s = PMap.add x v s

exception No_more_elements
exception Error of string

let value_int v = 
  match v with 
  | Z n -> n
  | T _ -> raise (Error "Bool type is used as Num type")

let value_bool v =
  match v with
  | T b -> b
  | Z _ -> raise (Error "Num type is used as Bool type")


let next : inst list * stack * state -> inst list * stack * state
=fun (c,e,s) -> 
  match c with 
  | [] -> (c, e, s)
  | hd::tl ->  
    match hd with
    | Push n -> (tl, (Z n)::e, s)
    | Add ->
        (match e with
        | [] -> raise No_more_elements
        | [a] -> raise No_more_elements
        | z1::z2::tl2 -> (tl, (Z ((value_int z1) + (value_int z2)))::tl2, s))
    | Mul -> 
        (match e with
        | [] -> raise No_more_elements
        | [a] -> raise No_more_elements
        | z1::z2::tl2 -> (tl, (Z ((value_int z1) * (value_int z2)))::tl2, s))
    | Sub -> 
        (match e with
        | [] -> raise No_more_elements
        | [a] -> raise No_more_elements
        | z1::z2::tl2 -> (tl, (Z ((value_int z1) - (value_int z2)))::tl2, s))
    | True -> (tl, (T true)::e, s)
    | False -> (tl, (T false)::e, s)
    | Eq -> 
        (match e with
        | [] -> raise No_more_elements
        | [a] -> raise No_more_elements
        | z1::z2::tl2 -> (tl, (T ((value_int z1) == (value_int z2)))::tl2, s))
    | Le ->
        (match e with
        | [] -> raise No_more_elements
        | [a] -> raise No_more_elements
        | z1::z2::tl2 -> (tl, (T ((value_int z1) <= (value_int z2)))::tl2, s))
    | And -> 
        (match e with
        | [] -> raise No_more_elements
        | [a] -> raise No_more_elements
        | t1::t2::tl2 -> (
          match (value_bool t1, value_bool t2) with
          | (true, true)  -> (tl, (T true)::tl2, s)
          | _ -> (tl, (T false)::tl2, s)
        ))
    | Neg -> 
        (match e with
        | [] -> raise No_more_elements
        | t1::tl2 -> (
          match (value_bool t1) with
          | true -> (tl, (T false)::tl2, s)
          | false -> (tl, (T true)::tl2, s)
        ))
    | Fetch x -> (tl, (Z (state_lookup x s))::e, s)
    | Store x -> 
      (match e with
        | [] -> raise No_more_elements
        | z::tl2 -> (tl, tl2, (state_bind x (value_int z) s))
      )
    | Noop -> (tl, e, s)
    | Branch (c1, c2) -> 
      (match e with
        | [] -> raise No_more_elements
        | t::tl2 -> (
          match (value_bool t) with
          | true -> ((List.append c1 tl), tl2, s)
          | false -> ((List.append c2 tl), tl2, s)
        )
      )
    | Loop (c1, c2) -> 
      let first = List.append c2 ((Loop(c1, c2))::[]) in
        let second = Branch(first, Noop::[]) in
          let third = List.append c1 (second::[]) in
            let final = List.append third tl in
              (final, e, s)
    | Read -> ([], e, s)
    | Print -> ([], e, s)



let run : cmd -> state
=fun c -> 
  let iconf = (c, [], state_empty) in
  let rec iter (c,e,s) = 
    match next (c,e,s) with
    | [], _, s  -> s
    | c', e',s' -> iter (c',e',s') in
    iter iconf
