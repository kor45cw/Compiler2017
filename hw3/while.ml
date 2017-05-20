(***********************************)
(*        Syntax of WHILE          *)
(***********************************)
type stmt = ASSIGN of var * aexp 
          | SKIP 
          | SEQ of stmt * stmt
          | IF of bexp * stmt * stmt 
          | WHILE of bexp * stmt
          | READ of var
          | PRINT of aexp 
and aexp  = NUM of int
          | VAR of var
          | ADD of aexp * aexp
          | SUB of aexp * aexp
          | MUL of aexp * aexp
and bexp  = TRUE | FALSE
          | EQ of aexp * aexp 
          | LE of aexp * aexp 
          | AND of bexp * bexp
          | NEG of bexp
and var = string

type program = stmt

(***********************************)
(* pretty printer for the langauge *)
(***********************************)
let p x = print_string (x)

let rec p_aexp e = 
  match e with
  | NUM i -> print_int i
  | VAR x -> print_string x
  | ADD (e1,e2) -> p_aexp e1; p "+"; p_aexp e2
  | SUB (e1,e2) -> p_aexp e1; p "-"; p_aexp e2
  | MUL (e1,e2) -> p_aexp e1; p "*"; p_aexp e2

and p_bexp e = 
  match e with
  | TRUE -> p "true"
  | FALSE -> p "false"
  | EQ (e1,e2) -> p_aexp e1; p "=="; p_aexp e2
  | LE (e1,e2) -> p_aexp e1; p "<="; p_aexp e2
  | NEG e -> p "!"; p_bexp e
  | AND (b1,b2) -> p_bexp b1; p"&&"; p_bexp b2

and p_stmt : stmt -> unit
=fun stmt -> 
  match stmt with
  | ASSIGN (x, exp) -> print_string x; p " = "; p_aexp exp 
  | SKIP -> p "skip"
  | SEQ (c1,c2) -> p_stmt c1; print_string "; "; p_stmt c2; 
  | IF (bexp,stmt1,stmt2) -> p "if ("; p_bexp bexp; p ") {"; p_stmt stmt1; p "} else {"; p_stmt stmt2; p "}"
  | WHILE (b, s) -> p "while ("; p_bexp b; p ") { "; p_stmt  s; p " }"
  | READ x -> p "read "; p x; p ""
  | PRINT e -> p "print "; p_aexp e; p ""

let pp : program -> unit
=fun p -> p_stmt p; print_endline ""

(***************************************************)
(*        Operational Semantics for WHILE          *)
(***************************************************)
type state = (var, int) PMap.t
let state_empty = PMap.empty
let state_lookup x s = PMap.find x s
let state_bind x v s = PMap.add x v s

let rec eval_a : aexp -> state -> int
=fun a s ->
  match a with
  | NUM n -> n
  | VAR x -> state_lookup x s
  | ADD (a1, a2) -> (eval_a a1 s) + (eval_a a2 s)
  | SUB (a1, a2) -> (eval_a a1 s) - (eval_a a2 s)
  | MUL (a1, a2) -> (eval_a a1 s) * (eval_a a2 s)

let rec eval_b : bexp -> state -> bool
=fun b s ->
  match b with
  | TRUE -> true
  | FALSE -> false
  | EQ (a1,a2) -> eval_a a1 s = eval_a a2 s
  | LE (a1,a2) -> eval_a a1 s <= eval_a a2 s
  | NEG b -> not (eval_b b s)
  | AND (b1,b2) -> (eval_b b1 s) && (eval_b b2 s)

let rec eval_stmt : stmt -> state -> state
=fun c s ->
  match c with
  | ASSIGN (x, exp) -> state_bind x (eval_a exp s) s
  | SKIP -> s
  | SEQ (c1, c2) -> eval_stmt c2 (eval_stmt c1 s)
  | IF (bexp, stmt1, stmt2) -> if eval_b bexp s then eval_stmt stmt1 s else eval_stmt stmt2 s
  | WHILE (b, c) -> if eval_b b s then eval_stmt (WHILE (b,c)) (eval_stmt c s) else s
  | READ x -> state_bind x (eval_a (NUM (read_int ())) s) s
  | PRINT e -> 
    (match e with
    | NUM n -> print_endline (string_of_int n); s
    | VAR x -> print_endline (string_of_int (state_lookup x s)); s
    | _ -> raise (Failure "print: not an integer"))

let run : stmt -> unit 
=fun pgm -> ignore (eval_stmt pgm state_empty)
