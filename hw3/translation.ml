open While
open M

let rec eval_a : While.aexp -> inst list -> inst list
=fun c s ->
	match c with
	| NUM i -> (Push i)::s
	| VAR v -> (Fetch v)::s
	| ADD (aexp1, aexp2) ->
		let first = (eval_a aexp1 s) in
			let second = (eval_a aexp2 first) in
				(Add::second)
	| SUB (aexp1, aexp2) -> 
		let first = (eval_a aexp1 s) in
			let second = (eval_a aexp2 first) in
				(Sub::second)
	| MUL (aexp1, aexp2) -> 
		let first = (eval_a aexp1 s) in
			let second = (eval_a aexp2 first) in
				(Mul::second)

let rec eval_b : While.bexp -> inst list -> inst list
=fun b s ->
	match b with
	| TRUE -> True::s
	| FALSE -> False::s
	| EQ (a1,a2) -> 
		let first = eval_a a1 s in
			let second = eval_b a2 first in
				(Eq)::second
	| LE (a1,a2) -> 
		let first = eval_a a1 s in
			let second = eval_b a2 first in
				(Le)::second
	| NEG bexp -> (Neg)::(eval_b bexp s)
	| AND (b1, b2) ->
		let first = eval_b b1 s in
			let second = eval_b b2 first in
				(And)::second



let rec eval_trans : stmt * inst list -> inst list
=fun (c, s) -> 
	match c with
	| ASSIGN (var, aexp) -> (Store var)::(eval_a aexp s)
	| SKIP -> Noop::s
	| SEQ (stmt1, stmt2) -> 
		let (c', s') = eval_trans (stmt1, s) in
			let (result_c, result_s) = eval_trans (stmt2, s') in
				result_s
	| IF (bexp, stmt1, stmt2) -> 
		let first = eval_b bexp s in
			let second_1 = eval_trans (stmt1, s) in 
				let second_2 = eval_trans (stmt2, s) in
					(Branch (second_1, second_2))::first
	| WHILE (bexp, stmt1) ->  
		let first = (eval_b bexp s) in
			let second = eval_trans stmt1 first in
				(Loop (second, s))::first
	| READ var -> Read::(Store var)::(Push (read_int ()))::s
	| PRINT aexp -> (Print)::(eval_a aexp s)


let trans : stmt -> inst list
= fun c ->
	eval_trans (c, [])