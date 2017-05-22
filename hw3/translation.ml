open While
open M

let rec trans_a : aexp -> inst list -> inst list
=fun c s ->
	match c with
	| NUM i -> List.append s [Push i]
	| VAR v -> List.append s [Fetch v]
	| ADD (aexp1, aexp2) ->
		let first = (trans_a aexp1 s) in
			let second = (trans_a aexp2 first) in
				List.append second [Add]
	| SUB (aexp1, aexp2) -> 
		let first = (trans_a aexp1 s) in
			let second = (trans_a aexp2 first) in
				List.append second [Sub]
	| MUL (aexp1, aexp2) -> 
		let first = (trans_a aexp1 s) in
			let second = (trans_a aexp2 first) in
				List.append second [Mul]


let rec trans_b : bexp -> inst list -> inst list
=fun b s ->
	match b with
	| TRUE -> List.append s [True]
	| FALSE -> List.append s [False]
	| EQ (a1, a2) ->
		let first = trans_a a1 s in
			let second = trans_a a2 first in
				List.append second [Eq]
	| LE (a1, a2) ->
		let first = trans_a a1 s in
			let second = trans_a a2 first in
				List.append second [Le]
	| AND (b1, b2) ->
		let first = trans_b b1 s in
			let second = trans_b b2 first in
				List.append second [And]
	| NEG bexp -> List.append (trans_b bexp s) [Neg]


let rec eval_trans : stmt -> inst list -> inst list
=fun c s -> 
	match c with
	| ASSIGN (var, aexp) -> List.append (trans_a aexp s) [Store var]
	| SKIP -> List.append s [Noop]
	| SEQ (stmt1, stmt2) -> 
		let s' = eval_trans stmt1 s in
			let result_s = eval_trans stmt2 s' in
				result_s
	| IF (bexp, stmt1, stmt2) -> 
		let first = trans_b bexp s in
			let second_1 = eval_trans stmt1 s in 
				let second_2 = eval_trans stmt2 s in
					List.append first [Branch (second_1, second_2)]
	| WHILE (bexp, stmt1) ->  
		let first = trans_b bexp s in
			let second = eval_trans stmt1 first in
				(Loop (second, s))::first
	| READ var -> List.append s (Read::(Store var)::[])
	| PRINT aexp -> List.append (trans_a aexp s) [Print]


let trans : stmt -> inst list
= fun c ->
	eval_trans c []