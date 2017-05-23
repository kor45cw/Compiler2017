open While
open M

let rec trans_a : aexp -> inst list
=fun c ->
	match c with
	| NUM i -> [Push i]
	| VAR v -> [Fetch v]
	| ADD (aexp1, aexp2) ->
		let first = trans_a aexp1 in
			let second = trans_a aexp2 in
				List.append second (List.append first [Add])
	| SUB (aexp1, aexp2) -> 
		let first = trans_a aexp1 in
			let second = trans_a aexp2 in
				List.append second (List.append first [Sub])
	| MUL (aexp1, aexp2) -> 
		let first = trans_a aexp1 in
			let second = trans_a aexp2 in
				List.append second (List.append first [Mul])


let rec trans_b : bexp -> inst list
=fun b ->
	match b with
	| TRUE -> [True]
	| FALSE -> [False]
	| EQ (a1, a2) ->
		let first = trans_a a1 in
			let second = trans_a a2 in
				List.append second (List.append first [Eq])
	| LE (a1, a2) ->
		let first = trans_a a1 in
			let second = trans_a a2 in
				List.append second (List.append first [Le])
	| AND (b1, b2) ->
		let first = trans_b b1 in
			let second = trans_b b2 in
				List.append second (List.append first [And])
	| NEG bexp -> List.append (trans_b bexp) [Neg]


let rec eval_trans : stmt -> inst list
=fun c -> 
	match c with
	| ASSIGN (var, aexp) -> List.append (trans_a aexp) [Store var]
	| SKIP -> [Noop]
	| SEQ (stmt1, stmt2) -> 
		let first = eval_trans stmt1 in
			let second = eval_trans stmt2 in
				List.append first second
	| IF (bexp, stmt1, stmt2) -> 
		let first = trans_b bexp in
			let second_1 = eval_trans stmt1 in 
				let second_2 = eval_trans stmt2 in
					List.append first [Branch (second_1, second_2)]
	| WHILE (bexp, stmt1) ->  
		let first = trans_b bexp in
			let second = eval_trans stmt1 in
				[Loop (first, second)]
	| READ var -> List.append [Read] [Store var]
	| PRINT aexp -> List.append (trans_a aexp) [Print]


let trans : stmt -> inst list
= fun c ->
	eval_trans c