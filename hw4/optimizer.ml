
let globalLabel = ref [] 


let eval_binary : int -> T.bop -> int -> int
=fun a op b ->
	match op with
	| T.ADD -> a + b
	| T.SUB -> a - b
	| T.MUL -> a * b
	| T.DIV -> a / b
	| T.LT -> if a < b then  1 else  0
	| T.LE -> if a <= b then  1 else  0
	| T.GT -> if a > b then  1 else  0
	| T.GE -> if a >= b then  1 else  0
	| T.EQ -> if a = b then  1 else  0
	| T.AND -> if a != 0 && b != 0 then 1 else 0
	| T.OR -> if a != 0 || b != 0 then  1 else  0

let eval_unary : T.uop -> int -> int
=fun op n ->
  match op with
  | T.MINUS -> (-n)
  | T.NOT -> if n = 0 then 1 else 0


let changeByFolding : T.instr list -> int -> T.instr -> T.instr list
=fun instr index inputInstr ->
	let temp = ref [] in 
	let count = ref 0 in 
	let present = ref false in 
	let _ = List.iter (fun instr -> 
		if !present then temp := List.append !temp [instr] else (
		count := !count + 1;
		if (!count - 1 <= index) then temp := List.append !temp [instr]
		else (
			match inputInstr with 
			| T.COPYC (x', n') ->
				(match instr with
				| T.ASSIGNV (x,o,y,z) -> 
					if (x == x') then (present := true;  temp := List.append !temp [instr];)
					else if (y == x') then temp := List.append !temp [(T.ASSIGNC (x, o, z, n'))] 
					else if (z == x') then temp := List.append !temp [(T.ASSIGNC (x, o, y, n'))] 
					else temp := List.append !temp [instr]
				| T.ASSIGNC (x,o,y,n) -> 
					if (x == x') then (present := true;  temp := List.append !temp [instr];)
					else if (y == x') then temp := List.append !temp [(T.COPYC (x, (eval_binary n' o n)))] 
					else temp := List.append !temp [instr]
				| T.ASSIGNU (x,o,y) -> 
					if (x == x') then (present := true;  temp := List.append !temp [instr];)
					else if (y == x') then temp := List.append !temp [(T.COPYC (x, (eval_unary o n')))] 
					else temp := List.append !temp [instr]
				| T.COPY (x,y) -> 
					if (y == x') then temp := List.append !temp [(T.COPYC (x, n'))] 
					else if (x == x') then (present := true;  temp := List.append !temp [instr];)
					else temp := List.append !temp [instr]
				| T.COPYC (x,n) -> if (x == x') then (present := true;  temp := List.append !temp [instr];) else temp := List.append !temp [instr]	
				| _ -> temp := List.append !temp [instr]
				)
			| T.COPY (x', y') -> 
				(match instr with 
				| T.ASSIGNV (x,o,y,z) -> 
					if (x == x') then (present := true;  temp := List.append !temp [instr];)
					else if (y == x') then temp := List.append !temp [(T.ASSIGNV (x, o, y', z))] 
					else if (z == x') then temp := List.append !temp [(T.ASSIGNV (x, o, y, y'))] 
					else temp := List.append !temp [instr]
				| T.ASSIGNC (x,o,y,n) -> 
					if (x == x') then (present := true;  temp := List.append !temp [instr];)
					else if (y == x') then temp := List.append !temp [(T.ASSIGNC (x, o, y', n))] 
					else temp := List.append !temp [instr]
				| T.ASSIGNU (x,o,y) -> if (x == x') then (present := true;  temp := List.append !temp [instr];) else temp := List.append !temp [instr]
				| T.COPY (x,y) -> 
					if (x == x') then (present := true;  temp := List.append !temp [instr];)
					else if (y == x') then temp := List.append !temp [(T.COPY (x, y'))] 
					else temp := List.append !temp [instr]
				| T.COPYC (x,n) -> if (x == x') then (present := true;  temp := List.append !temp [instr];) else temp := List.append !temp [instr]	
				| _ -> temp := List.append !temp [instr]
				)
			| _ -> temp := List.append !temp [instr]
		))
	) instr in
		!temp

let constantFolding : T.instr list -> T.instr list
=fun instr ->
	let temp = ref [] in
    	temp := instr;
    let count = ref 0 in
    let _ = List.iter (fun instr ->
    	(match instr with 
    	| T.COPYC (x, n) -> temp := (changeByFolding !temp !count instr)
    	| T.COPY (x, y) -> temp := (changeByFolding !temp !count instr)
    	| _ -> ()
    	);
    	count := !count + 1;
	) !temp in
		!temp

let checkDeadCodeNeed : T.program -> int -> T.var -> bool
=fun instr inputLabel inputX ->
	let delete = ref true in 
	let finish = ref false in 
	let _ = List.iter (fun (label, instr) ->
		if label = inputLabel then finish := true;
		if !finish then () else (
		match instr with 
		| T.ALLOC (x, n) -> if (x = inputX) then delete := false
		| T.ASSIGNV (x, o, y, z) -> if (x = inputX) then delete := false
		| T.ASSIGNC (x, o, y, n) -> if (x = inputX) then delete := false
		| T.ASSIGNU (x, o, y) -> if (x = inputX) then delete := false
		| T.COPY (x, y) -> if (x = inputX) then delete := false
		| T.COPYC (x, n) -> if (x = inputX) then delete := false
		| T.LOAD (x, (pre, index)) -> 
			if (x = inputX) then delete := false
			else if (pre = inputX) then delete := false
			else if (index = inputX) then delete := false
		| T.STORE ((pre, index), x) -> 
			if (x = inputX) then delete := false
			else if (pre = inputX) then delete := false
			else if (index = inputX) then delete := false
		| T.READ x -> if (x = inputX) then delete := false
		| T.WRITE x -> if (x = inputX) then delete := false
		| T.CJUMP (x, l) -> finish := true
		| T.CJUMPF (x, l) -> finish := true
		| _ -> ()
		)
	) instr in 
		!delete

let checkDeadcode : T.program -> int -> int -> T.var -> bool
=fun inputInstr index passingLabel inputX ->
	let count = ref 0 in 
	let present = ref false in
	let checkDead = ref true in 
	let _ = List.iter (fun (label, instr) -> 
		if !present then () else (
		count := !count + 1;
		if (!count - 1 > index) then (
			match instr with
			| T.ALLOC (x, n) -> 
				if (x = inputX) then present := true
			| T.ASSIGNV (x, o, y, z) -> 
				if (x = inputX) then present := true
				else if (y = inputX) then (checkDead := false; present := true)
				else if (z = inputX) then (checkDead := false; present := true)
			| T.ASSIGNC (x, o, y, n) -> 
				if (x = inputX) then present := true
				else if (y = inputX) then (checkDead := false; present := true)
			| T.ASSIGNU (x, o, y) -> 
				if (x = inputX) then present := true
				else if (y = inputX) then (checkDead := false; present := true)
			| T.COPY (x, y) -> 
				if (x = inputX) then present := true
				else if (y = inputX) then (checkDead := false; present := true)
			| T.COPYC (x, n) -> 
				if (x = inputX) then present := true
			| T.CJUMP (x, l) -> 
				if (x = inputX) then (checkDead := false; present := true)
			| T.CJUMPF (x, l) -> 
				if (x = inputX) then (checkDead := false; present := true)
			| T.LOAD (x, (pre, index)) -> 
				if (x = inputX) then (checkDead := false; present := true)
				else if (pre = inputX) then (checkDead := false; present := true)
				else if (index = inputX) then (checkDead := false; present := true)
			| T.STORE ((pre, index), x) -> 
				if (x = inputX) then (checkDead := false; present := true)
				else if (pre = inputX) then (checkDead := false; present := true)
				else if (index = inputX) then (checkDead := false; present := true)
			| T.READ x -> 
				if (x = inputX) then (checkDead := false; present := true)
			| T.WRITE x -> 
				if (x = inputX) then (checkDead := false; present := true)
			| T.UJUMP label -> 
				if (passingLabel != 0) then checkDead := checkDeadCodeNeed inputInstr label inputX
			| _ -> ()
		))
	) inputInstr in 
		!checkDead

let deadcodeElimination : T.program -> T.program
=fun inputInstr -> 
	let temp = ref [] in
    let count = ref 0 in
    let tempLabel = ref 0 in 
	let passingLabel = ref 0 in 
    let _ = List.iter (fun (label, instr) ->
    	if (label != 0) then (passingLabel := label);
    	(match instr with 
    	| T.ALLOC (x, n) -> 
    		if (checkDeadcode inputInstr !count !passingLabel x) then (
    			if label != 0 then (tempLabel := label; ())
	    		else ()
    		)
    		else (
    			if !tempLabel != 0 then (temp := List.append !temp [(!tempLabel, instr)]; tempLabel := 0)
	    		else temp := List.append !temp [(label, instr)]
    		)
    	| T.ASSIGNV (x,o,y,z) -> 
    		if (checkDeadcode inputInstr !count !passingLabel x) then (
    			if label != 0 then (tempLabel := label; ())
	    		else ()
    		)
    		else (
    			if !tempLabel != 0 then (temp := List.append !temp [(!tempLabel, instr)]; tempLabel := 0)
	    		else temp := List.append !temp [(label, instr)]
    		)
    	| T.ASSIGNC (x,o,y,n) -> 
    		if (checkDeadcode inputInstr !count !passingLabel x) then (
    			if label != 0 then (tempLabel := label; ())
	    		else ()
    		)
    		else (
    			if !tempLabel != 0 then (temp := List.append !temp [(!tempLabel, instr)]; tempLabel := 0)
	    		else temp := List.append !temp [(label, instr)]
    		)
    	| T.ASSIGNU (x,o,y) -> 
    		if (checkDeadcode inputInstr !count !passingLabel x) then (
    			if label != 0 then (tempLabel := label; ())
	    		else ()
    		)
    		else (
    			if !tempLabel != 0 then (temp := List.append !temp [(!tempLabel, instr)]; tempLabel := 0)
	    		else temp := List.append !temp [(label, instr)]
    		)
    	| T.COPY (x, y) -> 
    		if (checkDeadcode inputInstr !count !passingLabel x) then (
    			if label != 0 then (tempLabel := label; ())
	    		else ()
    		)
    		else (
    			if !tempLabel != 0 then (temp := List.append !temp [(!tempLabel, instr)]; tempLabel := 0)
	    		else temp := List.append !temp [(label, instr)]
    		)
    	| T.COPYC (x, n) -> 
    		if (checkDeadcode inputInstr !count !passingLabel x) then (
    			if label != 0 then (tempLabel := label; ())
	    		else ()
    		)
    		else (
    			if !tempLabel != 0 then (temp := List.append !temp [(!tempLabel, instr)]; tempLabel := 0)
	    		else temp := List.append !temp [(label, instr)]
    		)
    	| _ -> (
    			if !tempLabel != 0 then (temp := List.append !temp [(!tempLabel, instr)]; tempLabel := 0)
	    		else temp := List.append !temp [(label, instr)]
    		)
    	);
    	count := !count + 1;
	) inputInstr in
		!temp


let changeByPropagation : T.instr list -> int -> T.instr -> T.instr list
=fun instr index inputInstr ->
	let temp = ref [] in 
	let count = ref 0 in 
	let present = ref false in 
	let _ = List.iter (fun instr -> 
		if !present then temp := List.append !temp [instr] else (
		count := !count + 1;
		if (!count - 1 <= index) then temp := List.append !temp [instr]
		else (
			match inputInstr with
			| T.COPY (x', y') -> 
				(match instr with
				| T.ASSIGNV (x, o, y, z) ->
					if (x = x') then (present := true;  temp := List.append !temp [instr];)
					else if (y = x' && z = x') then temp := List.append !temp  [T.ASSIGNV (x, o, y', y')]
					else if (y = x') then temp := List.append !temp [T.ASSIGNV (x, o, y', z)]
					else if (z = x') then temp := List.append !temp [T.ASSIGNV (x, o, y, y')]
					else temp := List.append !temp [instr]
				| T.ASSIGNC (x, o, y, n) ->
					if (x = x') then (present := true;  temp := List.append !temp [instr];)
					else if (y = x') then temp := List.append !temp [T.ASSIGNC (x, o, y', n)]
					else temp := List.append !temp [instr]
				| T.ASSIGNU (x, o, y) -> 
					if (x = x') then (present := true;  temp := List.append !temp [instr];)
					else if (y = x') then temp := List.append !temp [T.ASSIGNU (x, o, y')]
					else temp := List.append !temp [instr]
				| T.COPY (x, y) -> 
					if (x = x') then (present := true;  temp := List.append !temp [instr];)
					else if (y = x') then temp := List.append !temp [T.COPY (x, y')]
					else temp := List.append !temp [instr]
				| T.COPYC (x, n) -> if (x = x') then (present := true;  temp := List.append !temp [instr];) else temp := List.append !temp [instr]
				| T.CJUMP (x, l) ->  if (x = x') then temp := List.append !temp [T.CJUMP (y', l)] else temp := List.append !temp [instr]
				| T.CJUMPF (x, l) -> if (x = x') then temp := List.append !temp [T.CJUMPF (y', l)] else temp := List.append !temp [instr]
				| T.STORE ((pre, index), x) -> 
					if (x = x') then temp := List.append !temp [T.STORE ((pre, index), y')]
					else if (pre = x') then temp := List.append !temp [T.STORE ((y', index), x)]
					else if (index = x') then temp := List.append !temp [T.STORE ((pre, y'), x)]
					else temp := List.append !temp [instr]
				| T.LOAD (x, (pre, index)) ->
					if (x = x') then temp := List.append !temp [T.LOAD (y', (pre, index))]
					else if (pre = x') then temp := List.append !temp [T.LOAD (x, (y', index))]
					else if (index = x') then temp := List.append !temp [T.LOAD (x, (pre, y'))]
					else temp := List.append !temp [instr]
				| T.READ x -> if (x = x') then temp := List.append !temp [T.READ y'] else temp := List.append !temp [instr]
				| T.WRITE x -> if (x = x') then temp := List.append !temp [T.WRITE y'] else temp := List.append !temp [instr]
				| _ -> temp := List.append !temp [instr]
				)
			| T.COPYC (x', n') -> 
				(match instr with
				| T.ASSIGNV (x, o, y, z) ->
					if (x = x') then (present := true;  temp := List.append !temp [instr];)
					else if (y = x' && z = x') then temp := List.append !temp [T.COPYC (x, eval_binary n' o n')]
					else if (y = x') then temp := List.append !temp [T.ASSIGNC (x, o, z, n')]
					else if (z = x') then temp := List.append !temp [T.ASSIGNC (x, o, y, n')]
					else temp := List.append !temp [instr]
				| T.ASSIGNC (x, o, y, n) ->
					if (x = x') then (present := true;  temp := List.append !temp [instr];)
					else if (y = x') then temp := List.append !temp [T.COPYC (x, (eval_binary n o n'))]
					else temp := List.append !temp [instr]
				| T.ASSIGNU (x, o, y) -> 
					if (x = x') then (present := true;  temp := List.append !temp [instr];)
					else if (y = x') then temp := List.append !temp [T.COPYC (x, (eval_unary o n'))]
					else temp := List.append !temp [instr]
				| T.COPY (x, y) -> 
					if (x = x') then (present := true;  temp := List.append !temp [instr];)
					else if (y = x') then temp := List.append !temp [T.COPYC (x, n')]
					else temp := List.append !temp [instr]
				| T.COPYC (x, n) -> 
					if (x = x') then (present := true;  temp := List.append !temp [instr];) 
					else temp := List.append !temp [instr]
				| T.READ x -> if (x = x') then (present := true;  temp := List.append !temp [instr];) else temp := List.append !temp [instr]
				| _ -> temp := List.append !temp [instr]
				)
			| _ -> temp := List.append !temp [instr]
		))
	) instr in 
		!temp


let copyPropagation : T.instr list -> T.instr list
=fun instr -> 
	let temp = ref [] in 
		temp := instr;
    let count = ref 0 in
    let totalLength = List.length instr in 

    while totalLength != !count do
    	let item = List.nth !temp !count in 
    	(match item with 
		| T.COPY (x, y) -> temp := (changeByPropagation !temp !count item)
		| T.COPYC (x, n) -> temp := (changeByPropagation !temp !count item)
		| _ -> ());
		count := !count + 1
	done; !temp

let subCopy : T.instr list -> int -> T.instr -> T.instr list
=fun instr index inputInstr ->
	let temp = ref [] in 
	let count = ref 0 in 
	let present = ref false in 
	let _ = List.iter (fun instr -> 
		if !present then temp := List.append !temp [instr] else (
		count := !count + 1;
		if (!count - 1 <= index) then temp := List.append !temp [instr]
		else (
			match inputInstr with
			| T.ASSIGNV (x, o, y, z) -> 
				(match instr with 
				| T.ASSIGNV (x', o', y', z') -> 
					if (z = z' && o = o' && y = y') then temp := List.append !temp [T.COPY (x', x)]
					else temp := List.append !temp [instr]
				| T.ASSIGNC (x', o', y', n') -> 
					if (z = x' || y = x') then (present := true;  temp := List.append !temp [instr];)
					else temp := List.append !temp [instr]
				| T.ASSIGNU (x', o', y') -> 
					if (z = x' || y = x') then (present := true;  temp := List.append !temp [instr];)
					else temp := List.append !temp [instr]
				| T.COPY (x', y') -> 
					if (z = x' || y = x') then (present := true;  temp := List.append !temp [instr];)
					else temp := List.append !temp [instr]
				| T.COPYC (x', n') -> 
					if (z = x' || y = x') then (present := true;  temp := List.append !temp [instr];)
					else temp := List.append !temp [instr]
				| T.LOAD (x', a) -> 
					if (z = x' || y = x') then (present := true;  temp := List.append !temp [instr];)
					else temp := List.append !temp [instr]
				| _ -> temp := List.append !temp [instr])
			| T.ASSIGNC (x, o, y, n) -> 
				(match instr with 
				| T.ASSIGNV (x', o', y', z') -> 
					if (y = x') then (present := true;  temp := List.append !temp [instr];)
					else temp := List.append !temp [instr]
				| T.ASSIGNC (x', o', y', n') -> 
					if (y = y' && o = o' && n = n') then temp := List.append !temp [T.COPY (x', x)] 
					else temp := List.append !temp [instr]
				| T.ASSIGNU (x', o', y') -> 
					if (y = x') then (present := true;  temp := List.append !temp [instr];)
					else temp := List.append !temp [instr]
				| T.COPY (x', y') -> 
					if (y = x') then (present := true;  temp := List.append !temp [instr];)
					else temp := List.append !temp [instr]
				| T.COPYC (x', n') -> 
					if (y = x') then (present := true;  temp := List.append !temp [instr];)
					else temp := List.append !temp [instr]
				| T.LOAD (x', a) -> 
					if (y = x') then (present := true;  temp := List.append !temp [instr];)
					else temp := List.append !temp [instr]
				| _ -> temp := List.append !temp [instr])
			| T.ASSIGNU (x, o, y) -> 
				(match instr with 
				| T.ASSIGNV (x', o', y', z') -> 
					if (y = x') then (present := true;  temp := List.append !temp [instr];) 
					else temp := List.append !temp [instr]
				| T.ASSIGNC (x', o', y', n') -> 
					if (y = x') then (present := true;  temp := List.append !temp [instr];)  
					else temp := List.append !temp [instr]
				| T.ASSIGNU (x', o', y') -> 
					if (y = y' && o = o') then temp := List.append !temp [T.COPY (x', x)]
					else temp := List.append !temp [instr]
				| T.COPY (x', y') -> 
					if (y = x') then (present := true;  temp := List.append !temp [instr];)  
					else temp := List.append !temp [instr]
				| T.COPYC (x', n') -> 
					if (y = x') then (present := true;  temp := List.append !temp [instr];)  
					else temp := List.append !temp [instr]
				| T.LOAD (x', a) -> 
					if (y = x') then (present := true;  temp := List.append !temp [instr];)  
					else temp := List.append !temp [instr]
				| _ -> temp := List.append !temp [instr])
			| _ -> temp := List.append !temp [instr]
		))
	) instr in 
		!temp


let subExpressionElimination : T.instr list -> T.instr list 
=fun instr -> 
	let temp = ref [] in 
		temp := instr;
    let count = ref 0 in
	let _ = List.iter (fun instr -> 
		(match instr with
		| T.ASSIGNV (x, o, y, z) -> temp := (subCopy !temp !count instr)
		| T.ASSIGNC (x, o, y, n) -> temp := (subCopy !temp !count instr)
		| T.ASSIGNU (x, o, y) -> temp := (subCopy !temp !count instr)
		| _ -> ()
		);
    	count := !count + 1;
	) !temp in 
		!temp


let blockOptimizer : T.instr list -> T.instr list
=fun instr ->
	let first = subExpressionElimination instr in 
	let second = copyPropagation first in 
	let third = constantFolding second in 
		third


let makeTProgram : T.instr list * int -> T.program
=fun (instr, inputIndex) -> 
	let temp = ref [] in
    let count = ref 0 in

	let _ = List.iter (fun instr ->
		count := !count + 1;
		if !count == 1 then ( 
			if inputIndex != 0 then (
				let itemIndex = List.nth !globalLabel (inputIndex - 1) in 
					temp := List.append !temp [(itemIndex, instr)]
			) else (
				temp := List.append !temp [(inputIndex, instr)] 
			)
		)
		else ( temp := List.append !temp [(0, instr)] )
	) instr in
		!temp

let divideBasicblock : T.program -> (T.instr list) list
=fun t->
	let result = ref [] in
	let temp = ref [] in
	let _ = List.iter (fun (label, instr) ->
		if label != 0 then
			(
				globalLabel := List.append !globalLabel [label];
				result := List.append !result [!temp];
				temp := [];
			)
		else temp := List.append !temp [instr]
	) t in
	let _ = ( result := List.append !result [!temp] ) in
		!result

let optimize : T.program -> T.program
=fun t -> 
	let result = ref [] in
	let resultList = divideBasicblock t in
	for i = 0 to (List.length resultList -1) do 
		let item = List.nth resultList i in
		let optimizeedItem = blockOptimizer item in 
			result := List.append !result (makeTProgram (optimizeedItem, i))
	done; 
	let resultReal = deadcodeElimination !result in 
		resultReal



