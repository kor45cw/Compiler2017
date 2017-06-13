
let changeByFolding : T.instr list -> int -> T.var -> int -> T.instr list
=fun instr index inputX inputN ->
	let temp = ref [] in 
	let insert l = temp := !temp @ [l] in
	let count = ref 0 in 
	let _ = List.iter (fun instr -> 
		count := !count + 1;
		if (!count - 1 <= index) then insert instr
		else (
			match instr with
			| T.ASSIGNV (x,o,y,z) -> 
				if (x == inputX) then ()
				else if (y == inputX) then insert (T.ASSIGNC (x, o, z, inputN))
				else if (z == inputX) then insert (T.ASSIGNC (x, o, y, inputN))
				else insert instr
			| T.ASSIGNC (x,o,y,n) -> 
				if (x == inputX) then () 
				else if (y == inputX) then insert (T.COPYC (x, inputN + n))
				else insert instr
			| T.ASSIGNU (x,o,y) -> if (x == inputX) then () else insert instr
			| T.COPY (x,y) -> 
				if (y == inputX) then insert (T.COPYC (x, inputN)) 
				else if (x == inputX) then ()
				else insert instr
			| T.COPYC (x,n) -> if (x == inputX) then () else insert instr
			| _ -> insert instr
		)
	) instr in
		!temp

let constantFolding : T.instr list -> T.instr list
=fun instr ->
	let temp = ref [] in
    	temp := instr;
    let count = ref 0 in
    let _ = List.iter (fun instr ->
    	(match instr with 
    	| T.COPYC (x, n) -> temp := (changeByFolding !temp !count x n)
    	| _ -> ()
    	);
    	count := !count + 1;
	) !temp in
		!temp

let checkDeadcode : T.instr list -> int -> T.var -> bool
=fun instr index inputX ->
	let count = ref 0 in 
	let present = ref false in 
	let _ = List.iter (fun instr -> 
		if !present then () else (
		count := !count + 1;
		if (!count - 1 > index) then (
			match instr with
			| T.ALLOC (x, n) -> if (x == inputX) then present := true
			| T.ASSIGNV (x, o, y, z) -> 
				if (x == inputX) then present := true
				else if (y == inputX) then present := true
				else if (z == inputX) then present := true
			| T.ASSIGNC (x, o, y, n) -> 
				if (x == inputX) then present := true
				else if (y == inputX) then present := true
			| T.ASSIGNU (x, o, y) -> 
				if (x == inputX) then present := true
				else if (y == inputX) then present := true
			| T.COPY (x, y) -> if (x == inputX) then present := true
			| T.COPYC (x, n) -> if (x == inputX) then present := true
			| T.CJUMP (x, l) -> if (x == inputX) then present := true
			| T.CJUMPF (x, l) -> if (x == inputX) then present := true
			| T.LOAD (x, a) -> if (x == inputX) then present := true
			| T.STORE (a, x) -> if (x == inputX) then present := true
			| T.READ x -> if (x == inputX) then present := true
			| T.WRITE x -> if (x == inputX) then present := true
			| _ -> ()
		))
	) instr in
		!present

let deadcodeElimination : T.instr list -> T.instr list 
=fun instr -> 
	let temp = ref [] in
	let insert l = temp := !temp @ [l] in
    let count = ref 0 in
    let _ = List.iter (fun instr ->
    	(match instr with 
    	| T.ALLOC (x, n) -> if (checkDeadcode !temp !count x) then insert instr else ()
    	| T.ASSIGNV (x,o,y,z) -> if (checkDeadcode !temp !count x) then insert instr else ()
    	| T.ASSIGNC (x,o,y,n) -> if (checkDeadcode !temp !count x) then insert instr else ()
    	| T.ASSIGNU (x,o,y) -> if (checkDeadcode !temp !count x) then insert instr else ()
    	| T.COPY (x, y) -> if (checkDeadcode !temp !count x) then insert instr else ()
    	| T.COPYC (x, n) -> if (checkDeadcode !temp !count x) then insert instr else ()
    	| _ -> insert instr
    	);
    	count := !count + 1;
	) instr in
		!temp


(* 블락을 나눴으니 블락마다 optimizer를 돌려야하는데 말이지 *)
let rec blockOptimizer : T.instr list * int -> T.program
=fun (instr, count) -> raise (Failure "optimizer: Not implemented")


let divideBasicblock : T.program -> (T.instr list) list
=fun t->
	let result = ref [] in
	let temp = ref [] in
    let insert l = temp := !temp @ [l] in
    let insert2 l = result := !result @ [l] in
	let _ = List.iter (fun (label, instr) ->
		if label != 0 then
			(
				insert2 !temp;
				temp := [];
			)
		else insert instr
	) t in
	let _ = insert2 ! temp in
		!result

let optimize : T.program -> T.program
=fun t -> 
	let resultList = divideBasicblock t in
	let countResult = List.length resultList in
		print_string (string_of_int countResult);
		raise (Failure "optimizer: Not implemented")