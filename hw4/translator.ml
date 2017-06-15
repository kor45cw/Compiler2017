
let newT = ref 0
let newL = ref 0

let createNewT : unit -> string
= fun () ->
  let _ = newT := !newT + 1 in
    "t" ^ string_of_int !newT

let createNewL : unit -> int
= fun () ->
  let _ = newL := !newL + 1 in
    !newL

let multipleListAppend : T.program -> T.program -> T.program -> T.program
=fun input1 input2 input3 ->
  let first = List.append input2 input3 in
    List.append input1 first

let transDecl : S.decl -> T.program
=fun decl ->
	match decl with
	| (typ, id) ->
		match typ with
		| S.TINT -> [(T.dummy_label, T.COPYC(id, 0))]
		| S.TARR n -> [(T.dummy_label, T.ALLOC(id, n))]

let rec transDecls : S.decls -> T.program
=fun decls ->
	match decls with
	| [] -> []
	| hd::tl ->
		let result = transDecl hd in
			List.append result (transDecls tl)


let rec transExp : S.exp -> T.var * T.program
=fun e ->
  let t0 = createNewT () in
  match e with
  | S.NUM number ->
        (t0, [(T.dummy_label, T.COPYC(t0, number))])
  | S.LV lv ->
     (match lv with
     | S.ID id ->
           (t0, [(T.dummy_label, T.COPY(t0, id))])
     | S.ARR (id, exp) ->
         let (var, prog) = transExp exp in
         let result = List.append prog [(T.dummy_label, T.LOAD(t0, (id, var)))] in
           (t0, result)
      )
  | S.ADD (exp1, exp2) ->
      let (var1, prog1) = transExp exp1 in
      let (var2, prog2) = transExp exp2 in
      let result = multipleListAppend prog1 prog2 [(T.dummy_label, T.ASSIGNV(t0, T.ADD, var1, var2))] in
        (t0, result)
  | S.SUB (exp1, exp2) ->
      let (var1, prog1) = transExp exp1 in
      let (var2, prog2) = transExp exp2 in
      let result = multipleListAppend prog1 prog2 [(T.dummy_label, T.ASSIGNV(t0, T.SUB, var1, var2))] in 
        (t0, result)
  | S.MUL (exp1, exp2) ->
      let (var1, prog1) = transExp exp1 in
      let (var2, prog2) = transExp exp2 in
      let result = multipleListAppend prog1 prog2 [(T.dummy_label, T.ASSIGNV(t0, T.MUL, var1, var2))] in
        (t0, result)
  | S.DIV (exp1, exp2) ->
      let (var1, prog1) = transExp exp1 in
      let (var2, prog2) = transExp exp2 in
      let result = multipleListAppend prog1 prog2 [(T.dummy_label, T.ASSIGNV(t0, T.DIV, var1, var2))] in
        (t0, result)
  | S.MINUS exp ->
      let (var, prog) = transExp exp in
      let result  = List.append prog [(T.dummy_label, T.ASSIGNU(t0, T.MINUS, var))] in
        (t0, result)
  | S.NOT exp ->
      let (var, prog) = transExp exp in
      let result = List.append prog [(T.dummy_label, T.ASSIGNU(t0, T.NOT, var))] in
        (t0, result)
  | S.LT (exp1, exp2) ->
      let (var1, prog1) = transExp exp1 in
      let (var2, prog2) = transExp exp2 in
      let result = multipleListAppend prog1 prog2 [(T.dummy_label, T.ASSIGNV(t0, T.LT, var1, var2))] in
        (t0, result)
  | S.LE (exp1, exp2) ->
      let (var1, prog1) = transExp exp1 in
      let (var2, prog2) = transExp exp2 in
      let result = multipleListAppend prog1 prog2 [(T.dummy_label, T.ASSIGNV(t0, T.LE, var1, var2))] in
        (t0, result)
  | S.GT (exp1, exp2) ->
      let (var1, prog1) = transExp exp1 in
      let (var2, prog2) = transExp exp2 in
      let result = multipleListAppend prog1 prog2 [(T.dummy_label, T.ASSIGNV(t0, T.GT, var1, var2))] in
        (t0, result)
  | S.GE (exp1, exp2) ->
      let (var1, prog1) = transExp exp1 in
      let (var2, prog2) = transExp exp2 in
      let result = multipleListAppend prog1 prog2 [(T.dummy_label, T.ASSIGNV(t0, T.GE, var1, var2))] in
        (t0, result)
  | S.EQ (exp1, exp2) ->
      let (var1, prog1) = transExp exp1 in
      let (var2, prog2) = transExp exp2 in
      let result = multipleListAppend prog1 prog2 [(T.dummy_label, T.ASSIGNV(t0, T.EQ, var1, var2))] in
        (t0, result) 
  | S.AND (exp1, exp2) ->
      let (var1, prog1) = transExp exp1 in
      let (var2, prog2) = transExp exp2 in
      let result = multipleListAppend prog1 prog2 [(T.dummy_label, T.ASSIGNV(t0, T.AND, var1, var2))] in
        (t0, result)
  | S.OR (exp1, exp2) ->
      let (var1, prog1) = transExp exp1 in
      let (var2, prog2) = transExp exp2 in
      let result = multipleListAppend prog1 prog2 [(T.dummy_label, T.ASSIGNV(t0, T.OR, var1, var2))] in
        (t0, result)

let rec transBlock : S.program -> T.program
=fun s ->
	match s with
	| (decls, stmts) -> 
		let resultDecls = transDecls decls in
		let resultStmts = transStmts stmts in
			List.append resultDecls resultStmts

and transStmt : S.stmt -> T.program
=fun stmt ->
	match stmt with
	| S.ASSIGN (lv, exp) -> 
		let (var, prog) = transExp exp in
			(match lv with 
			| S.ID id -> List.append prog [(T.dummy_label, T.COPY(id, var))]
			| S.ARR (id, exp2) -> 
				let (var2, prog2) = transExp exp2 in
          multipleListAppend prog prog2 [(T.dummy_label, T.STORE((id, var2), var))]
				)
	| S.IF (exp, stmt1, stmt2) -> 
		let l1 = createNewL() in
		let l2 = createNewL() in
		let l3 = createNewL() in
		let (var, prog) = transExp exp in
		let prog1 = transStmt stmt1 in
		let prog2 = transStmt stmt2 in
    let first = multipleListAppend prog [(T.dummy_label, T.CJUMP(var, l1))] [(T.dummy_label, T.UJUMP(l2))] in
    let second = multipleListAppend first [(l1, T.SKIP)] prog1 in
    let third = multipleListAppend second [(T.dummy_label, T.UJUMP(l3))] [(l2, T.SKIP)] in
      multipleListAppend third prog2 [(l3, T.SKIP)]
	| S.WHILE (exp, stmt1) ->
		let l1 = createNewL() in
		let l2 = createNewL() in 
		let (var, prog) = transExp exp in
		let prog1 = transStmt stmt1 in
    let first = multipleListAppend [(l1, T.SKIP)] prog [(T.dummy_label, T.CJUMPF(var, l2))] in
      multipleListAppend first prog1 (List.append [(T.dummy_label, T.UJUMP(l1))] [(l2, T.SKIP)])
	| S.DOWHILE (stmt1, exp) ->
		let prog1 = transStmt stmt1 in
		let progWhile = transStmt (S.WHILE (exp, stmt1)) in
			List.append prog1 progWhile
	| S.READ id -> [(T.dummy_label, T.READ id)]
	| S.PRINT exp ->
       let (var, prog) = transExp exp in
          List.append prog [(T.dummy_label, T.WRITE var)]
	| S.BLOCK block -> (transBlock block)


and transStmts : S.stmts -> T.program
=fun stmts ->
	match stmts with
	| [] -> []
	| hd::tl -> 
		let result = transStmt hd in
			List.append result (transStmts tl)


let translate : S.program -> T.program
=fun s -> 
	List.append (transBlock s) [(T.dummy_label, T.HALT)]
