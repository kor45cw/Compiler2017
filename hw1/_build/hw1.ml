open Regex

exception Not_implemented


(* Support OR function *)
let rec orFunc: Nfa.states -> (Nfa.t * Nfa.state) -> Nfa.t
=fun states (nfa, state) ->
	if BatSet.is_empty states then nfa
	else let item = BatSet.pop_min states in
		orFunc (snd item) (Nfa.add_epsilon_edge nfa (fst item, state), state)

let createNewState : unit -> Nfa.t
=fun () ->
	let newState = Nfa.create_new_nfa () in
	let addEpsilon = Nfa.add_epsilon_edge newState ((Nfa.get_initial_state newState), (Nfa.get_initial_state newState)) in
		addEpsilon

let rec evalNFA: Regex.t -> Nfa.t -> Nfa.t
=fun regex nfa ->
	match regex with
	| Empty ->
		let newState = Nfa.create_state nfa in
		let addEpsilonNew = Nfa.add_epsilon_edge (snd newState) (fst newState, fst newState) in
		let result = Nfa.add_final_state addEpsilonNew (fst newState) in
			result
	| Epsilon -> 
		let newState = Nfa.create_state nfa in
		let addEpsilon = Nfa.add_epsilon_edge (snd newState) ((Nfa.get_initial_state nfa), (fst newState)) in
		let addEpsilonNew = Nfa.add_epsilon_edge addEpsilon ((fst newState), (fst newState)) in
		let result = Nfa.add_final_state addEpsilonNew (fst newState) in
			result
	| Alpha alpha ->
		let newState = Nfa.create_state nfa in
		let addEdge = Nfa.add_edge (snd newState) ((Nfa.get_initial_state (snd newState)), alpha, (fst newState)) in
		let addEpsilonNew = Nfa.add_epsilon_edge addEdge ((fst newState), (fst newState)) in
		let result = Nfa.add_final_state addEpsilonNew (fst newState) in
			result
	| OR (left, right) ->
	(* 새로 nfa를 만들고 각각 기존의 initial 과 endpoint 를 연결 *)
		let v1 = evalNFA left (createNewState ()) in
		let v2 = evalNFA right (createNewState ()) in
		let addV1State = Nfa.add_states nfa (Nfa.get_states v1) in
		let addV1Egde = Nfa.add_edges addV1State (Nfa.get_edges v1) in
		let addV2State = Nfa.add_states	addV1Egde (Nfa.get_states v2) in
		let addV2Edge = Nfa.add_edges addV2State (Nfa.get_edges v2) in
		let mergeInitialV1 = Nfa.add_epsilon_edge addV2Edge (Nfa.get_initial_state nfa, Nfa.get_initial_state v1) in
		let mergeInitialV2 = Nfa.add_epsilon_edge mergeInitialV1 (Nfa.get_initial_state nfa, Nfa.get_initial_state v2) in
		let newState = Nfa.create_state mergeInitialV2 in
		let addEpsilonNew = Nfa.add_epsilon_edge (snd newState) ((fst newState), (fst newState)) in
		let mergeLastV1 = orFunc (Nfa.get_final_states v1) (addEpsilonNew, fst newState) in
		let mergeLastV2 = orFunc (Nfa.get_final_states v2) (mergeLastV1, fst newState) in
		let result = Nfa.add_final_state mergeLastV2 (fst newState) in
			result
	| CONCAT (left, right) ->
		let v1 = evalNFA left (createNewState ()) in
		let v2 = evalNFA right (createNewState ()) in
		let addV1State = Nfa.add_states nfa (Nfa.get_states v1) in
		let addV1Egde = Nfa.add_edges addV1State (Nfa.get_edges v1) in
		let addV2State = Nfa.add_states	addV1Egde (Nfa.get_states v2) in
		let addV2Edge = Nfa.add_edges addV2State (Nfa.get_edges v2) in
		let mergeInitialV1 = Nfa.add_epsilon_edge addV2Edge (Nfa.get_initial_state nfa, Nfa.get_initial_state v1) in
		let mergeV1V2 = orFunc (Nfa.get_final_states v1) (mergeInitialV1, Nfa.get_initial_state v2) in
		let newState = Nfa.create_state mergeV1V2 in
		let addEpsilonNew = Nfa.add_epsilon_edge (snd newState) ((fst newState), (fst newState)) in
		let mergeLastV2 = orFunc (Nfa.get_final_states v2) (addEpsilonNew, fst newState) in
		let result = Nfa.add_final_state mergeLastV2 (fst newState) in
			result
	| STAR regularExpression ->
		let v1 = evalNFA regularExpression (createNewState ()) in
		let addV1State = Nfa.add_states nfa (Nfa.get_states v1) in
		let addV1Egde = Nfa.add_edges addV1State (Nfa.get_edges v1) in
		let mergeInitialV1 = Nfa.add_epsilon_edge addV1Egde (Nfa.get_initial_state nfa, Nfa.get_initial_state v1) in
		let newState = Nfa.create_state mergeInitialV1 in
		let addEpsilonNew = Nfa.add_epsilon_edge (snd newState) ((fst newState), (fst newState)) in
		let mergeLastV1 = orFunc (Nfa.get_final_states v1) (addEpsilonNew, fst newState) in
		let addEpsilonInitialLast = Nfa.add_epsilon_edge mergeLastV1 ((Nfa.get_initial_state nfa), (fst newState)) in
		let addEpsilonLastInitial = Nfa.add_epsilon_edge addEpsilonInitialLast ((fst newState), (Nfa.get_initial_state nfa)) in
		let result = Nfa.add_final_state addEpsilonLastInitial (fst newState) in
			result

let regex2nfa : Regex.t -> Nfa.t
=fun regex ->
	let firstType = createNewState () in
	let result = evalNFA regex firstType in
		result


let rec getAllEpsilonEdge : Nfa.t -> (Nfa.states * Nfa.states) -> Nfa.states -> Nfa.states
=fun nfa (input, prev) result ->
	if BatSet.is_empty input then result
	else let checker = BatSet.pop_min input in
		let epsilonEdge = Nfa.get_next_state_epsilon nfa (fst checker) in
		let realResult = BatSet.union result epsilonEdge in
		let addPrev = BatSet.add (fst checker) prev in
		let tempUnion = BatSet.union (snd checker) epsilonEdge in
		let nextInput = BatSet.diff tempUnion addPrev in
			getAllEpsilonEdge nfa (nextInput, addPrev) realResult

let rec getAllAEdge : Nfa.t -> Nfa.states -> Nfa.states -> Nfa.states
=fun nfa states result ->
	if BatSet.is_empty states then result
	else let checker = BatSet.pop_min states in
		let aEdge = Nfa.get_next_state nfa (fst checker) A in
		let realResult = BatSet.union result aEdge in
		let realResult2 = BatSet.union realResult (getAllEpsilonEdge nfa (aEdge, BatSet.empty) BatSet.empty) in
			getAllAEdge nfa (snd checker) realResult2


let rec getAllBEdge : Nfa.t -> Nfa.states -> Nfa.states -> Nfa.states
=fun nfa states result ->
	if BatSet.is_empty states then result
	else let checker = BatSet.pop_min states in
		let bEdge = Nfa.get_next_state nfa (fst checker) B in
		let realResult = BatSet.union result bEdge in
		let realResult2 = BatSet.union realResult (getAllEpsilonEdge nfa (bEdge, BatSet.empty) BatSet.empty) in
			getAllBEdge nfa (snd checker) realResult2


let rec evalDFA : Nfa.t -> Dfa.t -> Dfa.state -> (Dfa.states * Dfa.states) -> Dfa.t
=fun nfa dfa state (notCheckState, prevChecked) ->
	let finalState = Nfa.get_final_states nfa in
	let stateA = getAllAEdge nfa state BatSet.empty in
	let stateB = getAllBEdge nfa state BatSet.empty in
	let isNotFinalStateA = BatSet.is_empty (BatSet.intersect finalState stateA) in
	let isNotFinalStateB = BatSet.is_empty (BatSet.intersect finalState stateB) in
	let isEmptyStateA = BatSet.is_empty stateA in
	let isEmptyStateB = BatSet.is_empty stateB in

	match (isEmptyStateA, isEmptyStateB) with
	| (true, true) -> 
		let addEmptyState = Dfa.add_state dfa BatSet.empty in
		let addEmptyEdgeA = Dfa.add_edge addEmptyState (state, A, BatSet.empty) in
		let addEmptyEdgeB = Dfa.add_edge addEmptyEdgeA (state, B, BatSet.empty) in
		let addEmptyEdgeASelf = Dfa.add_edge addEmptyEdgeB (BatSet.empty, A, BatSet.empty) in
		let addEmptyEdgeBSelf = Dfa.add_edge addEmptyEdgeASelf (BatSet.empty, B, BatSet.empty) in
		if BatSet.is_empty notCheckState then addEmptyEdgeBSelf
		else let item = BatSet.pop_min notCheckState in
			if BatSet.mem (fst item) prevChecked then
				evalDFA nfa addEmptyEdgeBSelf BatSet.empty ((snd item), prevChecked)
			else 
				let addPrevCheck = BatSet.add (fst item) prevChecked in
				evalDFA nfa addEmptyEdgeBSelf (fst item) ((snd item), addPrevCheck)
	| (true, false) -> 
		let addEmptyState = Dfa.add_state dfa BatSet.empty in
		let addEmptyEdgeA = Dfa.add_edge addEmptyState (state, A, BatSet.empty) in
		let addEmptyEdgeASelf = Dfa.add_edge addEmptyEdgeA (BatSet.empty, A, BatSet.empty) in
		let addEmptyEdgeBSelf = Dfa.add_edge addEmptyEdgeASelf (BatSet.empty, B, BatSet.empty) in
 		if BatSet.mem stateB prevChecked then
			let addEdgeB = Dfa.add_edge addEmptyEdgeBSelf (state, B, stateB) in
				evalDFA nfa addEdgeB BatSet.empty (BatSet.remove stateB notCheckState, prevChecked)
		else
			let addStateB = Dfa.add_state addEmptyEdgeBSelf stateB in
			let addEdgeB = Dfa.add_edge addStateB (state, B, stateB) in
			let addPrevCheck = BatSet.add stateB prevChecked in
				if isNotFinalStateB == false then 
					let addFinal = Dfa.add_final_state addEdgeB stateB in
					evalDFA nfa addFinal stateB (BatSet.remove stateB notCheckState, addPrevCheck)
				else 
					evalDFA nfa addEdgeB stateB (BatSet.remove stateB notCheckState, addPrevCheck)
	| (false, true) ->
		let addEmptyState = Dfa.add_state dfa BatSet.empty in
		let addEmptyEdgeB = Dfa.add_edge addEmptyState (state, B, BatSet.empty) in
		let addEmptyEdgeASelf = Dfa.add_edge addEmptyEdgeB (BatSet.empty, A, BatSet.empty) in
		let addEmptyEdgeBSelf = Dfa.add_edge addEmptyEdgeASelf (BatSet.empty, B, BatSet.empty) in
		if BatSet.mem stateA prevChecked then 
			let addEdgeA = Dfa.add_edge addEmptyEdgeBSelf (state, A, stateA) in
				evalDFA nfa addEdgeA BatSet.empty (BatSet.remove stateA notCheckState, prevChecked)
		else 
			let addStateA = Dfa.add_state addEmptyEdgeBSelf stateA in
			let addEdgeA = Dfa.add_edge addStateA (state, A, stateA) in
			let addPrevCheck = BatSet.add stateA prevChecked in
				if isNotFinalStateA == false then 
					let addFinal = Dfa.add_final_state addEdgeA stateA in
					evalDFA nfa addFinal stateA (BatSet.remove stateA notCheckState, addPrevCheck)
				else 
					evalDFA nfa addEdgeA stateA (BatSet.remove stateA notCheckState, addPrevCheck)
	
	| (false, false) ->
		let isExistPrevA = BatSet.mem stateA prevChecked in
		let isExistPrevB = BatSet.mem stateB prevChecked in

		match (isExistPrevA, isExistPrevB) with
		| (true, true) ->
			let addEdgeA = Dfa.add_edge dfa (state, A, stateA) in
			let addEdgeB = Dfa.add_edge addEdgeA (state, B, stateB) in
				evalDFA nfa addEdgeB BatSet.empty (notCheckState, prevChecked)
		| (false, true) ->
			let addStateA = Dfa.add_state dfa stateA in
			let addEdgeA = Dfa.add_edge addStateA (state, A, stateA) in
			let addEdgeB = Dfa.add_edge addEdgeA (state, B, stateB) in
			let addPrevCheck = BatSet.add stateA prevChecked in
				if isNotFinalStateA == false then 
					let addFinal = Dfa.add_final_state addEdgeB stateA in
					evalDFA nfa addFinal stateA (BatSet.remove stateA notCheckState, addPrevCheck)
				else 
					evalDFA nfa addEdgeB stateA (BatSet.remove stateA notCheckState, addPrevCheck)
		| (true, false) ->
			let addStateB = Dfa.add_state dfa stateB in
			let addEdgeA = Dfa.add_edge addStateB (state, A, stateA) in
			let addEdgeB = Dfa.add_edge addEdgeA (state, B, stateB) in
			let addPrevCheck = BatSet.add stateB prevChecked in
				if isNotFinalStateB == false then 
					let addFinal = Dfa.add_final_state addEdgeB stateB in
					evalDFA nfa addFinal stateB (BatSet.remove stateB notCheckState, addPrevCheck)
				else 
					evalDFA nfa addEdgeB stateB (BatSet.remove stateB notCheckState, addPrevCheck)
		| (false, false) -> 
			let addStateA = Dfa.add_state dfa stateA in
			let addStateB = Dfa.add_state addStateA stateB in
			let addEdgeA = Dfa.add_edge addStateB (state, A, stateA) in
			let addEdgeB = Dfa.add_edge addEdgeA (state, B, stateB) in
			let addPrevCheck = BatSet.add stateA prevChecked in
			let newNotCheck = BatSet.add stateB notCheckState in
				match (isNotFinalStateA, isNotFinalStateB) with
				| (false, false) ->
					let addFinalA = Dfa.add_final_state addEdgeB stateA in
					let addFinalB = Dfa.add_final_state addFinalA stateB in
					evalDFA nfa addFinalB stateA (BatSet.remove stateA newNotCheck, addPrevCheck)
				| (false, true) ->
					let addFinal = Dfa.add_final_state addEdgeB stateA in
					evalDFA nfa addFinal stateA (BatSet.remove stateA newNotCheck, addPrevCheck)
				| (true, false) ->
					let addFinal = Dfa.add_final_state addEdgeB stateB in
					evalDFA nfa addFinal stateA (BatSet.remove stateA newNotCheck, addPrevCheck)
				| (true, true) -> 
					evalDFA nfa addEdgeB stateA (BatSet.remove stateA newNotCheck, addPrevCheck)


let nfa2dfa : Nfa.t -> Dfa.t
=fun nfa ->
	let getFirstStates = getAllEpsilonEdge nfa (BatSet.singleton (Nfa.get_initial_state nfa), (BatSet.empty)) (BatSet.empty) in
	let finalState = Nfa.get_final_states nfa in
	let isNotFinalState = BatSet.is_empty (BatSet.intersect finalState getFirstStates) in
	let newDfa = Dfa.create_new_dfa getFirstStates in
		if isNotFinalState == false then
			let addFinal = Dfa.add_final_state newDfa getFirstStates in
			let result = evalDFA nfa addFinal (Dfa.get_initial_state addFinal) (BatSet.empty, BatSet.empty) in
				result
		else 
			let result = evalDFA nfa newDfa (Dfa.get_initial_state newDfa) (BatSet.empty, BatSet.empty) in
				result

(* Do not modify this function *)
let regex2dfa : Regex.t -> Dfa.t
=fun regex ->
  let nfa = regex2nfa regex in
  	(* Nfa.print nfa; *)
  let dfa = nfa2dfa nfa in
	(* Dfa.print dfa; *)
    dfa

(* run_dfa function *)
let rec eval: Dfa.t -> Dfa.state -> alphabet list -> Dfa.state
=fun dfa state str ->
	if BatSet.is_empty state then state
	else 
		match str with
		| [] -> state
		| hd::tl -> eval dfa (Dfa.get_next_state dfa state hd) tl

let run_dfa : Dfa.t -> alphabet list -> bool
=fun dfa str ->
	let lastState = eval dfa (Dfa.get_initial_state dfa) str in
	let result = Dfa.is_final_state dfa lastState in
		result
