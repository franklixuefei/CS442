open List (* Opening the List structure *)

(* Part A 1 *)
(* Signature for NFA structure *)
signature NFASIG = sig
  eqtype Q
  eqtype Sigma
  val init : Q
  val delta : Q * Sigma -> Q list
  val accepting : Q list
end

(* Outputting a structure that provides a single function run
that determines whether a given input is in the language specified by the NFA Spec *)
functor NFA (Spec:NFASIG) : sig val run : Spec.Sigma list -> bool end = struct
  fun run l =
  let
    fun run' s [] = [s]
    |   run' s (x::xs) =
          let
            val possibleStates = Spec.delta(s, x)
            fun union [] l = l
            |   union l [] = l
            |   union (x::xs) l =
                  if (exists (fn (y) => x = y) l) 
                    then union xs l
                  else x::(union xs l)
          in
            foldl (fn (x, y) => union x y) nil (map (fn (y) => run' y xs) possibleStates)
          end
    val final = run' Spec.init l
    fun intersect [] _ = []
    |   intersect _ [] = []
    |   intersect (x::xs) l =
          if (exists (fn (y) => x = y) l) 
            then x::(intersect xs l) 
          else intersect xs l
  in
    length (intersect final Spec.accepting) > 0
  end
end

(* Signature for DFA structure *)
signature DFASIG = sig
	eqtype Q
	eqtype Sigma
	val init : Q
	val delta : Q * Sigma -> Q
	val accepting : Q list
end

(* Part A 2 *)
(* A converter from a DFA to an NFA *)
functor DFAtoNFA (Spec:DFASIG): NFASIG = struct
	type Sigma = Spec.Sigma
	type Q = Spec.Q
	val init = Spec.init
	val accepting = Spec.accepting
	fun delta p = [Spec.delta p]
end

(* Outputting a structure that provides a single function run
that determines whether a given input is in the language specified by the DFA Spec *)
functor DFA (Spec:DFASIG) : sig val run : Spec.Sigma list -> bool end = struct
  fun run l =
  	let
  		fun run' s [] = s
  		|  	 run' s (x::xs) = run' (Spec.delta (s,x)) xs
  		
  		val final = run' Spec.init l
  		fun member _ [] = false
  		| 	 member x (y::ys) = x = y orelse member x ys
  	in
  		member final Spec.accepting
  	end
end

(* Part A 3 *)
(* Computing the intersection of two DFAs *)
functor Intersection (structure Spec1:DFASIG 
							structure Spec2:DFASIG 
							sharing type Spec1.Q = Spec2.Q
							sharing type Spec1.Sigma = Spec2.Sigma) : DFASIG = struct
	type Q = Spec1.Q list
	type Sigma = Spec1.Sigma
	fun cartesian [] = []
  	| 	 cartesian ([x]) = map (fn e => [e]) x
  	|	 cartesian (x::xs) =
    		let 
    			val tailCross = cartesian xs
    		in
      			foldr (fn (x',result) => foldr (fn (tc,l) => (x'::tc) :: l ) result tailCross) [] x
    		end
	val init = [Spec1.init, Spec2.init]
	val accepting = cartesian [Spec1.accepting, Spec2.accepting]
	fun delta (combStates, sigma) =
		[Spec1.delta (hd combStates, sigma), Spec2.delta(last combStates, sigma)]
end


(* ==============================Testing=============================== *)
(* A specific NFA *)
structure TestMachine = struct
  exception Bad
  type Sigma = char
  type Q = int
  val init = 0
  val accepting = [5]
  fun delta (0, #"a") = [1,2]
  |   delta (1, #"b") = [3]
  |   delta (2, #"b") = [4]
  |   delta (3, #"c") = [5]
  |   delta (4, #"a") = [6]
  |   delta (6, #"c") = [7]
  |   delta (7, #"a") = [5]
  |   delta _ = []
end

(* A specific NFA *)
structure EndsWith_ab_bc_ca = struct
	type Sigma = char
	type Q = int
	val init = 0
	val accepting = [2, 4, 6]
	fun delta (0, #"a") = [0, 3]
	|   delta (0, #"b") = [0, 1]
	|   delta (0, #"c") = [0, 5]
	|   delta (1, #"c") = [2]
	|   delta (3, #"b") = [4]
	|   delta (5, #"a") = [6]
	|	 delta _ = []
end

(* A specific DFA *)
structure Even0sOdd1s = struct
	exception Bad
	type Sigma = int
	datatype QAux = ODD | EVEN
	type Q = QAux * QAux
	val init = (EVEN, EVEN)
	val accepting = [(EVEN, ODD)]
	fun toggle ODD = EVEN
	| 	 toggle _ = ODD
	fun delta ((x,y), 0) = (toggle x, y)
	| 	 delta ((x,y), 1) = (x, toggle y)
	| 	 delta _ = raise Bad
end


(* Converting the DFA Even0sOdd1s to a NFA *)
structure Nfa = DFAtoNFA(Even0sOdd1s)

structure test1 = NFA(TestMachine)
structure test2 = NFA(Nfa)
structure test3 = NFA(EndsWith_ab_bc_ca)

(* two DFAs dfa1 and dfa2 to be intersected *)
structure dfa1 = struct
  exception Bad
  type Sigma = int
  type Q = char
  val init = #"a"
  val accepting = [#"b"]
  fun delta (#"a",0) = #"a"
  |   delta (#"a",1) = #"b"
  |   delta (#"b",0) = #"a"
  |   delta (#"b",1) = #"a"
  |   delta _ = raise Bad
end

structure dfa2 = struct
  exception Bad
  type Sigma = int
  type Q = char
  val init = #"c"
  val accepting = [#"c"]
  fun delta (#"c",0) = #"d"
  |   delta (#"c",1) = #"c"
  |   delta (#"d",0) = #"d"
  |   delta (#"d",1) = #"c"
  |   delta _ = raise Bad
end
	
(* computing the intersection of dfa1 and dfa2 *)	
structure intersectDfa = Intersection(structure Spec1 = dfa1
												 structure Spec2 = dfa2)
(* running the computed intersected DFA of dfa1 and dfa2 *)
structure intersectedRun = DFA(intersectDfa)


