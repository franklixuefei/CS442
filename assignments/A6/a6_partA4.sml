(* Part A 4 *)

If we tried to write a functor to compute a DFA that recognizes the union of the languages of two input automata, we would run into a problem where we could not determine exactly what the accepting states are, since all we know from a DFASIG are accepting states and initial state, without knowing what other states are, and computing union-ed DFA needs information about internal states. So, in order to make Union implementable, we could add a field in DFASIG indicating all states we could have for a specific DFA.

signature DFASIG = sig
	eqtype Q
	eqtype Sigma
	val states : Q list 	(* added *)
	val init : Q
	val delta : Q * Sigma -> Q
	val accepting : Q list
end
