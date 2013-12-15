(* Start of Part D Doc *)

(* By extending W *)

Letex:

Type Rule:

	 <x, TExc>+A |- e:t
	---------------------[Letex]
	A |- Letex(x, e) : t
	 
Pseudocode:

	W (A, Letex (x, e)) =
		let
			<s, t> = W ((x, TExc)+A, e)
		in
			<s, t>
		
			
Handle:

Type Rule:

	A |- e1:bool		A |- e2:t		A |- e3:t
	-------------------------------------------[Handle]
			A |- Handle(e1, e2, e3) : t
			
Pseudocode:

	W (A, Handle (e1, e2, e3)) = 
		let
			<s1, t1> = W (A, e1)
			s2 = unify (TExc, t1)
			<s3, t3> = W (s2 o s1 o A, e2)
			<s4, t4> = W (s3 o s2 o s1 o A, e3)
			s5 = unify (t3 s4, t4)
		in
			<s5 o s4 o s3 o s2 o s1, t4 s5>
			
*Note: TExc is the type of exception.


(* By placing in type environment *)

Raise:

	ForAll a, a
	
(* End of Part D Doc *)
	
