signature RATIONAL =
	sig
		type rational
		val mkRat: int * int -> rational
	end

structure Rational =
	struct
		datatype rational = Rat of int*int
		fun mkRat (x,y) = Rat(x,y)
	end
	

	
structure R:RATIONAL = Rational

structure O :> RATIONAL = Rational
