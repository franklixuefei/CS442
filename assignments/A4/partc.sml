open SMLofNJ.Cont

datatype 'a stream = Empty | S of ('a * (unit -> 'a stream))
fun scar (S (x, _)) = x
fun scdr (S (_, y)) = y()

fun mkseq start = S(start, fn() => mkseq(start+1))

fun foldr f i [] = i
|	 foldr f i l = f ((hd l),(foldr f i (tl l)))

fun scfoldr f i l = 
	let
		fun scfoldr' f i l k= 
			if l=[] then false
			else
			f((hd l), fn () => (scfoldr' f i (tl l) k), k)
	in
	callcc(
		fn k => (scfoldr' f i l k)
	)
	end

fun exists p l =
	scfoldr (fn (x,y,k) => 
		(if (p x) then true else if y() then (throw k true) else false)
	) false l
	
fun mklst length start = if length=0 then [] else start::(mklst (length-1) (start+1))
