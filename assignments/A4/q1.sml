datatype 'a stream = S of ('a * (unit -> 'a stream))

fun mkseq start = S(start, fn()=>mkseq(start+1))

fun scar (S (x, _)) = x

fun scdr (S (_, y)) = y()

fun lazymap f stream =
	S((f (scar stream)), fn()=>(lazymap f (scdr stream)))
