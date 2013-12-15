bird("A").
bird("B").
bird("C").
bird("D").
flies(X) :- bird(X), not(X = "D").

