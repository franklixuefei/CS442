sum_to(1,1):-!.
sum_to(N,R):- N1 is N-1, sum_to(N1,R1), R is R1+N.
