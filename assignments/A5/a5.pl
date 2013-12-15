% Part B
interpretHelper([],[]):-!.
interpretHelper([a, k |Rest], Result) :- interpretHelper(Rest, [LFirst|LRest]), append([k(LFirst)], LRest, Result), !.
interpretHelper([a, k(X)|Rest], Result) :- interpretHelper(Rest, [_|LRest]), append([X],LRest, Result), !.
interpretHelper([a, s |Rest], Result) :- interpretHelper(Rest, [LFirst|LRest]), append([s(LFirst)], LRest, Result), !.
interpretHelper([a, s(X)|Rest], Result) :- interpretHelper(Rest, [LFirst|LRest]), append([s(X,LFirst)],LRest, Result), !.
interpretHelper([a, s(X,Y)|Rest], Result) :- interpretHelper(Rest, [LFirst|LRest]), append([a,a,X,LFirst,a,Y,LFirst],LRest, Result), !.
interpretHelper([a, i |Rest], Result) :- interpretHelper(Rest, [LFirst|LRest]), append([LFirst], LRest, Result), !.
interpretHelper([a, v |Rest], Result) :- interpretHelper(Rest, [_|LRest]), append([v], LRest, Result), !.
interpretHelper([a, dot(X) |Rest], Result) :- interpretHelper(Rest, [LFirst|LRest]), append([LFirst], LRest, Result), write(X), !.
interpretHelper([a, r |Rest], Result) :- interpretHelper(Rest, [LFirst|LRest]), append([LFirst], LRest, Result), nl, !.
interpretHelper([a, d |Rest], Result) :- findLogicExpr(Rest, ToBeDelayed, RestTerms), append([promise(ToBeDelayed)], RestTerms, Result), !.
interpretHelper([a, promise(X)|Rest], Result) :- interpretHelper(Rest, Res),interpretHelper(X, Res2),append([a|Res2], Res, Result),!.
interpretHelper([a |Rest], Result) :- interpretHelper(Rest, Res), append([a], Res, Res2),interpretHelper(Res2, Result),!.
interpretHelper(X,X):-!.

findLogicExpr([a|Rest], LogicPart, RestPart) :- 
	findLogicExpr(Rest, FirstArgArray, FirstRest), 
	append([a], FirstArgArray, SecondLogic),
	findLogicExpr(FirstRest, SecondArgArray, SecondRest), 
	append(SecondLogic, SecondArgArray, LogicPart),
	append(SecondRest, [], RestPart), !.
findLogicExpr([X|Rest],[X],Rest):-!.
findLogicExpr([],[],[]):-!.



interpret([],[]):-!.
interpret([a|Rest], Result) :- interpretHelper([a|Rest], Res), interpret(Res, Result), !.
interpret(X,X):-!.

% Part C
sementicsAnalysis(['`'|Rest], Result) :- sementicsAnalysis(Rest, Res), append([a], Res, Result),!.
sementicsAnalysis(['.'|Rest], Result) :- sementicsAnalysis(Rest, [LFirst|LRest]), append([dot(LFirst)], LRest, Result),!.
sementicsAnalysis([X|Rest], Result):- sementicsAnalysis(Rest, Res), append([X], Res, Result),!.
sementicsAnalysis(X,X).

convertAsciisToChars([X|Rest], Result):- convertAsciisToChars(Rest, Res), name(Char, [X]), append([Char], Res, Result),!.
convertAsciisToChars(X, X).

interpretFromText(Input, Result):- 
	Input = [Asciis|Rest],
	convertAsciisToChars([Asciis|Rest], Chars), 
	sementicsAnalysis(Chars, ReqularResult), 
	interpret(ReqularResult, Result),!.

% Part D
evalBracketAbs(Var, app(M, N), Result) :- 
	evalBracketAbs(Var, M, MResult),
	Inter = app(comb(s), MResult),
	evalBracketAbs(Var, N, NResult), 
	Result = app(Inter, NResult), !.
evalBracketAbs(_, comb(i), app(comb(k), comb(i))):- !.
evalBracketAbs(_, comb(k), app(comb(k), comb(k))):- !.
evalBracketAbs(_, comb(s), app(comb(k), comb(s))):- !.
evalBracketAbs(_, func(X), app(comb(k), func(X))):- !.
evalBracketAbs(X, var(X), comb(i)) :- !.
evalBracketAbs(_, var(Y), app(comb(k), var(Y))) :- !.  


unlambdafyH(abs(var(X), Expr), Result) :- unlambdafyH(Expr, Res), evalBracketAbs(X, Res, Result), !.
unlambdafyH(app(M, N), Result) :- 
	unlambdafyH(M,MResult),
	unlambdafyH(N,NResult), 
	Result = (app(MResult, NResult)), !.
unlambdafyH(var(X), var(X)):- !.
unlambdafyH(func(X), func(X)):- !.
unlambdafyH(X, X).

normalize(app(X, Y), Result) :- 
	normalize(X, ResX), 
	append([a], ResX, Inter),
	normalize(Y, ResY),
	append(Inter, ResY, Result), !.
normalize(var(X), [X]) :- !.
normalize(func(X), [X]) :- !.
normalize(comb(X), [X]) :- !.

unlambdafy(Expr, Result) :- unlambdafyH(Expr, Res), normalize(Res, Result).
 
% chained together
evallambda(Expr, Result) :- unlambdafy(Expr, Res), interpret(Res, Result).
