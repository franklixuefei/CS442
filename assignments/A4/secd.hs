data Primitive = Succ | IsZero deriving Show

data Term = Var String | Abs String Term | App Term Term
          | Prim Primitive | INT Int

instance Show Term where
	show (Var x) = x
--	show (Abs x t) = "(\\x."++show(t)++")"
--	show (App t1 t2) = "("++show(t1)++" "++show(t2)++")"
	show (Abs x (App y t)) = "\\"++x++"."++"("++show(App y t)++")"
	show (Abs x t) = "\\"++x++"."++show(t)
	show (App (App a b) (App c d)) = show(App a b)++"("++show(App c d)++")"
	show (App (App a b) (Abs x t)) = show(App a b)++show(Abs x t)
	show (App (App a b) (Var x)) = show(App a b)++" "++show(Var x)
	show (App (Abs x t) (App a b)) = "("++show(Abs x t)++")"++"("++show(App a b)++")"
	show (App (Abs x t) (Abs y e)) = "("++show(Abs x t)++")"++show(Abs y e)
	show (App (Abs x t) (Var y)) = "("++show(Abs x t)++")"++show(Var y)
	show (App (Var x) (App a b)) = show(Var x)++"("++show(App a b)++")"
	show (App (Var x) (Abs y t)) = show(Var x)++show(Abs y t)
	show (App (Var x) (Var y)) = show(Var x)++" "++show(Var y)
	show (Prim p) = show p
	show (INT i) = show i

instance Show SECDConfig where
  show (SECD (s,e,c,d)) =
    "S = "++show(s)++"\n"++
    "E = "++show(e)++"\n"++
    "C = "++show(c)++"\n"++
    "D = "++show(d)++"\n"

applyPrim :: Primitive -> Term -> Term
applyPrim Succ (INT n) = INT (n+1)
applyPrim IsZero (INT n)
	| n == 0 = Abs "x" (Abs "y" (Var "x"))
	| otherwise = Abs "x" (Abs "y" (Var "y"))

data SContents = Scl (String, Term, [EContents]) | St Term
data EContents = Env (String, SContents)
data CContents = Apply | Ctrl Term
data DContents = Dump ([SContents], [EContents], [CContents])

instance Show SContents where
	show (Scl (s, t, e)) = "<"++s++","++show(t)++","++show(e)++">"
	show (St t) = show(t)
	
instance Show EContents where
	show (Env(x, s)) = "<"++x++","++show(s)++">"

instance Show CContents where
	show (Apply) = "@"
	show (Ctrl t) = show(t)

instance Show DContents where
	show (Dump (s, e, c)) = "<"++show(s)++","++show(e)++","++show(c)++">"

data SECDConfig = SECD ([SContents], [EContents], [CContents], [DContents])

lookUp :: (String, [EContents]) -> SContents
lookUp (x, Env(k,v):envs)
	| x == k = v
	| otherwise = lookUp(x,envs)

secdOneStep :: SECDConfig -> SECDConfig
secdOneStep (SECD (s, e, (Ctrl (Var x)):c', d)) = 
	SECD ((lookUp (x,e)):s, e, c', d)
secdOneStep (SECD (s, e, (Ctrl (Abs x m)):c', d)) = 
	SECD ((Scl (x, m, e)):s, e, c', d)
secdOneStep (SECD (s, e, (Ctrl (App m n)):c', d)) =
	SECD (s, e, (Ctrl n):(Ctrl m):Apply:c', d)
secdOneStep (SECD (s, e, (Ctrl (Prim prim)):c', d)) =
	SECD ((St (Prim prim)):s, e, c', d)
secdOneStep (SECD (s, e, (Ctrl (INT i)):c', d)) =
	SECD ((St (INT i)):s, e, c', d)
secdOneStep (SECD ((St (Prim prim)):(St n):s', e, Apply:c', d)) =
	SECD (s', e, (Ctrl (applyPrim prim n)):c', d)
secdOneStep (SECD ((Scl (x, m, e1)):n:s', e, Apply:c', d)) = 
	SECD ([], (Env (x, n)):e1, [Ctrl m], (Dump (s', e, c')):d)
secdOneStep (SECD ([m], e, [], (Dump (s', e', c')):d')) =
	SECD (m:s', e', c', d')
secdOneStep (SECD ([m], e, [], [])) =
	SECD ([m], e, [], [])

subst :: Term -> EContents -> Term
subst (Var x) (Env(y, s))
	| x == y = scltoterm(s)
	| otherwise = Var x
subst	(Abs x e) (Env(y, s))
	| x == y = Abs x e
	| otherwise = Abs x (subst e (Env(y, s)))
subst (App m n) e =
	App (subst m e) (subst n e)
	
scltoterm :: SContents -> Term
scltoterm (St t) = t
scltoterm (Scl (x, m, e)) = foldl subst (Abs x m) e

runSECD :: SECDConfig -> SECDConfig
runSECD (SECD (s, e, [], [])) = SECD (s, e, [], [])
runSECD (SECD (s, e, c, d)) = runSECD (secdOneStep (SECD (s, e, c, d)))


reduce  :: Term -> Term
reduce t = scltoterm (unwrapStack (runSECD (SECD ([], [], [Ctrl t], [])))) where
	unwrapStack (SECD ([m], _, _, _)) = m
	
--Testing
--main = print (secdOneStep (SECD ([], [], [Ctrl (App (Abs "z" (Var "z")) (App (Prim IsZero) (INT 2)))], [])))

