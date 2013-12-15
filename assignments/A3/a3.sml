(*  Starting point for CS 442/642 W13 Assignment 3

   These datatype declarations form an abstract syntax for Milner
   expressions.  All inputs to the type-inferencer are assumed to be
   syntactically valid Milner programs.  *)

datatype prim = Add | Neg | Mult | Div | And | Or | Not | Eq | Lt | Gt

datatype milner = Var of string
                | Abs of string * milner
                | App of milner * milner
                | If of milner * milner * milner
                | Let of string * milner * milner
                | Fix of string * milner
                | Int of int
                | Bool of bool
                | Prim of prim
                | Raise of milner 							(* Part D *)
                | Letex of string * milner 				(* Part D *)
                | Handle of milner * milner * milner (* Part D *)

datatype mtype = TInt | TBool | TVar of string | TAbs of string * mtype
               | Arrow of mtype * mtype | TExc (*this is the exception type*)

(* Generating Type Variables:
   We reserve varables of the form Zn, n an integer, n>=0. *)
val counter = ref 0

fun newtype () = 
   "Z" ^ Int.toString(!counter before counter := !counter + 1)


(* Environment: mapping from names to types *)

type env = string -> mtype


(* Start of Part A *)
fun pptype TInt = "int"
|   pptype TBool = "bool"
|   pptype TExc = "expn"
|   pptype (TVar x) = "'" ^ x
|   pptype (Arrow(a, b)) = "(" ^ pptype(a) ^ " -> " ^ pptype(b) ^ ")"
|   pptype (TAbs (x, e)) = "(FORALL " ^ x ^ " " ^ pptype(e) ^ ")"
(* End of Part A *)

(* Start of Part B, Part D *)
fun updateEnv ENV (x:string) (t:mtype) name = 
	if x = name then t else (ENV name)

exception LookupFailedNameNotFound

fun emptyenv(x:string):mtype = raise LookupFailedNameNotFound

(*
1. addition: add: int -> (int -> int)
2. numeric negation: neg: int -> int
3. multiplication: mult: int -> (int -> int) 
4. division: div: int -> (int -> int)
5. conjunction: and: bool -> (bool -> bool) 
6. disjunction: or: bool -> (bool -> bool)
7. logical negation: not: bool -> bool
8. numeric equality: eq: int -> (int -> bool) 
9. less than: lt: int -> (int -> bool)
10. greater than: gt: int -> (int -> bool)
11. raise exception: raise: FORALL alpha (alpha -> alpha)  (* Part D *)
*)

val intToIntInt = Arrow(TInt, Arrow(TInt, TInt))
val intToInt = Arrow(TInt, TInt)
val boolToBool = Arrow(TBool, TBool)
val boolToBoolBool = Arrow(TBool, Arrow(TBool, TBool))
val intToIntBool = Arrow(TInt, Arrow(TInt, TBool))
val generalType = 
	let
		val newVarStr = (newtype())
	in
		TAbs(newVarStr, (TVar newVarStr))
	end

val initenv = updateEnv (updateEnv (updateEnv (updateEnv (updateEnv (updateEnv (updateEnv (updateEnv (updateEnv (updateEnv (updateEnv emptyenv "add" intToIntInt) "neg" intToInt) "mult" intToIntInt) "div" intToIntInt) "and" boolToBoolBool) "or" boolToBoolBool) "not" boolToBool) "eq" intToIntBool) "lt" intToIntBool) "gt" intToIntBool) "raise" generalType;

fun emptySubst(t:mtype) = t

fun subst t a TInt = TInt
|   subst t a TBool = TBool
|   subst t a TExc = TExc
|	 subst t a (TVar b) =
		if a=b then t else (TVar b)
|   subst t a (TAbs (typevar_string, mtype)) = 
		if a=typevar_string then	(TAbs (typevar_string, mtype)) else (TAbs (typevar_string, (subst t a mtype)))
|   subst t a (Arrow (t1, t2)) = (Arrow ((subst t a t1), (subst t a t2)))


exception OccursCheckFailedCircularity
exception UnificationFailedTypeMismatch

fun occurs_check(t, TVar a) = (a = t)
|   occurs_check(t, TAbs (a, mtype)) = false
|   occurs_check(t, Arrow(a1, a2)) = occurs_check(t, a1) orelse occurs_check(t, a2)
|   occurs_check(t, (TBool|TInt|TExc)) = false

(* TAbs is impossible to occur in unify because lookup will never return a TAbs type so t in (s, t) will never contain a TAbs, and subst is therefore not possible to have a TAbs as parameter. So don't need to worry about e.g. s1 o A - a bound var in A will never be replaced. For example, (for all a, a->b->a)[c/a] => (for all a, a->b->a); but (for all a, a->b->a)[c/b] => (for all a, a->c->a) *)
fun unify(TInt, TInt) = emptySubst
|   unify(TBool, TBool) = emptySubst
|   unify(TExc, TExc) = emptySubst
|   unify(TVar a, t) = 
		if TVar a = t then emptySubst
		else if occurs_check(a, t) then raise OccursCheckFailedCircularity
		(*else if t = TExc then raise UnificationFailedTypeMismatch*)
		else subst t a (* substitute t for a: [t/a] *)
|   unify(t, TVar a) = unify(TVar a, t)
|   unify(Arrow(t1, t2), Arrow(t3, t4)) =
		let 
			val s1=unify(t1, t3)
			val s2=unify(s1 t2, s1 t4)
		in	 
			s2 o s1
		end
|   unify(_, _) = raise UnificationFailedTypeMismatch

(* a list of TVars (maybe TInts and TBools) *)
val freevarlist:mtype list = [];

fun eq (tar:mtype) (elem:mtype) = (tar = elem)

fun exists f l =
	if (null l) then false
	else if (f (hd l)) then true
	else exists f (tl l)

fun pushTVars TVAR FREEVARLIST = 
	if (exists (eq TVAR) FREEVARLIST)=false then TVAR :: FREEVARLIST
	else FREEVARLIST

(* output a list of variables that not occuring free (string in TVars) for use of quantify *)
fun getNotOccuringFreeVars (TVar a) FREEVARLIST = 
		if (exists (eq (TVar a)) FREEVARLIST) then []
		else [a]
|	 getNotOccuringFreeVars (TInt|TBool|TExc) FREEVARLIST = []
(* the following case is impossible *)
|	 getNotOccuringFreeVars (TAbs (x, e)) FREEVARLIST = x :: (getNotOccuringFreeVars e FREEVARLIST)
| 	 getNotOccuringFreeVars (Arrow(t1, t2)) FREEVARLIST = (getNotOccuringFreeVars t1 FREEVARLIST) @ (getNotOccuringFreeVars t2 FREEVARLIST)

fun quantify t notoccuringfreevarlist =
	if (null notoccuringfreevarlist) then t
	else (TAbs (hd notoccuringfreevarlist, quantify t (tl notoccuringfreevarlist)))

fun disquantify (TAbs (x, e)) = ((subst (TVar (newtype())) x) (disquantify e))
|	 disquantify (TVar a) = (TVar a)
|	 disquantify TInt = TInt
|	 disquantify TBool = TBool
|	 disquantify TExc = TExc
|	 disquantify (Arrow (t1, t2)) = (Arrow (t1, t2))

fun typeAssert (t1:mtype,t2:mtype) =
	if t1 = t2 then true
	else raise UnificationFailedTypeMismatch

(* Helper for W *)	
fun W' A F (Var x) = 
	let
		val sigma = (A x)
		val t = (disquantify sigma)
	in
		(emptySubst, t)
	end
|	 W' A F (Abs (x, e)) = 
		let
			val newVar = (TVar (newtype()))
			val (s, t) = (W' (updateEnv A x newVar) (pushTVars newVar F) e)
		in
			(s, Arrow(s newVar, t))
		end
|	 W' A F (App (e1, e2)) =
		let
			val newVar = (TVar (newtype()))
			val (s1, t1) = (W' A F e1)
			val (s2, t2) = (W' (s1 o A) (map s1 F) e2)
			val s3 = unify(s2 t1, Arrow(t2, newVar))
		in
			(s3 o s2 o s1, s3 newVar)
		end
|	 W' A F (If (e1, e2, e3)) =
		let
			val (s1, t1) = (W' A F e1)
			val s2 = unify(TBool, t1)
			val (s3, t3) = (W' (s2 o s1 o A) (((map s2) o (map s1)) F) e2)
			val (s4, t4) = (W' (s3 o s2 o s1 o A) (((map s3) o (map s2) o (map s1)) F) e3)
			val s5 = unify(s4 t3, t4)
		in
			(s5 o s4 o s3 o s2 o s1, s5 t4)
		end
| 	 W' A F (Fix (x, e)) =
		let
			val newVar = (TVar (newtype()))
			val (s1, t1) = (W' (updateEnv A x newVar) (pushTVars newVar F) e)
			val s2 = unify(s1 newVar, t1)
		in
			(s2 o s1, (s2 o s1) newVar)
		end
| 	 W' A F (Let (x, e1, e2)) =
		let
			val (s1, t1) = (W' A F e1)
			val sigma = (quantify t1 (getNotOccuringFreeVars t1 (map s1 F)))
			val (s2, t2) = (W' (updateEnv (s1 o A) x sigma) (map s1 F) e2)
		in
			(s2 o s1, t2)
		end
| 	 W' A F (Handle (e1, e2, e3)) = 
		let
			val (s1, t1) = (W' A F e1)
			val isSame = typeAssert (TExc, t1) (* if t1 is not TExc, an exception will raise *)
			val s2 = unify(TExc, t1)
			val (s3, t3) = (W' (s2 o s1 o A) (((map s2) o (map s1)) F) e2)
			val (s4, t4) = (W' (s3 o s2 o s1 o A) (((map s3) o (map s2) o (map s1)) F) e3)
			val s5 = unify(s4 t3, t4)
		in
			(s5 o s4 o s3 o s2 o s1, s5 t4)
		end
| 	 W' A F (Letex (x, e)) =
		let
			val (s, t) = (W' (updateEnv A x TExc) F e)
		in
			(s, t)
		end
|	 W' A F (Int (i:int)) = (emptySubst, TInt)
| 	 W' A F (Bool (b:bool)) = (emptySubst, TBool)
| 	 W' A F (Prim Add) = (W' A F (Var "add"))
| 	 W' A F (Prim Neg) = (W' A F (Var "neg"))
| 	 W' A F (Prim Mult) = (W' A F (Var "mult"))
| 	 W' A F (Prim Div) = (W' A F (Var "div"))
| 	 W' A F (Prim And) = (W' A F (Var "and"))
| 	 W' A F (Prim Or) = (W' A F (Var "or"))
| 	 W' A F (Prim Not) = (W' A F (Var "not"))
| 	 W' A F (Prim Eq) = (W' A F (Var "eq"))
| 	 W' A F (Prim Lt) = (W' A F (Var "lt"))
| 	 W' A F (Prim Gt) = (W' A F (Var "gt"))
|	 W' A F (Raise e) = 
		let
			val (s1, t1) = (W' A F e)
			val isSame = typeAssert (TExc, t1) (* if t1 is not TExc, an exception will raise *)
		in		
			(W' A F (Var "raise"))
		end

(* W:  Accepts the arguments A (environment) and E (expression).
    Returns the type of E in A.  *)

fun W A E = (W' A freevarlist E)

	
