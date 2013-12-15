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

datatype mtype = TInt | TBool | TVar of string
               | Arrow of mtype * mtype

(* Generating Type Variables:
   We reserve varables of the form Zn, n an integer, n>=0. *)
val counter = ref 0

fun newtype () = 
   "Z" ^ Int.toString(!counter before counter := !counter + 1)


(* Environment: mapping from names to types *)

(* type env =  ... *)


(* W:  Accepts the arguments A (environment) and E (expression).
    Returns the type of E in A.  *)
(* fun W A E = ... *)

