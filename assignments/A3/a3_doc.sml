(* Start of Part C Program Suite*)

(* Name Not Found *)

Var "c"
--------------------------------------------
- W initenv (Var "c");

uncaught exception LookupFailedNameNotFound
  raised at: a3.sml:53.38-53.62
--------------------------------------------




(* Circularity *)

Abs ("x", App(Var "x", Var "x"))
------------------------------------------------
- W initenv (Abs ("x", App(Var "x", Var "x")));

uncaught exception OccursCheckFailedCircularity
  raised at: a3.sml:109.41-109.69
------------------------------------------------




(* Type Mismatch *)

App (Int 1, Bool true)
------------------------------------------------
- W initenv (App (Int 1, Bool true));

uncaught exception UnificationFailedTypeMismatch
  raised at: a3.sml:120.25-120.54
------------------------------------------------




(* Exception Type Mismatch *)

Handle(Let("x", Abs("c", Var "c"), App(Var "x", Bool true)),
              App( App (Prim Add, App(App (Var "safediv", Int 8), Int 4)),
              							App(App (Var "safediv", Int 3), Int 0)),
              Int 0)
--------------------------------------------------------------------------
- W initenv (Handle(Let("x", Abs("c", Var "c"), App(Var "x", Bool true)),
              App( App (Prim Add, App(App (Var "safediv", Int 8), Int 4)),
              							App(App (Var "safediv", Int 3), Int 0)),
              Int 0));

uncaught exception UnificationFailedTypeMismatch
  raised at: a3.sml:158.13-158.42
--------------------------------------------------------------------------

(* End of Part C Program Suite*)
