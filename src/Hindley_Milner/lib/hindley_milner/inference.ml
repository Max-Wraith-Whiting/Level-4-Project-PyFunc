(* ENVIRONMENT *)
(* The general idea of this is to be the Γ in the type judgements.
   It's basically a map that holds type information of varibles (likely just values initially...) *)


(* Method to annotate expressions:
   Should take the expression and the context and return the typed expression *)

(* Method to collect constraints from an expression, returning a list of said constraints *)

(* Unification algorithm *)

(* Apply the subs *)

(* Method to convert constraints into substitutions  *)

(* Method for infering ontop of a parameterised type *)

(* ALG IMPLEMENTATION *)
(* 1. Annotate the exprs with place holders *)
(* 2. Generate constraints *)
(* 3. unify based on constraints *)
(* 4. run the substitutions on unresolved types *)
(* 5. Profit??? Or rather we get the final internally annotated expression *)

(* Will have to use mutable ref for algorithm J... Got to figure out how to do this. *)