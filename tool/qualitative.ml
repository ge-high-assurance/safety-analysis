(* 

Copyright © 2019 United States Government as represented by the Administrator of the 
National Aeronautics and Space Administration.  All Rights Reserved.

Author: Pete Manolios
Date: 2017-12-15

*)

(**
   The top-level functions for the qualitative manipulation of fault trees are as follows.

   - {b cutsets} : a function for generating the cutsets of a fault tree.

   FORMULAS

   Formulas used in modeling are represented with the {i formula} and {i cformula} types. 
   The top-level functions for manipulating formulas are as follows. 
   Typical users can ignore these functions.

   - {b formulaOfTree} : a function for translating a tree into a formula.

   - {b ssfc} : a function for simplifying formulas.

   - {b sop} : a function for generating the simplest DNF formula equivalent to
   its input.

*)

open Core ;;
open FaultTree;;

exception Error of string ;;
    
(** Code for translating a tree into a formula *)
let rec formulaOfTree t =
  match t with
    | Leaf (var, _, _) -> Var (var) 
    | SUM (tree) -> Sum(List.map tree ~f:formulaOfTree)
    | PRO (tree) -> Pro(List.map tree ~f:formulaOfTree) 
;;
 
 
(* Code for manipulating lists. The idea was to mimic some of the
   simplification of formulas as shown in SAE4671 for generating
   cut sets. Ultimately this code has to be applied to formulas.
*)

(**/**)
(** removeDups removes all duplicates in a list. *)
let rec removeDups l =
  match l with
    | [] -> []
    | hd :: tl ->
      hd :: removeDups (List.filter tl ~f:(fun x -> x<>hd)) ;;

(** subset checks that a is a subset of b: every element in a is in b *)
let subset a b = List.for_all a ~f:(fun x-> List.mem b x ~equal:(=));;

(** ssubset checks that a is a strict subset of b: 
   a is a subset of b and b is not a subset of a *)
let ssubset a b = (subset a b) && (not (subset b a));; 

(** esubset checks that there exists some list in l that is a subset of b *)
let esubset l b = List.exists l ~f:(fun x -> subset x b);;

(** essubset checks that there exists some list in l that is a strict subset of
    b *)
let essubset l b = List.exists l ~f:(fun x -> ssubset x b);;

(** At the top level, minSubets should be called with a single argument, x.  It
    returns the sublist of x, say s, containing minimal lists, i.e., no list in s
    is a subset of any other list in s and for every list in x there is a subset
    of it in s. We use essubset below so that if there are multiple equivalent
    minimal sets in s, the first one is used. *)
let rec minSubsets ?(y = []) x =
  match x with
    | [] -> List.rev y
    | hd::tl ->	if (esubset y hd) || (essubset tl hd) 
      then minSubsets ~y tl
      else minSubsets ~y:(hd::y) tl ;;

(** simplify is calls minSubsets but after removing duplicates *)
let simplify l = minSubsets (List.map l ~f:removeDups) ;;

(* Code for manipulating formulas. *)

(** We start with constant propagation.

    Constants and variables reduce to themselves.

    Empty sums reduce to false.

    Empty products reduce to true.

    Otherwise we recur on the arguments to a sum or product.

    If we have a true as an argument to a sum, we simplify the sum to true.

    If we have a false as an argument to a product, we simplify the product to
    false.

    We remove all false arguments to sum (since false is the identity for sum).

    We remove all true arguments to product (since true is the identity for
    product).
*)
let rec constant_prop e =
  match e with
    | TRUE         -> TRUE
    | FALSE        -> FALSE
    | Var v        -> Var v 
    | Sum ([])     -> FALSE
    | Sum x        ->
      let y=List.map x ~f:constant_prop in
      if (List.mem y ~equal:(=) TRUE)
      then TRUE
      else let w=(List.filter y ~f:(fun z -> z<>FALSE)) in
	   if w=[] then FALSE else Sum w
    | Pro ([])     -> TRUE
    | Pro x        -> 
      let y=List.map x ~f:constant_prop in
      if (List.mem y ~equal:(=) FALSE)
      then FALSE
      else let w=(List.filter y ~f:(fun z -> z<>TRUE)) in
	   if w=[] then TRUE else Pro w
;;

(* Given any formula, after constant_prop, I either wind up with a constant
   (i.e., TRUE or FALSE) or there are no constants in the resulting formula.

   Definition:

   We say that a formula is c-normal if every SUM and PRO is non-empty and
   contains no constants. We say that a formula is normal if it is c-normal and
   it is not a constant.

   This file contains a number of claims. These claims seems plausible, but I
   haven't really thought about them in any depth. They are a first set of
   properties that I think can help proving correctness.

   Claim:

   constant_prop returns a c-normal formula, no matter what it is given as
   input.

   For our application, our expectation is that we are always dealing with
   normal formulas.
*)

(** The definition of a normal formula *)
let rec normalFormula f =
  match f with
    | TRUE         -> false
    | FALSE        -> false
    | Var (_)      -> true 
    | Sum ([])     -> false
    | Sum x        -> List.for_all x ~f:normalFormula
    | Pro ([])     -> false
    | Pro x        -> List.for_all x ~f:normalFormula
;;

(** The definition of a cNormal formula *)
let cNormalFormula f =
  match f with
    | TRUE         -> true
    | FALSE        -> true
    | _ -> normalFormula f 
;;


(** Code for flattening formulas. For example, a formula of the form

   Sum[a; Sum[b; Sum[c; d; e]]]

   gets flattened into a formula of the form

   Sum[a; b; c; d; e]
*)
let rec flatten_formula e =
  match e with
    | Sum x ->
      let y=List.map x ~f:flatten_formula in
      let f=(fun z -> match z with Sum xs -> xs | _ -> [z]) in
      Sum (List.concat (List.map y ~f:f))
    | Pro x -> 
      let y=List.map x ~f:flatten_formula in
      let f=(fun z -> match z with Pro xs -> xs | _ -> [z]) in 
      Pro (List.concat (List.map y ~f:f))
    | _ -> e
;;

(* Note that flatten cannot introduce constants, so:

   Claim: Given normal/c-normal formula, it returns a 
   normal/c-normal formula *)


(** Code for simplifying formulas.

    Consider a formula such as 
    
    (OR (AND a b false) a (OR b a))

    After constant propagation and flattening, I wind up with

    (OR a b a)

    (See the example below.)

    This should get further simplified to 
    
    (OR a b)
    
    Claim:

    The function simplify_formula below implements this kind of simplification.
    Given a normal/c-normal formula, simplify_formula returns a normal/c-normal
    formula.

    Claim (not true):

    Given a flattened, normal/c-normal formula, simplify_formula returns a
    flattened, normal/c-normal formula.

    Reader: construct an example to see why this is not true.
*)
let rec simplify_formula_1 e =
  match e with
    | Sum x        ->
      let y=List.map x ~f:simplify_formula_1 in
      let z=removeDups y in
      if (List.length z) = 1 then List.hd_exn z else Sum z
    | Pro x        -> 
      let y=List.map x ~f:simplify_formula_1 in
      let z=removeDups y in
      if (List.length z) = 1 then List.hd_exn z else Pro z
    | _ -> e ;;

let simplify_formula e =
  simplify_formula_1 (flatten_formula (constant_prop e)) ;;

let psl e =
  match e with
    | Pro x -> x
    | Sum x -> x
    | _ -> [] ;;

let lift_formula e =
  match e with
    | Sum x        ->
      (let pros = List.filter x ~f:(fun z -> match z with | Pro _ -> true | _ -> false) in
       match pros with
	 | [] -> e
	 | p::ps ->
	   (let fvars = List.filter (psl p)
	      ~f:(fun z -> match z with
		| Var(_) -> true
		| _ -> false)
	        in
	    let afvars = List.filter fvars
	      ~f:(fun z ->
		List.for_all ps ~f:(fun w ->
		  match w with | Pro y -> List.mem y ~equal:(=) z | _ -> true) )
	       in
	    match afvars with
	      | [] -> e
	      | _ ->
		(let notpros =
		   List.filter x ~f:(fun z -> match z with | Pro _ -> false | _ -> true) in
		 let nl = List.map pros
		   ~f:(fun z -> match z with
		     | Pro y ->
		       (let n = List.filter y ~f:(fun w -> not (List.mem afvars ~equal:(=) w)) in
			match n with
			  | [] -> TRUE
			  | [a] -> a
			  | _ -> Pro n)
		     | _ -> z)
		    in
		 let spros = Sum(List.append notpros [Pro (List.append afvars [Sum nl])]) in
		 simplify_formula (flatten_formula (constant_prop spros)))))
    | Pro x        ->
      (let sums = List.filter x ~f:(fun z -> match z with | Sum _ -> true | _ -> false) in
       match sums with
	 | [] -> e
	 | s::ss ->
	   (let fvars = List.filter (psl s)
	      ~f:(fun z -> match z with
		| Var(_) -> true
		| _ -> false)
	       in
	    let afvars = List.filter fvars
	      ~f:(fun z ->
		List.for_all ss ~f:(fun w ->
		  match w with | Sum y -> List.mem y ~equal:(=) z | _ -> true) )
 	       in
	    match afvars with
	      | [] -> e
	      | _ ->
		(let notsums =
		   List.filter x ~f:(fun z -> match z with | Sum _ -> false | _ -> true) in
		 let nl = List.map sums
		   ~f:(fun z -> match z with
		     | Sum y ->
		       (let n = List.filter y ~f:(fun w -> not (List.mem afvars ~equal:(=) w)) in
			match n with
			  | [] -> FALSE
			  | [a] -> a
			  | _ -> Sum n)
		     | _ -> z)
		    in
		 let psums = Pro(List.append notsums [Sum (List.append afvars [Pro nl])]) in
		 simplify_formula (flatten_formula (constant_prop psums)))))
    | _ -> e ;;

let f_get_xvars z =
  match z with
    | Var(_) -> true
    | _ -> false ;;

let get_xvars x =
  List.filter x ~f:f_get_xvars ;;

let get_n_xvars x = 
  List.filter x ~f:(fun z -> not (f_get_xvars z)) ;;

let rec remove_nested_var e v c =
  if (e=v) then c
  else (match e with
    | Pro y -> Pro
      (List.map y ~f:(fun x -> remove_nested_var x v c))
    | Sum y -> Sum
      (List.map y ~f:(fun x -> remove_nested_var x v c))
    | _ -> e) ;;

let rec remove_nested_vars e vars c =
  match vars with
    | [] -> e
    | v::vs -> remove_nested_vars (remove_nested_var e v c) vs c ;;

let rec simplify_nested_formula e =
  match e with
    | Sum x  ->
      let xvars = get_xvars x in
      let lxvars = List.length xvars in
      let rst = get_n_xvars x in
      let ne =
	Sum (List.append xvars
	       (List.map rst ~f:(fun x -> remove_nested_vars x xvars FALSE))) in
      let res = simplify_formula ne in
      (match res with
	  Sum y -> if (List.length (get_xvars y)) > lxvars
	    then simplify_nested_formula res
	    else res
	| _ -> simplify_nested_formula res)
    | Pro x        ->
      let xvars = get_xvars x in
      let lxvars = List.length xvars in
      let rst = get_n_xvars x in
      let ne =
	Pro (List.append xvars
	       (List.map rst ~f:(fun x -> remove_nested_vars x xvars TRUE))) in
      let res = simplify_formula ne in 
      (match res with
	  Pro y -> if (List.length (get_xvars y)) > lxvars
	    then simplify_nested_formula res
	    else res
	| _ -> simplify_nested_formula res)
    | _ -> e ;;

let rec simplify_lift_formula e =
  match e with
    | Sum x        ->
      let y=List.map x ~f:simplify_lift_formula in
      let z=removeDups y in
      let w = if (List.length z) = 1 then List.hd_exn z else Sum z in
      let nw=simplify_nested_formula w in
      lift_formula nw
    | Pro x        -> 
      let y=List.map x ~f:simplify_lift_formula in
      let z=removeDups y in
      let w=if (List.length z) = 1 then List.hd_exn z else Pro z in
      let nw=simplify_nested_formula w in
      lift_formula nw
    | _ -> e
;;

let rec simplify_lift_nested_formula e =
  match e with
    | Sum x        ->
      let y=List.map x ~f:simplify_lift_nested_formula in
      let z=removeDups y in
      let w = if (List.length z) = 1 then List.hd_exn z else Sum z in
      let nw=simplify_nested_formula w in
      let l=lift_formula nw in
      simplify_nested_formula l
    | Pro x        -> 
      let y=List.map x ~f:simplify_lift_nested_formula in
      let z=removeDups y in
      let w=if (List.length z) = 1 then List.hd_exn z else Pro z in
      let nw=simplify_nested_formula w in
      let l=lift_formula nw in
      simplify_nested_formula l
    | _ -> e
;;

(** Code for computing a fixpoint *)
let rec fix f x =
  let y = f x in
  if (y=x) then x else fix f y ;;

(** Code for applying the above three transformations constant_prop,
    flatten_formula, and simplify.

    In general one has to apply flatten_formula and simplify_formula multiple
    times in order to reach a fixpoint, i.e., a formula that is simultaneously
    flattened and simplified.

*)
let sfcAux e = fix (fun z -> simplify_formula z) e ;;
let snfcAux e = fix (fun z -> simplify_nested_formula z) e ;;
let slfcAux e = fix (fun z -> simplify_lift_formula (simplify_formula z)) e ;;
let slnfcAux e = fix (fun z -> simplify_lift_nested_formula (simplify_formula z)) e ;;

(** This is a function that given a formula simplifies it by applying constant
    propagation, flattening, removal of duplicate literals and
    subexpressions. This is done until a fixpoint is reached.
*)
let sfc e = sfcAux e ;;
let slfc e = slfcAux  e ;;
let slnfc e = slnfcAux  e ;;

(* Code for determining whether a formula is in sum of products form. *)

(** Compute the depth of a formula *)
let rec depth_formula e =
  match e with
  | TRUE        -> 0
  | FALSE       -> 0
  | Var(_)      -> 0
  | Sum x       -> 
    let y=List.map x ~f:depth_formula in
    1 + List.fold_left y ~init:0 ~f:max
  | Pro x       -> 
    let y=List.map x ~f:depth_formula in
    1 + List.fold_left y ~init:0 ~f:max
;;


(** Checking if a formula is a product of variables or simpler *)
let pp e =
  match e with
  | TRUE   -> true
  | FALSE  -> true
  | Var(_) -> true
  | Pro x  -> List.for_all x ~f:(fun y -> depth_formula y = 0)
  | _      -> false
;;

(** Checking if a formula is a sum of products or simpler *)
let sopp e =
  match e with
  | TRUE   -> true
  | FALSE  -> true
  | Var(_) -> true
  | Sum x  -> List.for_all x ~f:pp 
  | _  -> pp e
;;

(** Code for distributing: needed to put formulas in SOP form *)

let rec insert_dist a acc =
  match acc with
      [] -> []
    | x::xs -> (a::x)::(insert_dist a xs) ;;

let rec dist ?(acc = [[]]) a =
  match a with
      [] -> List.map acc ~f:List.rev
    | x::xs ->
      dist
	~acc:(List.concat (List.map x ~f:(fun z -> insert_dist z acc)))
	xs ;;

(** Called on Pro[Sum(a1); ...; Sum(an)], returns Sum[Pro[dist...]]  Called on
    Sum[Pro(a1); ...; Pro(an)], returns Pro[Sum[dist...]]  The arguments to the
    outermost Sum or Pro can include Vars but not constants. Also the arguments
    cannot be empty Sums or Pros.  If you may have constants, use cdist_formula
    instead.
*)
let dist_formula a =
  match a with
    | Pro b ->
      let c=List.for_all b
	~f:(fun x -> match x with
	  | Var _ -> true
	  | Sum y -> y <> []
	  | _ -> false)
	in
      if c then let d=List.map b
		  ~f:(fun x -> match x with
		    | Var _ -> [x]
		    | Sum y -> y
		    | _ -> assert false)
		  in
		(Sum(List.map (dist d) ~f:(fun x -> Pro x) ))
      else a
    | Sum b ->
      let c=List.for_all b
	~f:(fun x -> match x with
	  | Var _ -> true
	  | Pro y -> y <> []
	  | _ -> false)
	in
      if c then let d=List.map b
		  ~f:(fun x -> match x with
		    | Var _ -> [x]
		    | Pro y -> y
		    | _ -> assert false)
		  in
		(Pro(List.map (dist d) ~f:(fun x -> Sum x) ))
      else a
    | _ -> a;;

(** This is just like dist_formula, except that constant propagation happens
    first.
    
    Claim: cdist_formula always returns a normal formula given a normal formula.
    dist_formula returns a c-normal formula given a c-normal formula.
*)
let cdist_formula a = dist_formula (constant_prop a) ;;


(* Distribution can blow things up.  So, we are going to simplify formulas by
   sorting arguments to sum, pro so that we can avoid some of the problems with
   distribution and we'll remove esubset formulas.
*)


(* Code for comparing formulas.  The idea is that constant before variables
   before sums & pros Wrt sums & pros we compare them by the "tree size" that
   takes length and structure into account.
*)

let rec tree_size f =
  match f with
  | TRUE        -> 1
  | FALSE       -> 1
  | Var(_)      -> 2
  | Sum x       -> 
    let y=List.map x ~f:tree_size in
    3 + List.fold_left y ~init:0 ~f:(+)
  | Pro x       -> 
    let y=List.map x ~f:tree_size in
    3 + List.fold_left y ~init:0 ~f:(+)
;;

let ltree_size l = List.fold_left (List.map l ~f:tree_size) ~init:0 ~f:(+) ;;

let rec first_diff a b =
  match (a, b) with
      ([], []) -> 0
    | ([], _)  -> -1
    | (_, [])  -> 1
    | (x::xs, y::ys) ->
      match form_comp x y with
	  0 -> first_diff xs ys
	| f -> f

(* -1 means f1 is smaller, 0 that it is equal, and 1 that it is bigger *)
and form_comp f1 f2 =
  match (f1, f2) with
      (TRUE, TRUE) -> 0
    | (TRUE, _) -> -1
    | (FALSE, TRUE) -> 1
    | (FALSE, FALSE) -> 0
    | (FALSE, _) -> -1
    | (Var _, TRUE) -> 1
    | (Var _, FALSE) -> 1
    | (Var a, Var b) -> compare a b
    | (Var _, _) -> -1
    | (Sum s, Pro p) ->
      if (ltree_size s) < (ltree_size p)
      then -1 else 1 
    | (Pro p, Sum s) -> 
      if (ltree_size p) <= (ltree_size s)
      then -1 else 1 
    | (Sum s, Sum t) ->
      let tss = ltree_size s  in
      let tst = ltree_size t in
      if tss < tst then -1
      else if tst < tss then 1
      else first_diff s t  
    | (Pro p, Pro q) -> 
      let tsp = ltree_size p  in
      let tsq = ltree_size q in
      if tsp < tsq then -1
      else if tsq < tsp then 1
      else first_diff p q  
    | (Sum _, _) -> 1
    | (Pro _, _) -> 1
;;

let rec sort_form f =
  match f with
    | Sum s -> Sum (removeDups (List.sort ~compare:form_comp (List.map s ~f:sort_form)))
    | Pro p -> Pro (removeDups (List.sort ~compare:form_comp (List.map p ~f:sort_form)))
    | _     -> f
;;

let ssfcAux e =
  fix (fun z -> sort_form (simplify_formula z)) e;;

let ssnfcAux e =
  fix (fun z -> sort_form (simplify_nested_formula z)) e;;

let sslfcAux e =
  fix (fun z -> sort_form (simplify_lift_formula (simplify_formula z))) e;;

let sslnfcAux e =
  fix (fun z -> sort_form (simplify_lift_nested_formula (simplify_formula z))) e;;


(**/**)
(** This is a function that given a formula simplifies it by applying constant
    propagation, flattening, removal of duplicate literals and subexpressions,
    and also sorts the resulting formula so that simpler terms are shown
    first. This is done until a fixpoint is reached.
*)
let ssfc e = ssfcAux (constant_prop e) ;;
let ssnfc e = ssnfcAux (constant_prop e) ;;
let sslfc e = sslfcAux (constant_prop e) ;;
let sslnfc e = sslnfcAux (constant_prop e) ;;

(**/**)
let subsume_form x a =
  match (x, a) with
    | (Sum s, Sum t) -> subset s t
    | (Pro p, Pro q) -> subset p q
    | (Var _, Sum s) -> List.mem s ~equal:(=) x
    | (Var _, Pro p) -> List.mem p ~equal:(=) x
    | _ -> (ssfc x)=(ssfc a) 
;;

let subsumed_form a f =
  List.exists f ~f:(fun x -> subsume_form x a);;

(** simp_pro_sum is really the guts of the absorption 
*)
let rec simp_pro_sum ?(y = []) f =
  match f with
    | [] -> List.rev y
    | hd::tl ->	if (subsumed_form hd tl) || (subsumed_form hd y)
      then simp_pro_sum ~y tl
      else simp_pro_sum ~y:(hd::y) tl ;;

let rec simplify_lift_abs_formula e =
  match e with
    | Sum x        ->
      let y=List.map x ~f:simplify_lift_abs_formula in
      let z=removeDups y in
      let w=if (List.length z) = 1 then List.hd_exn z else Sum z in
      let nw=simplify_nested_formula w in
      let l=sslfc (lift_formula nw) in
      (match l with
	| Pro s -> sslfc (Pro (simp_pro_sum s))
	| Sum s -> sslfc (Sum (simp_pro_sum s))
	| _ -> l)
    | Pro x        -> 
      let y=List.map x ~f:simplify_lift_abs_formula in
      let z=removeDups y in
      let w=if (List.length z) = 1 then List.hd_exn z else Pro z in
      let nw=simplify_nested_formula w in
      let l=sslfc (lift_formula nw) in
      (match l with
	| Pro s -> sslfc (Pro (simp_pro_sum s))
	| Sum s -> sslfc (Sum (simp_pro_sum s))
	| _ -> l)
    | _ -> e
;;

let nsimpAux e =
  sort_form (simplify_lift_abs_formula (simplify_formula e));;

let nsimp e = fix nsimpAux (constant_prop e);;

let rec simplify_lift_abs_nested_formula e =
  match e with
    | Sum x        ->
      let y=List.map x ~f:simplify_lift_abs_nested_formula in
      let z=removeDups y in
      let w=if (List.length z) = 1 then List.hd_exn z else Sum z in
      let nw=simplify_nested_formula w in
      let l=sslnfc (lift_formula nw) in
      let l2=
	(match l with
	  | Pro s -> sslnfc (Pro (simp_pro_sum s))
	  | Sum s -> sslnfc (Sum (simp_pro_sum s))
	  | _ -> l) in
      sslnfc (simplify_nested_formula l2)
    | Pro x        -> 
      let y=List.map x ~f:simplify_lift_abs_nested_formula in
      let z=removeDups y in
      let w=if (List.length z) = 1 then List.hd_exn z else Pro z in
      let nw=simplify_nested_formula w in
      let l=sslnfc (lift_formula nw) in
      let l2=
	(match l with
	  | Pro s -> sslnfc (Pro (simp_pro_sum s))
	  | Sum s -> sslnfc (Sum (simp_pro_sum s))
	  | _ -> l) in
      sslnfc (simplify_nested_formula l2)
    | _ -> e
;;

let try_f_no_depth f x =
  let a=f x in
  if (depth_formula a) > (depth_formula x) then x else a ;;

let try_f_no_depth_sop f x =
  let a=f x in
  let top_a = (match a with
    | Sum _ -> "Sum"
    | Pro _ -> "Pro"
    | _ -> "Other") in
  let top_x = (match x with
    | Sum _ -> "Sum"
    | Pro _ -> "Pro"
    | _ -> "Other") in
  let da = depth_formula a in
  let dx = depth_formula x in
  if (top_x = "Sum") && (top_a = "Pro") then
    (if da >= dx then x else a)
  else if top_x = "Sum" then
    (if da <= dx then a else x)
  else if da <= dx + 1 then a else x ;;

let update_f_sl f =
  match f with
    | Pro s ->
      let a=Pro (simp_pro_sum s) in
      try_f_no_depth sslnfc a 
    | Sum s -> 
      let a=Sum (simp_pro_sum s) in
      try_f_no_depth sslnfc a 
    | _ -> f ;;

let update_f_sl_sop f =
  match f with
    | Pro s ->
      let a=Pro (simp_pro_sum s) in
      try_f_no_depth_sop sslnfc a 
    | Sum s -> 
      let a=Sum (simp_pro_sum s) in
      try_f_no_depth_sop sslnfc a 
    | _ -> f ;;

let rec simplify_lift_abs_nested_formula_nodi e =
  match e with
    | Sum x        ->
      let y=List.map x ~f:simplify_lift_abs_nested_formula_nodi in
      let z=removeDups y in
      let w=if (List.length z) = 1 then List.hd_exn z else Sum z in
      let nw=simplify_nested_formula w in
      let l= try_f_no_depth sslnfc (try_f_no_depth lift_formula nw) in
      let l2= update_f_sl l in 
      let c = simplify_nested_formula l2 in
      try_f_no_depth sslnfc c 
    | Pro x        -> 
      let y=List.map x ~f:simplify_lift_abs_nested_formula_nodi in
      let z=removeDups y in
      let w=if (List.length z) = 1 then List.hd_exn z else Pro z in
      let nw=simplify_nested_formula w in
      let l= try_f_no_depth sslnfc (try_f_no_depth lift_formula nw) in
      let l2= update_f_sl l in 
      let c = simplify_nested_formula l2 in
      try_f_no_depth sslnfc c 
    | _ -> e
;;

let rec simplify_lift_abs_nested_formula_nodi_safe_sop e =
  match e with
    | Sum x        ->
      let y=List.map x ~f:simplify_lift_abs_nested_formula_nodi_safe_sop in
      let z=removeDups y in
      let w=if (List.length z) = 1 then List.hd_exn z else Sum z in
      let nw=simplify_nested_formula w in
      let l= try_f_no_depth_sop sslnfc (try_f_no_depth_sop lift_formula nw) in
      let l2= update_f_sl_sop l in 
      let c = simplify_nested_formula l2 in
      try_f_no_depth_sop sslnfc c
    | Pro x        -> 
      let y=List.map x ~f:simplify_lift_abs_nested_formula_nodi_safe_sop in
      let z=removeDups y in
      let w=if (List.length z) = 1 then List.hd_exn z else Pro z in
      let nw=simplify_nested_formula w in
      let l= try_f_no_depth_sop sslnfc (try_f_no_depth_sop lift_formula nw) in
      let l2= update_f_sl_sop l in 
      let c = simplify_nested_formula l2 in
      try_f_no_depth_sop sslnfc c 
    | _ -> e
;;

let nnsimpAux e =
  sort_form (simplify_lift_abs_nested_formula (simplify_formula e));;

let nnsimp e = fix nnsimpAux (constant_prop e);;

let nndsimpAux e =
  sort_form (simplify_lift_abs_nested_formula_nodi (simplify_formula e));;

let nndsimp e = fix nndsimpAux (constant_prop e);;

let nndsopsimpAux e =
  sort_form (simplify_lift_abs_nested_formula_nodi_safe_sop (simplify_formula e));;

let nndsopsimp e = fix nndsopsimpAux (constant_prop e);;

let rec getSum l =
  match l with
    | [] -> raise (Error "getSum: There should be a sum in here")
    | f::fs -> (match f with
	| Sum _ -> f
	| _ -> getSum fs) ;;

let rec butSum ?(acc = []) l =
  match l with
    | [] -> raise (Error "butSum: There should be a sum in here")
    | f::fs -> (match f with
	| Sum _ -> List.append (List.rev acc) fs
	| _ -> butSum ~acc:(f::acc) fs) ;;
  
(** sumofprod has to be called on a simplified formula.  In particular, it has
    to be flattened, otherwise you can get erroneous results. So, use sop
    instead, which does not have this restriction

    If the input is simplified, sumofprod returns a simplified formula.
*)
let rec sumofprod f =
  if (sopp f) then f
  else match f with
    | Sum s ->
      let s0 = simp_pro_sum s in
      let s1 = (List.map s0 ~f:sumofprod) in
      Sum s1
    | Pro s ->
      let s0 = simp_pro_sum s in
      let s1 = List.map s0 ~f:(fun x -> nndsopsimp (sumofprod x)) in
      let f0 = ssnfc (Pro s1) in
      (match f0 with
	| Pro z ->
	  if (List.length z >= 2) then
	    (let f1 = dist_formula
	       (Pro [List.nth_exn z 0; getSum (List.tl_exn z)]) in
	     let bsz = butSum (List.tl_exn z) in
	     if (bsz = []) then
	       sumofprod (ssnfc f1)
	     else
	       let f2 = sort_form (Pro ((ssnfc f1)::bsz)) in
	       sumofprod f2)
	  else raise (Error "sumofprod")
	| _ -> sumofprod f0)
    | _ -> f ;;

let sopAux f =
  let f0 = nnsimp f in
  let f1 = sumofprod f0 in
  nndsopsimp f1 ;;

(**/**)
(** Given any monotone Boolean formula, sop returns a simplified formula in DNF
    (sum of products form). The expectation is that it returns the simplest
    equivalent DNF formula.
*)
let sop f = fix sopAux f ;;

(** A function that given a fault tree generates the cutsets of the tree. *)
let cutsets t = sop (formulaOfTree t);;

let rec addSubsume c d =
  match d with
    | [] -> [c]
    | hd::tl -> if (subset hd c)
      then d
      else if (subset c hd)
      then addSubsume c tl
      else hd::(addSubsume c tl) ;;

let rec appSubsume x y =
  match x with
    | [] -> y
    | hd::tl -> addSubsume hd (appSubsume tl y) ;;

let addNoDup e l = if List.mem l e ~equal:(=) then l else e::l ;;

let rec appNoDup x y =
  match x with
    | [] -> y
    | hd::tl -> addNoDup hd (appNoDup tl y) ;;

let rec mocusL l p =
  match l with
    | [] -> []
    | hd::tl ->
      (match hd with
	| Var v -> mocusL tl (hd::p)
	| Pro s -> [(appNoDup s (List.append p tl))]
	| Sum s -> List.map s ~f:(fun z -> addNoDup z (List.append p tl))) ;; 

let loVarp l = List.for_all l ~f:(fun z -> match z with
  | Var v -> true
  | _ -> false) ;;

let rec mocusAux f acc =
  match f with
    | [] -> acc
    | hd::tl -> if loVarp hd
      then mocusAux tl (addSubsume hd acc)
      else mocusAux (appSubsume (mocusL hd []) tl) acc ;;

let mocus f = mocusAux [[f]] [] ;;

let mocusTree t = mocus (nnsimp (formulaOfTree t));;

(** A function that given a fault tree generates the cutsets of the
    tree using the MOCUS algorithm. *)

let mcutsets t =
  let m = mocusTree t in
  let f = Sum (List.map m ~f:(fun z -> Pro z)) in
  nndsopsimp f ;;

