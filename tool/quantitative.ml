(* 

Copyright © 2019 United States Government as represented by the Administrator of the 
National Aeronautics and Space Administration.  All Rights Reserved.

Author: Pete Manolios
Date: 2017-12-15

*)

(**
   The top-level functions for the quantitative analysis of fault trees are as follows.

   - {b probErrorCut} : a function that given a fault tree, computes the
   top-level probability of a fault.

   - {b probErrorCutImp} : a function that given a fault tree, computes
   importance measures.
   
   If cutsets are available, the next two functions can compute probabilities
   and importance metrics without having to recompute them.
   
   - {b probErrorCutC} : a function that given a fault tree and cutsets,
   computes the top-level probability of a fault.

   - {b probErrorCutCImp} : a function that given a fault tree and cutsets,
   computes importance measures.

*)

open Core ;;
open FaultTree ;;
open Qualitative ;;

(**/**)
(** A function to compute the probability of failure using an approximate
    method. The probability of failure is lambda * exposure time, which for
    reasonable values of lamba, exposure time is a good approximation.  Also, the
    assumption is that all events are independent and for sums, I use addition,
    another approximation. Notice that these approximations are conservative, so
    that I always get a probability that is >= the true probability.

    I do not handle the case where the same event appears in the fault tree. To
    deal with that, one has to use the cut set methods, which appear below.
*)
let rec probErrorApprox t =
  match t with
    | Leaf (_, l, e) -> l *. e
    | SUM (tree) ->
      List.fold_left (List.map tree ~f:probErrorApprox) ~init:0. ~f:( +. ) 
    | PRO (tree) ->
      List.fold_left (List.map tree ~f:probErrorApprox) ~init:1. ~f:( *. )  
;;

(** A function to compute the probability of failure using an exact method. The
    probability of failure is
    
    1-e^(-lambda * exposure time). 

    Also, the assumption is that all events are independent and for sums, I use
    the correct formula. Since I'm assuming independence, I can simplify the
    inclusion/exclusion formula as follows:

    P[Sum E] = 1-P[Pro E'] = 1 - *_{i in 1..n}P[ei']
    = 1 - *_{i in 1..n}(1-P[ei]) 
*)
let probDisjunction l =
  1. -. (List.fold_left 
	   (List.map ~f:(fun x -> 1. -. x) l)
	   ~init:1.
	   ~f:( *. )) ;;
  
let rec probError t =
  match t with
    | Leaf (_, l, e) -> 1.0 -. (exp (-. (l *. e)))
    | SUM (tree) ->
      probDisjunction (List.map tree ~f:probError)
    | PRO (tree) ->
      List.fold_left (List.map tree ~f:probError) ~init:1. ~f:( *. )  
;;

let rec eventProbsAux t =
  match t with
    | Leaf (var, _, _) -> [ (var, (probError t)) ] 
    | SUM s -> List.concat (List.map s ~f:eventProbsAux)
    | PRO p -> List.concat (List.map p ~f:eventProbsAux)
;;

(* There can be repeated events *)
let eventProbs t = removeDups (eventProbsAux t) ;;

let rec nexte e n =
  match e with
    | [] -> []
    | [a] -> if a=n then [] else [a+1]
    | a::x -> if a=n then
	let v = nexte x (n-1) in
	match v with
	  | [] -> []
	  | a::_ -> (a+1)::v
      else (a+1)::x ;;

let rec chooseiA ?(acc = []) e n =
  let e1 = nexte e n in
  if e1=[] then List.rev (e::acc)
  else chooseiA ~acc:(e::acc) e1 n ;;

let choosei i s =
  let l = List.length s in
  if (i<1 || i>l) then raise (Invalid_argument "i>l in choosei")
  else let e = List.rev (List.concat (chooseiA [0] (i-1))) in
       let c = chooseiA e (l-1) in
       List.map c ~f:(fun x -> (List.map x ~f:(fun y -> List.nth_exn s y))) ;;

let chooseiC i c = 
  let x = choosei i c in
  List.map x ~f:(fun y -> removeDups (List.concat y));;

let computeSopProb l pa =
  List.fold_left 
  (List.map l
     ~f:(fun y ->
       List.fold_left (List.map y ~f:(fun x -> List.Assoc.find_exn pa ~equal:(=) x) ) ~init:1. ~f:( *. ) )
  )
  ~init:0. ~f:( +. )
;;

let rec chs n k =
  if k=1 then (float n)
  else let n1 = (float (n+1)) in
       let k1 = (float k) in
       let t = (n1 -. k1) /. k1 in
       (chs n (k-1)) *. t ;;

(** This is the largest number of elements a subset of a cutset can contain. If
    we exceed this number, we stop calculating the probabilities and therefore
    return an interval which contains the true probability. *)
let max_choose_size = 100000. ;;

let rec probCutSop l pa len i m ans1 ans2 =
  if (chs len i) > max_choose_size then
    let a1 = min ans1 ans2 in
    let a2 = max ans1 ans2 in
    (a1, a2) 
  else if i=len then
    let ans = ans2 +. (m *. (computeSopProb (chooseiC i l) pa)) in
    (ans, ans)
  else let c = computeSopProb (chooseiC i l) pa in
       probCutSop l pa len (i+1) (-1. *. m) ans2 (ans2 +. (m *. c)) ;;

let probErrorCutCalc cs pa =
  match cs with
    | Var v ->
      let a = (List.Assoc.find_exn pa ~equal:(=) v) in (a, a)
    | Sum s ->
      let l = (List.map s ~f:(fun x -> match x with
	| Var v -> [v]
	| Pro p -> (List.map p ~f:(fun y -> match y with
	    | Var v -> v
	    | _ -> raise (Error "probErrorCutCalc 1 exception")) )
	| _ -> raise (Error "probErrorCutCalc 2 exception"))
		 ) in
      let len = List.length l in
      probCutSop l pa len 1 1. 0. 0.
    | Pro p ->
      let p1 = List.map p
	~f:(fun x -> match x with
	    Var v -> v
	  | _ ->  raise (Invalid_argument "In pro p of probErrorCutCalc"))
	 in
      let a = List.fold_left (List.map p1 ~f:(fun x -> List.Assoc.find_exn pa ~equal:(=) x)) ~init:1. ~f:( *. ) in
      (a, a)
    | _ -> raise (Invalid_argument "In probErrorCutCalc")
;;

(**/**)
(** A function that given a fault tree and cutsets, computes the top-level
    probability of a fault. *)
let probErrorCutC t cutset =
  let probAlist = eventProbs t in
  probErrorCutCalc cutset probAlist ;;

(** A function that given a fault tree, computes the top-level probability of a
    fault. *)
let probErrorCut t =
  let cutset = mcutsets t in
  probErrorCutC t cutset ;;

(** A function that given a fault tree and cutsets, computes importance measures. *)
let probErrorCutCImp t cutset =
  let probAlist = eventProbs t in
  let (plo, phi) = probErrorCutCalc cutset probAlist in
  let p = (plo +. phi) /. 2. in
  let cs = match cutset with
      Var _ -> [cutset]
    | Sum s -> s
    | _ -> raise (Error "probErrorCutImp exception") in 
  let cerr = List.map cs ~f:(fun x -> probErrorCutCalc x probAlist) in
  let c = List.map2_exn cs cerr
    ~f:(fun x y -> let (p1, _) = y in
		let imp = p1 /. p in 
		(x, p1, imp))
    in
  List.sort ~compare:(fun x y -> let (_, _, xi) = x in
				 let (_, _, yi) = y in
				 -(compare xi yi))
    c ;;

(** A function that given a fault tree, computes importance measures. *)
let probErrorCutImp t =
  let cutset = mcutsets t in
  probErrorCutCImp t cutset ;;

(**/**)
(** A generalization of majority. Instead of 2-of-3, this is
   n of L, where L is a list of nodes and n is <= len.L *)
let n_of_L n l =
  let ch = choosei n l in
  let s = List.map ch ~f:(fun x -> PRO x) in 
  if n=1 then (SUM l)
  else (SUM s) ;;

let lFromErr err = log(1. /. (1. -. err)) ;;
