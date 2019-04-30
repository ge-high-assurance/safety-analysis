(*

Copyright © 2019 United States Government as represented by the Administrator of the 
National Aeronautics and Space Administration.  All Rights Reserved.

*)

(** A fault tree is either: 
    a leaf containing a failure rate and exposure time OR
    an (n-ary) operator along with a list of trees. **)
type 'a ftree = 
  | Leaf of 'a * float * float 
  | SUM of 'a ftree list 
  | PRO of 'a ftree list ;; 

(** Type definitions for monotone Boolean formulas. *)
type 'a pexp = 
  | TRUE
  | FALSE
  | Var of 'a 
  | Sum of 'a pexp list
  | Pro of 'a pexp list
;;
