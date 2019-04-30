
(* From the paper:

   OpenFTA also exhibits scalability issues. Using the fault tree example presented
   on page XI-3 of~\cite{FT-Handbook} and assuming the probabilities of all basic
   events to be 2E-5, OpenFTA took about 40 minutes to calculate the top-level
   probability of this example, whereas our tool took less than a second.

*)

(* Example from fault tree handbook US Nuclear Regulatory commission 
   Page VIII-13.  *)

#use "top.ml";;

let k1 = Leaf("K1", lFromErr 3.e-5, 1.) ;;
let r = Leaf("R", lFromErr 1.e-4, 1.) ;;
let s1 = Leaf("S1", lFromErr 3.e-5, 1.) ;;
let s = Leaf("S", lFromErr 1.e-4, 1.) ;;
let t = Leaf("T", lFromErr 5.e-6, 1.) ;;
let k2 = Leaf("K2", lFromErr 3.e-5, 1.) ;;
let e5 = SUM [k1;r] ;;
let e4 = SUM [s1;e5] ;;
let e3 = PRO [s;e4] ;;
let e2 = SUM [e3;k2] ;;
let e1 = SUM [t;e2] ;;

cutsets e1;;
probErrorCut e1;;
probErrorCutImp e1;;

(* Example from fault tree handbook US Nuclear Regulatory 
   commission. 

   Page XI-3. The example did not include probabilities *)

let lFromErr err = log(1. /. (1. -. err)) ;;
let err1 = lFromErr 2.e-5;;

let s6 = Leaf("S6", err1, 1.) ;;
let p6 = Leaf("P6", err1, 1.) ;;
let e6 = Leaf("E6", err1, 1.) ;;
let g8 = SUM [s6;p6;e6] ;;
let s5 = Leaf("S5", err1, 1.) ;;
let p5 = Leaf("P5", err1, 1.) ;;
let g7 = SUM [p5;g8;s5] ;;
let s4 = Leaf("S4", err1, 1.) ;;
let p4 = Leaf("P4", err1, 1.) ;;
let e4 = Leaf("E4", err1, 1.) ;;
let g6 = SUM [s4;p4;e4] ;;
let g4 = SUM [g6;g7] ;;
let s3 = Leaf("S3", err1, 1.) ;;
let p3 = Leaf("P3", err1, 1.) ;;
let e3 = Leaf("E3", err1, 1.) ;;
let g5 = SUM [s3;p3;e3] ;;
let g3 = PRO [g4;g5] ;;
let s2 = Leaf("S2", err1, 1.) ;;
let p2 = Leaf("P2", err1, 1.) ;;
let g2 = SUM [p2;s2;g3] ;;
let e1 = Leaf("E1", err1, 1.) ;;
let g1 = SUM [g2;e1] ;;
let s1 = Leaf("S1", err1, 1.) ;;
let p1 = Leaf("P1", err1, 1.) ;;
let t = SUM [p1;g1;s1] ;;

cutsets t;;
probErrorCut t;;
probErrorCutImp t;;
