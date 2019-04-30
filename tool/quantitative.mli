(* 

Copyright © 2019 United States Government as represented by the Administrator of the 
National Aeronautics and Space Administration.  All Rights Reserved.

*)
val probErrorApprox : 'a FaultTree.ftree -> float
val probDisjunction : float Core.List.t -> float
val choosei : int -> 'a Core.List.t -> 'a Core.List.t Core.List.t
val probError : 'a FaultTree.ftree -> float
val probErrorCut : 'a FaultTree.ftree -> float * float
val probErrorCutImp :
  'a FaultTree.ftree -> ('a FaultTree.pexp * float * float) Core.List.t
val probErrorCutC : 'a FaultTree.ftree -> 'a FaultTree.pexp -> float * float
val probErrorCutCImp :
  'a FaultTree.ftree ->
  'a FaultTree.pexp -> ('a FaultTree.pexp * float * float) Core.List.t
val n_of_L : int -> 'a FaultTree.ftree Core.List.t -> 'a FaultTree.ftree
val lFromErr : float -> float
