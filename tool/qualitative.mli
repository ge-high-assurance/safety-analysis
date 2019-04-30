(* 

Copyright © 2019 United States Government as represented by the Administrator of the 
National Aeronautics and Space Administration.  All Rights Reserved.

*)
exception Error of string
val formulaOfTree : 'a FaultTree.ftree -> 'a FaultTree.pexp
val removeDups : 'a Core.List.t -> 'a list
val fix : ('a -> 'a) -> 'a -> 'a
val sfc : 'a FaultTree.pexp -> 'a FaultTree.pexp
val slfc : 'a FaultTree.pexp -> 'a FaultTree.pexp
val slnfc : 'a FaultTree.pexp -> 'a FaultTree.pexp
val ssfc : 'a FaultTree.pexp -> 'a FaultTree.pexp
val ssnfc : 'a FaultTree.pexp -> 'a FaultTree.pexp
val sslfc : 'a FaultTree.pexp -> 'a FaultTree.pexp
val sslnfc : 'a FaultTree.pexp -> 'a FaultTree.pexp
val nsimp : 'a FaultTree.pexp -> 'a FaultTree.pexp
val nnsimp : 'a FaultTree.pexp -> 'a FaultTree.pexp
val nndsimp : 'a FaultTree.pexp -> 'a FaultTree.pexp
val nndsopsimp : 'a FaultTree.pexp -> 'a FaultTree.pexp
val sop : 'a FaultTree.pexp -> 'a FaultTree.pexp
val cutsets : 'a FaultTree.ftree -> 'a FaultTree.pexp
val mcutsets : 'a FaultTree.ftree -> 'a FaultTree.pexp 
