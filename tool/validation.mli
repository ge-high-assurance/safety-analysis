(* 

Copyright © 2019 United States Government as represented by the Administrator of the 
National Aeronautics and Space Administration.  All Rights Reserved.

*)
val checkLibrary_nonEmptyFaults :
  Modeling.component list -> (string, string) Core.Result.t
val checkLibrary_componentUnique :
  Modeling.component list -> (string, string) Core.Result.t
val checkLibrary_formulasMakeSense :
  Modeling.component list -> (string, string) Core.Result.t
val checkLibrary_disjointInputFlowsandBasicEvents :
  Modeling.component list -> (string, string) Core.Result.t
val checkLibrary_allOutputFaultsHaveFormulas :
  Modeling.component list -> (string, string) Core.Result.t
val checkLibrary_listsAreConsistentLengths :
  Modeling.component list -> (string, string) Core.Result.t
val checkModel_inputFlowUnique :
  Modeling.model -> (string, string) Core.Result.t
val checkModel_instanceNameUnique :
  Modeling.model -> (string, string) Core.Result.t
val checkModel_validConnections :
  Modeling.model ->
  Modeling.component Core.List.t -> (string, string) Core.Result.t
val checkModel_cnameInstanceIsDefinedInLibrary :
  Modeling.model ->
  Modeling.component list -> (string, string) Core.Result.t
val checkModel_exposureOfBasicIsDefinedInLibrary :
  Modeling.model ->
  Modeling.component list -> (string, string) Core.Result.t