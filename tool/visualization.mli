(* 

Copyright © 2019 United States Government as represented by the Administrator of the 
National Aeronautics and Space Administration.  All Rights Reserved.

*)
val unflatten_exe : string
val dot_exe : string
val dot_core_gen_show_file :
  ?rend:Core.String.t ->
  ?splines:string ->
  ?layout:string ->
  ?overlap:string ->
  ?unflatten:bool ->
  Core.String.t ->
  (?splines:string ->
   ?layout:string -> ?overlap:string -> ?chan:Core.out_channel -> unit) ->
  unit
val dot_gen_show_direct_tree_file :
  ?rend:Core.String.t ->
  ?splines:string ->
  ?layout:string ->
  ?overlap:string ->
  ?unflatten:bool ->
  Core.String.t ->
  (Core.String.t * Core.String.t) FaultTree.ftree -> unit
val dot_gen_show_tree_file :
  ?rend:Core.String.t ->
  ?splines:string ->
  ?layout:string ->
  ?overlap:string ->
  ?unflatten:bool ->
  Core.String.t ->
  (Core.String.t * Core.String.t) FaultTree.ftree -> unit
val dot_gen_show_formula_file :
  ?rend:Core.String.t ->
  ?splines:string ->
  ?layout:string ->
  ?overlap:string ->
  ?unflatten:bool ->
  Core.String.t ->
  (Core.String.t * Core.String.t) FaultTree.pexp -> unit
val dot_gen_show_ph_file :
  ?rend:Core.String.t ->
  ?splines:string ->
  ?layout:string ->
  ?overlap:string ->
  ?unflatten:bool -> Modeling.model -> Core.String.t -> unit
val gen_funct_nodes :
  ?acc:int *
       (Modeling.instance * int * (int * string) Core.List.t *
        (int * string) Core.List.t)
       list ->
  Modeling.component Core.List.t ->
  Modeling.instance list ->
  int *
  (Modeling.instance * int * (int * string) Core.List.t *
   (int * string) Core.List.t)
  list
val gen_funct_edges :
  (Modeling.instance * 'a * ('b * 'c) Core.List.t *
   ('b * 'c) Core.List.t)
  Core.List.t ->
  ((string * 'c) * (string * 'c)) Core.List.t ->
  (('a * 'b) * ('a * 'b)) Core.List.t
val dot_gen_funct_nodes :
  (Modeling.instance * int * (int * string) Core.List.t *
   (int * string) Core.List.t)
  Core.List.t -> out_channel -> unit
val dot_gen_funct_edges :
  ((int * int) * (int * int)) Core.List.t -> out_channel -> unit
val dot_gen_funct_arch_chan_helper :
  string -> string -> string -> out_channel -> unit
val dot_gen_show_funct_file :
  ?rend:Core.String.t ->
  ?splines:string ->
  ?layout:string ->
  ?overlap:string ->
  ?unflatten:bool ->
  Modeling.component Core.List.t ->
  Modeling.model -> Core.String.t -> unit
val dot_gen_show_fault_file :
  ?rend:Core.String.t ->
  ?splines:string ->
  ?layout:string ->
  ?overlap:string ->
  ?unflatten:bool ->
  Modeling.component Core.List.t ->
  Modeling.model -> Core.String.t -> unit
