(* 

Copyright © 2019 United States Government as represented by the Administrator of the 
National Aeronautics and Space Administration.  All Rights Reserved.

*)
val find_form1 : ('a * 'b) Core.List.t -> 'a -> 'b
val expand_N_of :
  int -> 'a Modeling.formula Core.List.t -> 'a Modeling.formula
val exp_i : 'a -> 'b Modeling.formula -> ('a * 'b) Modeling.formula
val expand_c_form :
  (string list * string list Modeling.formula) Core.List.t ->
  string list Modeling.formula ->
  Modeling.component -> string list Modeling.formula
val f_to_pexp : 'a Modeling.formula -> 'a FaultTree.pexp
val pexp_to_f : 'a FaultTree.pexp -> 'a Modeling.formula
val fnnsimp : 'a Modeling.formula -> 'a Modeling.formula
val find_form :
  (string list * string list Modeling.formula) Core.List.t ->
  string list -> Modeling.component -> string list Modeling.formula
val find_form_f :
  (string list * string list Modeling.formula) Core.List.t ->
  string list Modeling.formula ->
  Modeling.component -> string list Modeling.formula
val get_i_name :
  Modeling.instance Core.List.t -> string -> Modeling.instance
val get_c_name :
  Modeling.component Core.List.t -> string -> Modeling.component
val filter_v :
  'a list Modeling.formula Core.List.t ->
  int ->
  ((string * 'a) * ('b * 'a)) Core.List.t ->
  Modeling.instance ->
  ('b * 'a list, int) Core.List.Assoc.t ->
  'a list Modeling.formula Core.List.t
val filter_exp :
  'a Modeling.formula Core.List.t -> 'a Modeling.formula Core.List.t
val add_to_acc_expand_form :
  'a -> ('a, int) Core.List.Assoc.t -> ('a * int) list
val expand_form :
  Modeling.component Core.List.t ->
  Modeling.instance Core.List.t ->
  ((string * string) * (string * string)) Core.List.t ->
  string list Modeling.formula ->
  Modeling.instance ->
  Modeling.component ->
  (string * string list, int) Core.List.Assoc.t ->
  string list Modeling.formula
val cons_form :
  Modeling.component Core.List.t ->
  Modeling.instance Core.List.t ->
  ((string * string) * (string * string)) Core.List.t ->
  string list Modeling.formula ->
  Modeling.instance ->
  (string * string list, int) Core.List.Assoc.t ->
  string list Modeling.formula
val get_c_ind : ?ind:int -> 'a -> 'a list -> int
val findList : 'a -> ('a * 'b) Core.List.t -> 'b -> 'b
val cons_tree :
  Modeling.component Core.List.t ->
  Modeling.instance Core.List.t ->
  string list Modeling.formula -> (string * string) FaultTree.ftree
val model_to_ftree :
  Modeling.component Core.List.t ->
  Modeling.model -> (string * string) FaultTree.ftree
val update_comp_form :
  'a ->
  'a list Modeling.formula -> 'a Core.List.t -> 'a list Modeling.formula
val update_comp_forms :
  ('a list * 'a list Modeling.formula) Core.List.t ->
  'a Core.List.t -> ('a list * 'a list Modeling.formula) Core.List.t
val update_component : Modeling.component -> Modeling.component
val update_library :
  Modeling.component Core.List.t -> Modeling.component Core.List.t
