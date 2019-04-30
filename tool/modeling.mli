(* 

Copyright © 2019 United States Government as represented by the Administrator of the 
National Aeronautics and Space Administration.  All Rights Reserved.

*)
type 'a formula =
    F of 'a
  | And of 'a formula list
  | Or of 'a formula list
  | N_of of int * 'a formula list
type 'a cformula = string list * 'a formula
type component = {
  name : string;
  faults : string list;
  input_flows : string list;
  basic_events : string list;
  event_info : (float * float) list;
  output_flows : string list;
  formulas : string list cformula list;
}
type instance = {
  i_name : string;
  c_name : string;
  exposures : (string * float) list;
  lambdas : (string * float) list;
}
val makeInstance :
  ?t:(string * float) list ->
  ?l:(string * float) list -> i:string -> c:string -> unit -> instance
type model = {
  instances : instance list;
  connections : ((string * string) * (string * string)) list;
  top_fault : string * string list formula;
}
