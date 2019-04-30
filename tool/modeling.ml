(* 

Copyright © 2019 United States Government as represented by the Administrator of the 
National Aeronautics and Space Administration.  All Rights Reserved.

Author: Pete Manolios
Date: 2017-12-15

*)

(**
   Fault trees should not be constructed manually by typical users. Instead,
   they should be synthesized from libraries of components and
   models. Components are represented with the {i component} type; component
   instances are represented with the {i instance} type; component libraries are
   just a list of components; and models are represented with the {i model}
   type. Here is an example of a library. It is just a list of components.

   {[
   let nasa_handbook_lib =
   [ {name         = "System"; 
      faults       = ["fault"]; 
      input_flows  = ["in"];
      basic_events = ["sys_fl"];
      event_info   = [(1.e-6, 1.)];
      output_flows = ["out"];
      formulas     = [(["out"; "fault"], Or [F ["sys_fl"]; F ["in"; "fault"]])] }];; 
   ]}

   Here is an example of a model. It is a list of component instances along with
   connection information and the identification of a top-level fault.

   {[
   let nasa_handbook_model =
   { instances =  [ makeInstance "Orbiter" "System" ();
                    makeInstance "Main Engine" "System" (); ];
   connections =  [ (("Orbiter", "in"), ("Main Engine", "out"));
                    (("Main Engine", "in"), ("Orbiter", "out")); ];
   top_fault =("Orbiter", F["out"; "fault"]) } ;;
   ]}
   
*)

(** Type definition for a formula used in cformulas. *)
type 'a formula =
    F of 'a                  (* atomic formula *)
  | And of 'a formula list   (* arbitrary-arity conjunction *)
  | Or  of 'a formula list   (* arbitrary-arity disjunction *)
  | N_of of int * ('a formula list);; (* n of m *)

(** Type definition for a cformula defining output flows. *)

type 'a cformula =
    (string list) * 'a formula ;;

(** Type definition for a component. *)
type component =
    {name         : string;
     faults       : string list;
     input_flows  : string list;
     basic_events : string list;
     event_info   : (float * float) list;
     output_flows : string list;
     formulas     : (string list) cformula list;
    } ;;

(** Type definition for an instance of a component. *)
type instance =
    {i_name: string;
     c_name: string;
     exposures: (string * float) list;
     lambdas: (string * float) list;
    } ;;

(** Function for making instances of components. *)
let makeInstance  ?(t = []) ?(l = []) ~i ~c () =
  {i_name = i; c_name = c; exposures = t; lambdas = l} ;;

(** Type definition of a model *)
type model =
    {instances  : instance list;
     connections: ((string * string) * (string * string)) list;
     top_fault  : (string * (string list) formula);
    };;

