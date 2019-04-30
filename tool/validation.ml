(* 

Copyright © 2019 United States Government as represented by the Administrator of the 
National Aeronautics and Space Administration.  All Rights Reserved.

Authors: Kit Siu, Hongwei Liao, Mike Noorman
Date: 2017-12-15

*)

(**
 
Validation checks are provided to assist the end-user in building component library and models created following the modeling constructs defined for fault tree. Checks include those for the component library only, for the model only, and for the model relative to the library.

Validation checks for component library only:

   - {b checkLibrary_nonEmptyFaults} : All components must support at least one flow, otherwise there is nothing to analyze.

   - {b checkLibrary_componentUnique} : A library of components is a list of components, such that no two components have the same name.

   - {b checkLibrary_formulasMakeSense} : Check that the formulas make sense. 
   
   - {b checkLibrary_disjointInputFlowsandBasicEvents} : Check that the names used for input flows, and basic events are disjoint.
   
   - {b checkLibrary_allOutputFaultsHaveFormulas} : All flows have a formula defined.
   
   - {b checkLibrary_listsAreConsistentLengths} : Check that the lists are all of consistent lengths.

Validation checks for component model only:

   - {b checkModel_inputFlowUnique} : Check that input flows have exactly 1 instance connected to them.
   
   - {b checkModel_instanceNameUnique} : All instance names are disjoint.
   
Validation checks for component model relative to a library:

   - {b checkModel_validConnections} : The connection information is a list of pairs consisting of an input flow and an instance.
   
   - {b checkModel_cnameInstanceIsDefinedInLibrary} : All c_names correspond to actual components in our component library.
   
   - {b checkModel_exposureOfBasicIsDefinedInLibrary} : Elements of the form (a, x) in exposures of an instance make sense.

*)

open Core ;;
open Modeling ;;
open Qualitative ;;

(** A function that checks that all components in a library support at least one flow. Otherwise there is nothing to analyze. 
*)
let rec checkLibrary_nonEmptyFaults library = 
    match library with
	| [] -> Ok "checkLibrary_nonEmptyFaults: pass" 
	| hd::tl -> 
		if (hd.faults <> []) then checkLibrary_nonEmptyFaults tl
		else Error ("Faults not defined for library component " ^ hd.name);		
	;;

(**/**)
(* Extract the component names from library, store to a list *)
let rec extractCompName library =
      match library with
      | [] -> []
      | hd::tl -> hd.name :: extractCompName tl ;;

(* Remove duplicate items from the sorted list*)
let rec deleteDuplicate sortedList =
  match sortedList with
  | [] -> []
  | x :: [] -> x :: []
  | x :: y :: rest ->
    if x = y 
    then (print_string ("Duplicate component: " ^ x ^ "\n"); deleteDuplicate (y :: rest))
    else x :: deleteDuplicate (y :: rest);;
(**/**)

(** A function that checks that a library of components is a list of components, such that no two components have the same name.
*) 
let checkLibrary_componentUnique library = 
  let componentList = extractCompName library in
  (* Sort the list *)
  let sortedList = List.sort ~compare:(fun x y -> ~-(Pervasives.compare x y)) componentList in  
    let uniqueList = deleteDuplicate sortedList in
    if (List.length componentList) = (List.length uniqueList)
    then Ok "checkLibrary_componentUnique: pass"
    else Error "Library component not unique - see above";;

(**/**)
let fm2fl f_h =      (* cformula to flow *)
 match f_h with
 | ([],_) -> ""
 | (_::[],_) -> ""
 | (_::_::_::_, _) -> ""
 | ([_;str2],_) -> str2 ;;


(* get more info to print for debug *)
let fm2flout f_h =      (* cformula to flow - output *)
 match f_h with
 | ([],_) -> ""
 | (_::[],_) -> ""
 | (_::_::_::_, _) -> ""
 | ([str1;_],_) -> str1;;

(* function to check the validity of F *)
let rec confirmValidF myF inflowL basicL refL = (* string list formula -> bool list *)
 let subsume a b = List.for_all a ~f:(fun x -> List.mem b ~equal:(=) x) in
 match myF with
 | F[]       -> [false]
 | F[a]      -> if subsume [a] (List.append basicL refL) then [true] else [false]
 | F[a;_]    -> if subsume [a] inflowL  then [true] else [false] 
 | F(_::_::_::_) -> [false]
 | And(a)    -> List.concat (List.map a ~f:(fun x -> confirmValidF x inflowL basicL refL))
 | Or(a)     -> List.concat (List.map a ~f:(fun x -> confirmValidF x inflowL basicL refL))
 | N_of(_,a) -> List.concat (List.map a ~f:(fun x -> confirmValidF x inflowL basicL refL)) ;; 
  
(* function to collect all the formula references *)
let rec collectFormulaReferences fmL =
 match fmL with 
 | [] -> []
 | hd::tl -> let (fm_ref, _) = hd in 
   List.hd_exn fm_ref :: (collectFormulaReferences tl) ;;

let rec checkCFormList_formulasMakeSense fmList inflowL basicL refL comp  =
 match fmList with
 | [] -> "pass"
 | hd::tl -> (* formula by formula *)
   let (_,myF) = hd in
   let validityList = confirmValidF myF inflowL basicL refL in
   if (List.fold ~init:true ~f:(&&) validityList)
   then checkCFormList_formulasMakeSense tl inflowL basicL refL comp 
   else "Invalid formula in component " ^ comp ^ ", check formula [" ^ (fm2flout hd) ^ "," ^ (fm2fl hd) ^ "]";;
(**/**)

(** A function that checks that the formulas of all library components make sense.  All formulas include formula of the form F [c;b] or F [c], where c is a subset of the variables in the union of input_flows, basic events, and c, where c can be defined as a local variable within the list of formulas. 
*)
let rec checkLibrary_formulasMakeSense l =
 match l with
 | [] -> Ok "checkLibrary_formulasMakeSense: pass"
 | hd::tl -> (* component by component *)
   let fm = hd.formulas in
   let iFL = hd.input_flows 
   and bL = hd.basic_events
   and rL = (collectFormulaReferences fm)
   and comp = hd.name in (* MN - comp name passed for debug printing*)
   let msg = (checkCFormList_formulasMakeSense fm iFL bL rL comp) in
   if msg = "pass"
   then checkLibrary_formulasMakeSense tl 
   else Error msg ;;


(** A function that checks that the library component names used for input flows and basic events are disjoint. 
*)
let rec checkLibrary_disjointInputFlowsandBasicEvents l =
	match l with
	| [] -> Ok "checkLibrary_disjointInputFlowsandBasicEvents: pass"
	| hd::tl -> (* component by component *)
		let iUb = List.append hd.input_flows hd.basic_events in (* may not work anymore*)
		if (iUb = []) || (List.length iUb) = (List.length (removeDups iUb))
		then checkLibrary_disjointInputFlowsandBasicEvents tl
		else Error ("Names used for input_flows and basic_events are not disjoint in component " ^ hd.name);;

(**/**)
let rec cf2flist cf = (* list of formula flows -> string list *)
  match cf with
  | [] -> []
  | hd::tl ->
    match hd with
    | ([str1;str2],_) -> List.append ([String.concat ~sep:"-" [str1; str2]]) (cf2flist tl) 
    | ([],_)          -> cf2flist tl 
    | (_::[], _)      -> cf2flist tl
    | (_::_::_::_, _) -> cf2flist tl ;;
(* double recursion needed here, faults2outfaultslist and outfaults2outfaultslist *)

let rec faults2outfaultslist out_str faults =
	match faults with
	| [] -> []
	| f_hd::f_tl -> List.append ([String.concat ~sep:"-" [out_str; f_hd]]) (faults2outfaultslist out_str f_tl) ;;

let rec outfaults2outfaultslist out faults = (* list of formula flows -> string list *)
	match out with
	| [] -> []
	| out_hd::out_tl ->	
		List.append (faults2outfaultslist out_hd faults) (outfaults2outfaultslist out_tl faults) ;;			
(**/**)

(** A function that checks that all library component flows have a formula defined.
*)
let rec checkLibrary_allOutputFaultsHaveFormulas l =
  match l with
  | [] -> Ok "checkLibrary_allOutputFaultsHaveFormulas: pass"
  | hd::tl -> (* component by component *)
    if ( List.sort ~compare:(fun x y -> ~- (Pervasives.compare x y)) (outfaults2outfaultslist hd.output_flows hd.faults)  
			= List.sort ~compare:(fun x y -> ~- (Pervasives.compare x y)) (cf2flist hd.formulas) )
    then checkLibrary_allOutputFaultsHaveFormulas tl
    else Error ("Not all output faults have formulas, check component " ^ (hd.name));;


(** A function that checks that the lists within each library component are all of consistent lengths, e.g., the length of basic_events is the same as the length of event_info. Also, the length of faults x output_flows is the length of formulas.
*)		
let rec checkLibrary_listsAreConsistentLengths l =
	match l with
	| [] -> Ok "checkLibrary_listsAreConsistentLengths: pass"
	| hd::tl -> (* component by component *)
		let b_l1 = List.length hd.basic_events = List.length hd.event_info
		and b_l2 = List.length hd.faults * List.length hd.output_flows = List.length hd.formulas
		in if b_l1 && b_l2
		then checkLibrary_listsAreConsistentLengths tl
		else if not(b_l1) && b_l2 then Error ("Basic events and event info are of inconsistent lengths in component " ^ hd.name)
		else if b_l1 && not(b_l2) then Error ("Faults and formulas lists are of inconsistent lengths in component " ^ hd.name)
		else Error ("Basic events & event info, faults & formulas are of inconsistent lengths in component " ^ hd.name)
		;;
		
(**/**)
(* Extract "input flow" tuple from the "connection" list of a model, store to a list *)
let rec extractInputFlow connectionList =
      match connectionList with
      | [] -> []
      | hd::tl ->
            let ((receiver, address), _) = hd in
            (receiver, address) :: extractInputFlow tl
      ;;

(* Remove duplicate items from the sorted list*)
let rec deleteDuplicate sortedList =
      match sortedList with
      | [] -> []
      | x :: [] -> x :: []
      | x :: y :: rest ->
            if x = y then deleteDuplicate (y :: rest)
            else x :: deleteDuplicate (y :: rest);;
(**/**)

(** A function that checks that in the connections of a model, input flows have at most 1 instance connected to them.
*)
let checkModel_inputFlowUnique model = 
  let inputFlowList = extractInputFlow model.connections in
  (* Sort the list *)
  let sortedList = List.sort ~compare:(fun x y -> ~- (Pervasives.compare x y)) inputFlowList in  
  let uniqueList = deleteDuplicate sortedList in
	if (List.length inputFlowList) = (List.length uniqueList)
	then Ok "checkModel_inputFlowUnique: pass"
	else Error "One of the input_flows in the model has more than one connection made to it.";;

(**/**)
(* Extract the "i_names" from the list of "instances" of a "model", store to a list *)
let rec extractIName instanceList = 
	match instanceList with
	| [] -> [] 
	| hd::tl -> 
		hd.i_name :: extractIName tl
	;;

(* Remove duplicate items from the sorted list*)
let rec deleteDuplicate sortedList = 
	match sortedList with 
	| [] -> [] 
	| x :: [] -> x :: [] 
	| x :: y :: rest -> 
		if x = y then deleteDuplicate (y :: rest) 
		else x :: deleteDuplicate (y :: rest);;
(**/**)

(**	A function that checks that in a model all instance names are disjoint.  
*)
let checkModel_instanceNameUnique model = 
  let iNameList = extractIName model.instances in
  (* Sort the list *)
  let sortedList = List.sort ~compare:(fun x y -> ~- (Pervasives.compare x y)) iNameList in	
  let uniqueList = deleteDuplicate sortedList in
	if (List.length iNameList) = (List.length uniqueList)
	then Ok "checkModel_instanceNameUnique: pass"
	else Error "Model instance names are not unique";;

(**/**)
let rec checkModel_validConnections_rec c i lib =
	match c with
	| [] -> Ok "checkModel_validConnections: pass"
	| hd::tl -> 
		let ((receiver, raddress), (sender, saddress)) = hd in
		(* --- check (receiver, raddress) duple --- *)
		(* get the instance *)
		let foundInst = [try List.find_exn i ~f:(fun x -> x.i_name = receiver) with Not_found -> (makeInstance ~i:"nil" ~c:"" ())] in
 		(* get the component from the library  *)
 		let foundComp = [try List.find_exn lib ~f:(fun x -> x.name = (List.hd_exn foundInst).c_name) with Not_found -> 
 			{name="nil"; faults=[]; input_flows=[]; basic_events=[]; event_info=[]; output_flows=[]; formulas=[]} ] in
		(* get the address *)
		let foundAddr = [try List.find_exn (List.hd_exn foundComp).input_flows ~f:(fun x -> x = raddress) with Not_found -> "nil"] and 
		(* --- check (sender, saddress) duple --- *)
		(* get the instance *) 
			foundSndr = [try List.find_exn i ~f:(fun x -> x.i_name = sender) with Not_found -> (makeInstance ~i:"nil" ~c:"" ())] in
 		(* get the component from the library *)
 		let foundSomp = [try List.find_exn lib ~f:(fun x -> x.name = (List.hd_exn foundSndr).c_name) with Not_found -> 
 			{name="nil"; faults=[]; input_flows=[]; basic_events=[]; event_info=[]; output_flows=[]; formulas=[]} ] in
		(* get the address *)
		let foundSddr = [try List.find_exn (List.hd_exn foundSomp).output_flows ~f:(fun x -> x = saddress) with Not_found -> "nil"] in

		if (List.hd_exn foundInst).i_name = "nil" then Error ("Invalid connection: this is not an instance of a component in the model: " ^ receiver)
		else if (List.hd_exn foundComp).name = "nil" then Error ("Invalid connection: this instantiation references a component that is not in the library: " ^ receiver)
		else if (List.hd_exn foundAddr) = "nil" then Error ("Invalid connection: this is not a valid component input from the library: (" ^ receiver ^ ", " ^ raddress ^ ")")
		else if (List.hd_exn foundSndr).i_name = "nil" then Error ("Invalid connection: this is not an instance of a component in the model: " ^ sender)
		else if (List.hd_exn foundSomp).name = "nil" then Error ("Invalid connection: this instantiation references a component that is not in the library: " ^ sender)
		else if (List.hd_exn foundSddr) = "nil" then Error ("Invalid connection: this is not a valid component input from the library: (" ^ sender ^ ", " ^ saddress ^ ")")
		else checkModel_validConnections_rec tl i lib ;;
(**/**)

(** A function that checks that the model connections, relative to a library, is a list of pairs consisting of an input flow and an instance.
*)
let checkModel_validConnections m lib = 
	let c = m.connections and i = m.instances in
	checkModel_validConnections_rec c i lib;;


(**/**)

(* list of name from library *)
let rec library2namelist l = 
	match l with
	| [] -> []
	| hd::tl -> hd.name :: library2namelist tl ;;
(**/**)

(** A function that checks that all in a model, relative to a library, the model c_names correspond to actual components in the component library.
*)
let rec checkModel_cnameInstanceIsDefinedInLibrary m l =
	(* build a list of names from library *)
	let nl = library2namelist l in
	match m.instances with
	| [] -> Ok "checkModel_cnameInstanceIsDefinedInLibrary: pass"
	| hd::tl -> (* go through instance by instance *)
		if (List.mem nl ~equal:(=) hd.c_name)
		then let newm = {instances=tl; connections=m.connections; top_fault=m.top_fault} in
		checkModel_cnameInstanceIsDefinedInLibrary newm l 
		else Error ("Invalid Component: this instantiation references a component that is not in the library: " ^ hd.c_name)
	;;

(**/**)

let rec library2eventlist l = (* list of basic events from library *)
	match l with
	| [] -> []
	| hd::tl -> List.append hd.basic_events (library2eventlist tl) ;;

let rec exposures2explist l = (* list of exposures *)
	match l with
	| [] -> []
	| hd::tl -> 
		let (en, _) = hd in en::(exposures2explist tl) ;;
(**/**)

(** A function that checks that the model elements, relative to a library, of the form (a, x) in exposures of an instance make sense, i.e., there is a basic event named a in the component being instantiated.
*)
let rec checkModel_exposureOfBasicIsDefinedInLibrary m l =
	(* build a list of basic events from library *)
	let nl = library2eventlist l in
	match m.instances with
	| [] -> Ok "checkModel_exposureOfBasicIsDefinedInLibrary: pass"
	(* go through instance by instance *)
	| hd::tl -> 
		let el = exposures2explist hd.exposures in
		let subsume a b = List.for_all a ~f:(fun x -> List.mem b ~equal:(=) x) in
		if (subsume el nl)
		then let newm = {instances=tl; connections=m.connections; top_fault=m.top_fault} in
		checkModel_exposureOfBasicIsDefinedInLibrary newm l 
		else Error "Model attempts to change an invalid basic_event of a library component"
	;;
