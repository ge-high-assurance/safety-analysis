(* 
   
Author: Pete Manolios
Date:   11/17/2017

Description: This file includes an example showing how formulas can 
  reused.


*)

#use "top.ml";;

let library =
  [

   {name	 = "Sensor";
    faults	 = ["ued"; "loa"];
    input_flows  = [];
    basic_events = ["senUedFlt";"senLoaFlt"];
    event_info	 = [(1.0e-8, 1.0);(1.0e-7, 1.0)];
    output_flows = ["out"]; 
    formulas	 = [(["out"; "ued"], F["senUedFlt"]);
		    (["out"; "loa"], F["senLoaFlt"])]};

   {name	 = "RIU";
    faults	 = ["ued"; "loa"];
    input_flows  = ["rin"];
    basic_events = ["riuUedFlt";"riuLoaFlt"];
    event_info	 = [(1.0e-8, 1.0) ;(1.0e-7, 1.0)];
    output_flows = ["out1"]; 
    formulas	 = [(["out1"; "ued"], Or[F["rin"; "ued"]; F["riuUedFlt"]]);
		    (["out1"; "loa"], Or[F["rin"; "loa"]; F["riuLoaFlt"]])]};

   {name	 = "Switch";
    faults	 = ["ued"; "loa"];
    input_flows  = ["swin1"; "swin2";];
    basic_events = ["swUedFlt";"swLoaFlt"];
    event_info	 = [(1.0e-10, 1.0); (1.0e-9, 1.0)];
    output_flows = ["out1"; "out2"]; 
    formulas	 = [(["out1"; "ued"], Or[F["swin1"; "ued"]; F["swUedFlt"]]);
		    (["out2"; "ued"], Or[F["swin2"; "ued"]; F["swUedFlt"]]);
		    (["out1"; "loa"], Or[F["swin1"; "loa"]; F["swLoaFlt"]]);
		    (["out2"; "loa"], Or[F["swin2"; "loa"]; F["swLoaFlt"]])]};
    
   {name	 = "GPM";
    faults	 = ["ued"; "loa"];
    input_flows  = ["gin1"; "gin2"; "gin3"; "gin4"];
    basic_events = ["gpmUedFlt"; "gpmLoaFlt"; "votePasses"; "voteFails"];
    event_info	 = [(1.0e-11, 1.0); (1.0e-10, 1.0); (0.08, 1.0); (0.9, 1.0)];
    output_flows = ["out"]; 
    formulas	 = [(["out"; "ued"],
		     Or[F["gpmUedFlt"];F["uedNotCaughtByVote"]]);
		    (["uedNotCaughtByVote"],
		     And[F["2uedFaults"]; F["votePasses"]]);
		    (["2uedFaults"],
		     And[Or[F["gin1"; "ued"]; F["gin3"; "ued"]];
			 Or[F["gin2"; "ued"]; F["gin4"; "ued"]]]);
		    (["out"; "loa"],
		     Or[F["gpmLoaFlt"];
			And[F["gin1"; "loa"]; F["gin3"; "loa"]];
			And[F["gin2"; "loa"]; F["gin4"; "loa"]];
			And[F["2uedFaults"]; F["voteFails"]]])]};

  ];;

let arch_ued =
  
  { instances =
      [makeInstance "Sensor1" "Sensor" ();
       makeInstance "Sensor2" "Sensor" ();
       makeInstance "RIU1" "RIU" (); 
       makeInstance "RIU2" "RIU" (); 
       makeInstance "SwitchA" "Switch" (); 
       makeInstance "SwitchB" "Switch" (); 
       makeInstance "GPM" "GPM" (); 
      ];
    connections =
      [ (("RIU1",  "rin"),  ("Sensor1", "out"));
	(("RIU2",  "rin"),  ("Sensor2", "out"));
	(("SwitchA", "swin1"), ("RIU1", "out1"));
	(("SwitchA", "swin2"), ("RIU2", "out1"));
	(("SwitchB", "swin1"), ("RIU1", "out1"));
	(("SwitchB", "swin2"), ("RIU2", "out1"));
	(("GPM",  "gin1"), ("SwitchA", "out1"));
	(("GPM",  "gin2"), ("SwitchA", "out2"));
	(("GPM",  "gin3"), ("SwitchB", "out1"));
	(("GPM",  "gin4"), ("SwitchB", "out2"));];
    top_fault = ("GPM", F["out"; "ued"])
  } ;;

dot_gen_show_ph_file ~rend:"jpg"  arch_ued "mp_arch_ued.gv";;
dot_gen_show_funct_file ~rend:"jpg" library arch_ued "mf_arch_ued.gv";;
dot_gen_show_fault_file ~rend:"jpg" library arch_ued "mflt_arch_ued.gv";;

let arch_ftree = model_to_ftree library arch_ued ;;
let arch_cutsets = cutsets arch_ftree;;
let arch_probErrorCut = probErrorCut arch_ftree ;;
let arch_probErrorCutImp = probErrorCutImp arch_ftree ;;

dot_gen_show_direct_tree_file ~rend:"jpg" "sfdued.gv" arch_ftree ;;
dot_gen_show_tree_file ~rend:"jpg" "sfued.gv" arch_ftree ;;
dot_gen_show_formula_file ~rend:"jpg" "scued.gv" arch_cutsets ;;

let arch_loa =
  {instances=arch_ued.instances;
   connections=arch_ued.connections;
   top_fault = ("GPM", F["out"; "loa"])} ;;

dot_gen_show_ph_file ~rend:"jpg" arch_loa "mp_arch_loa.gv";;
dot_gen_show_funct_file ~rend:"jpg" library arch_loa "mf_arch_loa.gv";;
dot_gen_show_fault_file ~rend:"jpg" library arch_loa "mflt_arch_loa.gv";;

let arch_loa_ftree = model_to_ftree library arch_loa ;;
let arch_loa_cutsets = cutsets arch_loa_ftree  ;;
let arch_loa_probErrorCut = probErrorCut arch_loa_ftree ;;
let arch_loa_probErrorCutImp = probErrorCutImp arch_loa_ftree ;;

dot_gen_show_direct_tree_file ~rend:"jpg" "sfdloa.gv" arch_loa_ftree ;;
dot_gen_show_tree_file ~rend:"jpg" "sfloa.gv" arch_loa_ftree ;;
dot_gen_show_formula_file ~rend:"jpg" "scloa.gv" arch_loa_cutsets ;;
