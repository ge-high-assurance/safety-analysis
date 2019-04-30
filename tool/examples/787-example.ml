(* This example is an architecture inspired by the B787 IMA architecture.

The B787 has five head down displays (HDD) and two head up displays (HUD). The
HDDs are the two primary flight displays (PFD) and the two navigation displays,
one set of each for the pilot and the co-pilot. This display in the middle is
the "aisle stand", which is for the two pilots to input information into the
flight management computers. In addition to these there are also two head-up
displays for added situational awareness.

Note that the architecture contains a mixture of integrated modular avionics
(IMA) and line replaceable units (LRU). While the B787 integrates most of its
functionality into its IMA, also known as the common core system (CCS), some
complex modules are LRUs, creating a sort of "hybrid" system. Our example is the
display application hosted on the CCS. We modeled the following mode: 

- sensor selection is simplex (no voting) 
- cross channel monitoring (ChA and ChB compare) 
- dual FMS, each interfaces with the PFD on their respective side
- no reversionary display mode *)

#use "top.ml";;

let b787_library = 
[ 
 {name         = "Sensor";
               faults     = ["ued"; "loa"];
               input_flows  = [];
               basic_events = ["sen_fl_ued"; "sen_fl_loa"];
               event_info   = [(1.0e-6, 1.0); (1.0e-6, 1.0)];
               output_flows = ["out"];
               formulas     = [(["out"; "ued"], F["sen_fl_ued"]);
                               (["out"; "loa"], F["sen_fl_loa"])]};

 {name         = "DCM3";
               faults             = ["ued"; "loa"];
               input_flows  = ["din1";"din2";"din3"];
               basic_events = ["dcm_fl_ued"; "dcm_fl_loa"];
               event_info   = [(1.0e-6, 1.0); (1.0e-5, 1.0)];
               output_flows = ["out1";"out2";"out3"];
               formulas     = [(["out1";"ued"], Or[F["din1";"ued"];F["dcm_fl_ued"]]);
                               (["out2";"ued"], Or[F["din2";"ued"];F["dcm_fl_ued"]]);
                               (["out3";"ued"], Or[F["din3";"ued"];F["dcm_fl_ued"]]);
                               (["out1";"loa"], Or[F["din1";"loa"];F["dcm_fl_loa"]]);
                               (["out2";"loa"], Or[F["din2";"loa"];F["dcm_fl_loa"]]);
                               (["out3";"loa"], Or[F["din3";"loa"];F["dcm_fl_loa"]]);]};

 {name         = "Switch7";
               faults             = ["ued"; "loa"];
               input_flows  = ["swin1";"swin2";"swin3";"swin4";"swin5";"swin6";"swin7"];
               basic_events = ["sw_fl_ued"; "sw_fl_loa"];
               event_info   = [(1.0e-6, 1.0); (1.0e-6, 1.0)];
               output_flows = ["out1"; "out2"; "out3"; "out4"; "out5"; "out6"; "out7"];
               formulas     = [(["out1";"ued"], Or[F["swin1";"ued"];F["sw_fl_ued"]]);
                               (["out2";"ued"], Or[F["swin2";"ued"];F["sw_fl_ued"]]);
                               (["out3";"ued"], Or[F["swin3";"ued"];F["sw_fl_ued"]]);
                               (["out4";"ued"], Or[F["swin4";"ued"];F["sw_fl_ued"]]);
                               (["out5";"ued"], Or[F["swin5";"ued"];F["sw_fl_ued"]]);
                               (["out6";"ued"], Or[F["swin6";"ued"];F["sw_fl_ued"]]);
                               (["out7";"ued"], Or[F["swin7";"ued"];F["sw_fl_ued"]]);
                               (["out1";"loa"], Or[F["swin1";"loa"];F["sw_fl_loa"]]);
                               (["out2";"loa"], Or[F["swin2";"loa"];F["sw_fl_loa"]]);
                               (["out3";"loa"], Or[F["swin3";"loa"];F["sw_fl_loa"]]);
                               (["out4";"loa"], Or[F["swin4";"loa"];F["sw_fl_loa"]]);
                               (["out5";"loa"], Or[F["swin5";"loa"];F["sw_fl_loa"]]);
                               (["out6";"loa"], Or[F["swin6";"loa"];F["sw_fl_loa"]]);
                               (["out7";"loa"], Or[F["swin7";"loa"];F["sw_fl_loa"]])]};

 (* both IRU and ADC are needed for display *)
 {name         = "GPM";
               faults             = ["ued"; "loa"];
               input_flows  = ["gin1A";"gin2A";"gin3A";"gin4A";"gin5A";"gin1B";"gin2B";"gin3B";"gin4B";"gin5B"];
               basic_events = ["gpm_fl_ued"; "gpm_fl_loa"];
               event_info   = [(2.0e-10, 1.0); (2.0e-10, 1.0)];
               output_flows = ["out"];
               formulas     = [(["out";"ued"], 
                                Or[
                                   Or[ And[F["gin1A";"ued"];F["gin1B";"ued"]]; 
                                          And[F["gin2A";"ued"];F["gin2B";"ued"]]; 
                                          And[F["gin3A";"ued"];F["gin3B";"ued"]] ];
                                   Or[ And[F["gin4A";"ued"];F["gin4B";"ued"]]; 
                                          And[F["gin5A";"ued"];F["gin5B";"ued"]] ]; 
                                   F["gpm_fl_ued"]] );
                               (["out";"loa"],                        
                                Or[
                                   And[ Or[F["gin1A";"loa"];F["gin1B";"loa"]]; 
                                          Or[F["gin2A";"loa"];F["gin2B";"loa"]]; 
                                          Or[F["gin3A";"loa"];F["gin3B";"loa"]] ];
                                   And[ Or[F["gin4A";"loa"];F["gin4B";"loa"]]; 
                                          Or[F["gin5A";"loa"];F["gin5B";"loa"]] ]; 
                                   F["gpm_fl_loa"]] )]};

 {name         = "GGM";
               faults         = ["ued"; "loa"];
               input_flows  = ["ginLA";"ginLB";"ginRA";"ginRB";];
               basic_events = ["ggm_fl_ued"; "ggm_fl_loa"];
               event_info   = [(2.0e-10, 1.0); (2.0e-10, 1.0)];
               output_flows = ["out1"; "out2"];
               formulas     = [(["out1";"ued"], Or[Or[F["ginLA";"ued"];F["ginLB";"ued"]]; F["ggm_fl_ued"]]);
                               (["out2";"ued"], Or[Or[F["ginRA";"ued"];F["ginRB";"ued"]]; F["ggm_fl_ued"]]);
                               (["out1";"loa"], Or[And[F["ginLA";"loa"];F["ginLB";"loa"]]; F["ggm_fl_loa"]]);
                               (["out2";"loa"], Or[And[F["ginRA";"loa"];F["ginRB";"loa"]]; F["ggm_fl_loa"]])]};

 {name            = "Display";
                  faults                  = ["ued";];
                  input_flows  = ["din"];
                  basic_events = ["display_fl_ued"];
                  event_info      = [(1.0e-10, 1.0);];
                  output_flows = ["out";];
                  formulas        = [(["out";"ued"], Or[F["din";"ued"]; F["display_fl_ued"]])]};
 ];;

(* ----- CHECK LIBRARY ----- *)
checkLibrary_componentUnique b787_library;;
checkLibrary_nonEmptyFaults b787_library;;
checkLibrary_disjointInputFlowsandBasicEvents b787_library;;
checkLibrary_listsAreConsistentLengths b787_library;;
checkLibrary_allOutputFaultsHaveFormulas b787_library;;
checkLibrary_formulasMakeSense b787_library;;

(* ----- MODEL ----- *)
let fmc_display =
{ instances =
	[makeInstance "iru_Left" "Sensor" ();
 	 makeInstance "iru_Ctr" "Sensor" ();
 	 makeInstance "iru_Right" "Sensor" ();
 	 makeInstance "adc_Left" "Sensor" ();
 	 makeInstance "adc_Right" "Sensor" ();
 	 makeInstance "dcm" "DCM3" ();
 	 makeInstance "sw1a" "Switch7" ();
 	 makeInstance "sw1b" "Switch7" ();
	 makeInstance "gpm1" "GPM" ();
	 makeInstance "gpm2" "GPM" ();
 	 makeInstance "ggmL" "GGM" ();
 	 makeInstance "ggmR" "GGM" ();
	 makeInstance "hud_Left" "Display" ();
	 makeInstance "hdd1" "Display" ();
	 makeInstance "hdd2" "Display" ();
	 makeInstance "hdd3" "Display" ();
	 makeInstance "hdd4" "Display" ();
	 makeInstance "hdd5" "Display" ();
	 makeInstance "hud_Right" "Display" ();
	];
  connections = 
  	[ (("dcm", "din1"), ("iru_Left","out"));
  	  (("dcm", "din2"), ("iru_Ctr","out"));
  	  (("dcm", "din3"), ("iru_Right","out"));
  	  (("sw1a", "swin1"), ("dcm","out1"));
  	  (("sw1a", "swin2"), ("dcm","out2"));
  	  (("sw1a", "swin3"), ("dcm","out3"));
  	  (("sw1a", "swin4"), ("adc_Left","out"));
  	  (("sw1a", "swin5"), ("adc_Right","out"));
  	  (("sw1b", "swin1"), ("dcm","out1"));
  	  (("sw1b", "swin2"), ("dcm","out2"));
  	  (("sw1b", "swin3"), ("dcm","out3"));
  	  (("sw1b", "swin4"), ("adc_Left","out"));
  	  (("sw1b", "swin5"), ("adc_Right","out"));
  	  (("gpm1", "gin1A"), ("sw1a","out1"));
  	  (("gpm1", "gin2A"), ("sw1a","out2"));
  	  (("gpm1", "gin3A"), ("sw1a","out3"));
  	  (("gpm1", "gin4A"), ("sw1a","out4"));
  	  (("gpm1", "gin5A"), ("sw1a","out5"));
  	  (("gpm1", "gin1B"), ("sw1b","out1"));
  	  (("gpm1", "gin2B"), ("sw1b","out2"));
  	  (("gpm1", "gin3B"), ("sw1b","out3"));
  	  (("gpm1", "gin4B"), ("sw1b","out4"));
  	  (("gpm1", "gin5B"), ("sw1b","out5"));
  	  (("gpm2", "gin1A"), ("sw1a","out1"));
  	  (("gpm2", "gin2A"), ("sw1a","out2"));
  	  (("gpm2", "gin3A"), ("sw1a","out3"));
  	  (("gpm2", "gin4A"), ("sw1a","out4"));
  	  (("gpm2", "gin5A"), ("sw1a","out5"));
  	  (("gpm2", "gin1B"), ("sw1b","out1"));
  	  (("gpm2", "gin2B"), ("sw1b","out2"));
  	  (("gpm2", "gin3B"), ("sw1b","out3"));
  	  (("gpm2", "gin4B"), ("sw1b","out4"));
  	  (("gpm2", "gin5B"), ("sw1b","out5"));
  	  (("sw1a", "swin6"), ("gpm1","out"));
  	  (("sw1a", "swin7"), ("gpm2","out"));
  	  (("sw1b", "swin6"), ("gpm1","out"));
  	  (("sw1b", "swin7"), ("gpm2","out"));
  	  (("ggmL", "ginLA"), ("sw1a","out6"));
  	  (("ggmL", "ginLB"), ("sw1b","out6"));
  	  (("ggmL", "ginRA"), ("sw1a","out7"));
  	  (("ggmL", "ginRB"), ("sw1b","out7"));
  	  (("ggmR", "ginLA"), ("sw1a","out6"));
  	  (("ggmR", "ginLB"), ("sw1b","out6"));
  	  (("ggmR", "ginRA"), ("sw1a","out7"));
  	  (("ggmR", "ginRB"), ("sw1b","out7"));
  	  (("hud_Left", "din"), ("ggmL","out1"));
  	  (("hdd1", "din"), ("ggmL","out1"));
  	  (("hdd2", "din"), ("ggmL","out1"));
  	  (("hdd3", "din"), ("ggmL","out1"));
  	  (("hdd4", "din"), ("ggmR","out2"));
  	  (("hdd5", "din"), ("ggmR","out2"));
  	  (("hud_Right", "din"), ("ggmR","out2"));
  	];
  top_fault = ("hdd1", F["out";"ued"]);
};;

(* ----- CHECK MODEL ----- *)
checkModel_instanceNameUnique fmc_display;;
checkModel_cnameInstanceIsDefinedInLibrary fmc_display b787_library;;
checkModel_exposureOfBasicIsDefinedInLibrary fmc_display b787_library;;
checkModel_validConnections fmc_display b787_library;;
checkModel_inputFlowUnique fmc_display;;

(* ----- ANALYSES ----- *)
let fmc_display_normal_ftree = model_to_ftree b787_library fmc_display ;;
let fmc_display_normal_cutsets = cutsets fmc_display_normal_ftree;;
probErrorCutImp fmc_display_normal_ftree;;
probErrorCut fmc_display_normal_ftree;;

(* ---- VISUALIZATIONS --- *)
dot_gen_show_funct_file b787_library fmc_display "func_787.gv";;
dot_gen_show_fault_file b787_library fmc_display "flt_787.gv";;
dot_gen_show_direct_tree_file "ft_787_ued.gv" fmc_display_normal_ftree;;
dot_gen_show_tree_file "t_787_ued.gv" fmc_display_normal_ftree ;;
