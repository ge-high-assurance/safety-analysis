#use "top.ml";;

let b777_library = 
  [{name = "DME";
    faults = ["loa"];
    input_flows = [];
    basic_events = ["dme_fl"];
    event_info = [(1e-06, 1.)];
    output_flows = ["out"];
    formulas = [(["out"; "loa"], F ["dme_fl"])]};
   {name = "MCDU";
    faults = ["loa"];
    input_flows = [];
    basic_events = ["mcdu_fl"];
    event_info = [(1e-06, 1.)];
    output_flows = ["out"];
    formulas = [(["out"; "loa"], F ["mcdu_fl"])]};
   {name = "IRU";
    faults = ["loa"];
    input_flows = [];
    basic_events = ["iru_fl"];
    event_info = [(1e-06, 1.)];
    output_flows = ["out"];
    formulas = [(["out"; "loa"], F ["iru_fl"])]};
   {name = "IOM22";
    faults = ["loa"];
    input_flows = ["iom_in1"; "iom_in2"];
    basic_events = ["iom_fl"];
    event_info = [(1e-06, 1.)];
    output_flows = ["A"; "B"];
    formulas =
       [(["A"; "loa"],
	 Or [And [F ["iom_in1"; "loa"]; F ["iom_in2"; "loa"]]; F ["iom_fl"]]);
	(["B"; "loa"], F ["A"; "loa"])]};
   {name = "IOM21";
    faults = ["loa"];
    input_flows = ["inA"; "inB"];
    basic_events = ["iom_fl"];
    event_info = [(1e-06, 1.)];
    output_flows = ["out"];
    formulas =
       [(["out"; "loa"],
	 Or [And [F ["inA"; "loa"]; F ["inB"; "loa"]]; F ["iom_fl"]])]};
   {name = "FMC";
    faults = ["loa"];
    input_flows = ["inA1"; "inA2"; "inA3"; "inB1"; "inB2"; "inB3"];
    basic_events = ["fmc_fl"];
    event_info = [(2e-10, 1.)];
    output_flows = ["outA"; "outB"];
    formulas =
       [(["outA"; "loa"],
	 Or
           [F ["fmc_fl"]; F ["inA1"; "loa"]; F ["inA2"; "loa"];
            F ["inA3"; "loa"]]);
	(["outB"; "loa"],
	 Or
           [F ["fmc_fl"]; F ["inB1"; "loa"]; F ["inB2"; "loa"];
            F ["inB3"; "loa"]])]};
   {name = "SG"; faults = ["loa"];
    input_flows = ["in"];
    basic_events = ["sg_fl"];
    event_info = [(1e-06, 1.)];
    output_flows = ["out"];
    formulas = [(["out"; "loa"], Or [F ["in"; "loa"]; F ["sg_fl"]])]};
   {name = "PFD";
    faults = ["loa"];
    input_flows = ["in1"; "in2"];
    basic_events = ["pfd_fl"];
    event_info = [(2e-10, 1.)];
    output_flows = ["out"];
    formulas =
       [(["out"; "loa"],
	 Or [And [F ["in1"; "loa"]; F ["in2"; "loa"]]; F ["pfd_fl"]])]};
   {name = "ND";
    faults = ["loa"];
    input_flows = ["in1"; "in2"];
    basic_events = ["nd_fl"];
    event_info = [(2e-10, 1.)];
    output_flows = ["out"];
    formulas =
       [(["out"; "loa"],
	 Or [And [F ["in1"; "loa"]; F ["in2"; "loa"]]; F ["nd_fl"]])]};
   {name = "BUS5_8";
    faults = ["loa"];
    input_flows = ["i1"; "i2"; "i3"; "i4"; "i5"];
    basic_events = [];
    event_info = [];
    output_flows = ["o11"; "o12"; "o13"; "o21"; "o22"; "o23"; "o4"; "o5"];
    formulas =
       [(["o11"; "loa"], F ["i1"; "loa"]); (["o21"; "loa"], F ["o11"; "loa"]);
	(["o12"; "loa"], F ["i2"; "loa"]); (["o22"; "loa"], F ["o12"; "loa"]);
	(["o13"; "loa"], F ["i3"; "loa"]); (["o23"; "loa"], F ["o13"; "loa"]);
	(["o4"; "loa"], F ["i4"; "loa"]); (["o5"; "loa"], F ["i5"; "loa"])]};
   {name = "IOM22_alt";
    faults = ["loa"];
    input_flows = ["iom_in1"; "iom_in2"];
    basic_events = ["iom_fl"];
    event_info = [(1e-06, 1.)];
    output_flows = ["A"; "B"];
    formulas =
       [(["A"; "loa"], Or [F ["iom_in1"; "loa"]; F ["iom_fl"]]);
	(["B"; "loa"], Or [F ["iom_in2"; "loa"]; F ["iom_fl"]])]};
   {name = "FMC6_2_alt";
    faults = ["loa"];
    input_flows = ["inA1"; "inA2"; "inA3"; "inB1"; "inB2"; "inB3"];
    basic_events = ["fmc_fl"];
    event_info = [(2e-10, 1.)];
    output_flows = ["outA"; "outB"];
    formulas =
       [(["outA"; "loa"],
	 Or
           [F ["fmc_fl"]; And [F ["inA1"; "loa"]; F ["inB2"; "loa"]];
            And [F ["inA2"; "loa"]; F ["inB3"; "loa"]];
            And [F ["inA3"; "loa"]; F ["inB1"; "loa"]]]);
	(["outB"; "loa"], F ["outA"; "loa"])]};
   {name = "SG2_1";
    faults = ["loa"];
    input_flows = ["in1"; "in2"];
    basic_events = ["sg_fl"];
    event_info = [(1e-06, 1.)];
    output_flows = ["out"];
    formulas =
       [(["out"; "loa"], Or [F ["in1"; "loa"]; F ["in2"; "loa"]; F ["sg_fl"]])]};
   {name = "IOM2_1_NO_INT_FAIL";
    faults = ["loa"];
    input_flows = ["inA"; "inB"];
    basic_events = [];
    event_info = [];
    output_flows = ["out"];
    formulas = [(["out"; "loa"], And [F ["inA"; "loa"]; F ["inB"; "loa"]])]};
   {name = "PILOT_4DU";
    faults = ["loa"];
    input_flows = ["in_pfd1"; "in_nd1"; "in_pfd2"; "in_nd2"];
    basic_events = [];
    event_info = [];
    output_flows = ["out"];
    formulas =
       [(["out"; "loa"],
	 And
           [F ["in_pfd1"; "loa"]; F ["in_pfd2"; "loa"]; F ["in_nd1"; "loa"];
            F ["in_nd2"; "loa"]])]}];;

let b777_model =
  {instances =
      [makeInstance "iru1" "IRU" ();
       makeInstance "iru2" "IRU" ();
       makeInstance "mcdu1" "MCDU" ();
       makeInstance "mcdu2" "MCDU" ();
       makeInstance "dme1" "DME" ();
       makeInstance "dme2" "DME" ();
       makeInstance "iom1" "IOM22" ();
       makeInstance "iom2" "IOM22" ();
       makeInstance "iom3" "IOM22" ();
       makeInstance "fmc1" "FMC" ();
       makeInstance "fmc2" "FMC" ();
       makeInstance "iom4" "IOM21" ();
       makeInstance "iom5" "IOM21" ();
       makeInstance "sg1" "SG" ();
       makeInstance "sg2" "SG" ();
       makeInstance "nd1" "ND" ();
       makeInstance "nd2" "ND" ();
       makeInstance "pfd1" "PFD" ();
       makeInstance "pfd2" "PFD" ();
      ];
   connections =
      [ (("iom1", "iom_in1"), ("iru1", "out"));
	(("iom1", "iom_in2"), ("iru2", "out"));
	(("iom2", "iom_in1"), ("mcdu1", "out"));
	(("iom2", "iom_in2"), ("mcdu2", "out"));
	(("iom3", "iom_in1"), ("dme1", "out"));
	(("iom3", "iom_in2"), ("dme2", "out"));
	(("fmc1", "inA1"), ("iom1", "A"));
	(("fmc1", "inA2"), ("iom2", "A"));
	(("fmc1", "inA3"), ("iom3", "A"));
	(("fmc1", "inB1"), ("iom1", "B"));
	(("fmc1", "inB2"), ("iom2", "B"));
	(("fmc1", "inB3"), ("iom3", "B"));
	(("fmc2", "inA1"), ("iom1", "A"));
	(("fmc2", "inA2"), ("iom2", "A"));
	(("fmc2", "inA3"), ("iom3", "A"));
	(("fmc2", "inB1"), ("iom1", "B"));
	(("fmc2", "inB2"), ("iom2", "B"));
	(("fmc2", "inB3"), ("iom3", "B"));
	(("iom4", "inA"), ("fmc1", "outA"));
	(("iom4", "inB"), ("fmc2", "outB"));
	(("iom5", "inA"), ("fmc2", "outA"));
	(("iom5", "inB"), ("fmc1", "outB"));
	(("sg1", "in"), ("iom4", "out"));
	(("sg2", "in"), ("iom5", "out"));
	(("pfd1", "in1"), ("sg1", "out"));
	(("pfd1", "in2"), ("sg2", "out"));
	(("pfd2", "in1"), ("sg1", "out"));
	(("pfd2", "in2"), ("sg2", "out"));
	(("nd1", "in1"), ("sg1", "out"));
	(("nd1", "in2"), ("sg2", "out"));
	(("nd2", "in1"), ("sg1", "out"));
	(("nd2", "in2"), ("sg2", "out"));
      ];
   top_fault =("pfd1", F["out"; "loa"])
  } ;;

dot_gen_show_ph_file     b777_model              "p777.gv";;
dot_gen_show_funct_file  b777_library b777_model "fn777.gv";;
dot_gen_show_fault_file  b777_library b777_model "fa777.gv";;

let b777_ftree = model_to_ftree b777_library b777_model ;;

(*
val b777_ftree : (string * string) ftree =
  SUM
   [Leaf (("pfd1", "pfd_fl"), 2e-10, 1.);
    PRO
     [SUM
       [SUM
         [Leaf (("iom4", "iom_fl"), 1e-06, 1.);
          PRO
           [SUM
             [Leaf (("fmc1", "fmc_fl"), 2e-10, 1.);
              SUM
               [Leaf (("iom1", "iom_fl"), 1e-06, 1.);
                PRO
                 [Leaf (("iru1", "iru_fl"), 1e-06, 1.);
                  Leaf (("iru2", "iru_fl"), 1e-06, 1.)]];
              SUM
               [Leaf (("iom2", "iom_fl"), 1e-06, 1.);
                PRO
                 [Leaf (("mcdu1", "mcdu_fl"), 1e-06, 1.);
                  Leaf (("mcdu2", "mcdu_fl"), 1e-06, 1.)]];
              SUM
               [Leaf (("iom3", "iom_fl"), 1e-06, 1.);
                PRO
                 [Leaf (("dme1", "dme_fl"), 1e-06, 1.);
                  Leaf (("dme2", "dme_fl"), 1e-06, 1.)]]];
            SUM
             [Leaf (("fmc2", "fmc_fl"), 2e-10, 1.);
              SUM
               [Leaf (("iom1", "iom_fl"), 1e-06, 1.);
                PRO
                 [Leaf (("iru1", "iru_fl"), 1e-06, 1.);
                  Leaf (("iru2", "iru_fl"), 1e-06, 1.)]];
              SUM
               [Leaf (("iom2", "iom_fl"), 1e-06, 1.);
                PRO
                 [Leaf (("mcdu1", "mcdu_fl"), 1e-06, 1.);
                  Leaf (("mcdu2", "mcdu_fl"), 1e-06, 1.)]];
              SUM
               [Leaf (("iom3", "iom_fl"), 1e-06, 1.);
                PRO
                 [Leaf (("dme1", "dme_fl"), 1e-06, 1.);
                  Leaf (("dme2", "dme_fl"), 1e-06, 1.)]]]]];
        Leaf (("sg1", "sg_fl"), 1e-06, 1.)];
      SUM
       [SUM
         [Leaf (("iom5", "iom_fl"), 1e-06, 1.);
          PRO
           [SUM
             [Leaf (("fmc2", "fmc_fl"), 2e-10, 1.);
              SUM
               [Leaf (("iom1", "iom_fl"), 1e-06, 1.);
                PRO
                 [Leaf (("iru1", "iru_fl"), 1e-06, 1.);
                  Leaf (("iru2", "iru_fl"), 1e-06, 1.)]];
              SUM
               [Leaf (("iom2", "iom_fl"), 1e-06, 1.);
                PRO
                 [Leaf (("mcdu1", "mcdu_fl"), 1e-06, 1.);
                  Leaf (("mcdu2", "mcdu_fl"), 1e-06, 1.)]];
              SUM
               [Leaf (("iom3", "iom_fl"), 1e-06, 1.);
                PRO
                 [Leaf (("dme1", "dme_fl"), 1e-06, 1.);
                  Leaf (("dme2", "dme_fl"), 1e-06, 1.)]]];
            SUM
             [Leaf (("fmc1", "fmc_fl"), 2e-10, 1.);
              SUM
               [Leaf (("iom1", "iom_fl"), 1e-06, 1.);
                PRO
                 [Leaf (("iru1", "iru_fl"), 1e-06, 1.);
                  Leaf (("iru2", "iru_fl"), 1e-06, 1.)]];
              SUM
               [Leaf (("iom2", "iom_fl"), 1e-06, 1.);
                PRO
                 [Leaf (("mcdu1", "mcdu_fl"), 1e-06, 1.);
                  Leaf (("mcdu2", "mcdu_fl"), 1e-06, 1.)]];
              SUM
               [Leaf (("iom3", "iom_fl"), 1e-06, 1.);
                PRO
                 [Leaf (("dme1", "dme_fl"), 1e-06, 1.);
                  Leaf (("dme2", "dme_fl"), 1e-06, 1.)]]]]];
        Leaf (("sg2", "sg_fl"), 1e-06, 1.)]]]
*)

dot_gen_show_direct_tree_file  "td777.gv" b777_ftree ;;
dot_gen_show_tree_file  "tr777.gv" b777_ftree ;;

let b777_cutsets = cutsets b777_ftree;;

(*
  Sum
   [Var ("iom1", "iom_fl"); Var ("iom2", "iom_fl"); Var ("iom3", "iom_fl");
    Var ("pfd1", "pfd_fl");
    Pro [Var ("dme1", "dme_fl"); Var ("dme2", "dme_fl")];
    Pro [Var ("fmc1", "fmc_fl"); Var ("fmc2", "fmc_fl")];
    Pro [Var ("iom4", "iom_fl"); Var ("iom5", "iom_fl")];
    Pro [Var ("iom4", "iom_fl"); Var ("sg2", "sg_fl")];
    Pro [Var ("iom5", "iom_fl"); Var ("sg1", "sg_fl")];
    Pro [Var ("iru1", "iru_fl"); Var ("iru2", "iru_fl")];
    Pro [Var ("mcdu1", "mcdu_fl"); Var ("mcdu2", "mcdu_fl")];
    Pro [Var ("sg1", "sg_fl"); Var ("sg2", "sg_fl")]]

*)

dot_gen_show_formula_file  "c777.gv" b777_cutsets ;;

let b777_probErrorCut = probErrorCut b777_ftree ;;
(*
  (3.00020249934e-06, 3.00020249934e-06)
*)

let b777_probErrorCutImp = probErrorCutImp b777_ftree ;;
(* 
  [(Var ("iom1", "iom_fl"), 9.99999499984e-07, 0.333310668264);
   (Var ("iom2", "iom_fl"), 9.99999499984e-07, 0.333310668264);
   (Var ("iom3", "iom_fl"), 9.99999499984e-07, 0.333310668264);
   (Var ("pfd1", "pfd_fl"), 2.00000016548e-10, 6.66621725007e-05);
   (Pro [Var ("dme1", "dme_fl"); Var ("dme2", "dme_fl")], 9.99998999969e-13,
    3.33310501604e-07);
   (Pro [Var ("iom4", "iom_fl"); Var ("iom5", "iom_fl")], 9.99998999969e-13,
    3.33310501604e-07);
   (Pro [Var ("iom4", "iom_fl"); Var ("sg2", "sg_fl")], 9.99998999969e-13,
    3.33310501604e-07);
   (Pro [Var ("iom5", "iom_fl"); Var ("sg1", "sg_fl")], 9.99998999969e-13,
    3.33310501604e-07);
   (Pro [Var ("iru1", "iru_fl"); Var ("iru2", "iru_fl")], 9.99998999969e-13,
    3.33310501604e-07);
   (Pro [Var ("mcdu1", "mcdu_fl"); Var ("mcdu2", "mcdu_fl")],
    9.99998999969e-13, 3.33310501604e-07);
   (Pro [Var ("sg1", "sg_fl"); Var ("sg2", "sg_fl")], 9.99998999969e-13,
    3.33310501604e-07);
   (Pro [Var ("fmc1", "fmc_fl"); Var ("fmc2", "fmc_fl")], 4.00000066192e-20,
    1.33324356033e-14)]
*)

(* Notice that failures of iom1-iom3 are what contribute most 
   to the probability of failure for our top-level failure.

   We now consider a different architecture, where we shuffle
   sensor-to-IOM connections. 
*)

let b777_model_shuffle =
  { instances =
      [makeInstance "iru1" "IRU" ();
       makeInstance "iru2" "IRU" ();
       makeInstance "mcdu1" "MCDU" ();
       makeInstance "mcdu2" "MCDU" ();
       makeInstance "dme1" "DME" ();
       makeInstance "dme2" "DME" ();
       makeInstance "iom1" "IOM22_alt" ();	(* New type for this example *)
       makeInstance "iom2" "IOM22_alt" ();
       makeInstance "iom3" "IOM22_alt" ();
       makeInstance "fmc1" "FMC6_2_alt" ();	(* New type for this example *)
       makeInstance "fmc2" "FMC6_2_alt" ();
       makeInstance "iom4" "IOM21" ();
       makeInstance "iom5" "IOM21" ();
       makeInstance "sg1" "SG" ();
       makeInstance "sg2" "SG" ();
       makeInstance "nd1" "ND" ();
       makeInstance "nd2" "ND" ();
       makeInstance "pfd1" "PFD" ();
       makeInstance "pfd2" "PFD" ();
      ];
    connections =
      [ (("iom1", "iom_in1"), ("iru1", "out"));
	(("iom1", "iom_in2"), ("dme2", "out"));
	(("iom2", "iom_in1"), ("mcdu1", "out"));
	(("iom2", "iom_in2"), ("iru2", "out"));
	(("iom3", "iom_in1"), ("dme1", "out"));
	(("iom3", "iom_in2"), ("mcdu2", "out"));
	(("fmc1", "inA1"), ("iom1", "A"));
	(("fmc1", "inA2"), ("iom2", "A"));
	(("fmc1", "inA3"), ("iom3", "A"));
	(("fmc1", "inB1"), ("iom1", "B"));
	(("fmc1", "inB2"), ("iom2", "B"));
	(("fmc1", "inB3"), ("iom3", "B"));
	(("fmc2", "inA1"), ("iom1", "A"));
	(("fmc2", "inA2"), ("iom2", "A"));
	(("fmc2", "inA3"), ("iom3", "A"));
	(("fmc2", "inB1"), ("iom1", "B"));
	(("fmc2", "inB2"), ("iom2", "B"));
	(("fmc2", "inB3"), ("iom3", "B"));
	(("iom4", "inA"), ("fmc1", "outA"));
	(("iom4", "inB"), ("fmc2", "outB"));
	(("iom5", "inA"), ("fmc2", "outA"));
	(("iom5", "inB"), ("fmc1", "outB"));
	(("sg1", "in"), ("iom4", "out"));
	(("sg2", "in"), ("iom5", "out"));
	(("pfd1", "in1"), ("sg1", "out"));
	(("pfd1", "in2"), ("sg2", "out"));
	(("pfd2", "in1"), ("sg1", "out"));
	(("pfd2", "in2"), ("sg2", "out"));
	(("nd1", "in1"), ("sg1", "out"));
	(("nd1", "in2"), ("sg2", "out"));
	(("nd2", "in1"), ("sg1", "out"));
	(("nd2", "in2"), ("sg2", "out"));
      ];
    top_fault =("pfd1", F["out"; "loa"])
  } ;;

dot_gen_show_ph_file  b777_model_shuffle "p777s.gv";;
dot_gen_show_funct_file  b777_library b777_model_shuffle "fn777s.gv";;
dot_gen_show_fault_file  b777_library b777_model_shuffle "fa777s.gv";;

let b777_s_ftree = model_to_ftree b777_library b777_model_shuffle;;

(*
  SUM
   [Leaf (("pfd1", "pfd_fl"), 2e-10, 1.);
    PRO
     [SUM
       [SUM
         [Leaf (("iom4", "iom_fl"), 1e-06, 1.);
          PRO
           [SUM
             [Leaf (("fmc1", "fmc_fl"), 2e-10, 1.);
              PRO
               [SUM
                 [Leaf (("iom1", "iom_fl"), 1e-06, 1.);
                  Leaf (("iru1", "iru_fl"), 1e-06, 1.)];
                SUM
                 [Leaf (("iom2", "iom_fl"), 1e-06, 1.);
                  Leaf (("iru2", "iru_fl"), 1e-06, 1.)]];
              PRO
               [SUM
                 [Leaf (("iom2", "iom_fl"), 1e-06, 1.);
                  Leaf (("mcdu1", "mcdu_fl"), 1e-06, 1.)];
                SUM
                 [Leaf (("iom3", "iom_fl"), 1e-06, 1.);
                  Leaf (("mcdu2", "mcdu_fl"), 1e-06, 1.)]];
              PRO
               [SUM
                 [Leaf (("iom3", "iom_fl"), 1e-06, 1.);
                  Leaf (("dme1", "dme_fl"), 1e-06, 1.)];
                SUM
                 [Leaf (("iom1", "iom_fl"), 1e-06, 1.);
                  Leaf (("dme2", "dme_fl"), 1e-06, 1.)]]];
            SUM
             [Leaf (("fmc2", "fmc_fl"), 2e-10, 1.);
              PRO
               [SUM
                 [Leaf (("iom1", "iom_fl"), 1e-06, 1.);
                  Leaf (("iru1", "iru_fl"), 1e-06, 1.)];
                SUM
                 [Leaf (("iom2", "iom_fl"), 1e-06, 1.);
                  Leaf (("iru2", "iru_fl"), 1e-06, 1.)]];
              PRO
               [SUM
                 [Leaf (("iom2", "iom_fl"), 1e-06, 1.);
                  Leaf (("mcdu1", "mcdu_fl"), 1e-06, 1.)];
                SUM
                 [Leaf (("iom3", "iom_fl"), 1e-06, 1.);
                  Leaf (("mcdu2", "mcdu_fl"), 1e-06, 1.)]];
              PRO
               [SUM
                 [Leaf (("iom3", "iom_fl"), 1e-06, 1.);
                  Leaf (("dme1", "dme_fl"), 1e-06, 1.)];
                SUM
                 [Leaf (("iom1", "iom_fl"), 1e-06, 1.);
                  Leaf (("dme2", "dme_fl"), 1e-06, 1.)]]]]];
        Leaf (("sg1", "sg_fl"), 1e-06, 1.)];
      SUM
       [SUM
         [Leaf (("iom5", "iom_fl"), 1e-06, 1.);
          PRO
           [SUM
             [Leaf (("fmc2", "fmc_fl"), 2e-10, 1.);
              PRO
               [SUM
                 [Leaf (("iom1", "iom_fl"), 1e-06, 1.);
                  Leaf (("iru1", "iru_fl"), 1e-06, 1.)];
                SUM
                 [Leaf (("iom2", "iom_fl"), 1e-06, 1.);
                  Leaf (("iru2", "iru_fl"), 1e-06, 1.)]];
              PRO
               [SUM
                 [Leaf (("iom2", "iom_fl"), 1e-06, 1.);
                  Leaf (("mcdu1", "mcdu_fl"), 1e-06, 1.)];
                SUM
                 [Leaf (("iom3", "iom_fl"), 1e-06, 1.);
                  Leaf (("mcdu2", "mcdu_fl"), 1e-06, 1.)]];
              PRO
               [SUM
                 [Leaf (("iom3", "iom_fl"), 1e-06, 1.);
                  Leaf (("dme1", "dme_fl"), 1e-06, 1.)];
                SUM
                 [Leaf (("iom1", "iom_fl"), 1e-06, 1.);
                  Leaf (("dme2", "dme_fl"), 1e-06, 1.)]]];
            SUM
             [Leaf (("fmc1", "fmc_fl"), 2e-10, 1.);
              PRO
               [SUM
                 [Leaf (("iom1", "iom_fl"), 1e-06, 1.);
                  Leaf (("iru1", "iru_fl"), 1e-06, 1.)];
                SUM
                 [Leaf (("iom2", "iom_fl"), 1e-06, 1.);
                  Leaf (("iru2", "iru_fl"), 1e-06, 1.)]];
              PRO
               [SUM
                 [Leaf (("iom2", "iom_fl"), 1e-06, 1.);
                  Leaf (("mcdu1", "mcdu_fl"), 1e-06, 1.)];
                SUM
                 [Leaf (("iom3", "iom_fl"), 1e-06, 1.);
                  Leaf (("mcdu2", "mcdu_fl"), 1e-06, 1.)]];
              PRO
               [SUM
                 [Leaf (("iom3", "iom_fl"), 1e-06, 1.);
                  Leaf (("dme1", "dme_fl"), 1e-06, 1.)];
                SUM
                 [Leaf (("iom1", "iom_fl"), 1e-06, 1.);
                  Leaf (("dme2", "dme_fl"), 1e-06, 1.)]]]]];
        Leaf (("sg2", "sg_fl"), 1e-06, 1.)]]]
*)

dot_gen_show_direct_tree_file  "td777s.gv" b777_s_ftree ;;
dot_gen_show_tree_file  "tr777s.gv" b777_s_ftree ;;

let b777_s_cutsets = cutsets b777_s_ftree;;
(*
  Sum
   [Var ("pfd1", "pfd_fl");
    Pro [Var ("dme1", "dme_fl"); Var ("dme2", "dme_fl")];
    Pro [Var ("dme1", "dme_fl"); Var ("iom1", "iom_fl")];
    Pro [Var ("dme2", "dme_fl"); Var ("iom3", "iom_fl")];
    Pro [Var ("fmc1", "fmc_fl"); Var ("fmc2", "fmc_fl")];
    Pro [Var ("iom1", "iom_fl"); Var ("iom2", "iom_fl")];
    Pro [Var ("iom1", "iom_fl"); Var ("iom3", "iom_fl")];
    Pro [Var ("iom1", "iom_fl"); Var ("iru2", "iru_fl")];
    Pro [Var ("iom2", "iom_fl"); Var ("iom3", "iom_fl")];
    Pro [Var ("iom2", "iom_fl"); Var ("iru1", "iru_fl")];
    Pro [Var ("iom2", "iom_fl"); Var ("mcdu2", "mcdu_fl")];
    Pro [Var ("iom3", "iom_fl"); Var ("mcdu1", "mcdu_fl")];
    Pro [Var ("iom4", "iom_fl"); Var ("iom5", "iom_fl")];
    Pro [Var ("iom4", "iom_fl"); Var ("sg2", "sg_fl")];
    Pro [Var ("iom5", "iom_fl"); Var ("sg1", "sg_fl")];
    Pro [Var ("iru1", "iru_fl"); Var ("iru2", "iru_fl")];
    Pro [Var ("mcdu1", "mcdu_fl"); Var ("mcdu2", "mcdu_fl")];
    Pro [Var ("sg1", "sg_fl"); Var ("sg2", "sg_fl")]]
*)

dot_gen_show_formula_file  "c777s.gv" b777_s_cutsets ;;

let b777_s_probErrorCut = probErrorCut b777_s_ftree ;;
(*
  (2.15999973584e-10, 2.15999973584e-10)
*)

let b777_s_probErrorCutImp = probErrorCutImp b777_s_ftree ;;
(*
  [(Var ("pfd1", "pfd_fl"), 2.00000016548e-10, 0.925926115773);
   (Pro [Var ("dme1", "dme_fl"); Var ("dme2", "dme_fl")], 9.99998999969e-13,
    0.00462962556603);
   (Pro [Var ("dme1", "dme_fl"); Var ("iom1", "iom_fl")], 9.99998999969e-13,
    0.00462962556603);
   (Pro [Var ("dme2", "dme_fl"); Var ("iom3", "iom_fl")], 9.99998999969e-13,
    0.00462962556603);
   (Pro [Var ("iom1", "iom_fl"); Var ("iom2", "iom_fl")], 9.99998999969e-13,
    0.00462962556603);
   (Pro [Var ("iom1", "iom_fl"); Var ("iom3", "iom_fl")], 9.99998999969e-13,
    0.00462962556603);
   (Pro [Var ("iom1", "iom_fl"); Var ("iru2", "iru_fl")], 9.99998999969e-13,
    0.00462962556603);
   (Pro [Var ("iom2", "iom_fl"); Var ("iom3", "iom_fl")], 9.99998999969e-13,
    0.00462962556603);
   (Pro [Var ("iom2", "iom_fl"); Var ("iru1", "iru_fl")], 9.99998999969e-13,
    0.00462962556603);
   (Pro [Var ("iom2", "iom_fl"); Var ("mcdu2", "mcdu_fl")],
    9.99998999969e-13, 0.00462962556603);
   (Pro [Var ("iom3", "iom_fl"); Var ("mcdu1", "mcdu_fl")],
    9.99998999969e-13, 0.00462962556603);
   (Pro [Var ("iom4", "iom_fl"); Var ("iom5", "iom_fl")], 9.99998999969e-13,
    0.00462962556603);
   (Pro [Var ("iom4", "iom_fl"); Var ("sg2", "sg_fl")], 9.99998999969e-13,
    0.00462962556603);
   (Pro [Var ("iom5", "iom_fl"); Var ("sg1", "sg_fl")], 9.99998999969e-13,
    0.00462962556603);
   (Pro [Var ("iru1", "iru_fl"); Var ("iru2", "iru_fl")], 9.99998999969e-13,
    0.00462962556603);
   (Pro [Var ("mcdu1", "mcdu_fl"); Var ("mcdu2", "mcdu_fl")],
    9.99998999969e-13, 0.00462962556603);
   (Pro [Var ("sg1", "sg_fl"); Var ("sg2", "sg_fl")], 9.99998999969e-13,
    0.00462962556603);
   (Pro [Var ("fmc1", "fmc_fl"); Var ("fmc2", "fmc_fl")], 4.00000066192e-20,
    1.85185238477e-10)]
*)
