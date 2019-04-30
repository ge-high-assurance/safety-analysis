#use "topfind" ;;
#thread ;;
#require "core" ;;
#require "core_extended" ;;
#directory "../_build";;

open Core ;;
open FaultTree ;;
open Qualitative ;;
open Quantitative ;;
open Modeling ;;
open Validation ;;
open FaultTreeSynthesis ;;
open Visualization ;;
open SafetyAnalysis ;;

#load "faultTree.cmo" ;;
#load "qualitative.cmo" ;;
#load "quantitative.cmo" ;;
#load "modeling.cmo" ;;
#load "validation.cmo" ;;
#load "faultTreeSynthesis.cmo" ;;
#load "visualization.cmo" ;;

#print_depth 1000 ;;
#print_length 10000 ;;

