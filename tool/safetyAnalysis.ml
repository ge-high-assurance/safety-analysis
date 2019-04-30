(* 

Copyright © 2019 United States Government as represented by the Administrator of the 
National Aeronautics and Space Administration.  All Rights Reserved.

Author: Pete Manolios
Date: 2017-12-15

*)

(**
   Description: This is a tool provides a compositional, model-based framework
   for modeling, visualizing and analyzing the safety and reliability of system
   architectures. See the installation-instructions.txt file for instructions on
   how to build and use the tool.

   MODELING:

   The modeling language is designed to provide a minimal set of capabilities
   for modeling architectures at the level required to perform safety/
   reliability analyses. The modeling language has evolved as we used it to
   model a variety of architectures. The modeling language requires users to
   describe safety in a compositional manner, as it is not possible to describe
   fault trees directly; instead one has to define how faults propagate through
   the components of an architecture. Once components are modeled in this way,
   we wind up with a library of components that can be shared throughout an
   organization, thereby allowing for consistency, reuse and rapid architectural
   development.

   Our modeling language allows us to define components, libraries
   and architectures. 

   A component consists of a name, a list of fault types supported (such as
   "ued" (undetected erroneous data) and "loa" (loss of availability)), a list
   (possibly empty) of input flow names (each input flow corresponds to a
   Boolean variable; you can think of it as an input port, if you wish), a list
   (possibly empty) of basic events (these are leafs in a fault tree, e.g., an
   internal component error), a list of basic event information consisting of
   pairs of floats corresponding to the failure rate and exposure time of the
   basic events (so this list has the same length as the list of basic events),
   a list of output flows and a list of named Boolean formulas.  The formulas
   allow us to define how faults propagate through the component using Boolean
   formulas that can included references to faults that propagate through the
   inputs, internal faults and defined equations. There should be a formula for
   each fault-type/output-flow combination. Notice that fault formulas can refer
   to the formulas for other faults. The types of formulas allowed are monotone
   Boolean formulas because for safety analysis, a fault not occurring never
   leads to another fault occurring, i.e., the propagation functions are
   monotonic. The basic operators allowed include references to atomic variables
   (such as faults propagated through inputs, basic events and references to
   defined equations), arbitrary-arity conjunctions, arbitrary-arity
   disjunctions and an N-of construct that corresponds to n or more of a list of
   formulas being true.

   An example of a component with no input flows is a sensor that can only fail
   due to an internal fault, a basic event.

   All components must support at least one fault type, otherwise there is
   nothing to analyze.

   A library of components is a list of components, such that no
   two components have the same name. 

   An architecture or a model is a set of component instantiations, connectivity
   information and a top-level fault.

   An instance is a record consisting of the instance name, the name of the
   component it is instantiating, a list of exposures and a list of lambdas. The
   exposures override the default exposure time of a basic event of the
   corresponding library component. This allows us to have different instances
   of the same component with different exposure times. The lambdas override the
   default lambdas of a basic event of the corresponding library component. This
   allows us to have different instances of the same component with different
   lambdas.

   The connection information is a list of pairs consisiting of an input flow
   and an output flow, both flows are pairs consisting of an instance name and a
   flow. The input flow has to exist in the list of instances of the model. The
   pair ((x, i), (y, o)) indicates that instance x's input flow i is fed by (or
   defined to be equal to) instance y's output flow o.

   See the examples in this distribution for examples of component, library and
   architecture definitions.
   
   We distinguish between components, as described above, and parameterized
   components, which we often just refer to as "Components" (notice the
   upper-case "C"). Components are really just functions that given a set of
   parameters generate components. We use Components because components are too
   restrictive. For example, consider a switch component that has two inputs and
   two outputs. The number of inputs and outputs may be what is needed for a
   particular model (architecture), but for this to be a reusable library, we
   would prefer that switches allow any number on inputs and outputs without
   requiring the library writer to create all such combinations of switches as
   individual components. Therefore we use Components, which are really
   component generators. Components take a collection of arguments, as
   determined by the Component owner, and generate components which are then
   used to define architectures. The use of inez, an extension of OCaml, allows
   us to easily define such Components.

   The rest of this section identifies some of the constraints that components
   and architectures should satisfy. Some of these constraints are checked by
   the validation code; see that file for related information.

   . Input flows have exactly 1 instance connected to them.  Notice that one
   instance can feed multiple input flows.

   . All component names are disjoint.

   . The formulas make sense, e.g., references to atomic variables correspond to
   inputs, events or defined equations, etc.

   . The names used for input flows, basic events and output flows are disjoint.

   . All input flows and defined equations have exactly one definition.

   . All instance names are disjoint.
   
   . Lists are all of consisitent lengths, e.g., the length of basic_events is
   the same as the length of input_flows.
   
   . All c_names correspond to actual components in our component library.

   . All instantiated components support the flow being analyzed by the model. 


   ANALYSES:

   Once architectures are modeled in our framework, it is possible to
   algorithmically synthesize fault trees. From the
   synthesized fault trees, we can use standard algorithms to perform qualitative
   and quantitative analyses to determine the safety of the modeled architecture.
   Therefore, our framework allows us to go from models to analyses with a push of
   a button.  

   By automating fault-tree construction and analysis, our framework provides a
   new paradigm, allowing significant automation in the area of
   safety/reliability analysis of architectures. For example, to analyze 30
   faults of interest for 5 architectures, using standard methods, 150 fault
   trees must be generated by a human, since a fault tree is required per fault.
   This makes it difficult to explore multiple architectures and it makes it
   easy to introduce errors when fault trees must be updated to reflect
   architectural changes, which often undergo numerous changes in the course of
   an industrial project.  We have experienced such issues and they are
   addressed in our framework by only allowing engineers to specify fault
   propagation logic at the component level, not the system level (as is
   required with manual fault tree construction). These components are reused,
   so errors in a component are much more likely to be found than errors in a
   specific fault tree. Also, updating fault trees is error-prone because it is
   easy to overlook the impact of a change to the architecture, but with our
   framework all of this happens automatically. Finally, there can be unintended
   consequences when architectures are modified and with manually constructed
   fault trees, it may take a while before these consequences are fully
   understood. With the fully automated approach our framework provides, these
   consequences are calculated immediately.

   FAULT TREE SYNTHESIS:

   We describe the fault tree synthesis algorithm informally.  The idea is
   simply to expand out the equations defining the top-level fault under
   consideration starting from the component instance that has the fault. If the
   fault involves more than one component, then we define a dummy component that
   is connected to all the required components. The algorithm expands out the
   fault propagation logic associated with component outputs. Recall that there
   may be defined equations, in which case these are expanded and simplified
   until we arrive at a formula in terms of the basic events of the component
   and the inputs of the component (really input/fault pairs). The architecture
   is consulted to determine what component instance outputs are connected to
   the identified inputs and the process repeats, i.e., we expand out the
   appropriate fault propagation logic associated with the newly discovered
   component instances as before. If there are no cycles, we eventually have a
   formula in terms of just basic events, potentially with many events appearing
   multiple times. 

   The fault tree generation algorithm supports cyclic dependencies between faults. 
   In certain architectures, there are cyclic dependencies between
   faults. The NASA Fault Tree Handbook version 1.1, includes a
   new section describing such feedback loops. The handbook describes how in the
   space shuttle, the orbiter sends a control signal to the main engine, which
   provides the feedback signal, so the failure of the orbiter depends on the
   failure of the main engine, which depends on the failure of the orbiter, ... .
   Our fault tree synthesis algorithm supports such loops. First, loops
   are detected when we notice a cycle in the expanded fault propagation
   definition. We note that one can legitimately expand out the definition of some
   fault more than once without there being a loop, so we have to take context into
   account, by only looking for loops along the path of ancestors of the fault tree
   node being generated. Once loops are found, they have to be broken in a way that
   makes sense, e.g.:

   a V a V a V a ...  


   should be set to a, which is equivalent to replacing all but the first
   occurrence of a with false. However, in the formula

   a ^ a ^ a ^ a ...  

   we should replace all but the first occurrence of a with true. Our algorithm
   breaks loops only in the context of conjunctions and disjunctions that include
   at least one basic event and replaces repeated events with the appropriate
   identity for the operator. If there are no basic events, then we there are a
   number of potential solutions to the equations and that indicates an error,
   which we report.

   We note that, in our framework, fault trees are really just an internal data
   structure whose primary purpose is to enable qualitative (cutsets) and
   quantitative (probabilistic) safety analyses of architectures under
   consideration. While a safety engineer may find the reduced fault trees useful
   in understanding the characteristics of an architecture, fault trees can
   actually be completely ignored. This is a paradigm shift, where instead of
   modeling the safety of a system using a fault tree (the current approach), a
   safety engineer just creates a model of an architecture using a library of
   components and the safety analyses are performed automatically by the framework,
   without requiring the safety engineer to manually define the global behavior of
   the architecture under consideration.

   Synthesized trees can often be significantly simplified, which makes it
   easier to use them to understand system behavior. Therefore, our framework
   includes a collection of formula transformations that when used (as is the
   default case) generate reduced fault trees which tend to be much simpler than
   the direct fault tree, but semantically equivalent.

   QUALITATIVE ANALYSIS

   From fault trees, we generate cutsets, which correspond to the minimal DNF
   formula that is equivalent to the fault trees. Since the formulas are
   monotonic, there is a unique minimal DNF formula that can be generated using
   algorithms such as the MOCUS and related algorithms. We use symbolic
   manipulation algorithms for both reducing fault trees and generating cutsets.

   An example of the simplifications used include the lift transformation, which
   is essentially distributivity in reverse.  The lift transformation is

   (and x...y  (or a..z ...1) (or a..z ...2) (or a..z ...3))
   =
   (and x...y (or a..z (and (or ...1) (or ...2) (or ...3))))

   We also support the dual. 

   Notice that this lifting can increase the depth of the tree and even the
   number of Boolean operators, but the number of variable occurrences
   decreases.

   Another example of a transformation we apply is:

   (or ... a ... (and ... (or ...a ...) ...) ...)
   =
   (or ... a ... (and ... (or ...false ...) ...) ...)

   We also support the dual.

   These transformation are used in conjunction with constant propagation and
   transformation for flattening, simplifying and sorting formulas.  In
   addition, where appropriate, transformations are applied until a fixpoint is
   reached. As the lift transformation highlights, the algorithms have to be
   careful to avoid loops, so depending on the context in which they are used
   some transformation are not applied unless they meet context-sensitive
   criteria.

   The cutsets are very useful because each term of the top-level disjunction
   corresponds to a set of basic events that together lead to the top-level fault
   under consideration. 

   QUANTITATIVE ANALYSIS

   From the cutsets, we can use standard techniques to determine the probability of
   that that top-level fault occurs. The probability of a failure for a basic event
   (from reliability theory) is 

   1 - e^(- lambda * tau)

   To compute the probability using cutsets requires the use of the
   inclusion-exclusion principle, which winds up becoming computationally
   prohibitive when we have a large number of cutsets.  Therefore, our framework
   generates a range of possible values by expanding out the terms that
   inclusion/exclusion gives up to a certain limit. Since inclusion/exclusion
   gives rise to a summation of (usually) smaller terms where the terms
   alternate between being positive and negative, stopping the summation at any
   point gives rise to an interval whose endpoints are the last two summations
   that includes the true probability of a top-level fault. Note that we assume
   that basic events are independent (as is standard in safety analysis and this
   is something that is validated via other means). The reason we need
   inclusion/exclusion is that basic events can be repeated. However, if we have
   a formula with no repeated events (as is the case with our reduced fault tree
   above), then the probability computations are straightforward. There are
   various specialized techniques for determining the probability of a top-level
   failure efficiently, and it would be of interest to incorporate some of these
   into the tool.

   Our framework also generates importance metrics, which order cutsets by their
   contribution to the top-level fault. This is very useful in understanding
   what parts of an architecture are primarily responsible for high
   probabilities, which allows safety engineers to consider new architectures
   that address these issues. The importance metrics report how much of the
   top-level fault probability is due to a particular cutset.

   VISUALIZATION

   The tool includes visualization capabilities, using graphviz. To use this,
   graphviz needs to be installed.

   Model visualizations include:

   Physical model: this is a graph whose nodes are component instances and whose
   edges correspond to one or more connections between components.  In this view
   of a model, if there are k outputs from component A that are connected to k
   inputs from component B, then only one edge is shown between the
   components. This gives us the highest-level view of the system, as is shows
   an abstract view showing only components and connectivity information
   (yes/no).

   Functional model: this is a graph whose nodes are ports (inputs/outputs) of
   components and whose edges correspond to connections between ports.  Edges
   are from port to port, so we get a mode detailed view of the model. For
   example, in this view of a model, if there are k outputs from component A
   that are connected to k inputs from component B, then k edges are shown
   between the components as there is an edge from one port to another if the
   first port's output is the input to the second port.

   Fault propagation model: This is a graph that extends the functional
   architecture to include nodes corresponding to basic events and shows what
   parts of the model can affect the probability of a the top-level fault. If an
   edge (corresponding to a flow of information from one component to another)
   can affect the top-level fault, it is colored red. If a basic event can
   affect the top-level fault, then a (set of) red edge(s) is added from that
   event to the output(s) along which the basic event propogates.

   USER'S MANUAL
   
   The top-level functions have been documented and a user's manual can be
   generated using ocamldoc in latex, html ... . The way things are set up, only
   what I identified as "top-level" functions are generated and annotated in the
   documentation, but with the appropriate command line option (-no-stop), one
   can also generate documentation for all of the documented functions and types
   in this file.

   NOTES

   This tool has been tested using a number of examples. Some of the
   architectures defined were non-trivial case studies. These examples helped us
   make a number of improvements to the tool, but our primary goal was to
   determine whether a compositional, model-based design approach to safety
   analysis, where fault trees are synthesized, can be a practical alternative
   to the manual construction of fault trees, which seems to be what safety
   engineers in avionics currenty do. All of the computationally-intensive code
   is therefore written in a functional style and can be improved significantly.

   KNOWN ISSUES

   If the visualizations generated have problems (e.g., node names extend beyond
   node shapes), then check that graphviz and your renderer are using the same
   fonts. This can cause problems, e.g., if graphviz is not using the same fonts
   as your renderer, then when graphviz determines the dimensions of bounding
   boxes, it will generate node shapes of the wrong size. 

*)

open Core 
open FaultTree 
open Qualitative
open Quantitative
open Modeling
open Validation
open FaultTreeSynthesis
open Visualization

