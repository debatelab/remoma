(* ::Package:: *)

(* ::Chapter:: *)
(*Package InductiveReasoning*)


(* ::Text:: *)
(*Provides functions *)


(* ::Item:: *)
(*for calculating DOJs,*)


(* ::Item:: *)
(*for calculating ConfirmationMeasures,*)


(* ::Item:: *)
(*for calculating CoherenceMeasures,*)


(* ::Item:: *)
(*for generating sigma-values for all positions in tau.*)


BeginPackage["DialecticalStructures`InductiveReasoning`", { "DialecticalStructures`BasicTDS`"}]


DOJ::usage = "Unconditional DOJ[posList,tau,bg,senIDs] and conditional DOJ[posList1,posList2,tau,bg,senIDs] degree of justification, taking list representations of positions as arguments. tau and bf are boolean formulas.";
DOJ::inconsistent = "Position `1` is inconsistent. Returning ComplexInfinity.";


Sigma::usage = "Sigma[tau, bg, senIDs] returns list of length 3^Length[senIDs] such that entry i equals the  number of complete and consistent positions that extend position i on tau given background assumptions bg.";
Doj::usage = "Doj[int,sigma,senIDs]: unconditional DOJ. Doj[int1,int2,sigma,senIDs]: partial degree of entailment of int1 by int2.";
Doj::incompatible = "Positions `1` and `2` are incompatible. Returning 0.";


ConfKO::usage = "ConfKO[int1,int2,sigma,senIDs] Kemeny Oppenheim Measure of Confirmation of int1 by int2.";
ConfPPDiff::usage = "ConfPPDiff[int1,int2,sigma,senIDs] Prior-posterior difference measure of confirmation, i.e. Pr(A|B)-Pr(A)."


MutualCoh::usage = "MutualCoh[int1_, int2_, sigma_, senIDs_] mutual coherence of integer-form positions int1 and int2.";
MutualCohApprox::usage = "MutualCohApprox[int1_, int2_, sigma_, senIDs_] statistically approximated mutual coherence of integer-form positions int1 and int2.";


Begin["`Private`"]


(* ::Subsubsection:: *)
(*DOJ*)


DOJ[posList_,tau_,bg_,senIDs_] := DOJ[posList,{},tau,bg,senIDs];
DOJ[posList1_,posList2_,tau_,bg_,senIDs_] := Module[{sigma12,sigma2},
	sigma2 = SatisfiabilityCount[
			(And @@ posList2) && tau && bg, senIDs
		];  
	If[
		sigma2 > 0,
		sigma12 = SatisfiabilityCount[
			(And @@ posList1) && (And @@ posList2) && tau && bg, senIDs
		];
		(sigma12 / sigma2),
		(Message[DOJ::inconsistent,posList2]; 1/0)
	]
];


(* ::Text:: *)
(*For a sentence pool with N sentences, there are 3^Npartial positions. To speed up calculations for given & fixed tau, we calculate the number of complete and consistent positions that extend every such partial position ex ante and store it in a big list \[Dash] sigma \[Dash] of length 3^N! *)


Sigma[tau_, bg_, senIDs_] := Module[{sigma},
   sigma = Monitor[
     Table[
      SatisfiabilityCount[
       (And @@ TVVectorToList[
           IntegerToTVVector[i, senIDs]
           ]) && tau && bg, senIDs
       ],
      {i, 3^Length[senIDs]}
      ],
     ProgressIndicator[i, {1, 3^Length[senIDs]}]
     ];
   sigma
   ];


Doj[int_,sigma_,senIDs_]:=sigma[[int]]/sigma[[1]];
Doj[int1_,int2_,sigma_,senIDs_]:=If[
  CompatibleQ[int1,int2,senIDs],
  sigma[[ PosIntersec[int1,int2,senIDs] ]]/sigma[[int2]],
  (Message[Doj::incompatible,int1,int2]; 0)
];


(* ::Subsubsection:: *)
(*Confirmation*)


(* ::Text:: *)
(*The Kemeny and Oppenheim measure of support is defined as*)


(* ::DisplayFormulaNumbered:: *)
(*Subscript[conf,KO](p1,p2):=((Pr(p2|p1)-Pr(p2|\[Not]p1))/(Pr(p2|p1)+Pr(p2|\[Not]p1))).*)


(* ::Text:: *)
(*In our framework, this translates to*)


(* ::DisplayFormulaNumbered:: *)
(*Subscript[conf, KO](p1,p2):=((Subscript[\[Sigma], p2,p1]/Subscript[\[Sigma], p1]-Subscript[\[Sigma], p2,\[Not]p1]/*)
(*\!\(\*SubscriptBox[\(\[Sigma]\), \(\[Not] p1\)]\))/(Subscript[\[Sigma], p2,p1]/Subscript[\[Sigma], p1]+Subscript[\[Sigma], p2,\[Not]p1]/*)
(*\!\(\*SubscriptBox[\(\[Sigma]\), \(\[Not] p1\)]\))).*)


(* ::Text:: *)
(*Consider also the following equations*)


(* ::DisplayFormulaNumbered:: *)
(*Subscript[\[Sigma], p]+*)
(*\!\(\*SubscriptBox[\(\[Sigma]\), \(\[Not] p\)]\)=\[Sigma] *)
(*Subscript[\[Sigma], q,\[Not]p]+Subscript[\[Sigma], q,p]=Subscript[\[Sigma], q]*)


(* ::Text:: *)
(*With the above equations, Kemeny Oppenheim transforms to*)


(* ::DisplayFormulaNumbered:: *)
(*Subscript[conf, KO](p1,p2):=(Subscript[\[Sigma], p2,p1]/Subscript[\[Sigma], p1]-(Subscript[\[Sigma], p2]-Subscript[\[Sigma], p2,p1])/(\[Sigma]-Subscript[\[Sigma], p1]))/(Subscript[\[Sigma], p2,p1]/Subscript[\[Sigma], p1]+(Subscript[\[Sigma], p2]-Subscript[\[Sigma], p2,p1])/(\[Sigma]-Subscript[\[Sigma], p1]))*)


(*Kemeny Oppenheim Measure of Confirmation of int1_ by int2_ *)
ConfKO[int1_,int2_,sigma_,senIDs_]:=Module[{doj21,doj2n1,sigma12},
	If[
		(*if int1 is contradictory*)
		sigma[[ int1 ]]==0,
		-1,(*following Technical Correction in Fitelson, originally 0*)
		If[
			(*if int2 is contradictory, i.e. a necessary falsehood*)
			sigma[[ int2 ]]==0,
			-1,
			If[
				(*if s1 and s2 are necessary truths*)
				sigma[[ int1 ]]==sigma[[ 1 ]]&&sigma[[ int2 ]]==sigma[[ 1 ]],
				1,
				If[
					(*if s1 is necessary truth but s2 is contingent*)
					sigma[[ int1 ]]==sigma[[ 1 ]],
					1,(*following Technical Correction in Fitelson, originally 0*)

					sigma12=If[CompatibleQ[int1,int2,senIDs],
						sigma[[ PosIntersec[int1,int2,senIDs] ]],
					0];
					doj21=sigma12/sigma[[ int1 ]];
					doj2n1=(sigma[[ int2 ]]-sigma12)/(sigma[[ 1 ]]-sigma[[ int1 ]]);
	
					(doj21-doj2n1)/(doj21+doj2n1)
				]
			]
		]
	]
];


(* ::Text:: *)
(*The prior - posterior difference (aka difference measure) :*)


(*Kemeny Oppenheim Measure of Confirmation of int1_ by int2_ *)
ConfPPDiff[int1_,int2_,sigma_,senIDs_]:=Module[{doj21,doj2n1,sigma12},
	If[
		(*if int1 is contradictory*)
		sigma[[ int1 ]]==0,
		-1,(*following Technical Correction in Fitelson, originally 0*)
		If[
			(*if int2 is contradictory, i.e. a necessary falsehood*)
			sigma[[ int2 ]]==0,
			-1,
			If[
				(*if s1 and s2 are necessary truths*)
				sigma[[ int1 ]]==sigma[[ 1 ]]&&sigma[[ int2 ]]==sigma[[ 1 ]],
				1,
				If[
					(*if s1 is necessary truth but s2 is contingent*)
					sigma[[ int1 ]]==sigma[[ 1 ]],
					1,(*following Technical Correction in Fitelson, originally 0*)

					Doj[int1,int2,sigma,senIDs]-Doj[int1,sigma,senIDs]
					
				]
			]
		]
	]
];


(* ::Subsubsection:: *)
(*Coherence*)


(* ::Text:: *)
(*The Douven-Meijs measures of coherence is defined as:*)


(* ::DisplayFormulaNumbered:: *)
(*P={p1,...,pn}*)
(*S={<X,Y>|X\[Subset]P,Y\[Subset]P , X\[Intersection]Y=\[Diameter]}*)
(*CohDM(P):=*)
(*\!\(\*UnderscriptBox[\(Mean\), \(\(\(<\)\(X\)\), Y > \**)
(*ButtonBox["\[Element]",*)
(*BaseStyle->"Link",*)
(*ButtonData->"paclet:ref/character/Element"] S\)]\)[conf(X,Y)]*)


(* ::Text:: *)
(*We modify the Douven-Meijs idea by stipulating that the mutual coherence of two positions is the mean degree of support of any non-empty subposition of one non-empty position by any non-empty subposition of the other position.*)


(* ::DisplayFormulaNumbered:: *)
(*P={p1,...,pn}*)
(*Q={q1,...,qn}*)
(*S={<X,Y>|(X\[SubsetEqual]P\[Wedge]Y\[SubsetEqual]Q)\[Vee](X\[SubsetEqual]Q\[Wedge]Y\[SubsetEqual]P)}*)
(*MutCoh(P,Q):=*)
(*\!\(\*UnderscriptBox[\(Mean\), \(\(\(<\)\(X\)\), Y > \**)
(*ButtonBox["\[Element]",*)
(*BaseStyle->"Link",*)
(*ButtonData->"paclet:ref/character/Element"] S\)]\)[Subscript[conf, KO](X,Y)]*)


(* ::Text:: *)
(*while using Kemeny and Oppenheim measure of support (see above). Strictly speaking, S is not a set (but a list): If, for example, y belongs to P and Q and Y is a subsets of both P and Q, then <y,Y> will figure twice in S. *)


(*Mutual Coherence Measure*)
(*positions int1 and int2 are given in integer-form*)
MutualCoh[int1_, int2_, sigma_, senIDs_] := Module[{},
   If[int1 > 1 && int2 > 1,
    Mean[
     Map[
      ConfKO[
        First[#],
        Last[#],
        sigma, senIDs
        ] &,
      Join[
       Tuples[{
         DeleteCases[SubpositionsI[int1,senIDs], 1],
         DeleteCases[SubpositionsI[int2, senIDs] , 1]
         }],
       Tuples[{
         DeleteCases[SubpositionsI[int2,senIDs], 1],
         DeleteCases[SubpositionsI[int1, senIDs], 1] 
         }]
       ](*end join*)
      ](*end map*)
     ],
    0 (*i.e., an empty position coheres neutrally with another *)
    ]
   ];

(*Approximated Mutual Coherence Measure*)
(*positions int1 and int2 are given in integer-form*)
(*Note the fixed sample size of 2000.*)
MutualCohApprox[int1_, int2_, sigma_, senIDs_] := Module[{},
   If[int1 > 1 && int2 > 1,
    Mean[
     Map[
      ConfKO[
        First[#],
        Last[#],
        sigma, senIDs
        ] &,
		RandomSample[
			Join[
				Tuples[{
					DeleteCases[SubpositionsI[int1,senIDs], 1],
					DeleteCases[SubpositionsI[int2, senIDs] , 1]
				}],
				Tuples[{
					DeleteCases[SubpositionsI[int2,senIDs], 1],
					DeleteCases[SubpositionsI[int1, senIDs], 1] 
				}]
			],(*end join*)
			UpTo[2000]
		](*end RandomSample*)
      ](*end map*)
     ],
    0 (*i.e., an empty position coheres neutrally with another *)
    ]
   ];


End[]


EndPackage[]
