(* ::Package:: *)

(* ::Chapter:: *)
(*Package PositionsAnalytics*)


(* ::Text:: *)
(*Provides functions *)


(* ::Item:: *)
(*for closing positions and checking whether they are closed, *)


(* ::Item:: *)
(*for determing principles that span a position,*)


(* ::Item:: *)
(*for measuring distance between positions*)


BeginPackage["DialecticalStructures`PositionsAnalytics`",{"DialecticalStructures`BasicTDS`"}]


DialecticallyClosedQ::usage = "DialecticallyClosedQ[int, tau, bg, senIDs] returns true iff position int is closed & consistent on dialectical structure tau assuming background claims bg and sentence pool senIDs.";
DialecticClosure::usage = "DialecticClosure[int_, tau_, bg_, senIDs_] returns the dialectic closure of position int";
DialecticallyConsistentQ::usage = "DialecticallyConsistentQ[int,tau,bg,senIDs] returns true iff position int is consistent on dialectical structure tau assuming background claims bg and sentence pool senIDs.";
DialecticallyClosedPositions::usage = "DialecticallyClosedPositions[tau, bg, senIDs] returns list of all positions (integer representation) that are closed & consistent on dialectical structure tau assuming background claims bg and sentence pool senIDs ";


EntailsQ::usage = "EntailsQ[int1_, int2_, sigma_, senIDs_], EntailsQ[posList1_, posList2_, tau_, bg_, senIDs_]: Returns true iff int1/posList1 entails int2/posList2.";


Principles::usage = "Principles[int_, sigma_, senIDs_] returns a smallest set of principles";
NPrinciples::usage = "NPrinciples[sigma_, senIDs_] returns a list with the number of principles for every position on senIDs.";


NormalizedCloseness::usage = "NormalizedCloseness[int1, int2, senIDs, param] assesses closeness of int1 relative to int2 (assuming, in BRT-terms, that one moves from int2 to int1).";


(* ::Text:: *)
(**)


Begin["`Private`"]


(* ::Section::Closed:: *)
(*Closure*)


DialecticallyClosedQ[int_,tau_,bg_,senIDs_] := Module[{posList,S},
	posList=IntegerToList[int,senIDs];

	If[Length[posList]==0,
		Return[True],
		Null;
	];

	If[
		Length[posList]==Length[senIDs],
		Return[SatisfiableQ[(And@@posList)&&bg&&tau,senIDs]],
		Null;
	];

	S=Complement[senIDs,(If[AtomQ[#],#,!#]&)/@posList];

	And@@Map[
		SatisfiableQ[(And@@posList)&&bg&&tau&&#,senIDs]&,
		Flatten[{#,!#}&/@S]
	]
];

DialecticClosure[int_, tau_, bg_, senIDs_] := Module[
   {posList, EntailedAtoms, AppendEntailedAtoms},
   
   posList = IntegerToList[int, senIDs];
   
   If[Length[posList] == 0 || Length[posList] == Length[senIDs],
   	Return[int], Null;
   ];
   
   (*The following function returns a list of all atomic statements entailed by pos p*)
   EntailedAtoms[p_] := Module[{comp},
     comp = Complement[senIDs, (If[AtomQ[#], #, ! #] &) /@ p];
     Select[
        Flatten[{#, ! #} & /@ comp],
        SatisfiabilityCount[(And @@ p) && bg && tau && ! #, senIDs] == 0 &
      ]
     ];
   
   (*The following function returns a position p' that consists in p plus all atomic statements entailed by p*)
   
   AppendEntailedAtoms[p_] := Union[p, EntailedAtoms[p]];
   
   (*The following fixedpoint is the dialectic closure of posList*)
  
    ListToInteger[
      FixedPoint[AppendEntailedAtoms, posList, 2*Length[senIDs]],
      senIDs
    ]
   ];


DialecticallyConsistentQ[int_,tau_,bg_,senIDs_] := Module[{posList},
	posList=IntegerToList[int,senIDs];

	If[Length[posList]==0,
		Return[True],
		Null;
	];

	SatisfiableQ[(And@@posList)&&bg&&tau,senIDs]
];

DialecticallyClosedPositions[tau_, bg_, senIDs_] := Module[{posListWithZeros},
   posListWithZeros = 
     Table[
      If[
		DialecticallyClosedQ[i,tau,bg,senIDs],
		i,
		0
	  ],
      {i, 3^Length[senIDs]}
      ];

   DeleteCases[posListWithZeros,0]
 ];


EntailsQ[int1_, int2_, sigma_, senIDs_] :=
  If[! CompatibleQ[int1, int2, senIDs], False,
   Equal[
    sigma[[ int1 ]],
    sigma[[ PosIntersec[int1, int2, senIDs] ]]
   ]
  ];
EntailsQ[posList1_, posList2_, tau_, bg_, senIDs_] :=
	!SatisfiableQ[
		(And @@ posList1) && !(And @@ posList2) && tau && bg,
		senIDs
	];


(* ::Section::Closed:: *)
(*Principles*)


(*Private function used in Principles[]: Takes a list of original positions in 
integer-format and returns  all subpositions of length-1 that entail some original 
position; if a position is not entailed by any of its subpositions, it is returned, too*)
Axiomatize[positions_, sigma_, senIDs_] := Module[{subpositions},
   DeleteDuplicates[
    Flatten[
     Function[
       position,
       If[SizeI[position, senIDs] == 1,
        {position},
        subpositions =
         Select[
          SubpositionsI[position, {SizeI[position, senIDs] - 1}, senIDs],
          EntailsQ[#, position, sigma, senIDs] &
          ];
        If[Length[subpositions] > 0, subpositions, {position}]
        ]
       ] /@ positions,
     1
     ](*end flatten*)
    ]
   ];


Principles[int_, sigma_, senIDs_] := Switch[SizeI[int, senIDs],
   0, {},
   1, int,
   _, First[MaximalBy[  FixedPoint[Axiomatize[#, sigma, senIDs] &, {int}], Length  ]]
 ];


NPrinciples[sigma_, senIDs_] := Module[{nPrinciples},
   nPrinciples = Monitor[
     Table[
      Count[
       IntegerToTVVector[Principles[i, sigma, senIDs], senIDs],
       x_ /; x > 0
       ],
      {i, 3^Length[senIDs]}
      ],
     ProgressIndicator[i, {1, 3^Length[senIDs]}]
     ];
   nPrinciples
   ];


(* ::Section:: *)
(*Distance*)


(* ::Text:: *)
(*Let P and Q be two partial positions. If we consider an arbitrary sentence s in the sentence pool, the following cases are possible:*)


(* ::ItemNumbered:: *)
(*Agreement. P(s)=Q(s). P and Q agree w.r.t. s.*)


(* ::ItemNumbered:: *)
(*Expansion. s is in the domain of Q, but not of P. Q expands P w.r.t. s.*)


(* ::ItemNumbered:: *)
(*Contraction. s is in the domain P, but not of Q. Q contracts P w.r.t. s.*)


(* ::ItemNumbered:: *)
(*Conflict. P(s)!=Q(s). P and Q conflict w.r.t. s.*)


(* ::Text:: *)
(*Let us say that, when moving from position Q to position P, the following variables say how frequently the above cases occur: *)


(* ::ItemNumbered:: *)
(*Agreement. Subscript[n, agr]*)


(* ::ItemNumbered:: *)
(*Expansion. Subscript[n, exp]*)


(* ::ItemNumbered:: *)
(*Contraction. Subscript[n, cont]*)


(* ::ItemNumbered:: *)
(*Conflict. Subscript[n, conf]*)


(* ::Text:: *)
(*Obviously,*)


(* ::Item:: *)
(*Subscript[n, agr]+Subscript[n, exp]+Subscript[n, cont]+Subscript[n, conf]=|senIDs|*)


(* ::Item:: *)
(*Subscript[n, cont]+Subscript[n, conf]<=|Dom(Q)|*)


(* ::Item:: *)
(*Subscript[n, exp]<=|Dom(P)|\.10*)


(* ::Text:: *)
(*We shall measure the normalized closeness of P to Q through*)


(* ::DisplayFormula:: *)
(*NormalizedCloseness(P,Q):=*)
(*1-((Subscript[\[Pi], cont] Subscript[n, cont]+Subscript[\[Pi], conf] Subscript[n, conf]+Subscript[\[Pi], exp] Subscript[n, exp])/(Max[Subscript[\[Pi], cont],Subscript[\[Pi], conf],Subscript[\[Pi], exp]]*|senIDs|))^2*)


(* ::Text:: *)
(*It seems a reasonable requirement that Subscript[\[Pi], exp], Subscript[\[Pi], cont]< Subscript[\[Pi], conf].*)


(*we assume that positions are given in integer format*)


NormalizedCloseness[int1_, int2_, senIDs_, param_]:=Module[
	(*We move from int2 to int1*)
	{ nCont,nConf,nExp,
	  \[Pi]Cont,\[Pi]Conf,\[Pi]Exp,
	  tv1,tv2 },
	tv1=IntegerToTVVector[int1, senIDs];
	tv2=IntegerToTVVector[int2, senIDs];
	nCont=Count[Transpose[{tv2,tv1}],x_/;(First[x]>0&&Last[x]==0)];
	nConf=Count[Transpose[{tv2,tv1}],x_/;(First[x]>0&&Last[x]>0&&First[x]!=Last[x])];
	nExp=Count[Transpose[{tv2,tv1}],x_/;(First[x]==0&&Last[x]>0)];
	\[Pi]Cont=Lookup[param, "ContractionPenalty"];
	\[Pi]Conf=Lookup[param, "ConflictPenalty"];
	\[Pi]Exp=Lookup[param, "ExpansionPenalty"];

	1-((\[Pi]Cont*nCont+\[Pi]Conf*nConf+\[Pi]Exp*nExp)/(Max[\[Pi]Cont,\[Pi]Conf,\[Pi]Exp]*Length[senIDs]))^2

];



End[]


EndPackage[]
