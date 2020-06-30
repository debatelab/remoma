(* ::Package:: *)

(* ::Chapter:: *)
(*Package BasicTDS*)


(* ::Text:: *)
(*Provides functions *)


(* ::Item:: *)
(*for generating dialectical structures, *)


(* ::Item:: *)
(*for calculating their properties,*)


(* ::Item:: *)
(*for representing positions in different ways,*)


(* ::Item:: *)
(*for calculating their properties*)


BeginPackage["DialecticalStructures`BasicTDS`"]


RandomTau::usage = "RandomTau[n,k,sp] generates a dialectical structure tau of n arguments with k premisses each drawn from sentence pool sp.";


TVVectorToList::usage = "TVVectorToList[tvv] converts the three-valued vector tvv into a list-representation of the position.";
ListToTVVector::usage = "ListToTVVector[pos, senIDs] transforms the list-representation pos into a three-valued vector";
TVVectorToInteger::usage = "TVVectorToInteger[tvv] converts the three-valued vector tvv into an integer-representation of the position.";
IntegerToTVVector::usage = "IntegerToTVVector[int, senIDs] converts the integer-representation into a three-valued vector representing the position.";
IntegerToList::usage = "IntegerToList[int_,senIDs_] converts the integer-representation of a position into its list-representation.";
ListToInteger::usage = "ListToInteger[pos,senIDs] converts the list-representation of a position into its integer-representation.";
SizeI::usage = "SizeI[int, senIDs] gives length of position int (integer representation).";
Domain::usage = "Domain[int, senIDs] returns domain of position int on senIDs as list of sentence-indices.";
DomainQ::usage = "Domain[int, i, senIDs] checks whether sentence-index i is in the domain of position int.";


RelativeOverlap::usage = "RelativeOverlap[tvv1, tvv2], RelativeOverlap[int1,int2, senIDs]";
CompatibleQ::usage = "CompatibleQ[tvv1, tvv2], CompatibleQ[int1, int2, senIDs]";
PosIntersec::usage = "PosIntersec[tvv1, tvv2], PosIntersec[int1, int2, senIDs]";
SubpositionI::usage = "SubpositionI[int, domain, senIDs] returns subposition of int defined on domain";
Subpositions::usage = "Subpositions[tvv], Subpositions[tvv, l]: all subpositions (of length l)";
SubpositionsI::usage = "SubpositionsI[int, senIDs], SubpositionsI[int, l, senIDs]: all subpositions (of length l)";


Begin["`Private`"]


(* ::Section::Closed:: *)
(*Dialectical Structures*)


RandomTau[n_,k_,sp_]:= Module[{a, NewArgument},
	(*function that randomly constructs novel argument with 2 premisses from sentence pool*)
	  NewArgument:=((And @@ #[[Range[k]]])\[Implies]#[[k+1]])&[
	    If[RandomChoice[{True,False}],#,!#]&/@RandomSample[sp,k+1]
	  ];

	(*create tau iteratively*)
	Last[
		NestList[
			(*Add arguments so that resulting tau is still satisfiable*)
			(a=NewArgument;
				While[SatisfiabilityCount[a&&#,sp]==0,a=NewArgument;];
				#&&a)&,
			(*initial argument*)
			NewArgument,
			(*number of iterations*)
			n-1
		](*end NestList*)
	](*end Last*)
](*end Module*)


(* ::Section:: *)
(*Positions*)


(* ::Text:: *)
(*We use three-valued vectors of size N to represent partial positions.*)


(* ::DisplayFormula:: *)
(*X=<Subscript[x,1],...,Subscript[x,N]>with Subscript[x,1]\[Element]{0,1,2}and*)
(*Subscript[x,1]=\!\(\**)
(*TagBox[GridBox[{*)
(*{"\[Piecewise]", GridBox[{*)
(*{"0", *)
(*RowBox[{*)
(*RowBox[{"X", " ", "assigns", " ", "no", " ", "truth"}], "-", *)
(*RowBox[{"value", " ", "to", " ", "sentence", " ", "i"}]}]},*)
(*{"1", *)
(*RowBox[{"sentence", " ", "i", " ", "is", " ", "true", " ", *)
(*RowBox[{"acc", ".", " ", "to"}], " ", "X"}]},*)
(*{"2", *)
(*RowBox[{"sentence", " ", "i", " ", "is", " ", "false", " ", *)
(*RowBox[{"acc", ".", " ", "to"}], " ", "X"}]}*)
(*},*)
(*AllowedDimensions->{2, Automatic},*)
(*Editable->True,*)
(*GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},*)
(*GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},*)
(*Selectable->True]}*)
(*},*)
(*GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},*)
(*GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}],*)
(*"Piecewise",*)
(*DeleteWithContents->True,*)
(*Editable->False,*)
(*SelectWithContents->True,*)
(*Selectable->False]\)*)


(* ::Text:: *)
(*The vector <0,...,0> is the empty position (True) that can be extended by position whatsoever.*)


(* ::Text:: *)
(*A three-valued vector, in turn, can be identified with an integer.*)


(*TVVectorToList[x] assumes Range[Length[x]] as sentence pool.*)
TVVectorToList[x_] := Union[
   Pick[Reverse[Range[Length[x]]], x, 1],
   Not /@ Pick[Reverse[Range[Length[x]]], x, 2]
 ];

ListToTVVector[pos_, senIDs_] :=
  If[
     MemberQ[pos, #],
     1,
     If[
      MemberQ[pos, ! #],
      2,
      0
     ]
   ] & /@ Reverse[senIDs];


TVVectorToInteger[x_] := FromDigits[x, 3] + 1;
IntegerToTVVector[int_, senIDs_] := IntegerDigits[int - 1, 3, Length[senIDs]];
IntegerToList[int_,senIDs_]:=TVVectorToList[IntegerToTVVector[int, senIDs]];
ListToInteger[pos_,senIDs_]:=TVVectorToInteger[  ListToTVVector[pos, senIDs]  ];


SizeI[int_, senIDs_] := Count[IntegerToTVVector[int, senIDs], i_ /; i > 0];


Domain[int_, senIDs_] := Flatten[Position[Reverse[IntegerToTVVector[int, senIDs]], i_ /; i > 0]];
DomainQ[int_, i_, senIDs_] := IntegerToTVVector[int, senIDs][[ -i ]] > 0;


(* ::Text:: *)
(*Let X and Y be two three-valued position vectors of length N. The relative overlap is defined as the size of the intersection of X and Y divided by the size of X. *)


RelativeOverlap[x_,y_]:=Count[Transpose[{x,y}],{a_,b_}/;a==b&&a>0]/Length[DeleteCases[x,0]];
RelativeOverlap[int1_,int2_, senIDs_]:=RelativeOverlap[
  IntegerToTVVector[int1, senIDs],
  IntegerToTVVector[int2, senIDs]
];


(* ::Text:: *)
(*Let X and Y be two three-valued vectors of length N. We define that X and Y are compatible if and only if for every i=1...N: If Subscript[x, i]!=0 and Subscript[y, i]!=0, then Subscript[x, i]=Subscript[y, i].*)


CompatibleQ[x_, y_] := ! MemberQ[x + y, 3];
CompatibleQ[int1_, int2_, senIDs_] := CompatibleQ[
   IntegerToTVVector[int1, senIDs],
   IntegerToTVVector[int2, senIDs]
 ]; 


(* ::Text:: *)
(*Let X and Y be two three-valued vectors of length N, representing two compatible partial positions. X\[Intersection]Y shall refer to the partial position such that a complete position extends X\[Intersection]Y if and only if it extends X and it extends Y. There exists precisely one such partial position X\[Intersection]Y and it can be constructed as follows:*)


(* ::DisplayFormula:: *)
(*X\[Intersection]Y=Z=<Subscript[z, 1],...,Subscript[z, N]> with *)
(*Subscript[z, i]=\!\(\**)
(*TagBox[GridBox[{*)
(*{"\[Piecewise]", GridBox[{*)
(*{*)
(*SubscriptBox["x", "i"], *)
(*FormBox[*)
(*RowBox[{*)
(*SubscriptBox["x", "i"], "=", *)
(*SubscriptBox["y", "i"]}],*)
(*TraditionalForm]},*)
(*{*)
(*SubscriptBox["x", "i"], *)
(*RowBox[{*)
(*RowBox[{*)
(*FormBox[*)
(*RowBox[{*)
(*SubscriptBox["x", "i"], "!=", *)
(*SubscriptBox["y", "i"]}],*)
(*TraditionalForm], " ", "and", " ", *)
(*SubscriptBox["y", "i"]}], "=", "0", " "}]},*)
(*{*)
(*SubscriptBox["y", "i"], *)
(*RowBox[{*)
(*RowBox[{*)
(*FormBox[*)
(*RowBox[{*)
(*SubscriptBox["x", "i"], "!=", *)
(*SubscriptBox["y", "i"]}],*)
(*TraditionalForm], " ", "and", " ", *)
(*SubscriptBox["x", "i"]}], "=", "0"}]}*)
(*},*)
(*AllowedDimensions->{2, Automatic},*)
(*Editable->True,*)
(*GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},*)
(*GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},*)
(*Selectable->True]}*)
(*},*)
(*GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},*)
(*GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}],*)
(*"Piecewise",*)
(*DeleteWithContents->True,*)
(*Editable->False,*)
(*SelectWithContents->True,*)
(*Selectable->False]\)*)


PosIntersec[x_, y_] := If[! CompatibleQ[x, y], Null,
   Max /@ Transpose[{x, y}]
   ];
PosIntersec[int1_, int2_, senIDs_] :=
  TVVectorToInteger[
   PosIntersec[
    IntegerToTVVector[int1, senIDs],
    IntegerToTVVector[int2, senIDs]
    ]
 ];


SubpositionI[int_,domain_,senIDs_]:=Module[{posTVV},
	posTVV=IntegerToTVVector[int, senIDs];
	TVVectorToInteger[
		ReplacePart[
			posTVV,
			(#->0&)/@(-Complement[Domain[int,senIDs],domain])
		]
	]
];


Subpositions[x_] := Subpositions[x, All];
Subpositions[x_, l_] := Module[
   {nonZeroInd},
   nonZeroInd = Flatten[Lookup[PositionIndex[x], {1, 2}, {}]];
   Function[rules,
     ReplacePart[x, rules]
     ] /@
    Map[
     # -> 0 &,
     Subsets[nonZeroInd, If[l === All, l, Length[nonZeroInd] - l]],
     {2}
     ]
   ];
SubpositionsI[int_, senIDs_] := TVVectorToInteger /@ Subpositions[IntegerToTVVector[int, senIDs]];
SubpositionsI[int_, l_, senIDs_] := TVVectorToInteger /@ Subpositions[IntegerToTVVector[int, senIDs], l];


End[]


EndPackage[]
