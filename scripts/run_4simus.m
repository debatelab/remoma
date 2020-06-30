(* ::Package:: *)

(* ::Title:: *)
(*Run 4 Standard RE Cases*)


SetDirectory[$HomeDirectory];
If[!MemberQ[$Path,#],AppendTo[$Path, #]]&[FileNameJoin[{"git","remoma","packages","DialecticalStructures"}]];
If[!MemberQ[$Path,#],AppendTo[$Path, #]]&[FileNameJoin[{"git","remoma","packages","ReflectiveEquilibrium"}]];
<<BasicTDS`;
<<InductiveReasoning`;
<<PositionsAnalytics`;
<<ReflectiveEquilibrium`;


SetDirectory[NotebookDirectory[]];
If[!DirectoryQ["../results"], CreateDirectory["../results"]];
SetDirectory["../results"];


ensembleDir="four-cases-new";
If[!DirectoryQ[ensembleDir], CreateDirectory[ensembleDir]];
SetDirectory[ensembleDir];


Map[
  Function[
    initialC,
	RESimu[<|
		"numSen"->7,
		"numArg"->7,
		"initialCom"->initialC,
		"accountFunction"->"NormalizedCloseness",
		"alpha"->0.4,
		"beta"->0.8,
		"minOverlapT"->0,"minOverlapC"->0,(*candidate theories/commitments below threshold are discarded*)
		"sampleRatio"->1.0,(*ratio of all candidate positions considered in the. choice and comm. adj.*)
		"ConflictPenalty"->1,"ContractionPenalty"->1,"ExpansionPenalty"->0,
		"ExpansionPenaltyAccount"->0.3,
		"nMax"->10
	|>]>>ensembleDir<>"#"<>IntegerString[Length[FileNames[ensembleDir<>"*"]]+1,10,6]<>".m"
  ],
  {118,121,1090,1333}
];






