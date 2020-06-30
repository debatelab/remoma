(* ::Package:: *)

(* ::Title:: *)
(*Run Ensemble of RE-Processes with Random Sample of Weights of Achievement Function *)


SetDirectory[$HomeDirectory];
If[!MemberQ[$Path,#],AppendTo[$Path, #]]&[FileNameJoin[{"git","remoma","packages","DialecticalStructures"}]];
If[!MemberQ[$Path,#],AppendTo[$Path, #]]&[FileNameJoin[{"git","remoma","packages","ReflectiveEquilibrium"}]];
<<DialecticalStructures`BasicTDS`;
<<DialecticalStructures`InductiveReasoning`;
<<DialecticalStructures`PositionsAnalytics`;
<<ReflectiveEquilibrium`ReflectiveEquilibrium`;


SetDirectory[NotebookDirectory[]];
If[!DirectoryQ["../results"], CreateDirectory["../results"]];
SetDirectory["../results"];


ensembleDir="par-narrow_sample-new";
If[!DirectoryQ[ensembleDir], CreateDirectory[ensembleDir]];
SetDirectory[ensembleDir];
ensembleSize=500;


(***Set up***)

numSen=7;
senIDs=Range[numSen];
tau=(1\[Implies]3)&&(1\[Implies]4)&&(1\[Implies]5)&&(1\[Implies]!6)&&(2\[Implies]5)&&(2\[Implies]6)&&(2\[Implies]7)&&(2\[Implies]!4);
sigma=Sigma[tau, True, senIDs];
nPrinciples=NPrinciples[sigma, senIDs];
closedPositions=DialecticallyClosedPositions[tau, True, senIDs];
standardInitialCommitments={118,121,1090,1333};
 
(***Call simulations***)

Table[
	RESimu[<|
		"numSen"->7,
		"numArg"->7,
		"tau"->tau,
		"sigma"->sigma,
		"nPrinciples"->nPrinciples,
		"closedPositions"->closedPositions,
		"initialCom"->RandomChoice[standardInitialCommitments],
		"accountFunction"->"NormalizedCloseness",
		"alpha"->RandomReal[{.3,.5}], (*use {0,1} for full variation*)
		"beta"->RandomReal[{.6,.99}], (*use {0,1} for full variation*)
		"minOverlapT"->0,"minOverlapC"->0,(*candidate theories/commitments below threshold are discarded*)
		"sampleRatio"->1.0,(*ratio of all candidate positions considered in the. choice and comm. adj.*)
		"ConflictPenalty"->1,"ContractionPenalty"->1,"ExpansionPenalty"->0,
		"ExpansionPenaltyAccount"->0.3,
		"nMax"->10
	|>]>>ensembleDir<>"#"<>IntegerString[Length[FileNames[ensembleDir<>"*"]]+1,10,6]<>".m"
  ,
  ensembleSize
];















