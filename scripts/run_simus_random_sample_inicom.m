(* ::Package:: *)

(* ::Title:: *)
(*Run Ensemble of RE-Processes with Random Sample of Initial Commitments*)


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


ensembleDir="run_simus_random_sample_inicom-new";
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
 
(***Call simulations***)

Map[
  Function[
    initialC,
	RESimu[<|
		"numSen"->7,
		"numArg"->7,
		"tau"->tau,
		"sigma"->sigma,
		"nPrinciples"->nPrinciples,
		"closedPositions"->closedPositions,
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
  (RandomInteger[Length[sigma]-1,ensembleSize]+1)
];









