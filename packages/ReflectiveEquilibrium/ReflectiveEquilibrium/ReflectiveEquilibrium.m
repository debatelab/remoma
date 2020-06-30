(* ::Package:: *)

(* ::Chapter:: *)
(*Package ReflectiveEquilibrium*)


(* ::Text:: *)
(*Provides functions *)


(* ::Item:: *)
(*for calculating Simplicity, Account, Closeness*)


(* ::Item:: *)
(*for Theory Choice and Commitment Adjustment,*)


(* ::Item:: *)
(*for the main simulation loop,*)


(* ::Item:: *)
(*for plotting simulation results,*)


(* ::Item:: *)
(*for writing and reading simulation results to and from files.*)


BeginPackage["ReflectiveEquilibrium`",{"DialecticalStructures`BasicTDS`", "DialecticalStructures`InductiveReasoning`", "DialecticalStructures`PositionsAnalytics`"}]


Closeness::usage = "Closeness[args__]";
Simplicity::usage = "Simplicity[pos_, nPrinciplesPos_, senIDs_]";
AccountFunction::usage = "AccountFunction[param_]";


TheoryChoice::usage = "TheoryChoice[comOld, theoOld, initialCom, param, sigma, nPrinciples, closedPositions, senIDs]";
CommitmentAdjustment::usage = "CommitmentAdjustment[comOld, theoNew, initialCom, param, sigma, senIDs]";


RESimu::usage = "RESimu[parameters], where the parameters is an association with the following keys: numSen, numArg, initialCom, accountFunction, alpha, beta, minOverlapT, minOverlapC, sampleRatio, ConflictPenalty, ContractionPenalty, ExpansionPenalty, nMax.";


PlotRE::usage = "PlotRE[{{\"parameters\",parameters},{\"senIDs\",senIDs},{\"tau\",tau},{\"posEvolution\",posEvolution}}]";


(* ::Text:: *)
(**)


Begin["`Private`"]


(* ::Section:: *)
(*Evaluative Functions*)


Closeness[args__]:=NormalizedCloseness[args];


Simplicity[pos_, nPrinciplesPos_, senIDs_]:=1-((nPrinciplesPos-1)/SizeI[pos,senIDs])^2;


AccountFunction[param_]:=Module[{Account,param2},
  Switch[Lookup[param,"accountFunction","MutualCoh"],
	"MutualCohApprox",
	  Account[com_,theo_,sig_,sIDs_]:=(1+MutualCohApprox[com,theo,sig,sIDs])/2,
	"MutualCoh",
	  Account[com_,theo_,sig_,sIDs_]:=(1+MutualCoh[com,theo,sig,sIDs])/2,
	"NormalizedCloseness",
	  param2=param;
	  param2["ExpansionPenalty"]=param["ExpansionPenaltyAccount"];
	  Account[com_,theo_,sig_,sIDs_]:=Closeness[theo,com,sIDs,param2],
	_, (*use MutualCoh as default*)
	  Account[com_,theo_,sig_,sIDs_]:=(1+MutualCoh[com,theo,sig,sIDs])/2
  ];
  Account
];


(* ::Section:: *)
(*Choice Steps*)


(*Returns a new theory*)
(*Theories and commitments are represented through integers*)
TheoryChoice[comOld_,theoOld_,initialCom_,param_,sigma_,nPrinciples_,closedPositions_,senIDs_]:=Module[
	{Account,alpha,Score,scores,sampleRatio,sample,maxima,temp},

	(*Which measure of explanatory power do we use?*)
	Account=AccountFunction[param];

	(*Set up sample of candidate theories*)
	sampleRatio=Lookup[param,"sampleRatio",1];
	sample=DeleteCases[closedPositions,1];
	If[sampleRatio<1,sample=RandomSample[sample,UpTo[Round[sampleRatio*Length[sample]]]]];

	(*Discard all candidate theories that overlap with commitments by less than threshold*)
	temp=Cases[sample,p_/;RelativeOverlap[comOld,p,senIDs]<Lookup[param,"minOverlapT",0.5]];
	If[Length[sample]>Length[temp],sample=Complement[sample,temp]];

	(*Set parameter alpha*)
	If[!KeyMemberQ[param,"alpha"],Print["Missing parameter 'alpha' in TheoryChoice Step. Using default value."];];
	alpha=Lookup[param,"alpha",0.5];

	(*Define objective function to maximize*)
	Score[pos_]:=
		(*Convex combination of account and simplicity*)
		alpha*Account[comOld,pos,sigma,senIDs]+
		(1-alpha)*Simplicity[pos,nPrinciples[[ pos ]],senIDs];

	(*Calculate scores according to objective function*)
	PrintTemporary["TheoryChoice: Calculating scores ..."];
	scores = Monitor[
		Table[
		  {
			sample[[ i ]],
			Score[sample[[ i ]]]
		  },
		  {i, Length[sample]}
		],
	    ProgressIndicator[i, {1,  Length[sample]}]
	  ];
	PrintTemporary["TheoryChoice: ... done."];

	(*Determine maxima*)
	PrintTemporary["TheoryChoice: Choosing position so as to maximize simplicity & fit with given commitments"];
	maxima=First/@MaximalBy[
		scores,
		Last
	];

	(*Pick maximum randomly, if necessary*)
	If[Length[maxima]==1,
		PrintTemporary["TheoryChoice: Maximum found."];First[maxima],
		If[MemberQ[maxima,theoOld],
			PrintTemporary["TheoryChoice: Previous theory part of maxima. Keeping it."];theoOld,
			PrintTemporary["TheoryChoice: ",Length[maxima]," maxima found. Randomly picking one."];RandomChoice[maxima]
		]
	]

];


(*Returns a new set of commitments*)
CommitmentAdjustment[comOld_,theoNew_,initialCom_,param_,sigma_,senIDs_]:=Module[
	{Account,beta,Score,scores,sampleRatio,sample,maxima,paramPenalty,temp},

	(*Which measure of explanatory power do we use?*)
	Account=AccountFunction[param];

	sample=Range[3^Length[senIDs]-1]+1;(*={2,3,4,...,3^Length[senIDs]}*)
	(*Create random sample, of which comOld should be a part*)
	sampleRatio=Lookup[param,"sampleRatio",1];
	If[sampleRatio<1,
		sample=Union[
			{comOld},
			RandomSample[sample,UpTo[Round[sampleRatio*Length[sample]]]]
		]
	];

	(*Discard all candidate new commitments that overlap with old commitments by less than threshold*)
	temp=Cases[sample,p_/;RelativeOverlap[comOld,p,senIDs]<Lookup[param,"minOverlapC",0.8]];
	If[Length[sample]>Length[temp],sample=Complement[sample,temp]];


	(*Set parameter beta*)
	If[!KeyMemberQ[param,"beta"],Print["Missing parameter 'beta' in CommitmentAdjustment Step. Using default value."];];
	beta=Lookup[param,"beta",0.5];

	(*Set parameters for penalty function*)
	paramPenalty=KeyTake[param,{"ConflictPenalty","ContractionPenalty","ExpansionPenalty"}];

	(*Define objective function to maximize*)
	Score[pos_]:=
		(*Convex combination of account with given theory and conservation of previous commitments*)
		beta*Account[pos,theoNew,sigma,senIDs]+
		(1-beta)*Closeness[pos,initialCom,senIDs,paramPenalty];

	(*Calculate scores according to objective function*)
	PrintTemporary["CommitmentAdjustment: Calculating scores ..."];
	scores = Monitor[
		Table[
			{
				sample[[ i ]],
				Score[sample[[ i ]]]
			},
			{i, Length[sample]}
		],
		ProgressIndicator[i, {1,  Length[sample]}]
	];
	PrintTemporary["CommitmentAdjustment: ... done."];

	(*Determine maxima*)
	PrintTemporary["CommitmentAdjustment: Choosing position so as to maximize conservation of previous commitments & fit with given theory"];
	maxima=First/@MaximalBy[
		scores,
		Last
	];

	(*Pick maximum randomly, if necessary*)
	If[Length[maxima]==1,
		PrintTemporary["CommitmentAdjustment: Maximum found."];First[maxima],
		If[
			MemberQ[maxima,comOld],
			Print["CommitmentAdjustment: Previous commitments belong to maxima. Keeping them."];comOld,
			PrintTemporary["CommitmentAdjustment: ",Length[maxima]," maxima found. Randomly picking one."];RandomChoice[maxima]
		]
	]

];


(* ::Section:: *)
(*Simulation Loop*)


RESimu[parameters1_]:=Module[
	{
		senIDs,
		parameters=parameters1,
		nMax,(*stop after nMax iterations at the latest*)
		tau,sigma,nPrinciples,closedPositions,
		initialCom,posEvolution,Loop
	},

  nMax=Lookup[parameters,"nMax",10];
  senIDs=Range[Lookup[parameters,"numSen",8]];

  (*1. Generate a dialectical structure*)
  tau=Lookup[
    parameters,
    "tau",
    (1\[Implies]3)&&(1\[Implies]4)&&(1\[Implies]5)&&(1\[Implies]!6)&&(2\[Implies]5)&&(2\[Implies]6)&&(2\[Implies]7)&&(2\[Implies]!4)
    ];
  PrintTemporary["tau generated: ",tau];

  PrintTemporary["Creating sigma..."];
  sigma = Lookup[
    parameters,
    "sigma",
    Sigma[tau, True, senIDs]
    ];
  PrintTemporary["...done."];
 
  PrintTemporary["Creating nPrinciples..."];
  nPrinciples = Lookup[
    parameters,
    "nPrinciples",
    NPrinciples[sigma, senIDs]
    ];
  PrintTemporary["...done."];

  PrintTemporary["Creating closedPositions..."];
  closedPositions = Lookup[
    parameters,
    "closedPositions",
    DialecticallyClosedPositions[tau, True, senIDs]
    ];
  PrintTemporary["...done."];
 

  (*2. Determine initial commitments Subscript[C, 0] by choosing a random partial position,which is not necessarily dialectically consistent (but minimally consistent)*)
  initialCom=Lookup[parameters,"initialCom",RandomInteger[{2,3^Length[senIDs]}]];

  (*3.+4. We define a LOOP that integrates Theory Choice Step and Commitment Adjustment Step*)
  Loop[comTheoPair_]:=Module[{newCom,newThe},
    PrintTemporary["Next loop ..."];
    PrintTemporary[comTheoPair];
    newThe=TheoryChoice[
      Lookup[comTheoPair,"COM"],
      Lookup[comTheoPair,"THE"],
      initialCom,
      parameters,
      sigma, 
      nPrinciples,
      closedPositions,
      senIDs
    ];
    newCom=CommitmentAdjustment[
      Lookup[comTheoPair,"COM"],
      newThe,
      initialCom,
      parameters,
      sigma,
      senIDs
    ];
    Return[<|"THE"->newThe,"COM"->newCom|>]
  ];

  (*5. Iterate steps 3 and 4 until a fixed point -- concerning the commitments! -- is reached.*)
  posEvolution=FixedPointList[
    Loop,
    <|"THE"->1,"COM"->initialCom|>,
    nMax,
    SameTest->(Lookup[#1,"COM"]==Lookup[#2,"COM"]&)
  ];

  PrintTemporary["Equilibirum found! Returning simulation results."];

  {{"parameters",parameters},{"senIDs",senIDs},{"tau",tau},{"posEvolution",posEvolution}}
];


(* ::Section:: *)
(*Plotting*)


PlotRE[reData_]:=Module[
  { senIDs,posevo,tau,re=reData,
    FormatTheo,FormatCom ,sigma },

  senIDs=Cases[re,{"senIDs",_}][[1,2]];
  tau=Cases[re,{"tau",_}][[1,2]];
  posevo=Cases[re,{"posEvolution",_}][[1,2]];

  PrintTemporary["PlotRE: Creating sigma..."];sigma = Sigma[tau, True, senIDs];PrintTemporary["PlotRE: ...done."];


  FormatTheo[int_]:=Module[{v,posList,principles},
    (*Determine principles*)
    posList=IntegerToList[int,senIDs];
    principles=IntegerToList[Principles[int, sigma, senIDs],senIDs];

    v=MapIndexed[
      If[#1==0,
        (*First[#2] is not assigned truth-value by int: check whether its implied*)
        Switch[{!SatisfiableQ[First[#2]&&And@@posList&&tau,senIDs],!SatisfiableQ[!First[#2]&&And@@posList&&tau,senIDs]},
          {True,False}, -2,
          {False,True}, -1,
          _, 0
        ],
        (*First[#2] is assigned truth-value by int: check whether it's a principle*)
        Switch[MemberQ[principles,First[#2]]||MemberQ[principles,!First[#2]],
          True, #1+2, (*principle*)
          False, #1
        ]
      ]&,
      Reverse[IntegerToTVVector[int,senIDs]]
    ];
    (*Format*)
	v/.{
		1->Style["T",Darker[Blue]],
		2->Style["F",Darker[Blue]],
		3->Style["T",Darker[Blue],Underlined], (*principle*)
		4->Style["F",Darker[Blue],Underlined], (*principle*)
		-1->Style["T",LightGray],
		-2->Style["F",LightGray],
		0->""
	}
  ];

  FormatCom[int_]:=Module[{v,posList},
	(*Determine implications*)
	posList=IntegerToList[int,senIDs];
	v=MapIndexed[
	  If[#1==0,
		Switch[{!SatisfiableQ[First[#2]&&And@@posList&&tau,senIDs],!SatisfiableQ[!First[#2]&&And@@posList&&tau,senIDs]},
			{True,False},-2,
			{False,True},-1,
			_,0
		],
		#1
	  ]&,
	  Reverse[IntegerToTVVector[int,senIDs]]
	];
	v/.{
		1->Style["T",Black],
		2->Style["F",Black],
		-1->Style["T",LightGray],
		-2->Style["F",LightGray],
		0->""
	}
  ];

  TableForm[
	Flatten[{FormatTheo[First[#]],FormatCom[Last[#]]}&/@Values[posevo],1],
	TableHeadings->{Flatten[Table[
	{
		Subscript["T",i-1],
		Style[Subscript["C",i-1],
		  If[
			SatisfiableQ[And@@IntegerToList[Lookup[posevo[[i]],"COM"],senIDs]&&tau,senIDs],
			Plain,Red
		  ]
		]
	},{i,Length[posevo]}]],senIDs},
	TableSpacing->{1,1}
  ]
];


End[]


EndPackage[]
