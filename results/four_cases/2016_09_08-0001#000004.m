{{"parameters", <|"numSen" -> 7, "numArg" -> 7, "initialCom" -> 1333, 
   "accountFunction" -> "NormalizedCloseness", "alpha" -> 0.4, "beta" -> 0.8, 
   "minOverlapT" -> 0, "minOverlapC" -> 0, "sampleRatio" -> 1., 
   "ConflictPenalty" -> 1, "ContractionPenalty" -> 1, 
   "ExpansionPenalty" -> 0, "ExpansionPenaltyAccount" -> 0.3, 
   "nMax" -> 10|>}, {"senIDs", {1, 2, 3, 4, 5, 6, 7}}, 
 {"tau", Implies[1, 3] && Implies[1, 4] && Implies[1, 5] && 
   Implies[1,  !6] && Implies[2, 5] && Implies[2, 6] && Implies[2, 7] && 
   Implies[2,  !4]}, {"posEvolution", {<|"THE" -> 1, "COM" -> 1333|>, 
   <|"THE" -> 1340, "COM" -> 1340|>, <|"THE" -> 611, "COM" -> 611|>, 
   <|"THE" -> 611, "COM" -> 611|>}}}
