{{"parameters", <|"numSen" -> 7, "numArg" -> 7, "initialCom" -> 1090, 
   "accountFunction" -> "NormalizedCloseness", "alpha" -> 0.4, "beta" -> 0.8, 
   "minOverlapT" -> 0, "minOverlapC" -> 0, "sampleRatio" -> 1., 
   "ConflictPenalty" -> 1, "ContractionPenalty" -> 1, 
   "ExpansionPenalty" -> 0, "ExpansionPenaltyAccount" -> 0.3, 
   "nMax" -> 10|>}, {"senIDs", {1, 2, 3, 4, 5, 6, 7}}, 
 {"tau", Implies[1, 3] && Implies[1, 4] && Implies[1, 5] && 
   Implies[1,  !6] && Implies[2, 5] && Implies[2, 6] && Implies[2, 7] && 
   Implies[2,  !4]}, {"posEvolution", {<|"THE" -> 1, "COM" -> 1090|>, 
   <|"THE" -> 1122, "COM" -> 1122|>, <|"THE" -> 1113, "COM" -> 1113|>, 
   <|"THE" -> 1113, "COM" -> 1113|>}}}