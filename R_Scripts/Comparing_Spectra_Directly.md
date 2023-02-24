
     # Determine the columns that contain the spectra.  Here the spectra columns range from 2 - 1155, with the metadata start in column 1156.
     Sable_2017_2019[1:4, c(1:4, 1153:1157)] 
                              filenames     12490     12482     12474      3610      3603      3595 age_structure_id   Barcode
     1 SABLEFISH_COMBO201701203A_2_OD1.0 0.4218516 0.4214812 0.4218767 0.6086814 0.6070352 0.6034645 102118142-SABL-O 102118142
     2 SABLEFISH_COMBO201701203A_3_OD1.0 0.4033358 0.4044081 0.4050675 0.6221674 0.6203897 0.6174445 102118143-SABL-O 102118143
     3 SABLEFISH_COMBO201701203A_6_OD1.0 0.4066261 0.4073936 0.4081993 0.6152601 0.6138538 0.6109281 102118146-SABL-O 102118146
     4 SABLEFISH_COMBO201701203A_7_OD1.0 0.3954711 0.3966139 0.3982748 0.6274921 0.6246090 0.6208161 102118147-SABL-O 102118147
     
     
     # Using the TMA (Traditional Method of Aging) column, find file names that were aged to be 1 year old:
     Sable_2017_2019[Sable_2017_2019$TMA == 1 & !is.na(Sable_2017_2019$TMA), ][1:3, c('filenames', 'TMA', '12490', '12482', '12474', '12467')]
                                filenames TMA     12490     12482     12474     12467
     1   SABLEFISH_COMBO201701203A_2_OD1.0   1 0.4218516 0.4214812 0.4218767 0.4225301
     10 SABLEFISH_COMBO201701203A_14_OD1.0   1 0.4180041 0.4192649 0.4201036 0.4197980
     11 SABLEFISH_COMBO201701203A_15_OD1.0   1 0.4216180 0.4224118 0.4230497 0.4232008
     
     
     # Save 3 vectors of spectra from the age 1 Sablefish
     (Otie_1A <- Sable_2017_2019[Sable_2017_2019$filenames == 'SABLEFISH_COMBO201701203A_2_OD1.0', 2:1155])[1:10]
     Otie_1B <- Sable_2017_2019[Sable_2017_2019$filenames == 'SABLEFISH_COMBO201701203A_15_OD1.0', 2:1155]
     Otie_1C <- Sable_2017_2019[Sable_2017_2019$filenames == 'SABLEFISH_COMBO201701203A_14_OD1.0', 2:1155]
     
     
     # Find the sum of the absolute differences for the 3 vectors. (We don't want correlation or R-squared here, since vectors of numbers can be correlated but separated by a delta.)
     sum(abs(Otie_1A - Otie_1A))
     [1] 0
     
     sum(abs(Otie_1A - Otie_1B))
     [1] 3.286354
     
     sum(abs(Otie_1A - Otie_1C))
     [1] 1.284246
     
     sum(abs(Otie_1B - Otie_1C))
     [1] 3.727268
     
     
     # Do the same for Sablefish with a TMA of 5 
     Sable_2017_2019[Sable_2017_2019$TMA == 5 & !is.na(Sable_2017_2019$TMA), ][1:3, c('filenames', 'TMA', '12490', '12482', '12474', '12467')]
                                  filenames TMA     12490     12482     12474     12467
     127 SABLEFISH_COMBO201701203A_171_OD1.0   5 0.3976701 0.3984869 0.3992108 0.3992122
     173 SABLEFISH_COMBO201701203A_231_OD1.0   5 0.3994965 0.4003064 0.4008479 0.4011689
     238 SABLEFISH_COMBO201701203A_324_OD1.0   5 0.4164396 0.4172910 0.4182080 0.4181522
     
     # Note that sum of the absolute differences is large between the age 5 fish and the age 1 fish.
     Otie_5A <- Sable_2017_2019[Sable_2017_2019$filenames == 'SABLEFISH_COMBO201701203A_171_OD1.0', 2:1155]
     sum(abs(Otie_1A - Otie_5A))
     [1] 34.17674
     
     Otie_5B <- Sable_2017_2019[Sable_2017_2019$filenames == 'SABLEFISH_COMBO201701203A_231_OD1.0', 2:1155]
     sum(abs(Otie_1A - Otie_5B))
     [1] 30.20351
     
     
     # The difference between the age 5 fish is smaller
     sum(abs(Otie_5A - Otie_5B))
     [1] 5.259298
     
     
     # Try the same with age 10 Sablefish
     Sable_2017_2019[Sable_2017_2019$TMA == 10 & !is.na(Sable_2017_2019$TMA), ][1:3, c('filenames', 'TMA', '12490', '12482', '12474', '12467')]
                                   filenames TMA     12490     12482     12474     12467
     80  SABLEFISH_COMBO201701203A_115_OD1.0  10 0.3888220 0.3894408 0.3902158 0.3910199
     81  SABLEFISH_COMBO201701203A_116_OD1.0  10 0.3894672 0.3900470 0.3902689 0.3904091
     115 SABLEFISH_COMBO201701203A_157_OD1.0  10 0.4037862 0.4047136 0.4055516 0.4060116
     
     # Large difference
     Otie_10A <- Sable_2017_2019[Sable_2017_2019$filenames == 'SABLEFISH_COMBO201701203A_115_OD1.0', 2:1155]
     sum(abs(Otie_1A - Otie_10A))
     [1] 38.48886
     
     Otie_10B <- Sable_2017_2019[Sable_2017_2019$filenames == 'SABLEFISH_COMBO201701203A_116_OD1.0', 2:1155]
     sum(abs(Otie_1A - Otie_10B))
     [1] 37.40644
     
     # Small difference
     sum(abs(Otie_10A - Otie_10B))
     [1] 1.614045
     
     
     # Difference for age 5 vs. age 10
     sum(abs(Otie_5A - Otie_10A))
     [1] 4.581376
     
     sum(abs(Otie_5B - Otie_10B))
     [1] 8.75515

