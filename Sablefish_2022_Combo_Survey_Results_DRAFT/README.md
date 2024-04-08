## Sablefish 2022 Combo Survey Results ##
A Fully Connected Neural Net (FCNN) training model using 1,528 otoliths with 20 random full fold models was run to compare Near Infrared Scans (NIRS) to the Traditional Method of Aging (TMA).  Fifteen otoliths were not used for training to look for bias. The total number of oties predicted was 1,553.

Metadata was also added to the scans for a better fit.  

The best [prediction](/Sablefish_2022_Combo_Survey_Results_DRAFT/Sable_2022_Combo_Scans_Otie_Wgt_Fish_Len_Weight_Depth_Lat_Agreement_Fig.png), with an impressive R Squared of 0.9669, was found using NIRS scans with the metadata of otolith weight, fish length, fish weight, depth, and latitude.  Other metadata looked at, but not included due to poor performance in this particulear FCNN model were sex, month, and days into the year.


### Models with a smaller number of otoltiths in the training model 
A Fully Connected Neural Net (FCNN) training model using 750 otoliths with 20 random full fold models was conducted.  Fifteen otoliths were not used for training to look for bias. The total number of oties predicted was 1,553.

A [model](/Sablefish_2022_Combo_Survey_Results_DRAFT/Sable_2022_Combo_Scans_Otie_Wgt_Fish_Len_Weight_Depth_750N_Agreement_Fig.png), with an R Squared of 0.9476, was found using the NIRS scans with the metadata of otolith weight, fish length, fish weight, depth (a model with latitude has not yet been run).  In comparison, the model identical to the first model above, except without latitude, has an R squared of 0.9526. This [figure](/Sablefish_2022_Combo_Survey_Results_DRAFT/TMA_minus_NN_Age_Rounded_vs_TMA_Jittered_Left_Out_Oties_Highlighted_750N.png) highlights in red those otliths that were not part of the training model, but were only predicted.  Note that there is no bias. Models with 500 and 250 otiliths in the training model did not perform as well.


### Metadata only models
A Fully Connected Neural Net (FCNN) training model using 1,513 otoliths with 20 random full fold models was executed.  Thirty otoliths were not used for training to look for bias. The total number of oties predicted was 1,553.

The best [model](/Sablefish_2022_Combo_Survey_Results_DRAFT/Sable_2022_Combo_Otie_Wgt_Fish_Len_Weight_Depth_Metadata_Only_Agreement_Fig.png), only using metadata (no scans), had an R squared of 0.8770 using otolith weight, fish length, fish weight, and depth. Adding latitude to the metadata only model did not work well.


