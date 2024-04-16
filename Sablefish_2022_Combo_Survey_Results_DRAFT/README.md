## Sablefish 2022 Combo Survey Results ##
A Fully Connected Neural Net (FCNN) training model using 1,538 otoliths with 20 random full fold models was run to compare Near Infrared Scans (NIRS) to the Traditional Method of Aging (TMA).  Fifteen otoliths were not used for training to look for bias. The total number of oties predicted was 1,553.

Metadata was also added to the scans for a better fit.  

The best [prediction](/Sablefish_2022_Combo_Survey_Results_DRAFT/Sable_2022_Combo_Scans_Otie_Wgt_Fish_Len_Weight_Depth_Lat_Agreement_Fig.png), with an impressive R Squared of 0.9669, was found using NIRS scans with the metadata of otolith weight, fish length, fish weight, depth, and latitude. 
The stats are:

       Correlation R_squared   RMSE     MAE  SAD     APE    N
          0.983321  0.966921 1.9744 1.01095 1570 6.25705 1553
     
     
       FSA (Simple Fisheries Stock Assessment Methods) package's agePrecision() stats:     
          n validn R PercAgree    ASD   ACV    AAD   APE
       1553   1553 2     54.09 0.7148 8.849 0.5055 6.257

Other metadata looked at, but not included due to poor performance in this particular FCNN model, were sex, month, and days into the year. The same model except without latitude, has an R squared of 0.9526. The scans only model has an R squared of 0.9378, with a predicted N of 1,556 since there is no missing metadata.

### Notes on the Agreement Figures and Stats
- SAD is the Sum of Absolute Deviations
- The weighted sum of the numbers in the figure is equal to SAD. With zero weight for those on the one-to-one line (in red), a weight of one for those numbers one absolute value off the one-to-one line (in gold), and so forth.
- SAD/N is the commonly used Mean Absolute Error (MAE)
- RMSE is the square Root of Mean Squared Error
- APE is the average percent agreement
- The FCNN model minimized MSE (the square of RMSE) internally and SAD (with ties broken with RMSE) was used in the external loops.
- The Delta is added to the NN predicted value before rounding; e.g. 4.6 + -0.2 rounds to 4 not 5. The Delta is found by searching over the best fit to TMA ages for values of 0 to -0.45 in steps of -0.05. (The TMA is currently only availabe in round years of estimated age.)

### Models with a smaller number of otoltiths in the training model 
A FCNN training model using 750 randomly selected otoliths with 20 random full fold models was conducted. The total number of oties predicted was 1,553.

A [model](/Sablefish_2022_Combo_Survey_Results_DRAFT/Sable_2022_Combo_Scans_Otie_Wgt_Fish_Len_Weight_Depth_750N_Agreement_Fig.png), with an R Squared of 0.9476, was found using the NIRS scans along with the metadata of otolith weight, fish length, fish weight, and depth. The same model including latitude was found to fit slightly less well (R squared = 0.9418). This [figure](/Sablefish_2022_Combo_Survey_Results_DRAFT/TMA_minus_NN_Age_Rounded_vs_TMA_Jittered_Left_Out_Oties_Highlighted_750N.png) highlights in red those otoliths that were not part of the training model but were only predicted.  Note that there is no bias. Models with 500 and 250 otiliths in the training model did not perform as well, even when stratified random sampling of otoliths was tried.


### Metadata only models
A FCNN training model using 1,513 otoliths with 20 random full fold models was executed.  Forty otoliths were not used for training to look for bias. The total number of oties predicted was 1,553.

The best [model](/Sablefish_2022_Combo_Survey_Results_DRAFT/Sable_2022_Combo_Otie_Wgt_Fish_Len_Weight_Depth_Metadata_Only_Agreement_Fig.png), only using metadata (no scans), had an R squared of 0.8770 using otolith weight, fish length, fish weight, and depth. Adding latitude to the metadata only model did not work well.


         Correlation R_squared    RMSE     MAE  SAD     APE    N
            0.936458  0.876953 3.79319 1.93883 3011 11.1122 1553
       
       
         FSA (Simple Fisheries Stock Assessment Methods) package's agePrecision() stats:       
            n validn R PercAgree   ASD   ACV    AAD   APE
         1553   1553 2     42.82 1.371 15.71 0.9694 11.11



