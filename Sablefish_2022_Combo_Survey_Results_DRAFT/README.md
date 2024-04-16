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

Other metadata looked at, but not included due to poor performance in this particular FCNN model, were sex, month, and days into the year. The same model, except without latitude, has an R squared of 0.9526. The scans only model (no metadata) has an R squared of 0.9378, with a predicted N of 1,556 since there is no missing metadata.

### Notes on the Agreement Figures and Stats
- SAD is the Sum of Absolute Deviations
- The weighted sum of the numbers in the figure is equal to SAD. With zero weight for those on the one-to-one line (in red), a weight of one for those numbers one absolute value off the one-to-one line (in gold), and so forth.
- SAD/N is the commonly used Mean Absolute Error (MAE)
- RMSE is the square Root of Mean Squared Error
- APE is the Average Percent Error
- The FCNN model minimized MSE (the square of RMSE) internally and SAD (with ties broken with RMSE) was used in the external loops.
- The Delta is added to the NN predicted value before rounding; e.g. 4.6 + -0.2 rounds to 4 not 5. The Delta is found by searching over the best fit to TMA ages for values of 0 to -0.45 in steps of -0.05. (The TMA is currently only available in round years of estimated age.)

### Models with a smaller number of otoliths in the training model 
A FCNN training model using 750 randomly selected otoliths with 20 random full fold models was conducted. The total number of oties predicted was 1,553.

A [model](/Sablefish_2022_Combo_Survey_Results_DRAFT/Sable_2022_Combo_Scans_Otie_Wgt_Fish_Len_Weight_Depth_750N_Agreement_Fig.png), with an R Squared of 0.9476, was found using the NIRS scans along with the metadata of otolith weight, fish length, fish weight, and depth. The same model including latitude was found to fit slightly less well (R squared = 0.9418). This [figure](/Sablefish_2022_Combo_Survey_Results_DRAFT/TMA_minus_NN_Age_Rounded_vs_TMA_Jittered_Left_Out_Oties_Highlighted_750N.png) highlights in red those otoliths that were not part of the training model but were only predicted.  Note that there is no bias. Models with 500 and 250 otoliths in the training model did not perform as well, even when stratified random sampling of otoliths was tried.


### Metadata only models
A FCNN training model using 1,513 otoliths with 20 random full fold models was executed.  Forty otoliths were not used for training to look for bias. The total number of oties predicted was 1,553.

The best [model](/Sablefish_2022_Combo_Survey_Results_DRAFT/Sable_2022_Combo_Otie_Wgt_Fish_Len_Weight_Depth_Metadata_Only_Agreement_Fig.png), only using metadata (no scans), had an R squared of 0.8770 using otolith weight, fish length, fish weight, and depth. Adding latitude to the metadata only model did not work well. The stats for the metadata only model are:


         Correlation R_squared    RMSE     MAE  SAD     APE    N
            0.936458  0.876953 3.79319 1.93883 3011 11.1122 1553
       
       
         FSA (Simple Fisheries Stock Assessment Methods) package's agePrecision() stats:       
            n validn R PercAgree   ASD   ACV    AAD   APE
         1553   1553 2     42.82 1.371 15.71 0.9694 11.11

### Double Reads
For 2022 Combo Sablefish there were 396 TMA double reads for which ager 'NWFSC_1' was the original ager on exactly half of the reads, and the original ager on the other half was 'NWFSC_2'.  Of those double reads, 369 had NN predicted ages. NWFSC_1 was the original ager for ~52% and NWFSC_2 was the original ager for ~48% of those 369. This [figure](/Sablefish_2022_Combo_Survey_Results_DRAFT/Sable_2022_Combo_Double_Rds_NWFSC_1_vs_NWFSC_2.png), with a APE score of 3.704% (R2 = 0.9707), plots the double reads of NWFSC_2 vs NWFSC_1. This [figure](/Sablefish_2022_Combo_Survey_Results_DRAFT/Sable_2022_Combo_Double_Rds_NWFSC_1_vs_NN_Pred_Rd.png), with a APE score of 6.378% (R2 = 0.9573), plots the NN predicted age vs NWFSC_1. TMA age readers strive to achieve an APE score of less than 5% for their double reads (Patrick McDonald, personal communication).

### The Final Data

The final data has the NN predicted median, the 0.025 lower quantile, and the 0.975 uppper quantile over the number of full 10 fold models (20 in this caese). The is TMA is also given, if available.



                                          filenames NN_Pred_Median Lower_Quantile_0.025 Upper_Quantile_0.975 Num_of_Full_10_Fold_Models TMA
     1   SABL_COMBO2022_NIR0022A_PRD_1_102157421_O1        10.8008               3.9460              13.8160                         20  14
     2  SABL_COMBO2022_NIR0022A_PRD_10_102157430_O1         5.5932               4.6599               6.8086                         20   6
     3 SABL_COMBO2022_NIR0022A_PRD_100_102157520_O1         7.3155               5.6351               7.9963                         20   6
     4  SABL_COMBO2022_NIR0022A_PRD_11_102157431_O1        11.2928               9.1743              12.9363                         20  16
     5  SABL_COMBO2022_NIR0022A_PRD_12_102157432_O1         4.6883               3.6430               5.3899                         20   6
     ...

The quantiles are a reflection of the NN models precision based on the 20 full 10-fold randomized models, not the accuracy to the TMA Age (which, after all, may not be available).  This [figure](/Sablefish_2022_Combo_Survey_Results_DRAFT/Sable_2022_Combo_Scans_Otie_Wgt_Fish_Len_Weight_Depth_Lat_TMA_Sorted_Subset.png), based on the best prediction model above, 
shows the quantile range on a subset (for clarity) of the predicted data where TMA ages are sorted and this [figure](/Sablefish_2022_Combo_Survey_Results_DRAFT/Sable_2022_Combo_Scans_Otie_Wgt_Fish_Len_Weight_Depth_Lat_Predicted_Ages_Sorted_Subset.png) shows the same except the predicted ages are sorted. Note that the median, as is well known, is a robust measure of central tendency. 

Using 40 full 10-fold randomized models is often a small improvement over 20 full 10-fold models, but 60 full 10-fold models has been seen, in limited testing, to not work as well.
     

















