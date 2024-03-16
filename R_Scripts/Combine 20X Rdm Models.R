


# ======== New Method - Save the 'newScans.pred.ALL' from each model run, and combine them right before Pred_median and New_Ages are created. ==========

# Run 1 
 
load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_BEST_750N_Run_1\\Predicted_Ages\\newScans.pred.ALL, 15 Mar 2024.RData")
# headTail(newScans.pred.ALL, 3, 3)

load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_BEST_750N_Run_1\\Predicted_Ages\\NN Predicted Ages, 14 Mar 2024.RData")
# headTail(New_Ages, 3)


# Run 3

load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_BEST_750N_Run_3\\Predicted_Ages\\newScans.pred.ALL, 15 Mar 2024.RData")
# headTail(newScans.pred.ALL, 3, 3)
 
# load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_BEST_750N_Run_3\\Predicted_Ages\\NN Predicted Ages, 6 Mar 2024.RData") 
# headTail(New_Ages, 3) 


# Combine the runs

newScans.pred.ALL <- rbind(newScans.pred.ALL_1, newScans.pred.ALL)
nrow(newScans.pred.ALL)/max(newScans.pred.ALL$Index)

Pred_median <- r(data.frame(NN_Pred_Median = aggregate(list(NN_Pred_Median = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), median, na.rm = TRUE)[,2], 
      Lower_Quantile_0.025 = aggregate(list(Quantile_0.025 = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), quantile, probs = 0.025, na.rm = TRUE)[,2],
      Upper_Quantile_0.975 = aggregate(list(Quantile_0.975 = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), quantile, probs = 0.975, na.rm = TRUE)[,2],
	  N = aggregate(list(N = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), length)[,2]), 4)
 
   if(verbose) {	 
   cat("\n\nPred_median:\n\n")  
   headTail(Pred_median, 3, 3)
   cat(paste0("\n\n--- Note: The quantiles are a reflection of the NN models precision based on ", nrow(newScans.pred.ALL)/nrow(New_Ages)/10, " full 10-fold randomized models, not the accuracy to a TMA Age ---\n\n"))   
   }


New_Ages <- data.frame(filenames = New_Ages$filenames, Pred_median)

New_Ages[is.na(New_Ages$NN_Pred_Median), ]

New_Ages <- New_Ages[!is.na(NN_Pred_Median), ]








# ======================================== OLD =================================================================================

# Saved the 40 Rdm_models here: C:\ALL_USR\JRW\SIDT\Sablefish 2022 Combo\Sable_Combo_2022_FCNN_model_40_Rdm_models # 20X 1 and 2

# Saved the 60 Rdm_models here: C:\ALL_USR\JRW\SIDT\Sablefish 2022 Combo\Sable_Combo_2022_FCNN_model_60_Rdm_models





setwd("C:/ALL_USR/JRW/SIDT/Predict_NN_Ages")

# 1
base::load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_15_Dec_2023_09_54_18.RData")
Rdm_models_1 <- Rdm_models

# 2
base::load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt_Run_2\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_4_Jan_2024_03_05_13.RData")
Rdm_models_2 <- Rdm_models

# 3
base::load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt_Run_3\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_4_Feb_2024_00_20_22.RData")


Rdm_models <- c(Rdm_models_1, Rdm_models_2, Rdm_models)
rm(Rdm_models_1, Rdm_models_2)

length(Rdm_models)

save(Rdm_models, SG_Variables_Selected, roundingDelta, file = "Sable_Combo_2022_FCNN_model_60_Rdm_models.RData")


# Copy and paste the script to run
Predict_NN_Age_Script.R



# --- 20X 1 and 3 ---

setwd("C:/ALL_USR/JRW/SIDT/Predict_NN_Ages")

# 1
base::load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_15_Dec_2023_09_54_18.RData")
Rdm_models_1 <- Rdm_models

# 3
base::load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt_Run_3\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_4_Feb_2024_00_20_22.RData")


Rdm_models <- c(Rdm_models_1, Rdm_models)
rm(Rdm_models_1)

length(Rdm_models)


save(Rdm_models, SG_Variables_Selected, roundingDelta, file = "Sable_Combo_2022_FCNN_model_40_Rdm_models_Runs_1_3.RData")




# --- 20X 1 and 3 for 750 oties in the training set ---

setwd("C:/ALL_USR/JRW/SIDT/Predict_NN_Ages")


# 3
base::load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_BEST_750N_Run_3\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_6_Mar_2024_13_15_47.RData")
Rdm_models_3 <- Rdm_models
metaDataVar <- (1:length(SG_Variables_Selected))[is.na(as.numeric(substring(SG_Variables_Selected, 2)))]
print(SG_Variables_Selected[metaDataVar])
# [1] "length_prop_max"     "structure_weight_dg" "Weight_kg"           "Depth_m"      


# 1
base::load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_BEST_750N_Run_1\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_14_Mar_2024_16_05_42.RData")
metaDataVar <- (1:length(SG_Variables_Selected))[is.na(as.numeric(substring(SG_Variables_Selected, 2)))]
print(SG_Variables_Selected[metaDataVar])
# [1] "length_prop_max"     "structure_weight_dg" "Depth_prop_max"      "Weight_prop_max"    


Rdm_models <- c(Rdm_models, Rdm_models_3)
rm(Rdm_models_3)

length(Rdm_models)

save(Rdm_models, SG_Variables_Selected, roundingDelta, file = "Sable_Combo_2022_FCNN_model_40_Rdm_models_Runs_1_3_750N.RData")























ls.RData("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt_Run_3\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_4_Feb_2024_00_20_22.RData")

Seed_Fold           numeric      0
22                           Seed_Main           numeric      0
23                          Seed_Model    



# Check random number seeds

# 1
extractRData('Seed_Fold', "C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_15_Dec_2023_09_54_18.RData")
[1] 727

extractRData('Seed_Main', "C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_15_Dec_2023_09_54_18.RData")
[1] 707

extractRData('Seed_Model',"C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_15_Dec_2023_09_54_18.RData")
[1] 727


# 2
extractRData('Seed_Fold', "C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt_Run_2\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_4_Jan_2024_03_05_13.RData")
[1] 747

extractRData('Seed_Main', "C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt_Run_2\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_4_Jan_2024_03_05_13.RData")
[1] 747

extractRData('Seed_Model',"C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt_Run_2\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_4_Jan_2024_03_05_13.RData")
[1] 727



# 3
extractRData('Seed_Fold', "C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt_Run_3\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_4_Feb_2024_00_20_22.RData")
[1] 787

extractRData('Seed_Main', "C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt_Run_3\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_4_Feb_2024_00_20_22.RData")
[1] 807

extractRData('Seed_Model', "C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt_Run_3\\Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_4_Feb_2024_00_20_22.RData")
[1] 727








 source("C:\\ALL_USR\\JRW\\SIDT\\Predict_NN_Ages\\Predict_NN_Age.R")




SG_Variables_Selected[metaDataVar]


data.frame(Model_Spectra.sg[, sort(Model_Spectra.iPLS.F$var.selected)], length_prop_max = Model_Spectra_Meta$length_cm/max(Model_Spectra_Meta$length_cm), 
                                           structure_weight_dg = 10* Model_Spectra_Meta$structure_weight_g) # dg = decigram




extractRData('roundingDelta', file = "Sable_Combo_2022_FCNN_model_60_Rdm_models.RData")




