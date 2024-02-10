

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




