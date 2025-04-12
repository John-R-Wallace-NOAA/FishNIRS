 
 
 sourceFunctionURL <- function (URL,  type = c("function", "script")[1]) {
         " # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() "
         if (!any(installed.packages()[, 1] %in% "httr"))  install.packages("httr") 
         File.ASCII <- tempfile()
         if(type == "function")
           on.exit(file.remove(File.ASCII))
         getTMP <- httr::GET(gsub(' ', '%20', URL))
         
         if(type == "function") {
           write(paste(readLines(textConnection(httr::content(getTMP))), collapse = "\n"), File.ASCII)
           source(File.ASCII)
         } 
         if(type == "script") {
           fileName <- strsplit(URL, "/")[[1]]
           fileName <- rev(fileName)[1]
           write(paste(readLines(textConnection(httr::content(getTMP))), collapse = "\n"), fileName)
         }  
}


sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/panel.xyplot.loess.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/agreementFigure.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Predict_NN_Age_Wrapper.R")


# source("C:/SIDT/Train_NN_Model/agreementFigure.R")
# source("C:/SIDT/Predict_NN_Ages/Predict_NN_Age.R")
source("C:/SIDT/Predict_NN_Ages/Predict_NN_Age_Wrapper.R")



# Spectra_Set needs to be in position '1' as in: assign("Spectra_Set", "Sable_Combo_2022", pos = 1)
# Spectra_Set <- "CLPR_Combo_1985__2024"
Spectra_Set <- "Sable_Combo_Comm"
 
 



# --- Create a Rdm_model_X.RData R list of lists to jump forward for parallel processing ---

load("C:\\SIDT\\For Home Rig\\SIDT\\Train_NN_Model\\Rdm_model_1.RData")

Jump_Past_N <- 15
Rdm_models <- list()
Rdm_folds_index <- list()
for (i in 1:Jump_Past_N) {
   Rdm_models[[i]] <- list(i)
   Rdm_folds_index[[i]] <- list(i)
} 

length(Rdm_models)
length(Rdm_folds_index)

Iter <- Jump_Past_N  

save(Iter, i, j, Cor, CA_diag, SAD, learningRate, layer_dropout_rate, Seed_Fold, Seed_Model, Seed_Main, Rdm_models, 
               Rdm_folds_index, SG_Variables_Selected, roundingDelta, file = paste0("C:/SIDT/For Home Rig/SIDT/Train_NN_Model/Rdm_model_", length(Rdm_folds_index), ".RData"))



# --- Rdm_model Stitching from Parallel Processing Runs ---

setwd("C:/SIDT/Sable_Comm/Sable_Combo_Comm_2017__2022_Str_Wgt_Sex")

base::load("C:\\SIDT\\Sable_Comm\\Sable_Combo_Comm_2017__2022_Str_Wgt_Sex\\Rdm_model_1__5.RData")
length(Rdm_models)
lapply(Rdm_models, length)
Rdm_models_1__5 <- Rdm_models[1:5]


base::load("C:\\SIDT\\Sable_Comm\\Sable_Combo_Comm_2017__2022_Str_Wgt_Sex\\Rdm_model_6__15.RData")
length(Rdm_models)
lapply(Rdm_models, length)
Rdm_models_6__15 <- Rdm_models[6:15]


base::load("C:\\SIDT\\Sable_Comm\\Sable_Combo_Comm_2017__2022_Str_Wgt_Sex\\Rdm_model_16__20.RData")
length(Rdm_models)
lapply(Rdm_models, length)
Rdm_models_16__20 <- Rdm_models[16:20]



Rdm_models <- c(Rdm_models_1__5, Rdm_models_6__15, Rdm_models_16__20)
length(Rdm_models)

save(Rdm_models, SG_Variables_Selected, roundingDelta , file = "C:/SIDT/Sable_Comm/Sable_Combo_Comm_2017__2022_Str_Wgt_Sex/Sable_Combo_Comm_FCNN_model_ver_1_20_Rdm_model.RData")


# --- Create a Fake Pred_Median_TMA (if needed) ---

load("C:\\SIDT\\Sable_Comm\\SABL_Combo_Comm_2017__2022_Model_Spectra_Meta_ALL_GOOD_DATA.RData")

Fake_Pred <- Model_Spectra_Meta$TMA + rnorm(length(Model_Spectra_Meta$TMA), 0, 5)
Sable_Combo_Comm_NN_Pred_Median_TMA <- data.frame(filenames = Model_Spectra_Meta$filenames, NN_Pred_Median = Fake_Pred, 
                                           Lower_Quantile_0.025 = Fake_Pred - 15, Upper_Quantile_0.975 = Fake_Pred + 15, TMA = Model_Spectra_Meta$TMA)

save(Sable_Combo_Comm_NN_Pred_Median_TMA, file = "C:/SIDT/Sable_Comm/Sable_Combo_Comm_2017__2022_Str_Wgt_Sex/Sable_Combo_Comm_2017__2022_NN_Pred_Median_TMA_FAKE.RData")


# ------------------------------------------

 
 
 
# !!! No bias correction !!!  !!! No von Bert !!!
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/Sable_Comm/Sable_Combo_Comm_2017__2022_Str_Wgt_Sex", Multi_Year = TRUE, Use_Session_Report_Meta = FALSE, 
  Bias_Adj_Factor_Ages = NULL, Bias_Reduction_Factor = NULL, Lowess_smooth_para = 2/3, TMA_Ages = TRUE, TMA_Ages_Only = FALSE, # TMA_Ages_Only = FALSE => Predictions without TMA
  # Metadata_Extra = c("Ship", "Cruise", "Haul", "Date_UTC", "Length_cm", "Sex", "Weight_kg", "Net_Partition"), 
  Metadata_Extra = c("structure_weight_g", "Length_cm", "Sex", "Month"), Meta_Data_Factors = c("Sex", "Month"), scanUniqueName = 'filenames', Debug_plotly.Spec = TRUE,
  Graph_Metadata = c("structure_weight_g", "Length_cm", "Sex", "Month"), F_vonBert = NULL, M_vonBert = NULL,
      Metadata_Extra_File = "C:\\SIDT\\Sable_Comm\\SABL_Combo_Comm_2017__2024_Model_Spectra_Meta_ALL_GOOD_DATA_TMA_NAs.RData", 
  Model_Spectra_Meta_Path = "C:\\SIDT\\Sable_Comm\\SABL_Combo_Comm_2017__2024_Model_Spectra_Meta_ALL_GOOD_DATA_TMA_NAs.RData", main = "Metadata: Str Wgt, Sex") 
	  
   

 
 
# !!! Bias correction with Keli's 2016 von Bert !!!
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/Sable_Comm/Sable_Combo_Comm_2017__2022_Str_Wgt_Sex", Multi_Year = TRUE, Use_Session_Report_Meta = FALSE, 
  Bias_Adj_Factor_Ages = c(7, 8:70), Bias_Reduction_Factor = 1.25, Lowess_smooth_para = 2/3, TMA_Ages = TRUE, TMA_Ages_Only = FALSE, # TMA_Ages_Only = FALSE => Predictions without TMA
  # Metadata_Extra = c("Ship", "Cruise", "Haul", "Date_UTC", "Length_cm", "Sex", "Weight_kg", "Net_Partition"), 
  Metadata_Extra = c("structure_weight_g", "Length_cm", "Sex", "Month"), Meta_Data_Factors = c("Sex", "Month"), scanUniqueName = 'filenames', Debug_plotly.Spec = TRUE,
  Graph_Metadata = c("structure_weight_g", "Length_cm", "Sex", "Month"), F_vonBert = list(Linf = 64, k = 0.32, t0 = 0), M_vonBert = list(Linf = 57, k = 0.41, t0 = 0),
      Metadata_Extra_File = "C:\\SIDT\\Sable_Comm\\SABL_Combo_Comm_2017__2024_Model_Spectra_Meta_ALL_GOOD_DATA_TMA_NAs.RData", Delta_Given = -0.15,
  Model_Spectra_Meta_Path = "C:\\SIDT\\Sable_Comm\\SABL_Combo_Comm_2017__2024_Model_Spectra_Meta_ALL_GOOD_DATA_TMA_NAs.RData", main = "Metadata: Str Wgt, Sex") 
	  
  

  
    Bias_Adj_Factor_Ages = c(10, 30:70), Bias_Reduction_Factor = 1.0, 
 
 
 
 Graph_Metadata = c("structure_weight_g", "Length_cm", "Sex", "Month"); F_vonBert = list(Linf = 64, k = 0.32, t0 = 0); M_vonBert = list(Linf = 57, k = 0.41, t0 = 0); Multi_Year = TRUE; TMA_Ages_Only = FALSE; TMA_Ages = TRUE
 Metadata_Extra = c("structure_weight_g", "Length_cm", "Sex", "Month"); Metadata_Extra_File = "C:\\SIDT\\Sable_Comm\\SABL_Combo_Comm_2017__2024_Model_Spectra_Meta_ALL_GOOD_DATA_TMA_NAs.RData";  Meta_Data_Factors = c("Sex", "Month")
 
 
 
 




	  
	  