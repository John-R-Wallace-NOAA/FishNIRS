 
 
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

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/agreementFigure.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Predict_NN_Age_Wrapper.R")


# source("C:/SIDT/Train_NN_Model/agreementFigure.R")
# source("C:/SIDT/Predict_NN_Ages/Predict_NN_Age.R")
source("C:/SIDT/Predict_NN_Ages/Predict_NN_Age_Wrapper.R")



# Trained model from 2023 with added bias correction predicting 474 oties with TMA age from the 2024 net retention study 
 
# Spectra_Set needs to be in position '1' as in: assign("Spectra_Set", "Sable_Combo_2022", pos = 1)
Spectra_Set <- "CLPR_Combo_2010__2024"
 
 
 
# CLPR_Combo_2010__2024 with Str_Wgt, Sex, and Month in the Model
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/CLPR_Combo_2010__2024/CLPR_Combo_2010__2024_Str_Wgt_Sex_Month", Multi_Year = TRUE, Use_Session_Report_Meta = FALSE, 
  Bias_Adj_Factor_Ages = c(12, 13:15), Bias_Reduction_Factor = 1.25, Lowess_smooth_para = 2/3, TMA_Ages = TRUE, TMA_Ages_Only = FALSE, # TMA_Ages_Only = FALSE => Predictions without TMA
  # Metadata_Extra = c("Ship", "Cruise", "Haul", "Date_UTC", "Length_cm", "Sex", "Weight_kg", "Net_Partition"), 
  Metadata_Extra = c("Length_cm", "Sex", "Month"), Meta_Data_Factors = c("Sex", "Month"),
  Graph_Metadata = c("structure_weight_dg", "Length_cm", "Sex", "Month"), 
  Metadata_Extra_File = "C:/SIDT/CLPR_Combo_2010__2024/CLPR_SWFSC_2010__2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData", 
  Model_Spectra_Meta_Path = "C:/SIDT/CLPR_Combo_2010__2024/CLPR_SWFSC_2010__2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData", main = "Metadata: Str Wgt, Sex, Month") 
	  
  
 # Also tried Bias_Adj_Factor_Ages = c(12, 9:15), (12, 10:15), (12, 11:15) all at Bias_Reduction_Factor = 1.25
 
 

 
# CLPR_Combo_2010__2024 with Str_Wgt, Sex, and Month in the Model  !!!Survey doesn't happen when the fishery happens and fishery oties were collected in March
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/CLPR_Combo_2010__2024/CLPR_Combo_2010__2024_Str_Wgt_Sex_Month", Multi_Year = TRUE, Use_Session_Report_Meta = FALSE, 
  Bias_Adj_Factor_Ages = c(12, 13:15), Bias_Reduction_Factor = 1.25, Lowess_smooth_para = 2/3, TMA_Ages = TRUE, TMA_Ages_Only = FALSE, # TMA_Ages_Only = FALSE => Predictions without TMA
  # Metadata_Extra = c("Ship", "Cruise", "Haul", "Date_UTC", "Length_cm", "Sex", "Weight_kg", "Net_Partition"), 
  Metadata_Extra = c("Length_cm", "Sex", "Month"), Meta_Data_Factors = c("Sex", "Month"),
  Graph_Metadata = c("structure_weight_dg", "Length_cm", "Sex", "Month"), 
  Metadata_Extra_File = "C:/SIDT/CLPR_Combo_2010__2024/CLPR_SWFSC_2010__2024_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData", 
  Model_Spectra_Meta_Path = "C:/SIDT/CLPR_Combo_2010__2024/CLPR_SWFSC_2010__2024_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData", main = "Metadata: Str Wgt, Sex, Month") 
	  
      
 
# CLPR_Combo_2010__2024_Comm_Model (CA) with Str_Wgt and Sex  # 1.00 and 1.25 bias factor tried
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/CLPR_Combo_2010__2024/CLPR_Combo_2010__2024_Str_Wgt_Sex", Multi_Year = TRUE, Use_Session_Report_Meta = FALSE, 
  Bias_Adj_Factor_Ages = c(12, 13:15), Bias_Reduction_Factor = 1.25, Lowess_smooth_para = 2/3, TMA_Ages = TRUE, TMA_Ages_Only = FALSE, # TMA_Ages_Only = FALSE => Predictions without TMA
  # Metadata_Extra = c("Ship", "Cruise", "Haul", "Date_UTC", "Length_cm", "Sex", "Weight_kg", "Net_Partition"), 
  Metadata_Extra = c("Length_cm", "Sex", "Month"), Meta_Data_Factors = c("Sex", "Month"),
  Graph_Metadata = c("structure_weight_dg", "Length_cm", "Sex", "Month"), 
  Metadata_Extra_File = "C:/SIDT/CLPR_Combo_2010__2024/CLPR_SWFSC_2010__2024_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData", 
  Model_Spectra_Meta_Path = "C:/SIDT/CLPR_Combo_2010__2024/CLPR_SWFSC_2010__2024_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData", main = "Metadata: Str Wgt, Sex, Month") 
	  
 
 
# CLPR_Combo_2010__2024_CA_OR_Comm with Str_Wgt and Sex  # 1.00 and 1.25 bias factor tried
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/CLPR_Combo_2010__2024/CLPR_Combo_2010__2024_Str_Wgt_Sex", Multi_Year = TRUE, Use_Session_Report_Meta = FALSE, 
  Bias_Adj_Factor_Ages = c(12, 13:15), Bias_Reduction_Factor = 1.25, Lowess_smooth_para = 2/3, TMA_Ages = TRUE, TMA_Ages_Only = FALSE, # TMA_Ages_Only = FALSE => Predictions without TMA
  # Metadata_Extra = c("Ship", "Cruise", "Haul", "Date_UTC", "Length_cm", "Sex", "Weight_kg", "Net_Partition"), 
  Metadata_Extra = c("Length_cm", "Sex", "Month"), Meta_Data_Factors = c("Sex", "Month"),
  Graph_Metadata = c("structure_weight_dg", "Length_cm", "Sex", "Month"), 
  Metadata_Extra_File = "C:/SIDT/CLPR_Combo_2010__2024/CLPR_SWFSC_2010__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData", 
  Model_Spectra_Meta_Path = "C:/SIDT/CLPR_Combo_2010__2024/CLPR_SWFSC_2010__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData", main = "Metadata: Str Wgt, Sex, Month") 
	  





































	  
	  