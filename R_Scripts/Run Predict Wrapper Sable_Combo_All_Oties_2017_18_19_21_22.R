 
 
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


# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Predict_NN_Age_Wrapper.R")
 
 
# source("C:/SIDT/Train_NN_Model/Read_OPUS_Spectra.R")
# source("C:/SIDT/Train_NN_Model/agreementFigure.R")
source("C:/SIDT/Predict_NN_Ages/Predict_NN_Age_Wrapper.R")


# Spectra_Set <- "Sable_Combo_2023"  # Spectra_Set needs to be in position '1' as in: assign("Spectra_Set", "Sable_Combo_2022", pos = 1)
Spectra_Set <- "Sable_Combo_Multi_17_22"
 
 
 
# When Model_Spectra_Meta exists    # Having 'Multi' in the the Spectra_Set sets Use_Session_Report_Meta to FALSE
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/Sablefish Combo Multi Year/Sable_Combo_All_Oties_2017_18_19_21_22", Multi_Year = TRUE, 
      Model_Spectra_Meta_Path = "C:/SIDT/Sablefish Combo Multi Year/Sable_Combo_All_Oties_2017_18_19_21_22/Sable_Combo_Multi_17_22_Model_Spectra_Meta_ALL_GOOD_DATA.RData", main = Spectra_Set) 
    
	
	  
	  
# When Model_Spectra_Meta does not exists and Read_OPUS_Spectra() needs to be used to read in the scans which have been placed into: C:\SIDT\Predict_NN_Ages\New_Scans
# *** Meta_Path argument is needed ***
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/Sablefish Combo Multi Year/Sable_Combo_All_Oties_2017_18_19_21_22", 
Meta_Path = "C:/SIDT/Sable_Combo_2023/SABL_COMBO_2023_NIRS_Scanning_Session_Report_For_NWFSC.xlsx", Multi_Year = FALSE, Folds_Num = 5)
	  
	  	 

# ======================= Put all Combo years together ======================================

setwd("C:/SIDT/Sablefish Combo Multi Year/Sable_Combo_All_Oties_2017_18_19_21_22")

	  
load("C:/SIDT/Sablefish Combo Multi Year/Sable_Combo_All_Oties_2017_18_19_21_22/Sable_Combo_Multi_17_22_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
headTail(Model_Spectra_Meta,2,2,2,75)
Table(Model_Spectra_Meta$sample_year)
Model_Spectra_Meta_Combo_2017__2022 <- Model_Spectra_Meta


load("C:\\SIDT\\Sablefish\\Sable_Combo_2015_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
headTail(Model_Spectra_Meta,2,2,2,75)
Model_Spectra_Meta_Combo_2015 <- Model_Spectra_Meta


load("C:\\SIDT\\Sablefish\\Sable_Combo_2016_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
headTail(Model_Spectra_Meta,2,2,2,75)
Model_Spectra_Meta_Combo_2016 <- Model_Spectra_Meta


load("C:\\SIDT\\Sablefish\\Sable_Combo_2023_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
headTail(Model_Spectra_Meta,2,2,2,75)
Model_Spectra_Meta_Combo_2023 <- Model_Spectra_Meta


load("C:\\SIDT\\Sablefish\\Sable_Combo_2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
headTail(Model_Spectra_Meta,2,2,2,75)
Model_Spectra_Meta_Combo_2024 <- Model_Spectra_Meta


Model_Spectra_Meta_Combo_2017__2022$Sex_prop_max <- Model_Spectra_Meta_Combo_2017__2022$Month_Scaled <- NULL

Columns <- names(Model_Spectra_Meta_Combo_2017__2022)

Columns[!Columns %in% names(Model_Spectra_Meta_Combo_2015)]
Columns[!Columns %in% names(Model_Spectra_Meta_Combo_2016)]
Columns[!Columns %in% names(Model_Spectra_Meta_Combo_2023)]
Columns[!Columns %in% names(Model_Spectra_Meta_Combo_2024)]


Model_Spectra_Meta <- rbind(Model_Spectra_Meta_Combo_2017__2022, Model_Spectra_Meta_Combo_2015[, Columns], Model_Spectra_Meta_Combo_2016[, Columns], 
                          Model_Spectra_Meta_Combo_2023[, Columns], Model_Spectra_Meta_Combo_2024[, Columns])
                          
Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)

headTail(Model_Spectra_Meta,2,2,2,75)

save(Model_Spectra_Meta, file = "C:/SIDT/Sablefish Combo Multi Year/Sable_Combo_Multi_15__24_Model_Spectra_Meta_ALL_GOOD_DATA.RData")



# --- No bias correction with Kelli's 2016 von Bert with fixed T0 at zero thru 2024 ---
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/Sablefish Combo Multi Year/Sable_Combo_All_Oties_2017_18_19_21_22", Multi_Year = TRUE, Use_Session_Report_Meta = FALSE, 
  Bias_Adj_Factor_Ages = NULL, Bias_Reduction_Factor = NULL, Lowess_smooth_para = 2/3, TMA_Ages = TRUE, TMA_Ages_Only = FALSE, # TMA_Ages_Only = FALSE => Predictions without TMA
  # Metadata_Extra = c("Ship", "Cruise", "Haul", "Date_UTC", "Length_cm", "Sex", "Weight_kg", "Net_Partition"), 
  Metadata_Extra = c("structure_weight_g", "Length_cm", "Weight_kg", "Depth_m", "Latitude_dd", "Sex", "Month"), Meta_Data_Factors = c("Sex", "Month"), scanUniqueName = 'filenames', Debug_plotly.Spec = TRUE,
  Graph_Metadata = c("structure_weight_g", "Length_cm", "Weight_kg", "Depth_m", "Latitude_dd", "Sex", "Month"), F_vonBert = list(Linf = 64, k = 0.32, t0 = 0), M_vonBert = list(Linf = 57, k = 0.41, t0 = 0),
      Metadata_Extra_File = "C:/SIDT/Sablefish Combo Multi Year/Sable_Combo_Multi_15__24_Model_Spectra_Meta_ALL_GOOD_DATA.RData", Delta_Given = list(-0.15, NULL)[[2]],
  Model_Spectra_Meta_Path = "C:/SIDT/Sablefish Combo Multi Year/Sable_Combo_Multi_15__24_Model_Spectra_Meta_ALL_GOOD_DATA.RData", main = "Metadata: Str Wgt, Len, Wgt, Depth, Lat") 
	
 
 # For debug 
 Graph_Metadata = c("structure_weight_g", "Length_cm", "Weight_kg", "Depth_m", "Latitude_dd", "Sex", "Month"); F_vonBert = list(Linf = 64, k = 0.32, t0 = 0); M_vonBert = list(Linf = 57, k = 0.41, t0 = 0)
 Multi_Year = TRUE; TMA_Ages_Only = FALSE; TMA_Ages = TRUE
 Metadata_Extra = c("structure_weight_g", "Length_cm", "Weight_kg", "Depth_m", "Latitude_dd", "Sex", "Month"); Metadata_Extra_File = "C:/SIDT/Sablefish Combo Multi Year/Sable_Combo_Multi_15__24_Model_Spectra_Meta_ALL_GOOD_DATA.RData"  
 Meta_Data_Factors = c("Sex", "Month")
  
 

# --- Fix up New_Ages for sharing ---
load("C:\\SIDT\\Sablefish Combo Multi Year\\Sable_Combo_All_Oties_2015__2024\\Predicted_Ages_No_Bias_Corr\\NN Predicted Ages, 17 Apr 2025.RData")
New_Ages$Used_NN_Model[is.na(New_Ages$Used_NN_Model)] <- FALSE 
New_Ages$structure_weight_dg <- New_Ages$Index <- New_Ages$Length_prop_max <- New_Ages$Weight_prop_max <- New_Ages$Depth_prop_max <- New_Ages$Latitude_prop_max <- NULL
names(New_Ages)[grep('Year', names(New_Ages))] <- "Year_Project"
names(New_Ages)[grep('Pred_Age_Bias_Corr_plus_Delta_rounded', names(New_Ages))] <- "Pred_Age_Corr_plus_Delta_rounded"
names(New_Ages)[grep('Pred_Age_Bias_Corr_plus_Delta_rounded_Minus_TMA', names(New_Ages))] <- "Pred_Age_Corr_plus_Delta_rounded_Minus_TMA"
sum(Table(New_Ages$Year_Project, New_Ages$Used_NN_Model))
headTail(New_Ages)
save(New_Ages, file = "C:\\SIDT\\Sablefish Combo Multi Year\\Sable_Combo_All_Oties_2015__2024\\Predicted_Ages_No_Bias_Corr\\NN Predicted Ages, 17 Apr 2025.RData")


 


 
# --- Bias correction with Kelli's 2016 von Bert with fixed T0 at zero thru 2024 ---
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/Sablefish Combo Multi Year/Sable_Combo_All_Oties_2017_18_19_21_22", Multi_Year = TRUE, Use_Session_Report_Meta = FALSE, 
  Bias_Adj_Factor_Ages = c(56, 30:70), Bias_Reduction_Factor = 0.8, Lowess_smooth_para = 2/3, TMA_Ages = TRUE, TMA_Ages_Only = FALSE, # TMA_Ages_Only = FALSE => Predictions without TMA
  # Metadata_Extra = c("Ship", "Cruise", "Haul", "Date_UTC", "Length_cm", "Sex", "Weight_kg", "Net_Partition"), 
  Metadata_Extra = c("structure_weight_g", "Length_cm", "Weight_kg", "Depth_m", "Latitude_dd", "Sex", "Month"), Meta_Data_Factors = c("Sex", "Month"), scanUniqueName = 'filenames', Debug_plotly.Spec = TRUE,
  Graph_Metadata = c("structure_weight_g", "Length_cm", "Weight_kg", "Depth_m", "Latitude_dd", "Sex", "Month"), F_vonBert = list(Linf = 64, k = 0.32, t0 = 0), M_vonBert = list(Linf = 57, k = 0.41, t0 = 0),
      Metadata_Extra_File = "C:/SIDT/Sablefish Combo Multi Year/Sable_Combo_Multi_15__24_Model_Spectra_Meta_ALL_GOOD_DATA.RData", Delta_Given = list(0, NULL)[[1]],
  Model_Spectra_Meta_Path = "C:/SIDT/Sablefish Combo Multi Year/Sable_Combo_Multi_15__24_Model_Spectra_Meta_ALL_GOOD_DATA.RData", main = "Metadata: Str Wgt, Len, Wgt, Depth, Lat") 
	


 # For debug 
 Graph_Metadata = c("structure_weight_g", "Length_cm", "Weight_kg", "Depth_m", "Latitude_dd", "Sex", "Month"); F_vonBert = list(Linf = 64, k = 0.32, t0 = 0); M_vonBert = list(Linf = 57, k = 0.41, t0 = 0)
 Multi_Year = TRUE; TMA_Ages_Only = FALSE; TMA_Ages = TRUE
 Metadata_Extra = c("structure_weight_g", "Length_cm", "Weight_kg", "Depth_m", "Latitude_dd", "Sex", "Month"); Metadata_Extra_File = "C:/SIDT/Sablefish Combo Multi Year/Sable_Combo_Multi_15__24_Model_Spectra_Meta_ALL_GOOD_DATA.RData"  
 Meta_Data_Factors = c("Sex", "Month")
  
  
# Tried 
Bias_Adj_Factor_Ages = c(7, 8:70), Bias_Reduction_Factor = 1.25  # Huge gap :-/
Bias_Adj_Factor_Ages = c(23, 30:70), Bias_Reduction_Factor = 0.8 # small gap
Bias_Adj_Factor_Ages = c(7, 30:70), Bias_Reduction_Factor = 0.8,  # gap
Bias_Adj_Factor_Ages = c(0, 30:70), Bias_Reduction_Factor = 0.8 # screws up zero


# --- Fix up New_Ages for sharing ---
load("C:\\SIDT\\Sablefish Combo Multi Year\\Sable_Combo_All_Oties_2015__2024\\Predicted_Ages_Bias_Corr\\NN Predicted Ages, 18 Apr 2025.RData")
New_Ages$Used_NN_Model[is.na(New_Ages$Used_NN_Model)] <- FALSE 
New_Ages$structure_weight_dg <- New_Ages$Index <- New_Ages$Length_prop_max <- New_Ages$Weight_prop_max <- New_Ages$Depth_prop_max <- New_Ages$Latitude_prop_max <- NULL
names(New_Ages)[grep('Year', names(New_Ages))] <- "Year_Project"
sum(Table(New_Ages$Year_Project, New_Ages$Used_NN_Model))
sum(Table(New_Ages$Year_Project, New_Ages$Bias_Adj))
headTail(New_Ages)
save(New_Ages, file = "C:\\SIDT\\Sablefish Combo Multi Year\\Sable_Combo_All_Oties_2015__2024\\Predicted_Ages_Bias_Corr\\NN Predicted Ages, 18 Apr 2025.RData")


 







