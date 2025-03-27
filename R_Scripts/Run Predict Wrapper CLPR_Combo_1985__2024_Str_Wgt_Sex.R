 
 
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
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/agreementFigure.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Predict_NN_Age_Wrapper.R")


# source("C:/SIDT/Train_NN_Model/agreementFigure.R")
# source("C:/SIDT/Predict_NN_Ages/Predict_NN_Age.R")
source("C:/SIDT/Predict_NN_Ages/Predict_NN_Age_Wrapper.R")



# Spectra_Set needs to be in position '1' as in: assign("Spectra_Set", "Sable_Combo_2022", pos = 1)
# Spectra_Set <- "CLPR_Combo_1985__2024"
Spectra_Set <- "CLPR_SWFSC_1985__2024"
 
 
 
 # =============================== NOT Bias Correction ========================================
 
# CLPR_Combo_1985__2024_CA_OR_Comm with Str_Wgt and Sex  !!! No bias correction !!!
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/CLPR_Combo_1985__2024/CLPR_Combo_1985__2024_Str_Wgt_Sex", Multi_Year = TRUE, Use_Session_Report_Meta = FALSE, 
  Bias_Adj_Factor_Ages = NULL, Bias_Reduction_Factor = NULL, Lowess_smooth_para = 2/3, TMA_Ages = TRUE, TMA_Ages_Only = FALSE, # TMA_Ages_Only = FALSE => Predictions without TMA
  # Metadata_Extra = c("Ship", "Cruise", "Haul", "Date_UTC", "Length_cm", "Sex", "Weight_kg", "Net_Partition"), 
  Metadata_Extra = c("structure_weight_g", "Length_cm", "Sex", "Month", "Latitude_dd"), Meta_Data_Factors = c("Sex", "Month"), scanUniqueName = 'filenames', Debug_plotly.Spec = TRUE,
  Graph_Metadata = c("structure_weight_g", "Length_cm", "Sex", "Month", "Latitude_dd"), F_vonBert = list(Linf = 48.7031, k = 0.2037, t0 = 0.2245), M_vonBert = list(Linf = 35.6675, k = 0.3166, t0 = 0.3336),
  Metadata_Extra_File = "C:/SIDT/CLPR_Combo_1985__2024/CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData", 
  Model_Spectra_Meta_Path = "C:/SIDT/CLPR_Combo_1985__2024/CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData", main = "Metadata: Str Wgt, Sex") 
	  
     
# !!! After moving C:\SIDT\Predict_NN_Ages\Predicted_Ages run "C:\SIDT\Chilipepper\Fit Von Bert & Double Reads\Add Double Reads to New Ages.R" for double read figures !!!
 
 
 
            Estimate  Std. Error   t value Pr(>|t|)
Linf 48.7030996 0.156198260 311.80309        0
K     0.2037147 0.002702931  75.36808        0
t0   -0.2244905 0.003609395  62.19617        0


Males

      Estimate  Std. Error   t value Pr(>|t|)
Linf 35.6674871 0.112558762 316.87881        0
K     0.3165704 0.005529543  57.25074        0
t0   -0.3335626 0.005954828  56.01549        0



   
      
       
 # =============================== Bias Correction ========================================
 
# CLPR_Combo_1985__2024_CA_OR_Comm with Str_Wgt and Sex  
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/CLPR_Combo_1985__2024/CLPR_Combo_1985__2024_Str_Wgt_Sex", Multi_Year = TRUE, Use_Session_Report_Meta = FALSE, 
  Bias_Adj_Factor_Ages = c(12, 13:15), Bias_Reduction_Factor = 1.25, Lowess_smooth_para = 2/3, TMA_Ages = TRUE, TMA_Ages_Only = FALSE, # TMA_Ages_Only = FALSE => Predictions without TMA
  # Metadata_Extra = c("Ship", "Cruise", "Haul", "Date_UTC", "Length_cm", "Sex", "Weight_kg", "Net_Partition"), 
  Metadata_Extra = c("structure_weight_g", "Length_cm", "Sex", "Month", "Latitude_dd"), Meta_Data_Factors = c("Sex", "Month"), scanUniqueName = 'filenames', Debug_plotly.Spec = TRUE,
  Graph_Metadata = c("structure_weight_g", "Length_cm", "Sex", "Month", "Latitude_dd"), F_vonBert = list(Linf = 48.4386, k = 0.2064, t0 = 0.2800), M_vonBert = list(Linf = 35.6959, k = 0.3156, t0 = 0.3796),
  Metadata_Extra_File = "C:/SIDT/CLPR_Combo_1985__2024/CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData", 
  Model_Spectra_Meta_Path = "C:/SIDT/CLPR_Combo_1985__2024/CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData", main = "Metadata: Str Wgt, Sex") 
	  
    
# !!! After moving C:\SIDT\Predict_NN_Ages\Predicted_Ages run "C:\SIDT\Chilipepper\Fit Von Bert & Double Reads\Add Double Reads to New Ages.R" for double read figures !!!
 
   
    


























	  
	  