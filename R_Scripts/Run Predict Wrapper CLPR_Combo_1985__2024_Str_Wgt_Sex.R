 
 
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



load("C:\\SIDT\\CLPR_Combo_1985__2024\\CLPR_Combo_1985__2024_Str_Wgt_Sex\\Predicted_Ages NO BIAS Corr\\NN Predicted Ages, 26 Mar 2025.RData")
Table(!New_Ages$Used_NN_Model)


New_Ages$Used_NN_Model[is.na(New_Ages$Used_NN_Model)] <- FALSE

save(New_Ages, file = "C:\\SIDT\\CLPR_Combo_1985__2024\\CLPR_Combo_1985__2024_Str_Wgt_Sex\\Predicted_Ages NO BIAS Corr\\NN Predicted Ages, 26 Mar 2025.RData")

load("C:\\SIDT\\CLPR_Combo_1985__2024\\CLPR_Combo_1985__2024_Str_Wgt_Sex\\Predicted_Ages NO BIAS Corr\\NN Predicted Ages, 26 Mar 2025.RData")
Table(!New_Ages$Used_NN_Model)




change(New_Ages[New_Ages$Used_NN_Model, ])


Table(New_Ages$TMA, New_Ages$Pred_Age_Bias_Corr_plus_Delta_rounded)
sum(Table(New_Ages$Used_NN_Model))
Table(!is.na(New_Ages$TMA))
Table(New_Ages$Used_NN_Model, !is.na(New_Ages$TMA))




change(New_Ages[New_Ages$Used_NN_Model, ])


Spectra in NN Model                       4,618  # Red circles in Metadata_Length_cm_vs_NN_Pred_Median.png and Metadata_structure_weight_g_vs_NN_Pred_Median.png
Spectra with TMA but not in the NN model    198  # Purple circles in the same figures
Spectra without TMA predicted by NN model 2,980  # Red circles in the same figures
Total Spectra                             7,796

Percent predicted ages without TMA:  38.2%  (100 * 2,980/7,796)

   
      
       
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
 
   
    

#  ============ 2004 Triennial Only - NOT Bias Correction ====================
 
# CLPR_Combo_1985__2024_CA_OR_Comm with Str_Wgt and Sex  !!! No bias correction !!!
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/CLPR_Combo_1985__2024/CLPR_Combo_1985__2024_Str_Wgt_Sex", Multi_Year = FALSE, Use_Session_Report_Meta = FALSE, 
  Bias_Adj_Factor_Ages = NULL, Bias_Reduction_Factor = NULL, Lowess_smooth_para = 2/3, TMA_Ages = TRUE, TMA_Ages_Only = FALSE, # TMA_Ages_Only = FALSE => Predictions without TMA
  # Metadata_Extra = c("Ship", "Cruise", "Haul", "Date_UTC", "Length_cm", "Sex", "Weight_kg", "Net_Partition"), 
  Metadata_Extra = c("structure_weight_g", "Length_cm", "Sex", "Month", "Latitude_dd"), Meta_Data_Factors = c("Sex", "Month"), scanUniqueName = 'filenames', Debug_plotly.Spec = TRUE,
  Graph_Metadata = c("structure_weight_g", "Length_cm", "Sex", "Month", "Latitude_dd"), F_vonBert = list(Linf = 48.7031, k = 0.2037, t0 = 0.2245), M_vonBert = list(Linf = 35.6675, k = 0.3166, t0 = 0.3336),
  Metadata_Extra_File = "C:/SIDT/Chilipepper/CLPR_Triennial_2004_Model_Spectra_Meta_ALL_GOOD_DATA.RData", Corr_Calc = FALSE, # All TMA are missing so need Corr_Calc = FALSE
  Model_Spectra_Meta_Path = "C:/SIDT/Chilipepper/CLPR_Triennial_2004_Model_Spectra_Meta_ALL_GOOD_DATA.RData", main = "Metadata: Str Wgt, Sex") 
	  
    



#  ============ With 2004 Triennial - Low scans only - NOT Bias Correction ====================

# CLPR_Combo_1985__2024_CA_OR_Comm with Str_Wgt and Sex  !!! No bias correction !!!
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/CLPR_Combo_1985__2024/CLPR_Combo_1985__2024_Str_Wgt_Sex", Multi_Year = FALSE, Use_Session_Report_Meta = FALSE, 
  Bias_Adj_Factor_Ages = NULL, Bias_Reduction_Factor = NULL, Lowess_smooth_para = 2/3, TMA_Ages = TRUE, TMA_Ages_Only = FALSE, # TMA_Ages_Only = FALSE => Predictions without TMA
  # Metadata_Extra = c("Ship", "Cruise", "Haul", "Date_UTC", "Length_cm", "Sex", "Weight_kg", "Net_Partition"), 
  Metadata_Extra = c("structure_weight_g", "Length_cm", "Sex", "Month", "Latitude_dd"), Meta_Data_Factors = c("Sex", "Month"), scanUniqueName = 'filenames', Debug_plotly.Spec = TRUE,
  Graph_Metadata = c("structure_weight_g", "Length_cm", "Sex", "Month", "Latitude_dd"), F_vonBert = list(Linf = 48.7031, k = 0.2037, t0 = 0.2245), M_vonBert = list(Linf = 35.6675, k = 0.3166, t0 = 0.3336),
      Metadata_Extra_File = "C:/SIDT/CLPR_Combo_1985__2024/CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA_Low_Tri.RData", 
  Model_Spectra_Meta_Path = "C:/SIDT/CLPR_Combo_1985__2024/CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA_Low_Tri.RData", main = "Metadata: Str Wgt, Sex") 
	  
    



#  ============ With 2004 Triennial - All Tri scans - NOT Bias Correction ====================

# CLPR_Combo_1985__2024_CA_OR_Comm with Str_Wgt and Sex  !!! No bias correction !!!
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/CLPR_Combo_1985__2024/CLPR_Combo_1985__2024_Str_Wgt_Sex", Multi_Year = TRUE, Use_Session_Report_Meta = FALSE, 
  Bias_Adj_Factor_Ages = NULL, Bias_Reduction_Factor = NULL, Lowess_smooth_para = 2/3, TMA_Ages = TRUE, TMA_Ages_Only = FALSE, # TMA_Ages_Only = FALSE => Predictions without TMA
  # Metadata_Extra = c("Ship", "Cruise", "Haul", "Date_UTC", "Length_cm", "Sex", "Weight_kg", "Net_Partition"), 
  Metadata_Extra = c("structure_weight_g", "Length_cm", "Sex", "Month", "Latitude_dd"), Meta_Data_Factors = c("Sex", "Month"), scanUniqueName = 'filenames', Debug_plotly.Spec = TRUE,
  Graph_Metadata = c("structure_weight_g", "Length_cm", "Sex", "Month", "Latitude_dd"), F_vonBert = list(Linf = 48.7031, k = 0.2037, t0 = 0.2245), M_vonBert = list(Linf = 35.6675, k = 0.3166, t0 = 0.3336),
      Metadata_Extra_File = "C:/SIDT/CLPR_Combo_1985__2024/CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData", 
  Model_Spectra_Meta_Path = "C:/SIDT/CLPR_Combo_1985__2024/CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData", main = "Metadata: Str Wgt, Sex") 
	  
    
    
    
# ----------- Look at high/low absorbance by tray 

load("C:/SIDT/Chilipepper/CLPR_Triennial_2004_Model_Spectra_Meta_ALL_GOOD_DATA.RData")

plotly.Spec(Model_Spectra_Meta, N_Samp = 'All', colorGroup = 'Tray', htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure Triennial by Tray'), Debug = TRUE)  

# metadata <- openxlsx::read.xlsx("C:/SIDT/Chilipepper/Chilipepper_2004_Triennial_Otolith_Weights_SWFSC.xlsx", sheet = 1, detectDates = TRUE) # Load in ancillary data 

New_Ages <- match.f(New_Ages, Model_Spectra_Meta, 'specimen_id',  'specimen_id', 'Tray')



Table(New_Ages$Scan_Pos, New_Ages$Tray)
      
          1    2    3    4    5    6    7 <NA>
  High   41   15   79   69    0    0    0  146
  Low    59   80   20   30   98   97   50 7650

    
# ------------ Predicted age by high/low absorbance by 2004 Triennial vs all other year/fishery groups -----------------


Table(New_Ages$Pred_Age_Bias_Corr_plus_Delta_rounded, New_Ages$Scan_Pos, New_Ages$Year %in% '2004_Triennial')

     
           Other     
       Year/Fishery    2004_Triennial
          Groups     
                     
Pred            Absorbance           # Absorbance above 1.05 = 'High'; Absorbance below or equal to 1.05 = 'Low'
 Age    High  Low        High  Low  
  0        0  641     0     3    1
  1        0 1300     1   103  180
  2        0  661     2     6   21
  3        0  331     3     7    5
  4        0  337     4     6   18
  5        0  488     5    32   64
  6        0  522     6    33   71
  7        0  524     7     7   23
  8        0  485     8     2    7
  9        0  503     9     2   12
  10       0  483     10    0    5
  11       1  431     11    0    7
  12       4  299     12    1    7
  13       7  200     13    1    3
  14       9  127     14    0    1
  15      13  110     15    0    1
  16      12   72     16    1    2
  17       7   37     17    0    3
  18       7   33     18    0    2
  19      13   21     19    0    0
  20      11    8     20    0    0
  21      18   16     21    0    1
  22      21   16     22    0    0
  23      17    4     23    0    0
  24       5    1     24    0    0
  25       1    0     25    0    0


# -------------------------


Ages seem fine, the first-derivative transformation appears to have taken care of the issue, see Fig. 2 here:

    https://cdnsciencepub.com/doi/pdf/10.1139/cjfas-2023-0045
    
However, ....    






#  ============ With 2023-24 CA Rec - NOT Bias Correction 22 year/fishery groups ====================

# CLPR_Combo_1985__2024_CA_OR_Comm with Str_Wgt and Sex  !!! No bias correction !!!
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/CLPR_Combo_1985__2024/CLPR_Combo_1985__2024_Str_Wgt_Sex", Multi_Year = TRUE, Use_Session_Report_Meta = FALSE, 
  Bias_Adj_Factor_Ages = NULL, Bias_Reduction_Factor = NULL, Lowess_smooth_para = 2/3, TMA_Ages = TRUE, TMA_Ages_Only = FALSE, # TMA_Ages_Only = FALSE => Predictions without TMA
  # Metadata_Extra = c("Ship", "Cruise", "Haul", "Date_UTC", "Length_cm", "Sex", "Weight_kg", "Net_Partition"), 
  Metadata_Extra = c("structure_weight_g", "Length_cm", "Sex", "Month", "Latitude_dd"), Meta_Data_Factors = c("Sex", "Month"), scanUniqueName = 'filenames', Debug_plotly.Spec = TRUE,
  Graph_Metadata = c("structure_weight_g", "Length_cm", "Sex", "Month", "Latitude_dd"), F_vonBert = list(Linf = 48.7031, k = 0.2037, t0 = 0.2245), M_vonBert = list(Linf = 35.6675, k = 0.3166, t0 = 0.3336),
      Metadata_Extra_File = "C:/SIDT/CLPR_Combo_1985__2024/CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData", 
  Model_Spectra_Meta_Path = "C:/SIDT/CLPR_Combo_1985__2024/CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData", main = "Metadata: Str Wgt, Sex") 
	  
    


































	  
	  