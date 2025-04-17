 
 
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
	  
   

 
 
# --- Bias correction with Kelli's 2016 von Bert with fixed T0 at zero thru 2024 ---
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/Sable_Combo_Comm/Sable_Combo_Comm_2017__2022_2024_Str_Wgt_Sex", Multi_Year = TRUE, Use_Session_Report_Meta = FALSE, 
  Bias_Adj_Factor_Ages = c(7, 8:70), Bias_Reduction_Factor = 1.25, Lowess_smooth_para = 2/3, TMA_Ages = TRUE, TMA_Ages_Only = FALSE, # TMA_Ages_Only = FALSE => Predictions without TMA
  # Metadata_Extra = c("Ship", "Cruise", "Haul", "Date_UTC", "Length_cm", "Sex", "Weight_kg", "Net_Partition"), 
  Metadata_Extra = c("structure_weight_g", "Length_cm", "Sex", "Month"), Meta_Data_Factors = c("Sex", "Month"), scanUniqueName = 'filenames', Debug_plotly.Spec = TRUE,
  Graph_Metadata = c("structure_weight_g", "Length_cm", "Sex", "Month"), F_vonBert = list(Linf = 64, k = 0.32, t0 = 0), M_vonBert = list(Linf = 57, k = 0.41, t0 = 0),
      Metadata_Extra_File = "C:\\SIDT\\Sable_Combo_Comm\\SABL_Combo_Comm_2017__2024_Model_Spectra_Meta_ALL_GOOD_DATA_TMA_NAs.RData", Delta_Given = list(-0.15, NULL)[[2]],
  Model_Spectra_Meta_Path = "C:\\SIDT\\Sable_Combo_Comm\\SABL_Combo_Comm_2017__2024_Model_Spectra_Meta_ALL_GOOD_DATA_TMA_NAs.RData", main = "Metadata: Str Wgt, Sex") 
	  
  

 # Tried
   Bias_Adj_Factor_Ages = c(10, 30:70), Bias_Reduction_Factor = 1.0, 
 
 
 # For debug and the figure below
 Graph_Metadata = c("structure_weight_g", "Length_cm", "Sex", "Month"); F_vonBert = list(Linf = 64, k = 0.32, t0 = 0); M_vonBert = list(Linf = 57, k = 0.41, t0 = 0); Multi_Year = TRUE; TMA_Ages_Only = FALSE; TMA_Ages = TRUE
 Metadata_Extra = c("structure_weight_g", "Length_cm", "Sex", "Month"); Metadata_Extra_File = "C:\\SIDT\\Sable_Comm\\SABL_Combo_Comm_2017__2024_Model_Spectra_Meta_ALL_GOOD_DATA_TMA_NAs.RData";  Meta_Data_Factors = c("Sex", "Month")
 
 
 
browsePlot('
               par(mfrow = c(2, 1))        
               for(Year in c("2023_Combo_survey", "2024_Combo_survey")) {
                    New_Ages_Year <- New_Ages_Good[New_Ages_Good$Year %in% Year, ]
                    gPlot(New_Ages_Year, "Pred_Age_Bias_Corr_plus_Delta_rounded", "Pred_Age_Bias_Corr_plus_Delta_rounded_Minus_TMA", ylab = "rnd(NN Pred Age + Delta)-TMA", xFunc = jitter, ylim = c(-xlim[2], xlim[2]), xlim = xlim,
                               xlab = "rnd(NN Pred Age + Delta) (jittered)", main = paste0(Year, ", Delta: ", Delta), grid = FALSE, vertLineEachPoint = TRUE, col = "#ffffff00")
                    points(jitter(New_Ages_Year$Pred_Age_Bias_Corr_plus_Delta_rounded[New_Ages_Year$Used_NN_Model]), New_Ages_Year$Pred_Age_Bias_Corr_plus_Delta_rounded_Minus_TMA[New_Ages_Year$Used_NN_Model])
                    if(any(!is.na(New_Ages_Year$Pred_Age_Bias_Corr_plus_Delta_rounded[!New_Ages_Year$Used_NN_Model])))
                       points(jitter(New_Ages_Year$Pred_Age_Bias_Corr_plus_Delta_rounded[!New_Ages_Year$Used_NN_Model]), New_Ages_Year$Pred_Age_Bias_Corr_plus_Delta_rounded_Minus_TMA[!New_Ages_Year$Used_NN_Model], col = "red", pch = ifelse(sum(!New_Ages_Year$Used_NN_Model) > 100, 1, 19))
                    lowess.line(New_Ages_Year$Pred_Age_Bias_Corr_plus_Delta_rounded, New_Ages_Year$Pred_Age_Bias_Corr_plus_Delta_rounded_Minus_TMA, smoothing.param = 0.05, col = "green")
                    abline(lsfit(New_Ages_Year$Pred_Age_Bias_Corr_plus_Delta_rounded, New_Ages_Year$Pred_Age_Bias_Corr_plus_Delta_rounded_Minus_TMA), col = "dodgerblue")
              }
') 


i <- "Length_cm"  
browsePlot('
  par(mfrow = c(2, 1))  
  xlim <- c(min(New_Ages_Good$NN_Pred_Median, na.rm = TRUE) - 1, max(New_Ages_Good$NN_Pred_Median, na.rm = TRUE) + 1)
 # if(is.numeric(New_Ages_Good[, i]))
 #   ylim <- c(min(New_Ages_Good[, i], na.rm = TRUE) - 0.2, max(New_Ages_Good[, i], na.rm = TRUE) + 0.2)  
  for(Year in c("2023_Combo_survey", "2024_Combo_survey")) {
        if(!all(is.na(New_Ages_Good[New_Ages_Good$Year %in% Year, i]))) {
          if(is.null(New_Ages_Good$Sex_F))
              New_Ages_Year <- New_Ages_Good[New_Ages_Good$Year %in% Year, c("NN_Pred_Median", "Used_NN_Model", "TMA", i)]
          else
              New_Ages_Year <- New_Ages_Good[New_Ages_Good$Year %in% Year, c("NN_Pred_Median", "Used_NN_Model", "Sex_F", "Sex_M", "TMA", i)] # Sex_U will be zeros for both Sex_F & Sex_M
          if(is.numeric(New_Ages_Year[, i]))  { 
             ylim <- c(min(New_Ages_Good[, i], na.rm = TRUE) - 0.1, max(New_Ages_Good[, i], na.rm = TRUE) + 0.1)                         
             gPlot(New_Ages_Year, "NN_Pred_Median", i, xlab = "NN Predicted Median (Purple) or TMA (cyan)", ylab = i, ylim = ylim , xlim = xlim, main = list(paste0(Year, ": ", i, " vs NN Pred"), cex = 0.95), Type = "n")                        
             if(any(!is.na(New_Ages_Year$NN_Pred_Median[!New_Ages_Year$Used_NN_Model])))                            
                 points(New_Ages_Year$NN_Pred_Median[!New_Ages_Year$Used_NN_Model], New_Ages_Year[!New_Ages_Year$Used_NN_Model, i], col = ifelse(is.na(New_Ages_Year$TMA[!New_Ages_Year$Used_NN_Model]), "red", "purple"),
                     pch = if(is.null(New_Ages_Year$Sex_F)) 19 else New_Ages_Year$Sex_F[!New_Ages_Year$Used_NN_Model] + New_Ages_Year$Sex_M[!New_Ages_Year$Used_NN_Model] * 4) # Sex_U will be zeros which are squares
             points(New_Ages_Year$TMA[!New_Ages_Year$Used_NN_Model], New_Ages_Year[!New_Ages_Year$Used_NN_Model, i], col = "cyan",
                 pch = if(is.null(New_Ages_Year$Sex_F)) 19 else New_Ages_Year$Sex_F[!New_Ages_Year$Used_NN_Model] + New_Ages_Year$Sex_M[!New_Ages_Year$Used_NN_Model] * 4)
             if(i == "Length_cm" & !is.null(F_vonBert)) {
               lines(sort(New_Ages_Year$NN_Pred_Median), F_vonBert$Linf  * (1 - exp(-F_vonBert$k * sort(New_Ages_Year$NN_Pred_Median) - F_vonBert$t0)), col = "green", lwd = 1.5)
               lines(sort(New_Ages_Year$NN_Pred_Median), M_vonBert$Linf  * (1 - exp(-M_vonBert$k * sort(New_Ages_Year$NN_Pred_Median) - M_vonBert$t0)), col = "dodgerblue", lwd = 1.5)
             }
          }
        } else {
          plot(0, 1, xlab = "", ylab = "", type = "n", xaxt = "n", yaxt = "n", bty = "n", main = list(Year, cex = 0.95))
          box()
          text(0, 1, paste0("No ", i, " Currently Available"))                           
        }                        
  }       
',  file = "C:/SIDT/Predict_NN_Ages/Predicted_Ages/Length_vs_NN_Pred_Ages_and_TMA.png")


# --- Fix up New_Ages for sharing ---
load("C:\\SIDT\\Sable_Combo_Comm\\Sable_Combo_Comm_2017__2022_2024_Str_Wgt_Sex\\Predicted_Ages_2015_2024_Bias_Corr\\NN Predicted Ages, 16 Apr 2025.RData")
New_Ages$Used_NN_Model[is.na(New_Ages$Used_NN_Model)] <- FALSE 
New_Ages$structure_weight_dg <- New_Ages$Index <- NULL
names(New_Ages)[grep('Year', names(New_Ages))] <- "Year_Project"
sum(Table(New_Ages$Year[New_Ages$Used_NN_Model], New_Ages$project[New_Ages$Used_NN_Model]))


[1] 6002    
               
                    CACOMM COMBO ORCOMM WA_OnBoardObserver WA_WhitingBycatchShoreside WACOMM
  2017_Combo_survey      0  1099      0                  0                          0      0
  2018_CA_Comm         101     0      0                  0                          0      0
  2019_Combo_survey      0   750      0                  0                          0      0
  2020_CA_Comm          24     0      0                  0                          0      0
  2020_Observer          0     0      0                314                          0      0
  2020_OR_Comm           0     0   1228                  0                          0      0
  2020_WA_Comm           0     0      0                  0                          1    206
  2021_Observer          0     0      0                161                          0      0
  2021_OR_Comm           0     0    165                  0                          0      0
  2021_WA_Comm           0     0      0                  0                         35     29
  2022_Combo_survey      0  1553      0                  0                          0      0
  2022_WA_Comm           0     0      0                  0                          0    336


 sum(Table(New_Ages$Year_Project, New_Ages$project))
                   
                    CACOMM COMBO ORCOMM WA_OnBoardObserver WA_WhitingBycatchShoreside WACOMM
  2015_Combo_survey      0  1064      0                  0                          0      0
  2016_Combo_survey      0   991      0                  0                          0      0
  2017_Combo_survey      0  1099      0                  0                          0      0
  2018_CA_Comm         197     0      0                  0                          0      0
  2018_Combo_survey      0  1322      0                  0                          0      0
  2019_Combo_survey      0   750      0                  0                          0      0
  2020_CA_Comm          24     0      0                  0                          0      0
  2020_Observer          0     0      0                314                          0      0
  2020_OR_Comm           0     0   1228                  0                          0      0
  2020_WA_Comm           0     0      0                  0                          1    206
  2021_CA_Comm          46     0      0                  0                          0      0
  2021_Combo_survey      0  2064      0                  0                          0      0
  2021_Observer          0     0      0                210                          0      0
  2021_OR_Comm           0     0    445                  0                          0      0
  2021_WA_Comm           0     0      0                  0                         35    104
  2022_CA_Comm         442     0      0                  0                          0      0
  2022_Combo_survey      0  1553      0                  0                          0      0
  2022_OR_Comm           0     0   3542                  0                          0      0
  2022_WA_Comm           0     0      0                  0                          0    887
  2023_Combo_survey      0  1698      0                  0                          0      0
  2023_OR_Comm           0     0    874                  0                          0      0
  2023_WA_Comm           0     0      0                  0                          0   1178
  2024_CA_Comm         631     0      0                  0                          0      0
  2024_Combo_survey      0  1181      0                  0                          0      0
  2024_OR_Comm           0     0    670                  0                          0      0
[1] 22756



headTail(New_Ages)
save(New_Ages, file = "C:\\SIDT\\Sable_Combo_Comm\\Sable_Combo_Comm_2017__2022_2024_Str_Wgt_Sex\\Predicted_Ages_2015_2024_Bias_Corr\\NN Predicted Ages, 16 Apr 2025.RData")

sum(Table(New_Ages$Year[!is.na(New_Ages$TMA)], New_Ages$project[!is.na(New_Ages$TMA)]))

	 
     

# ====== Remove all 2023 and 2024 prediction data since the NN model through 2022 does not predict those ages well =====

load("C:\\SIDT\\Sable_Combo_Comm\\SABL_Combo_Comm_2017__2024_Model_Spectra_Meta_ALL_GOOD_DATA_TMA_NAs.RData")
Table(Model_Spectra_Meta$sample_year)

Model_Spectra_Meta <- Model_Spectra_Meta[!Model_Spectra_Meta$sample_year %in% c("2023_Combo_survey", "2023_OR_Comm", "2023_WA_Comm", "2024_CA_Comm", "2024_Combo_survey", "2024_OR_Comm"), ]
Table(Model_Spectra_Meta$sample_year)
save(Model_Spectra_Meta, file = "C:\\SIDT\\Sable_Combo_Comm\\SABL_Combo_Comm_2017__2022_Model_Spectra_Meta_ALL_GOOD_DATA_TMA_NAs.RData")
  
  
 
 
# --- Bias correction with Kelli's 2016 von Bert with fixed T0 at zero thru 2022 ---
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/Sable_Combo_Comm/Sable_Combo_Comm_2017__2022_2024_Str_Wgt_Sex", Multi_Year = TRUE, Use_Session_Report_Meta = FALSE, 
   Bias_Adj_Factor_Ages = c(23, 30:70), Bias_Reduction_Factor = 0.8, Lowess_smooth_para = 2/3, TMA_Ages = TRUE, TMA_Ages_Only = FALSE, # TMA_Ages_Only = FALSE => Predictions without TMA
  # Metadata_Extra = c("Ship", "Cruise", "Haul", "Date_UTC", "Length_cm", "Sex", "Weight_kg", "Net_Partition"), 
  Metadata_Extra = c("structure_weight_g", "Length_cm", "Sex", "Month"), Meta_Data_Factors = c("Sex", "Month"), scanUniqueName = 'filenames', Debug_plotly.Spec = TRUE,
  Graph_Metadata = c("structure_weight_g", "Length_cm", "Sex", "Month"), F_vonBert = list(Linf = 64, k = 0.32, t0 = 0), M_vonBert = list(Linf = 57, k = 0.41, t0 = 0),
      Metadata_Extra_File = "C:\\SIDT\\Sable_Combo_Comm\\SABL_Combo_Comm_2017__2022_Model_Spectra_Meta_ALL_GOOD_DATA_TMA_NAs.RData", Delta_Given = list(-0.25, NULL)[[1]],
  Model_Spectra_Meta_Path = "C:\\SIDT\\Sable_Combo_Comm\\SABL_Combo_Comm_2017__2022_Model_Spectra_Meta_ALL_GOOD_DATA_TMA_NAs.RData", main = "Metadata: Str Wgt, Sex") 
	  
   
  
 # Tried
 1 Bias_Adj_Factor_Ages = c(7, 8:70), Bias_Reduction_Factor = 1.25 
 2 Bias_Adj_Factor_Ages = c(16, 28:65), Bias_Reduction_Factor = 1.20
 3 Bias_Adj_Factor_Ages = c(30, 30:65), Bias_Reduction_Factor = 1.20
 4 Bias_Adj_Factor_Ages = c(56, 30:65), Bias_Reduction_Factor = 1.25
 5 Bias_Adj_Factor_Ages = c(56, 30:70), Bias_Reduction_Factor = 1.225
 6 Bias_Adj_Factor_Ages = c(56, 30:70), Bias_Reduction_Factor = 1.15
 7 Bias_Adj_Factor_Ages = c(56, 30:70), Bias_Reduction_Factor = 1.0
 8 Bias_Adj_Factor_Ages = c(56, 30:70), Bias_Reduction_Factor = 0.5
 9 Bias_Adj_Factor_Ages = c(56, 30:70), Bias_Reduction_Factor = 0.5
10 Bias_Adj_Factor_Ages = c(23, 30:70), Bias_Reduction_Factor = 0.25  # forgot to increase Bias_Reduction_Factor
11 Bias_Adj_Factor_Ages = c(23, 30:70), Bias_Reduction_Factor = 1.0  # Slight overshot
12 Bias_Adj_Factor_Ages = c(23, 30:70), Bias_Reduction_Factor = 0.8   
13 Bias_Adj_Factor_Ages = c(7, 30:70), Bias_Reduction_Factor = 0.8 # bad curve    
14 Bias_Adj_Factor_Ages = c(20, 30:70), Bias_Reduction_Factor = 0.8 # very close to 12    
15 Bias_Adj_Factor_Ages = c(15, 30:70), Bias_Reduction_Factor = 0.8  # Too low on early TMA - Model 12 is better


# --- Fix up New_Ages for sharing ---
load("C:\\SIDT\\Sable_Combo_Comm\\Sable_Combo_Comm_2017__2022_2024_Str_Wgt_Sex\\Predicted_Ages_2015__2022_Bias_Corr\\NN Predicted Ages, 16 Apr 2025.RData")
New_Ages$Used_NN_Model[is.na(New_Ages$Used_NN_Model)] <- FALSE 
New_Ages$structure_weight_dg <- New_Ages$Index <- NULL
names(New_Ages)[grep('Year', names(New_Ages))] <- "Year_Project"
sum(Table(New_Ages$Year_Project, New_Ages$project))

     
                   CACOMM COMBO ORCOMM WA_OnBoardObserver WA_WhitingBycatchShoreside WACOMM
  2015_Combo_survey      0  1064      0                  0                          0      0
  2016_Combo_survey      0   991      0                  0                          0      0
  2017_Combo_survey      0  1099      0                  0                          0      0
  2018_CA_Comm         197     0      0                  0                          0      0
  2018_Combo_survey      0  1322      0                  0                          0      0
  2019_Combo_survey      0   750      0                  0                          0      0
  2020_CA_Comm          24     0      0                  0                          0      0
  2020_Observer          0     0      0                314                          0      0
  2020_OR_Comm           0     0   1228                  0                          0      0
  2020_WA_Comm           0     0      0                  0                          1    206
  2021_CA_Comm          46     0      0                  0                          0      0
  2021_Combo_survey      0  2064      0                  0                          0      0
  2021_Observer          0     0      0                210                          0      0
  2021_OR_Comm           0     0    445                  0                          0      0
  2021_WA_Comm           0     0      0                  0                         35    104
  2022_CA_Comm         442     0      0                  0                          0      0
  2022_Combo_survey      0  1553      0                  0                          0      0
  2022_OR_Comm           0     0   3542                  0                          0      0
  2022_WA_Comm           0     0      0                  0                          0    887
  
[1] 16524


headTail(New_Ages)
save(New_Ages, file = "C:\\SIDT\\Sable_Combo_Comm\\Sable_Combo_Comm_2017__2022_2024_Str_Wgt_Sex\\Predicted_Ages_2015__2022_Bias_Corr\\NN Predicted Ages, 16 Apr 2025.RData")

     
# --- No Bias correction with Kelli's 2016 von Bert with fixed T0 at zero thru 2022 ---
Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/Sable_Combo_Comm/Sable_Combo_Comm_2017__2022_2024_Str_Wgt_Sex", Multi_Year = TRUE, Use_Session_Report_Meta = FALSE, 
  Bias_Adj_Factor_Ages = NULL, Bias_Reduction_Factor = NULL, Lowess_smooth_para = 2/3, TMA_Ages = TRUE, TMA_Ages_Only = FALSE, # TMA_Ages_Only = FALSE => Predictions without TMA
  # Metadata_Extra = c("Ship", "Cruise", "Haul", "Date_UTC", "Length_cm", "Sex", "Weight_kg", "Net_Partition"), 
  Metadata_Extra = c("structure_weight_g", "Length_cm", "Sex", "Month"), Meta_Data_Factors = c("Sex", "Month"), scanUniqueName = 'filenames', Debug_plotly.Spec = TRUE,
  Graph_Metadata = c("structure_weight_g", "Length_cm", "Sex", "Month"), F_vonBert = list(Linf = 64, k = 0.32, t0 = 0), M_vonBert = list(Linf = 57, k = 0.41, t0 = 0),
      Metadata_Extra_File = "C:\\SIDT\\Sable_Combo_Comm\\SABL_Combo_Comm_2017__2022_Model_Spectra_Meta_ALL_GOOD_DATA_TMA_NAs.RData", Delta_Given = list(-0.15, NULL)[[1]],
  Model_Spectra_Meta_Path = "C:\\SIDT\\Sable_Combo_Comm\\SABL_Combo_Comm_2017__2022_Model_Spectra_Meta_ALL_GOOD_DATA_TMA_NAs.RData", main = "Metadata: Str Wgt, Sex") 
	  
   
# --- Fix up New_Ages for sharing ---
load("C:\\SIDT\\Sable_Combo_Comm\\Sable_Combo_Comm_2017__2022_2024_Str_Wgt_Sex\\Predicted_Ages_2015__2022\\NN Predicted Ages, 16 Apr 2025.RData")
New_Ages$Used_NN_Model[is.na(New_Ages$Used_NN_Model)] <- FALSE 
New_Ages$structure_weight_dg <- New_Ages$Index <- NULL
names(New_Ages)[grep('Year', names(New_Ages))] <- "Year_Project"
names(New_Ages)[grep('Pred_Age_Bias_Corr_plus_Delta_rounded', names(New_Ages))] <- "Pred_Age_Corr_plus_Delta_rounded"
names(New_Ages)[grep('Pred_Age_Bias_Corr_plus_Delta_rounded_Minus_TMA', names(New_Ages))] <- "Pred_Age_Corr_plus_Delta_rounded_Minus_TMA"
sum(Table(New_Ages$Year_Project, New_Ages$project))

                  
                    CACOMM COMBO ORCOMM WA_OnBoardObserver WA_WhitingBycatchShoreside WACOMM
  2015_Combo_survey      0  1064      0                  0                          0      0
  2016_Combo_survey      0   991      0                  0                          0      0
  2017_Combo_survey      0  1099      0                  0                          0      0
  2018_CA_Comm         197     0      0                  0                          0      0
  2018_Combo_survey      0  1322      0                  0                          0      0
  2019_Combo_survey      0   750      0                  0                          0      0
  2020_CA_Comm          24     0      0                  0                          0      0
  2020_Observer          0     0      0                314                          0      0
  2020_OR_Comm           0     0   1228                  0                          0      0
  2020_WA_Comm           0     0      0                  0                          1    206
  2021_CA_Comm          46     0      0                  0                          0      0
  2021_Combo_survey      0  2064      0                  0                          0      0
  2021_Observer          0     0      0                210                          0      0
  2021_OR_Comm           0     0    445                  0                          0      0
  2021_WA_Comm           0     0      0                  0                         35    104
  2022_CA_Comm         442     0      0                  0                          0      0
  2022_Combo_survey      0  1553      0                  0                          0      0
  2022_OR_Comm           0     0   3542                  0                          0      0
  2022_WA_Comm           0     0      0                  0                          0    887
  
[1] 16524





headTail(New_Ages)
save(New_Ages, file = "C:\\SIDT\\Sable_Combo_Comm\\Sable_Combo_Comm_2017__2022_2024_Str_Wgt_Sex\\Predicted_Ages_2015__2022\\NN Predicted Ages, 16 Apr 2025.RData")


     
     
     
     
     
     
     
     
     
     
     
     
     
	  