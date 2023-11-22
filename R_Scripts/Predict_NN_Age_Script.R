

###################################
#       Need >= R ver 3.0         #
###################################

# ------------------------------------ Main User Setup ------------------------------------------------------------

   if(interactive()) 
         setwd(ifelse(.Platform$OS.type == 'windows', "C:/ALL_USR/JRW/SIDT/Predict_NN_Ages", "/more_home/h_jwallace/SIDT/Predict_NN_Ages"))   # Change path to the Spectra Set's .GlobalEnv as needed
   Spectra_Set <- c("Hake_2019", "Sable_2017_2019", "Sable_2022")[2]
   TMA_Ages <- c(TRUE, FALSE)[2] # Are TMA ages available and are they to be used?
   verbose <- c(TRUE, FALSE)[1]
   plot <- c(TRUE, FALSE)[1]
   Max_N_Spectra <- list(50, 200, 'All')[[3]]  # Max number of new spectra to be plotted in the spectra figure. (All spectra in the 'New_Scans' folder will be assigned an age regardless of the number plotted in the figure.)
   spectraInterp = c('stats_splinefun_lowess', 'prospectr_resample')[1]
   opusReader = c('pierreroudier_opusreader', 'philippbaumann_opusreader2')[2]
   
   
   # (1) Hake 2019, BMS
   if(Spectra_Set == "Hake_2019") {
      NN_Model <- 'FCNN Model/Hake_2019_FCNN_20_Rdm_models_1_Apr_2023.RData'   # Change path to the Spectra Set's NN model as needed - 10-20 random models each with 10-fold complete 'k-fold' models.
      shortNameSegments <- c(2, 4) # Segments 2 and 4 of the spectra file name, e.g.: (PACIFIC, HAKE, BMS201906206C, 1191, OD1) => (HAKE, 1191)
      shortNameSuffix <- 'BMS'
      opusReader <- 'pierreroudier_opusreader'
      fineFreqAdj <- 150
   }
    
   # (2) Sablefish 2017 & 2019, Combo survey
   if(Spectra_Set == "Sable_2017_2019") { 
      NN_Model <- 'FCNN Model/Sablefish_2017_2019_Rdm_models_22_Mar_2023_14_57_26.RData'
      shortNameSegments <- c(1, 3) # Segments 1 and 3 of the spectra file name, e.g.: (SABLEFISH, COMBO201701203A, 28, OD1) => (SABLEFISH, 28)
      shortNameSuffix <- 'Year'
      yearPosition <- c(6, 9) # e.g. COMBO201701203A => 2017 (Segment used (see above) is: shortNameSegments[1] + 1)
      fineFreqAdj <- 0
      opusReader <- 'pierreroudier_opusreader'
      if(TMA_Ages)
        TMA_Meta <- "C:/ALL_USR/JRW/SIDT/Sablefish/Keras_CNN_Models/Sable_2017_2019 21 Nov 2022.RData"  # If used, change path to the main sepectra/metadata save()'d data frame which contains TMA ages.  Matching done via 'filenames'.
   }  
   
   # (3) Sablefish 2022, Combo survey
   if(Spectra_Set == "Sable_2022") { 
      NN_Model <- 'FCNN Model/Sablefish_2017_2019_Rdm_models_22_Mar_2023_14_57_26.RData'
      shortNameSegments <- c(1,5) # Segments 1 and 3 of the spectra file name, e.g.: (SABLEFISH, COMBO201701203A, 28, OD1) => (SABLEFISH, 28)
      shortNameSuffix <- 'Year'
      yearPosition <- c(6, 9) # e.g. COMBO201701203A => 2017 (Segment used (see above) is: shortNameSegments[1] + 1)
      fineFreqAdj <- 0
      if(TMA_Ages)
        TMA_Meta <- "C:/ALL_USR/JRW/SIDT/Sablefish/Keras_CNN_Models/Sable_2017_2019 21 Nov 2022.RData"  # If used, change path to the main sepectra/metadata save()'d data frame which contains TMA ages.  Matching done via 'filenames'.
   }  

      
   # You will need a 'GITHUB_PAT' from GitHub set somewhere in R (If you need help, search the Web how to get one from GitHub.)
   # Sys.setenv(GITHUB_PAT = "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")   # If you set GITHUB_PAT here, uncomment this line. Note, do not share your GITHUB_PAT, nor load it onto GitHib.
   # Sys.getenv("GITHUB_PAT") 
    
   #  --- Conda TensorFlow environment ---
   Conda_TF_Eniv <- ifelse(.Platform$OS.type == 'windows', "C:/m3/envs/tf", "/more_home/h_jwallace/Python/tf_cpu_only/bin")  # Change this path as needed
   Sys.setenv(RETICULATE_PYTHON = Conda_TF_Eniv) 
   Sys.getenv("RETICULATE_PYTHON") 
   
  #  ---- Note if you get this error: < Error in `[.data.frame`(data.frame(prospectr::savitzkyGolay(newScans.RAW, : undefined columns selected > or you know that 
  #         the new spectra scan(s) do not have the same freq. as the model expects, then add the file 'FCNN\AAA_********_Correct_Scan_Freq' to your scans and an interpolation will be done. ---
  #         '********' is based on the spectra set you are currently working with, e.g. AAA_PACIFIC_HAKE_2019_Correct_Scan_Freq. If you add this file, the first NN age reported will be from this file, and can be ignored/removed.
  #         If the batch run is crashing, you can first try to adding this file to your scans to see if that fixes the issue.
 
# -----------------------------------------------------------------------------------------------------------------
 
 
if(!interactive())
   options(width = 120)

# --- Put new spectra scans in a separate folder and enter the name of the folder below ---
Spectra_Path <- "New_Scans"    
 
# --- The NN predicted ages will go in the path defined below ---
Predicted_Ages_Path <- "Predicted_Ages"
dir.create(Predicted_Ages_Path, showWarnings = FALSE)


#  ----------------- Packages ------------------------
if (!any(installed.packages()[, 1] %in% "R.utils")) 
     install.packages("R.utils") 

if (!any(installed.packages()[, 1] %in% "ggplot2")) 
     install.packages("ggplot2") 

if (!any(installed.packages()[, 1] %in% "plotly")) 
     install.packages("plotly")      
     
if (!any(installed.packages()[, 1] %in% "tensorflow")) 
     install.packages("tensorflow")
     
if (!any(installed.packages()[, 1] %in% "keras")) 
     install.packages("keras") 

library(R.utils)     
library(ggplot2)
library(plotly)        
library(tensorflow)
library(keras)  

# --- Download functions from GitHub ---
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

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Date.R") 
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/sort.f.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R") 
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/extractRData.R")  
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/saveHtmlFolder.R")

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Predict_NN_Age.R")


# --- TensorFlow Load and Math Check  ---
a <- tf$Variable(5.56)
cat("\n\nTensorFlow Math Check\n\na = "); print(a)
b <- tf$Variable(2.7)
cat("\nb = "); print(b)
cat("\na + b = "); print(a + b)
cat("\n\n")

k_clear_session() 


# ============= Pause here when interactively submitting code to R =================

# --- Use Predict_NN_Age() to find the NN predicted ages ---  
(fileNames <- dir(path = Spectra_Path))[1:10]
if(exists('shortNameSuffix') && shortNameSuffix == 'Year')
   shortNameSuffix. <- apply(matrix(fileNames, ncol = 1), 1, function(x) substr(get.subs(x, sep = "_")[shortNameSegments[1] + 1], yearPosition[1], yearPosition[2]))

if(exists('shortNameSuffix') && shortNameSuffix != 'Year')
   shortNameSuffix. <- shortNameSuffix
   
if(!exists('shortNameSuffix'))
    shortNameSuffix. <- NULL

##### This is the main call to Predict_NN_Age() #####
# New_Ages <- Predict_NN_Age(Conda_TF_Eniv, Spectra_Path, NN_Model, plot = plot, NumRdmModels = 1,  htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure for New Ages'), spectraInterp = spectraInterp, fineFreqAdj = fineFreqAdj,
#      Predicted_Ages_Path = Predicted_Ages_Path,  shortNameSegments = shortNameSegments, shortNameSuffix = shortNameSuffix., N_Samp = min(c(length(fileNames), Max_N_Spectra)), verbose = verbose) # One random model for faster testing

New_Ages <- Predict_NN_Age(Conda_TF_Eniv, Spectra_Path, NN_Model, plot = plot, htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure for New Ages'), spectraInterp = spectraInterp, fineFreqAdj = fineFreqAdj, opusReader = opusReader, 
   Predicted_Ages_Path = Predicted_Ages_Path,  shortNameSegments = shortNameSegments, shortNameSuffix = shortNameSuffix., N_Samp = min(c(length(fileNames), Max_N_Spectra)), verbose = verbose) # Use the max number of random model replicates available
if(verbose) head(New_Ages, 20)

# For testing Predict_NN_Age(): plot = TRUE; NumRdmModels = 1;  htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure for New Ages'); N_Samp = min(c(length(fileNames), Max_N_Spectra))                                      
      

# --- Save() ages and write out to a CSV file ---
save(New_Ages, file = paste0(Predicted_Ages_Path, '/NN Predicted Ages, ', Date(" "), '.RData'))
write.csv(New_Ages, file = paste0(Predicted_Ages_Path, '/NN Predicted Ages, ', Date(" "), '.csv'), row.names = FALSE)


# --- Create plots with age estimates and quantile credible intervals ---
New_Ages <- data.frame(Index = 1:nrow(New_Ages), New_Ages)  # Add 'Index' as the first column in the data frame
print(New_Ages[1:5, ])

Delta <- extractRData('roundingDelta', file = NN_Model)  # e.g. the rounding Delta for 2019 Hake is zero.  
New_Ages$Age_Rounded <- round(New_Ages$NN_Pred_Median + Delta)
New_Ages$Rounded_Age <- factor(" ") # This is needed for ggplotly plotting below

cat(paste0("\n\nUsing a rounding Delta of ", Delta, "\n\n"))

# - Plot by order implied by the spectra file names -
g <- ggplotly(ggplot(New_Ages, aes(Index, NN_Pred_Median)) +  
geom_point() +
geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
geom_point(aes(Index, Age_Rounded, color = Rounded_Age)) + scale_color_manual(values = c(" " = "green")), dynamicTicks = TRUE)
print(g)
saveHtmlFolder(paste0(Predicted_Ages_Path, '/Predicted_Ages_Order_by_File_Names'), view = !interactive())
if(!interactive()) Sys.sleep(3)

     
# - Plot by sorted NN predicted ages -
New_Ages_Sorted <- sort.f(New_Ages, 'NN_Pred_Median') # Sort 'New_ages' by 'NN_Pred_Median', except for "Index" (see the next line below)
New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  # Reset Index for graphing

print(New_Ages_Sorted[1:5, ])
if(verbose) head(New_Ages_Sorted, 20)

g <- ggplotly(ggplot(New_Ages_Sorted, aes(Index, NN_Pred_Median)) +  
geom_point() +
geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
geom_point(aes(Index, Age_Rounded, color = Rounded_Age)) + scale_color_manual(values = c(" " = "green")), dynamicTicks = TRUE)
print(g)
saveHtmlFolder(paste0(Predicted_Ages_Path, '/Predicted_Ages_Sorted'), view = !interactive())



# --- Check against TMA ages, if available ---

if(TMA_Ages) {

   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/load.R") 
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/match.f.R") 
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/agreementFigure.R")

   load(TMA_Meta, str = verbose) # This is JRWToolBox::load() not base::load()
   Delta <- extractRData('roundingDelta', file = NN_Model) 
   
   New_Ages$Age_Rounded <- round(New_Ages$NN_Pred_Median + Delta)
   New_Ages$Rounded_Age <- factor(" ")
   New_Ages$TMA <- NULL # Clear old TMA before updating
   New_Ages <- match.f(New_Ages, Sable_2017_2019, 'filenames', 'filenames', 'TMA')   # Change as needed
   
   if(verbose & !interactive())  Sys.sleep(3)
   g <- ggplotly(ggplot(New_Ages, aes(TMA, NN_Pred_Median)) +  
   geom_point() +
   geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
   geom_point(aes(TMA, Age_Rounded, color = Rounded_Age)) + scale_color_manual(values = c(" " = "green")), dynamicTicks = TRUE)
   print(g)
   saveHtmlFolder(paste0(Predicted_Ages_Path, '/TMA vs Predicted_Ages'), view = !interactive())
     
   #  pdf(width = 16, height = 10, file = paste0(Predicted_Ages_Path, '/Agreement_Figure.png'))
   png(width = 16, height = 10, units = 'in', res = 600, file = paste0(Predicted_Ages_Path, '/Agreement_Figure.png'))
   
   agreementFigure(New_Ages$TMA, New_Ages$NN_Pred_Median, Delta = Delta, full = TRUE)
   
   dev.off()
   browseURL(paste0(getwd(), "/", Predicted_Ages_Path, '/Agreement_Figure.png'), browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
    if(verbose & !interactive())  Sys.sleep(5)
}   

#   # --- Find bad scans ---
#   for ( i in fileNames)  {
#      print(i)
#      try(newScans.RAW <- opusreader::opus_read(paste(Spectra_Path, i , sep = "/"), simplify = TRUE, wns_digits = 0)[[2]] )
#   }
#   
#   
#   # Bad scans for Sablefish 2017: 505, 506, 507, 509, 511-514, 516-518, 520-522, 525, 527, 529-533, 535-538




# --------------------------- Sable 2022 results ---------------------------------------

#  
#  1      AAA_SABLEFISH_2017_2019_Correct_Scan_Freq        17.6464              13.9933              21.5621
#  2   SABL_COMBO2022_NIR0022A_PRD_1_102157421_O1.0         3.6336               0.0000              61.4710
#  3  SABL_COMBO2022_NIR0022A_PRD_10_102157430_O1.0         0.7022               0.0000              49.3235
#  4  SABL_COMBO2022_NIR0022A_PRD_11_102157431_O1.0         2.0845               0.0000              69.9334
#  5  SABL_COMBO2022_NIR0022A_PRD_12_102157432_O1.0         0.0000               0.0000              41.8585
#  6  SABL_COMBO2022_NIR0022A_PRD_13_102157433_O1.0         3.1252               0.0000              71.0005
#  7  SABL_COMBO2022_NIR0022A_PRD_14_102157434_O1.0         0.0949               0.0000              63.9751
#  8  SABL_COMBO2022_NIR0022A_PRD_16_102157436_O1.0        15.7131               0.0000             124.0428
#  9  SABL_COMBO2022_NIR0022A_PRD_18_102157438_O1.0         0.0000               0.0000              50.6900
#  10 SABL_COMBO2022_NIR0022A_PRD_19_102157439_O1.0         0.0000               0.0000              45.5209
#  11  SABL_COMBO2022_NIR0022A_PRD_2_102157422_O1.0         2.2233               0.0000              79.3303
#  12 SABL_COMBO2022_NIR0022A_PRD_20_102157440_O1.0         0.0000               0.0000              41.8450
#  13 SABL_COMBO2022_NIR0022A_PRD_22_102157442_O1.0         0.0000               0.0000              53.9383
#  14 SABL_COMBO2022_NIR0022A_PRD_23_102157443_O1.0         0.0000               0.0000              58.7917
#  15  SABL_COMBO2022_NIR0022A_PRD_3_102157423_O1.0        24.3620               0.0000             157.7207
#  16  SABL_COMBO2022_NIR0022A_PRD_4_102157424_O1.0        14.2413               0.0000             127.3682
#  17  SABL_COMBO2022_NIR0022A_PRD_5_102157425_O1.0        18.6732               0.0000             154.2573
#  18  SABL_COMBO2022_NIR0022A_PRD_8_102157428_O1.0         8.9960               0.0000             111.4452
#  19  SABL_COMBO2022_NIR0022A_PRD_9_102157429_O1.0         0.0000               0.0000              38.5855
#  
#                                         filenames NN_Pred_Median Lower_Quantile_0.025 Upper_Quantile_0.975
#  1      AAA_SABLEFISH_2017_2019_Correct_Scan_Freq        17.2298              14.3179              19.7127
#  2   SABL_COMBO2022_NIR0022A_PRD_1_102157421_O1.0        20.9187               2.1042              64.0652
#  3  SABL_COMBO2022_NIR0022A_PRD_10_102157430_O1.0        12.4935               1.0803              51.6040
#  4  SABL_COMBO2022_NIR0022A_PRD_11_102157431_O1.0        18.9714               1.8046              73.5381
#  5  SABL_COMBO2022_NIR0022A_PRD_12_102157432_O1.0         6.1469               0.0000              44.4637
#  6  SABL_COMBO2022_NIR0022A_PRD_13_102157433_O1.0        20.9600               2.0852              74.9418
#  7  SABL_COMBO2022_NIR0022A_PRD_14_102157434_O1.0        14.0958               1.4431              66.4475
#  8  SABL_COMBO2022_NIR0022A_PRD_16_102157436_O1.0        46.2707               5.9626             130.9777
#  9  SABL_COMBO2022_NIR0022A_PRD_18_102157438_O1.0         9.6152               0.9829              52.7414
#  10 SABL_COMBO2022_NIR0022A_PRD_19_102157439_O1.0         7.8744               0.4882              39.0371
#  11  SABL_COMBO2022_NIR0022A_PRD_2_102157422_O1.0        22.0301               2.1802              82.2269
#  12 SABL_COMBO2022_NIR0022A_PRD_20_102157440_O1.0         6.9186               0.0000              42.4356
#  13 SABL_COMBO2022_NIR0022A_PRD_22_102157442_O1.0        11.2753               1.1978              55.9882
#  14 SABL_COMBO2022_NIR0022A_PRD_23_102157443_O1.0        10.8651               0.0000              62.8732
#  15  SABL_COMBO2022_NIR0022A_PRD_3_102157423_O1.0        71.6705              16.5027             149.7824
#  16  SABL_COMBO2022_NIR0022A_PRD_4_102157424_O1.0        45.7589               6.2917             139.8399
#  17  SABL_COMBO2022_NIR0022A_PRD_5_102157425_O1.0        62.1559               9.1665             154.8167
#  18  SABL_COMBO2022_NIR0022A_PRD_8_102157428_O1.0        39.7262               4.0958             115.5056
#  19  SABL_COMBO2022_NIR0022A_PRD_9_102157429_O1.0         6.5939               0.2451              39.2401


