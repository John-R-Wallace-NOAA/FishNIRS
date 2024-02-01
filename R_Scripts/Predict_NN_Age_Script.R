

###################################
#       Need >= R ver 3.0         #
###################################

# ------------------------------------ Main User Setup ------------------------------------------------------------

 if(interactive()) {
       setwd(ifelse(.Platform$OS.type == 'windows', "C:/ALL_USR/JRW/SIDT/Predict_NN_Ages", "/more_home/h_jwallace/SIDT/Predict_NN_Ages"))   # Change path to the Spectra Set's .GlobalEnv as needed
 getwd()
 }		 
 if(!interactive())   
       options(width = 120)      
       
Spectra_Set <- c("Hake_2019", "Sable_2017_2019", "Sable_Combo_2022")[3]
Spectra_Path <- "New_Scans"    # Put new spectra scans in a separate folder and enter the name of the folder below
Predicted_Ages_Path <- "Predicted_Ages" # The NN predicted ages will go in the path defined below
dir.create(Predicted_Ages_Path, showWarnings = FALSE)
metadata <- c(TRUE, FALSE)[1] # Will metadata be used
TMA_Ages <- c(TRUE, FALSE)[1] # Are TMA ages available and are they to be used?
verbose <- c(TRUE, FALSE)[1]
plot <- c(TRUE, FALSE)[1]
# Default number of new spectra to be plotted in spectra figures. (The plot within Read_OPUS_Spectra() is given a different default below). 
# All spectra in the Spectra_Path folder will be assigned an age regardless of the number plotted in the figure.
Max_N_Spectra <- list(50, 200, 'All')[[2]] 

   
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

library(lattice)
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

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/load.R") 
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Date.R") 
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/gPlot.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/sort.f.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R") 
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/extractRData.R")  
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/saveHtmlFolder.R")

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly_spectra.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Read_OPUS_Spectra.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Predict_NN_Age.R")


# -----------------------------------------------------------------------------------------------------------------   
   
   
   
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
   Model_Spectra_Meta <- "C:/ALL_USR/JRW/SIDT/Sablefish/Keras_CNN_Models/Sable_2017_2019 21 Nov 2022.RData"  # If used, change path to the main sepectra/metadata save()'d data frame which contains TMA ages.  Matching done via 'filenames'.
}  

# (3) Sablefish 2022, Combo survey
if(Spectra_Set == "Sable_Combo_2022") { 
   NN_Model <- 'FCNN Model/Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_15_Dec_2023_09_54_18.RData'
}  

   
# You will need a 'GITHUB_PAT' from GitHub set somewhere in R (If you need help, search the Web how to get one from GitHub.)
# Sys.setenv(GITHUB_PAT = "**************")   # If you set GITHUB_PAT here, uncomment this line. Note, do not share your GITHUB_PAT, nor load it onto GitHib.
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

    
N_Samp <- ifelse(is.numeric(Max_N_Spectra), min(c(length(fileNames), Max_N_Spectra)), 'All')

Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set, Spectra_Path = Spectra_Path, TMA_Ages = TRUE, Max_N_Spectra = 300, verbose = verbose, 
                               plot = plot, htmlPlotFolder = paste0('Predicted_Ages/', Spectra_Set, '_Spectra_Sample_of_300'))

##### This is the main call to Predict_NN_Age() #####
# New_Ages <- Predict_NN_Age(Conda_TF_Eniv, Spectra_Path, NN_Model, plot = plot, NumRdmModels = 1,  htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure for New Ages'), spectraInterp = spectraInterp, fineFreqAdj = fineFreqAdj,
#      Predicted_Ages_Path = Predicted_Ages_Path,  shortNameSegments = shortNameSegments, shortNameSuffix = shortNameSuffix., N_Samp = N_Samp, verbose = verbose) # One random model for faster testing

# New_Ages <- Predict_NN_Age(Conda_TF_Eniv, Spectra_Path, Model_Spectra_Meta, NN_Model, plot = plot, htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure for New Ages'), spectraInterp = spectraInterp, fineFreqAdj = fineFreqAdj, opusReader = opusReader, 
#    Predicted_Ages_Path = Predicted_Ages_Path,  shortNameSegments = shortNameSegments, shortNameSuffix = shortNameSuffix., N_Samp = N_Samp, verbose = verbose) # Use the max number of random model replicates available

New_Ages <- Predict_NN_Age(Conda_TF_Eniv, Spectra_Path, Model_Spectra_Meta, NN_Model, plot = plot, htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure for New Ages'),  
                                    Predicted_Ages_Path = Predicted_Ages_Path, N_Samp = N_Samp, verbose = verbose) # Use the max number of random model replicates available
head(New_Ages, 5)

# For testing Predict_NN_Age(): plot = TRUE; NumRdmModels = c(1, 20)[2];  htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure for New Ages'); N_Samp = N_Samp                                    
      

# --- save() ages and write out to a CSV file ---
save(New_Ages, file = paste0(Predicted_Ages_Path, '/NN Predicted Ages, ', Date(" "), '.RData'))
write.csv(New_Ages, file = paste0(Predicted_Ages_Path, '/NN Predicted Ages, ', Date(" "), '.csv'), row.names = FALSE)


# ----- Create plots with age estimates and quantile credible intervals -----

New_Ages <- data.frame(Index = 1:nrow(New_Ages), New_Ages)  # Add 'Index' as the first column in the data frame
New_Ages[1:5, ]

Delta <- extractRData('roundingDelta', file = NN_Model) # e.g. the rounding Delta for 2019 Hake is zero.  
New_Ages$Age_Rounded <- round(New_Ages$NN_Pred_Median + Delta)
cat(paste0("\n\nUsing a rounding Delta of ", Delta, "\n\n"))


# --- If TMA ages are not available ---
if(!TMA_Ages) {

   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/browsePlot.R") 

   cols <- 'green'
    
   # -- Plot by order implied by the spectra file names --
   g <- ggplot(New_Ages, aes(Index, NN_Pred_Median)) +  
   geom_point() +
   geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
   geom_point(aes(Index, Age_Rounded, col = cols)) + 
   scale_color_manual(labels = 'Rounded Age', values = cols, name = ' ')
   browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/Predicted_Ages_Order_by_File_Names.png'))
    
   
   # -- Plot by sorted NN predicted ages --
   New_Ages_Sorted <- sort.f(New_Ages, 'NN_Pred_Median') # Sort 'New_ages' by 'NN_Pred_Median', except for "Index" (see the next line below)
   New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  # Reset Index for graphing
   if(verbose) head(New_Ages_Sorted, 10)
   
   g <- ggplot(New_Ages_Sorted, aes(Index, NN_Pred_Median)) +  
   geom_point() +
   geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
   geom_point(aes(Index, Age_Rounded, col = cols)) + 
   scale_color_manual(labels = 'Rounded Age', values = cols, name = ' ')
   browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/Predicted_Ages_Sorted.png'))
}


# --- Use TMA ages, if available ---
if(TMA_Ages) {

   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/match.f.R") 
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/browsePlot.R") 
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/agreementFigure.R")

   New_Ages$TMA <- NULL # Clear old TMA before updating
   if(length(get.subs(get.subs(New_Ages$filenames[1], sep = "."))) == 2)
       New_Ages$filenames <- get.subs(New_Ages$filenames, sep = ".")[1,]
   New_Ages <- match.f(New_Ages, Model_Spectra_Meta, 'filenames', 'filenames', 'TMA')  
   
   # -- Spectra Figure with TMA for New Ages --
   plotly.Spec(Model_Spectra_Meta, N_Samp = N_Samp, htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure with TMA for New Ages'))
   
   # -- Agreement Figures --
   # browsePlot('agreementFigure(New_Ages$TMA, New_Ages$NN_Pred_Median, Delta = Delta, full = TRUE)', file = paste0(Predicted_Ages_Path, '/Agreement_Figure.pdf'), pdf = TRUE)   
   browsePlot('agreementFigure(New_Ages$TMA, New_Ages$NN_Pred_Median, Delta = Delta, full = TRUE)', file = paste0(Predicted_Ages_Path, '/Agreement_Figure.png'))
   browsePlot('agreementFigure(New_Ages$TMA, New_Ages$NN_Pred_Median, Delta = Delta, full = FALSE)', file = paste0(Predicted_Ages_Path, '/Agreement_Figure_Zoomed.png'))
   if(verbose & !interactive())  Sys.sleep(5)
  
   cols <- c('green', 'red')
   pchs <- c(16, 1)
   
   # -- Plot by order implied by the spectra file names - ggplotly() changes how scale_color_manual() works ????????????????? --
   g <- ggplot(New_Ages, aes(Index, NN_Pred_Median)) +  
   geom_point() +
   geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
   geom_point(aes(Index, Age_Rounded, col = cols[1])) + 
   geom_point(aes(Index + 0.1, TMA, col = cols[2])) + 
   scale_color_manual(labels = c('Rounded Age', 'TMA'), values = cols, name = ' ')
   browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/Predicted_Ages_Order_by_File_Names.png'), width = 18, height = 10,)
   
   # New_Ages$Rounded_Age <- factor(" ") # This is needed for ggplotly plotting below
   # g <- ggplotly(ggplot(New_Ages, aes(TMA, NN_Pred_Median)) +  
   # geom_point() +
   # geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
   # geom_point(aes(TMA, Age_Rounded, color = Rounded_Age)) + scale_color_manual(values = c(" " = "green")), dynamicTicks = TRUE)
   # print(g)
   # unlink(paste0(Predicted_Ages_Path, '/Predicted_Ages_Order_by_File_Names'), recursive = TRUE)
   # saveHtmlFolder(paste0(Predicted_Ages_Path, '/TMA vs Predicted_Ages'), view = !interactive())
   
   
   # -- Plot by sorted NN predicted ages --
   New_Ages_Sorted <- sort.f(New_Ages, 'NN_Pred_Median') # Sort 'New_ages' by 'NN_Pred_Median', except for "Index" (see the next line below)
      #  New_Ages_Sorted <- sort.f(New_Ages[sample(1:nrow(New_Ages_Sorted), 200), ], 'NN_Pred_Median') 
      #  New_Ages_Sorted <- New_Ages_Sorted[New_Ages_Sorted$NN_Pred_Median <= 10, ]
   New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  # Reset Index for graphing
   if(verbose) head(New_Ages_Sorted, 20)
   g <- ggplot(New_Ages_Sorted, aes(Index, NN_Pred_Median)) + 
   geom_point() +
   geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
   geom_point(aes(Index, Age_Rounded, col = cols[1]), pch = pchs[1]) + 
   geom_point(aes(Index, TMA, col = cols[2]), pch = pchs[2]) +  
   scale_color_manual(labels = c('Rounded Age', 'TMA'), values = cols, name = ' ')  
   browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/Predicted_Ages_Sorted.png'))
   
   # https://r-graphics.org/recipe-scatter-shapes   
   # scale_color_manual(labels = c('Rounded Age', 'TMA'), values = list(colour = cols, pch = pchs), aesthetics = c('colour', 'shape'), name = ' ') 
   # scale_shape_manual(values = pchs)
   # scale_fill_manual(values = c(cols[1], NA), guide = guide_legend(override.aes = list(shape = pchs[2])))
   # guides(fill=guide_legend(override.aes=list(shape=16))) +
   # scale_shape_manual(values = pchs, guide = guide_legend(override.aes = list(alpha = 1, size = 10)))
  
   
   # -- Plot SUBSET of data by sorted NN predicted ages --
   set.seed(707)
   New_Ages_Sorted <- sort.f(New_Ages[sample(1:nrow(New_Ages), 150), ], 'NN_Pred_Median') 
   New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  # Reset Index for graphing
   if(verbose) head(New_Ages_Sorted, 20)
   g <- ggplot(New_Ages_Sorted, aes(Index, NN_Pred_Median)) + 
   geom_point() +
   geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
   geom_point(aes(Index, Age_Rounded, col = cols[1]), pch = pchs[1]) + 
   geom_point(aes(Index, TMA, col = cols[2]), pch = pchs[2]) +  
   scale_color_manual(labels = c('Rounded Age', 'TMA'), values = cols, name = ' ') 
   browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/Predicted_Ages_Sorted_Subset.png'))
  
  
   # # -- Plot of relative error by sorted NN predicted ages --
   # New_Ages_Sorted <- sort.f(New_Ages, 'NN_Pred_Median') 
   # 
   # New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  # Reset Index for graphing
   # if(verbose) head(New_Ages_Sorted, 20)
   # 
   # g <- ggplot(New_Ages_Sorted, aes(Index, abs(TMA - NN_Pred_Median)/TMA)) + 
   # geom_point(col = as.character(New_Ages_Sorted$TMA))
   # # ylim(0, 6)
   # 
   # browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/Predicted_Ages_Sorted_Subset.png'))
   # rm(g)
   
   # # -- Plot of relative error by sorted TMA age --
   # New_Ages_Sorted <- sort.f(New_Ages, 'TMA')
   # New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  # Reset Index for graphing
   # if(verbose) head(New_Ages_Sorted, 20)
   # 
   # g <- ggplot(New_Ages_Sorted, aes(Index, abs(TMA - NN_Pred_Median)/ifelse(TMA == 0, 1, TMA))) + 
   # geom_point(col = as.character(New_Ages_Sorted$TMA)) 
   # # ylim(0, 6)
   # 
   # g <- xyplot(abs(TMA - NN_Pred_Median)/ifelse(TMA == 0, 1, TMA) ~ Index, group = TMA, data = New_Ages_Sorted)
   # 
   # browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/Relative_absolute_error_by_Index.png'))
   # rm(g)
   # 
   # # -- Plot of relative error by sorted TMA age --
   # New_Ages_Sorted <- sort.f(New_Ages, 'TMA')
   # New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  # Reset Index for graphing
   # if(verbose) head(New_Ages_Sorted, 20)
   # 
   # g <- ggplot(New_Ages_Sorted, aes(TMA, abs(TMA - NN_Pred_Median)/ifelse(TMA == 0, 1, TMA))) + 
   # geom_point(col = as.character(New_Ages_Sorted$TMA))
   # # ylim(0, 6)
   # 
   # browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/Relative_absolute_error_by_sorted_TMA_age.png'))
   # rm(g)
   
   
   # -- Relative absolute error by TMA age --
   g <- xyplot(abs(TMA - NN_Pred_Median)/ifelse(TMA == 0, 1, TMA) ~ TMA, group = TMA, data = New_Ages)
   browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/Relative_absolute_error_by_TMA_age.png'))
   
   # -- Plot of relative error by sorted TMA age --   
   New_Ages_Sorted <- sort.f(New_Ages, 'TMA')
   New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  # Reset Index for graphing
   if(verbose) head(New_Ages_Sorted, 20)
   g <- xyplot(abs(TMA - NN_Pred_Median)/ifelse(TMA == 0, 1, TMA) ~ Index, group = TMA, data = New_Ages_Sorted)
   browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/Relative_absolute_error_by_sorted_TMA.png'))
   
   # -- Plot by sorted TMA --
   New_Ages_Sorted <- sort.f(New_Ages, 'TMA') # Sort 'New_ages' by 'NN_Pred_Median', except for "Index" (see the next line below)
   New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  # Reset Index for graphing
   New_Ages_Sorted <- na.omit(New_Ages_Sorted)
   if(verbose) head(New_Ages_Sorted, 20)
   g <- ggplot(New_Ages_Sorted, aes(Index, NN_Pred_Median)) +  
   geom_point() +
   geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
   geom_point(aes(Index, TMA, col = cols[2])) + 
   geom_point(aes(Index, Age_Rounded, col = cols[1])) + 
   scale_color_manual(labels = c('Rounded Age', 'TMA'), values = cols, name = ' ')
   browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/TMA_Sorted.png'))
   
   
   # -- Plot SUBSET of data by sorted by TMA --
   set.seed(707)
   New_Ages_Sorted <- sort.f(New_Ages[sample(1:nrow(New_Ages), 150), ], 'TMA') # Sort 'New_ages' by TMA, except for "Index" (see the next line below)
   New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  # Reset Index for graphing
   New_Ages_Sorted <- na.omit(New_Ages_Sorted)
   if(verbose) head(New_Ages_Sorted, 20)
   g <- ggplot(New_Ages_Sorted, aes(Index, NN_Pred_Median)) +  
   geom_point() +
   geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
   geom_point(aes(Index, TMA, col = cols[2])) + 
   geom_point(aes(Index, Age_Rounded, col = cols[1])) + 
   scale_color_manual(labels = c('Rounded Age', 'TMA'), values = cols, name = ' ')
   browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/TMA_Sortedd_Subset.png'))


   # -- Plot ALL THE DATA by sorted difference --
   # Back to all data; round(NN_Pred_Median + Delta); jitter TMA; vertical line for each unique TMA - without standard grid            
   xlim <- c(min(New_Ages$TMA) - 1.25, max(New_Ages$TMA) + 1.25)   
   New_Ages$TMA_Minus_Age_Rounded <- New_Ages$TMA - New_Ages$Age_Rounded
   browsePlot('set.seed(707); gPlot(New_Ages, "TMA", "TMA_Minus_Age_Rounded", ylab = "TMA - Age_Rounded", xFunc = jitter, ylim = c(-xlim[2], xlim[2]), xlim = xlim,
                  grid = FALSE, vertLineEachPoint = TRUE)', file = paste0(Predicted_Ages_Path, '/TMA_minus_NN_Age_Rounded_vs_TMA_Jittered.png')) 
				  
				  
   # -- Plot ALL THE DATA by sorted difference, highlighting those oties that were left out of the NN model--
   Sable_Combo_2022_NN_Pred_Median_TMA <- extractRData('Sable_Combo_2022_NN_Pred_Median_TMA', "C:/ALL_USR/JRW/SIDT/Sablefish 2022 Combo/Sable_Combo_2022_NN_Fish_Len_Otie_Wgt/Sable_Combo_2022_FCNN_model_ver_1_20_Pred_Median_TMA_15_Dec_2023_12_23_01.RData")
   dim(Sable_Combo_2022_NN_Pred_Median_TMA)
   # [1] 1513    5
         
   New_Ages_Good <- New_Ages[!is.na(New_Ages$NN_Pred_Median), ]
   dim(New_Ages_Good)
   # [1] 1528    4
   
   Sable_Combo_2022_NN_Pred_Median_TMA$Used_NN_Model <- TRUE
   New_Ages_Good <- match.f(New_Ages_Good, Sable_Combo_2022_NN_Pred_Median_TMA, 'filenames', 'filenames', 'Used_NN_Model')
   New_Ages_Good$Used_NN_Model[is.na(New_Ages_Good$Used_NN_Model)] <- FALSE 
   Table(New_Ages_Good$Used_NN_Model)
   # FALSE  TRUE 
   #    15  1513   # 15 Oties left out of the NN model
     
   xlim <- c(min(New_Ages_Good$TMA) - 1.25, max(New_Ages_Good$TMA) + 1.25)   
   New_Ages_Good$TMA_Minus_Age_Rounded <- New_Ages_Good$TMA - New_Ages_Good$Age_Rounded
   browsePlot('
       set.seed(707)
       gPlot(New_Ages_Good, "TMA", "TMA_Minus_Age_Rounded", ylab = "TMA - Age_Rounded", xFunc = jitter, ylim = c(-xlim[2], xlim[2]), xlim = xlim,
                  grid = FALSE, vertLineEachPoint = TRUE, col = "white")
       set.seed(707)
       points(jitter(New_Ages_Good$TMA[New_Ages_Good$Used_NN_Model]), New_Ages_Good$TMA_Minus_Age_Rounded[New_Ages_Good$Used_NN_Model])
       points(New_Ages_Good$TMA[!New_Ages_Good$Used_NN_Model], New_Ages_Good$TMA_Minus_Age_Rounded[!New_Ages_Good$Used_NN_Model], col = "red", pch = 19)
                 
   ', file = paste0(Predicted_Ages_Path, '/TMA_minus_NN_Age_Rounded_vs_TMA_Jittered.png')) 
   











  
Model_Spectra_Meta_PRED <- Model_Spectra_Meta
dim(Model_Spectra_Meta_PRED)
[1] 1554  538


load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt\\Sable_Combo_2022_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
dim(Model_Spectra_Meta)
[1] 1528  536


test <- match.f(Model_Spectra_Meta, New_Ages_Good, 'filenames', 'filenames', 'Used_NN_Model')
test[is.na(test$Used_NN_Model), ]

# This otie looks good - so why is it not in New_Ages_Good. It is not one the fifteen that was left out for testing.
test[is.na(test$Used_NN_Model), c(1:2, 500:537)]
                                       filenames     X8000     X4016     X4008     X4000     X3992     X3984     X3976     X3968     X3960     X3952 project sample_year pacfin_code_id sequence_number age_structure_id specimen_id TMA
441 SABL_COMBO2022_NIR0022D_PRD_477_102189617_O1 0.2107899 0.4067253 0.4105164 0.4145668 0.4185508 0.4219342 0.4243378 0.4252431 0.4239198 0.4206884   COMBO        2022           SABL             477 102189617-SABL-O   102189617   1
    length_cm weight_kg sex structure_weight_g NWFSC_NIR_Project NWFSC_NIR_Scan_Session age_structure_side_scan crystallized_scan percent_crystallized_scan broken_scan tip_only_scan anterior_tip_missing posterior_tip_missing
441        35      0.36   M             0.0092               PRD               NIR0022D                       L             FALSE                         0        <NA>          <NA>                 <NA>                  <NA>
    percent_missing_scan tissue_present_scan tissue_level_scan oil_clay_contamination_scan stained_scan contamination_other_scan notes_scan shortName Used_NN_Model
441                    0                <NA>                 0                        <NA>           NA                       NA       <NA>  SABL_477            NA


   
sort(Model_Spectra_Meta[SaveOutOties, 'filenames'])
 [1] "SABL_COMBO2022_NIR0022A_PRD_56_102157476_O1"   "SABL_COMBO2022_NIR0022B_PRD_139_102157559_O1"  "SABL_COMBO2022_NIR0022C_PRD_265_102188545_O1"  "SABL_COMBO2022_NIR0022E_PRD_600_102133190_O1" 
 [5] "SABL_COMBO2022_NIR0022F_PRD_656_102133246_O1"  "SABL_COMBO2022_NIR0022G_PRD_1008_102133338_O1" "SABL_COMBO2022_NIR0022G_PRD_1026_102133356_O1" "SABL_COMBO2022_NIR0022G_PRD_1086_102133416_O1"
 [9] "SABL_COMBO2022_NIR0022G_PRD_897_102187647_O1"  "SABL_COMBO2022_NIR0022G_PRD_928_102187678_O1"  "SABL_COMBO2022_NIR0022H_PRD_1131_102187951_O1" "SABL_COMBO2022_NIR0022H_PRD_1226_102150796_O1"
[13] "SABL_COMBO2022_NIR0022H_PRD_1279_102167329_O1" "SABL_COMBO2022_NIR0022I_PRD_1496_102187406_O1" "SABL_COMBO2022_NIR0022I_PRD_1504_102187414_O1"


dim(New_Ages_Good)
[1] 1527    4

sort(New_Ages_Good[!New_Ages_Good$Used_NN_Model, 'filenames'])
 [1] "SABL_COMBO2022_NIR0022A_PRD_56_102157476_O1"   "SABL_COMBO2022_NIR0022B_PRD_139_102157559_O1"  "SABL_COMBO2022_NIR0022C_PRD_265_102188545_O1"  "SABL_COMBO2022_NIR0022E_PRD_600_102133190_O1" 
 [5] "SABL_COMBO2022_NIR0022F_PRD_656_102133246_O1"  "SABL_COMBO2022_NIR0022G_PRD_1008_102133338_O1" "SABL_COMBO2022_NIR0022G_PRD_1026_102133356_O1" "SABL_COMBO2022_NIR0022G_PRD_1086_102133416_O1"
 [9] "SABL_COMBO2022_NIR0022G_PRD_897_102187647_O1"  "SABL_COMBO2022_NIR0022G_PRD_928_102187678_O1"  "SABL_COMBO2022_NIR0022H_PRD_1131_102187951_O1" "SABL_COMBO2022_NIR0022H_PRD_1226_102150796_O1"
[13] "SABL_COMBO2022_NIR0022H_PRD_1279_102167329_O1" "SABL_COMBO2022_NIR0022I_PRD_1496_102187406_O1" "SABL_COMBO2022_NIR0022I_PRD_1504_102187414_O1"
   
		  
   
}

graphics.off()


#   # --- Find bad scans ---
#   for ( i in fileNames)  {
#      print(i)
#      try(newScans.RAW <- opusreader::opus_read(paste(Spectra_Path, i , sep = "/"), simplify = TRUE, wns_digits = 0)[[2]] )
#   }
#   
#   
#   # Bad scans for Sablefish 2017: 505, 506, 507, 509, 511-514, 516-518, 520-522, 525, 527, 529-533, 535-538


