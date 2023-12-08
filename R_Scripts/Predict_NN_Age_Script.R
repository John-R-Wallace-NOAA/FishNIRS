

###################################
#       Need >= R ver 3.0         #
###################################

# ------------------------------------ Main User Setup ------------------------------------------------------------

   if(interactive()) 
         setwd(ifelse(.Platform$OS.type == 'windows', "C:/ALL_USR/JRW/SIDT/Predict_NN_Ages", "/more_home/h_jwallace/SIDT/Predict_NN_Ages"))   # Change path to the Spectra Set's .GlobalEnv as needed
   if(!interactive())   
         options(width = 120)      
         
   Spectra_Set <- c("Hake_2019", "Sable_2017_2019", "Sable_Combo_2022")[3]
   Spectra_Path <- "New_Scans"    # Put new spectra scans in a separate folder and enter the name of the folder below
   Predicted_Ages_Path <- "Predicted_Ages" # The NN predicted ages will go in the path defined below
   dir.create(Predicted_Ages_Path, showWarnings = FALSE)
   TMA_Ages <- c(TRUE, FALSE)[1] # Are TMA ages available and are they to be used?
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
   if(Spectra_Set == "Sable_Combo_2022") { 
      NN_Model <- 'FCNN Model/Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_3_Dec_2023_09_24_09.RData'
      shortNameSegments <- c(1,5) # Segments 1 and 3 of the spectra file name, e.g.: (SABLEFISH, COMBO201701203A, 28, OD1) => (SABLEFISH, 28)
      shortNameSuffix <- 'Year'
      yearPosition <- c(6, 9) # e.g. COMBO201701203A => 2017 (Segment used (see above) is: shortNameSegments[1] + 1)
      fineFreqAdj <- 0
      if(TMA_Ages)
        TMA_Meta <- "C:/ALL_USR/JRW/SIDT/Sablefish 2022 Combo/Sable_Combo_2022_Model_Spectra_Meta_ALL_GOOD_DATA.RData"  # If used, change path to the main sepectra/metadata save()'d data frame which contains TMA ages.  Matching done via 'filenames'.
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

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly_spectra.R")
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

    
N_Samp <- ifelse(is.numeric(Max_N_Spectra), min(c(length(fileNames), Max_N_Spectra)), 'All')

##### This is the main call to Predict_NN_Age() #####
# New_Ages <- Predict_NN_Age(Conda_TF_Eniv, Spectra_Path, NN_Model, plot = plot, NumRdmModels = 1,  htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure for New Ages'), spectraInterp = spectraInterp, fineFreqAdj = fineFreqAdj,
#      Predicted_Ages_Path = Predicted_Ages_Path,  shortNameSegments = shortNameSegments, shortNameSuffix = shortNameSuffix., N_Samp = N_Samp, verbose = verbose) # One random model for faster testing

New_Ages <- Predict_NN_Age(Conda_TF_Eniv, Spectra_Path, NN_Model, plot = plot, htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure for New Ages'), spectraInterp = spectraInterp, fineFreqAdj = fineFreqAdj, opusReader = opusReader, 
   Predicted_Ages_Path = Predicted_Ages_Path,  shortNameSegments = shortNameSegments, shortNameSuffix = shortNameSuffix., N_Samp = N_Samp, verbose = verbose) # Use the max number of random model replicates available
if(verbose) head(New_Ages, 20)

# For testing Predict_NN_Age(): plot = TRUE; NumRdmModels = 1;  htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure for New Ages'); N_Samp = N_Samp)                                      
      

# --- save() ages and write out to a CSV file ---
save(New_Ages, file = paste0(Predicted_Ages_Path, '/NN Predicted Ages, ', Date(" "), '.RData'))
write.csv(New_Ages, file = paste0(Predicted_Ages_Path, '/NN Predicted Ages, ', Date(" "), '.csv'), row.names = FALSE)

# --- Create plots with age estimates and quantile credible intervals ---
New_Ages <- data.frame(Index = 1:nrow(New_Ages), New_Ages)  # Add 'Index' as the first column in the data frame
print(New_Ages[1:5, ])

Delta <- extractRData('roundingDelta', file = NN_Model) # e.g. the rounding Delta for 2019 Hake is zero.  
New_Ages$Age_Rounded <- round(New_Ages$NN_Pred_Median + Delta)
cat(paste0("\n\nUsing a rounding Delta of ", Delta, "\n\n"))

# --- If TMA ages are not available ---

if(!TMA_Ages) {

   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/browserPlot.R") 

   cols <- 'green'
    
   # -- Plot by order implied by the spectra file names --
   g <- ggplot(New_Ages, aes(Index, NN_Pred_Median)) +  
   geom_point() +
   geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
   geom_point(aes(Index, Age_Rounded, col = cols)) + 
   scale_color_manual(labels = 'Rounded Age', values = cols, name = ' ')
   browserPlot('print(g)', file = paste0(Predicted_Ages_Path, '/Predicted_Ages_Order_by_File_Names.png'))
    
   
   # -- Plot by sorted NN predicted ages --
   New_Ages_Sorted <- sort.f(New_Ages, 'NN_Pred_Median') # Sort 'New_ages' by 'NN_Pred_Median', except for "Index" (see the next line below)
   New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  # Reset Index for graphing
   if(verbose) head(New_Ages_Sorted, 10)
   
   g <- ggplot(New_Ages_Sorted, aes(Index, NN_Pred_Median)) +  
   geom_point() +
   geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
   geom_point(aes(Index, Age_Rounded, col = cols)) + 
   scale_color_manual(labels = 'Rounded Age', values = cols, name = ' ')
   browserPlot('print(g)', file = paste0(Predicted_Ages_Path, '/Predicted_Ages_Sorted.png'))
}


# --- Use TMA ages, if available ---

if(TMA_Ages) {

   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/load.R") 
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/match.f.R") 
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/browserPlot.R") 
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/agreementFigure.R")

   load(TMA_Meta, str = verbose) # This is JRWToolBox::load() (see directly above) not base::load()
   
   New_Ages$TMA <- NULL # Clear old TMA before updating
   if(length(get.subs(get.subs(New_Ages$filenames[1], sep = "."))) == 2)
       New_Ages$filenames <- get.subs(New_Ages$filenames, sep = ".")[1,]
   New_Ages <- match.f(New_Ages, Model_Spectra_Meta, 'filenames', 'filenames', 'TMA')  
   
   plotly.Spec(Model_Spectra_Meta, N_Samp = N_Samp, htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure with TMA for New Ages'))
   
   # browserPlot('agreementFigure(New_Ages$TMA, New_Ages$NN_Pred_Median, Delta = Delta, full = TRUE)', file = paste0(Predicted_Ages_Path, '/Agreement_Figure.pdf'), pdf = TRUE)   
   browserPlot('agreementFigure(New_Ages$TMA, New_Ages$NN_Pred_Median, Delta = Delta, full = TRUE)', file = paste0(Predicted_Ages_Path, '/Agreement_Figure.png'))
   browserPlot('agreementFigure(New_Ages$TMA, New_Ages$NN_Pred_Median, Delta = Delta, full = FALSE)', file = paste0(Predicted_Ages_Path, '/Agreement_Figure_Zoomed.png'))
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
   browserPlot('print(g)', file = paste0(Predicted_Ages_Path, '/Predicted_Ages_Order_by_File_Names.png'), width = 18, height = 10,)
   
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
    
   browserPlot('print(g)', file = paste0(Predicted_Ages_Path, '/Predicted_Ages_Sorted.png'))
   
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
   
   browserPlot('print(g)', file = paste0(Predicted_Ages_Path, '/Predicted_Ages_Sorted_Subset.png'))
  
  
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
   # browserPlot('print(g)', file = paste0(Predicted_Ages_Path, '/Predicted_Ages_Sorted_Subset.png'))
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
   # browserPlot('print(g)', file = paste0(Predicted_Ages_Path, '/Relative_absolute_error_by_Index.png'))
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
   # browserPlot('print(g)', file = paste0(Predicted_Ages_Path, '/Relative_absolute_error_by_sorted_TMA_age.png'))
   # rm(g)
   
   
   # -- Relative absolute error by TMA age --
   g <- xyplot(abs(TMA - NN_Pred_Median)/ifelse(TMA == 0, 1, TMA) ~ TMA, group = TMA, data = New_Ages)
   browserPlot('print(g)', file = paste0(Predicted_Ages_Path, '/Relative_absolute_error_by_TMA_age.png'))
   
   # -- Plot of relative error by sorted TMA age --   
   New_Ages_Sorted <- sort.f(New_Ages, 'TMA')
   New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  # Reset Index for graphing
   if(verbose) head(New_Ages_Sorted, 20)
   g <- xyplot(abs(TMA - NN_Pred_Median)/ifelse(TMA == 0, 1, TMA) ~ Index, group = TMA, data = New_Ages_Sorted)
   browserPlot('print(g)', file = paste0(Predicted_Ages_Path, '/Relative_absolute_error_by_sorted_TMA.png'))
   
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
   browserPlot('print(g)', file = paste0(Predicted_Ages_Path, '/TMA_Sorted.png'))
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


