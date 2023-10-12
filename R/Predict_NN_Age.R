
# --- Use 2019 Hake for an example of the steps needed to use the Predict_NN_Age() function ---

# # Get the variables selected from the column names of the data used in the NN model building
#   SG_Variables_Selected <- names(Hake_spectra_2019.sg.iPLS) 
#   SG_Variables_Selected <- as.numeric(substring(names(Sable_Spectra_2017_2019.sg.iPLS), 2))  # Sablefish
#   SG_Variables_Selected <- names(Sable_Spectra_2017_2019.sg.iPLS)  # Sablefish

# Save the rounding Delta for the current NN model 
# roundingDelta <- 0
# roundingDelta <- -0.2 # Sablefish

# # Add 'SG_Variables_Selected' and 'roundingDelta' to the NN Model '.RData' file
# JRWToolBox::resave(SG_Variables_Selected, roundingDelta, file = 'FCNN Model/Hake_2019_FCNN_20_Rdm_models_1_Apr_2023.RData')
# JRWToolBox::resave(SG_Variables_Selected, roundingDelta, file = 'FCNN Model/Sablefish_2017_2019_Rdm_models_22_Mar_2023_14_57_26.RData') # Sablefish 

# --- Test Predict_NN_Age() function ---

# - Conda TensorFlow environment -
# Conda_TF_Eniv <- "C:/Users/John.Wallace/AppData/Local/miniconda3/envs/tf" # Work desktop and laptop  
# # Conda_TF_Eniv <- "C:/m/envs/tf_cpu_only") # NWFSC GPU machine - CPU only   
# # Conda_TF_Eniv <- "C:/m/envs/tf") # NWFSC GPU machine - GPU version breaks in R, but works in Python  

# library(tensorflow)
# library(keras)   
# Sys.setenv("RETICULATE_PYTHON" = Conda_TF_Eniv) 


# - TensorFlow Math Check  -
# a <- tf$Variable(5.56)
# b <- tf$Variable(2.7)
# k
# Spectra_Path <- "New_Scans" # Put new spectra scans in a separate folder and put the name of the folder here
# NN_Model <- 'FCNN Model/Hake_2019_FCNN_20_Rdm_models_1_Apr_2023.RData'  # 20 Random Models

# Predict_NN_Age(Conda_TF_Eniv, Spectra_Path, NN_Model)



Predict_NN_Age <- function(Conda_TF_Eniv, Spectra_Path, NN_Model, plot = TRUE, htmlPlotFolder = NULL, NumRdmModels = NULL, shortNameSegments = c(2,4), shortNameSuffix = NULL, ...) {
   
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
   
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/r.R")   
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/renum.R")   
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R")   
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/saveHtmlFolder.R") 
   
   
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Correlation_R_squared_RMSE_MAE_SAD.R")
   
   '%r1%' <- function (e1, e2) 
   {
      ifelse(e1%%e2 == 0, e2, e1%%e2)
   }

   
   base::load(NN_Model)  # Need SG_Variables_Selected and Rdm_models from NN_Model
   
   if (!any(installed.packages()[, 1] %in% "remotes")) 
     install.packages("remotes") 
   
   if (!any(installed.packages()[, 1] %in% "opusreader")) 
     remotes::install_github("pierreroudier/opusreader")   # https://github.com/pierreroudier/opusreader
     
   if (!any(installed.packages()[, 1] %in% "tensorflow")) 
     install.packages("tensorflow")
     
   if (!any(installed.packages()[, 1] %in% "keras")) 
     install.packages("keras") 

   if (!any(installed.packages()[, 1] %in% "prospectr")) 
     install.packages("prospectr") 
   
   # --- Setup for TensorFlow and Keras ---
   require(tensorflow)
   require(keras) 
   require(prospectr)   
   
   # --- Change this path to where your Conda TensorFlow environment is located. ---
   Sys.setenv("RETICULATE_PYTHON" = Conda_TF_Eniv) 
   
   # # --- Test TensorFlow environment ---
   # a <- tf$Variable(5.56)
   # b <- tf$Variable(2.7)
   # cat("\nTensorFlow Math Check (5.56 + 2.70):\n")
   # print(a + b)
   # cat("\n\n")
   # k_clear_session()
   
   # --- Create a character vector of all spectral files within 'Spectra_Path'---   
   fileNames <- dir(path = Spectra_Path)
   cat(paste0("\nNumber of Spectral Files Read In: ", length(fileNames), "\n\n"))
   
   shortName <- apply(matrix(fileNames, ncol = 1), 1, function(x) paste(get.subs(x, sep = "_")[shortNameSegments], collapse = "_"))
   if(!is.null(shortNameSuffix))
       shortName <- paste0(shortName, "_", shortNameSuffix)
   
   newScans.RAW <- opusreader::opus_read(paste(Spectra_Path, fileNames, sep = "/"), simplify = TRUE, wns_digits = 0)[[2]] 
      
   if(plot) {
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")
     rowNums <- 1:nrow(newScans.RAW)
     if(length(rowNums <= 26^2))
       plotly.Spec(data.frame(filenames = fileNames, newScans.RAW, Otie = paste0(LETTERS[floor((rowNums - 0.001)/26) + 1], LETTERS[rowNums %r1% 26], '_', rowNums), shortName = shortName), colorGroup = 'Otie', ...)
     else
       plotly.Spec(data.frame(filenames = fileNames, newScans.RAW, Otie = factor(rowNums), shortName = shortName), colorGroup = 'Otie', ...) 
  
     if(!is.null(htmlPlotFolder))
       saveHtmlFolder(htmlPlotFolder, view = !interactive())
   }    

   cat("\nDimension of Spectral File Matrix Read In:", dim(newScans.RAW), "\n\n")
   newScans <- data.frame(prospectr::savitzkyGolay(newScans.RAW, m = 1, p = 2, w = 15))[, SG_Variables_Selected]   # SG_Variables_Selected is part of the NN_Model .RData file.
  
  
  if(is.null(NumRdmModels))
      N <- length(Rdm_models)
  else
      N <- NumRdmModels
 
   newScans.pred.ALL <- NULL
   for(j in 1:N) {
      Fold_models <- Rdm_models[[j]]
      for (i in 1:length(Fold_models)) {      
            newScans.pred <- as.vector(predict(keras::unserialize_model(Fold_models[[i]], custom_objects = NULL, compile = TRUE), as.matrix(1000 * newScans)))
            newScans.pred.ALL <- rbind(newScans.pred.ALL, data.frame(Index = 1:nrow(newScans), newScans.pred = newScans.pred))
     }
   }  
      
   Pred_median <- r(data.frame(NN_Pred_Median = aggregate(list(NN_Pred_Median = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), median)[,2], 
      Lower_Quantile_0.025 = aggregate(list(Quantile_0.025 = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), quantile, probs = 0.025)[,2],
      Upper_Quantile_0.975 = aggregate(list(Quantile_0.975 = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), quantile, probs = 0.975)[,2]), 4)
    
   cat(paste0("\n\n--- Note: The quantiles are a reflection of the NN models precision based on ", N, " full 10-fold randomized models, not the accuracy to a TMA Age ---\n\n"))    
   data.frame(filenames = fileNames, Pred_median)
}





