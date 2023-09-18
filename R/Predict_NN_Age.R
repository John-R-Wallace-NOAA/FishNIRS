
#   SG_Variables_Selected <- names(Hake_spectra_2019.sg.iPLS) # Get the variables selected from the column names of the data used in the NN model building
#   save(SG_Variables_Selected, file = 'FCNN Model/Hake_2019_SG_Variables_Selected.RData')

# --- Conda TensorFlow environment ---
# Conda_TF_Eniv <- "C:/Users/John.Wallace/AppData/Local/miniconda3/envs/tf" # Work desktop and laptop  
# # Conda_TF_Eniv <- "C:/m/envs/tf_cpu_only") # NWFSC GPU machine - CPU only   
# # Conda_TF_Eniv <- "C:/m/envs/tf") # NWFSC GPU machine - GPU version breaks in R, but works in Python  

# library(tensorflow)
# library(keras)   
# Sys.setenv("RETICULATE_PYTHON" = Conda_TF_Eniv) 

# --- TensorFlow Math Check  ---
# a <- tf$Variable(5.56)
# b <- tf$Variable(2.7)
# k_clear_session() 
 
 
# PATH <- "New_Scans" # Put new spectra scans in a separate folder and put the name of the folder here
# SG_Variables_Selected <- 'FCNN Model/Hake_2019_SG_Variables_Selected.RData' # iPLS variable selection on Savitzky-Golay smoothed raw data
# NN_Model <- 'FCNN Model/Hake_2019_CNN_15_Rdm_models_21_Apr_2023_08_18_29.RData'  # 10 Random Models
# (Hake_2019_Pred_ages <- Predict_NN_Age(Conda_TF_Eniv, PATH, SG_Variables_Selected, NN_Model))


Predict_NN_Age <- function(Conda_TF_Eniv, PATH, SG_Variables_Selected, NN_Model) {
   
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
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Correlation_R_squared_RMSE_MAE_SAD.R")
   
   base::load(SG_Variables_Selected)
   base::load(NN_Model)
   
   # --- Setup for TensorFlow and Keras ---
   require(tensorflow)
   require(keras)   
   
   # --- Change this path to where your Conda TensorFlow environment is located. ---
   Sys.setenv("RETICULATE_PYTHON" = Conda_TF_Eniv) 
   
   # # --- Test TensorFlow environment ---
   # a <- tf$Variable(5.56)
   # b <- tf$Variable(2.7)
   # cat("\nTensorFlow Math Check (5.56 + 2.70):\n")
   # print(a + b)
   # cat("\n\n")
   # k_clear_session()
   
   if (!any(installed.packages()[, 1] %in% "opusreader")) 
     remotes("pierreroudier/opusreader")   # https://github.com/pierreroudier/opusreader
     
   if (!any(installed.packages()[, 1] %in% "tensorflow")) 
     install.packages("tensorflow")
     
   if (!any(installed.packages()[, 1] %in% "keras")) 
     install.packages("keras")  
     
   # --- Create a list of all spectral files within 'PATH'---   
   listspc <- dir(path = PATH)
   cat(paste0("\nNumber of Spectral Files Read In: ", length(listspc), "\n\n"))
   
   newScans.RAW <- opusreader::opus_read(paste(PATH, listspc, sep = "/"), simplify = TRUE)[[2]] 
   cat("\nDimension of Spectral File Matrix Read In:", dim(newScans), "\n\n")
   newScans <- data.frame(prospectr::savitzkyGolay(newScans.RAW, m = 1, p = 2, w = 15))[, SG_Variables_Selected]
   Fold_models <- Rdm_models[[1]]
    
   newScans.pred.ALL <- NULL
   for (i in 1:length(Fold_models)) {      
         newScans.pred <- as.vector(predict(keras::unserialize_model( Fold_models[[i]], custom_objects = NULL, compile = TRUE), as.matrix(1000 * newScans)))
         newScans.pred.ALL <- rbind(newScans.pred.ALL, data.frame(Index = 1:nrow(newScans), newScans.pred = newScans.pred))
   }
     
   Pred_median <- r(data.frame(NN_Pred_Median = aggregate(list(NN_Pred_Median = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), median)[,2], 
      Lower_Quantile_0.025 = aggregate(list(Quantile_0.025 = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), quantile, probs = 0.025)[,2],
      Upper_Quantile_0.975 = aggregate(list(Quantile_0.975 = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), quantile, probs = 0.975)[,2]), 1)
    
   cat(paste0("\n\n--- Note: The quantiles are a reflection of the NN models precision based on ", length(Fold_models), " randomized models, not the accuracy to a TMA Age ---\n\n"))    
   data.frame(filenames = listspc, Pred_median)
}





