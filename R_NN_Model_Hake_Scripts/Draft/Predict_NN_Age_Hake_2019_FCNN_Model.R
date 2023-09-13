

# setwd("C:/SIDT/Hake Data 2019") # Change this path as needed.
setwd("C:/ALL_USR/JRW/SIDT/Hake 2019")


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


# --- Setup for TensorFlow and Keras ---
lib(tensorflow)
lib(keras)


# --- Change this path to where your Conda TensorFlow environment is located. ---
Sys.setenv("RETICULATE_PYTHON" = "C:/Users/John.Wallace/AppData/Local/miniconda3/envs/tf") # Work desktop and laptop
# Sys.setenv("RETICULATE_PYTHON" = "C:/m/envs/tf_cpu_only") # NWFSC GPU machine - CPU only
# Sys.setenv("RETICULATE_PYTHON" = "C:/m/envs/tf") # NWFSC GPU machine - GPU version breaks in R, but works in Python
Sys.getenv("RETICULATE_PYTHON")

# Test TensorFlow environment
a <- tf$Variable(5.56)
b <- tf$Variable(2.7)
a + b

k_clear_session()


               
base::load('Hake_spectra_2019.sg.iPLS.RData')
base::load('Hake_TMA_2019.RData')                 

# And load saved 'Rdm_models' and 'Rdm_folds_index' (plus other info) from a saved NN model run [see the save(...) above]
base::load('FCNN Model/Hake_2019_FCNN_20_Rdm_models_1_Apr_2023_09_26_40.RData')


newScans <- Hake_spectra_2019.sg.iPLS[1050:1080,]

newScans.pred.ALL <- NULL
for (i in 1:length(Fold_models)) {      
      newScans.pred <- as.vector(predict(keras::unserialize_model( Fold_models[[i]], custom_objects = NULL, compile = TRUE), as.matrix(1000 * newScans)))
      newScans.pred.ALL <- rbind(newScans.pred.ALL, data.frame(Index = 1:nrow(newScans), newScans.pred = newScans.pred))
}

# newScans.pred.ALL

r(data.frame(aggregate(list(NN_Pred_Median = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), median), 
   Lower_Quantile_0.025 = aggregate(list(Quantile_0.025 = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), quantile, probs = 0.025)[,2],
   Upper_Quantile_0.975 = aggregate(list(Quantile_0.975 = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), quantile, probs = 0.975)[,2]), 1)

