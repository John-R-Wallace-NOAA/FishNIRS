
#  --- Conda TensorFlow environment ---
Conda_TF_Eniv <- "C:/Users/John.Wallace/AppData/Local/miniconda3/envs/tf"

library(tensorflow)
library(keras)   
Sys.setenv("RETICULATE_PYTHON" = Conda_TF_Eniv) 

# --- TensorFlow Math Check  ---
a <- tf$Variable(5.56)
b <- tf$Variable(2.7)
a + b
k_clear_session() 

# --- Download the Predict_NN_Age() function from the FishNIRS repo on GitHub

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
   
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Predict_NN_Age.R")


Spectra_Path <- "New_Scans" # Put new spectra scans in a separate folder and put the name of the folder here

#  ---- Note if you get this error: < Error in `[.data.frame`(data.frame(prospectr::savitzkyGolay(newScans.RAW, : undefined columns selected > or you know that 
#         the new spectra scan(s) do not have the same freq. as the model expects, then add the file 'FCNN\PACIFIC_HAKE_AAA_Correct_Scan_Freq' to your scans and an interpolation will be done. ---

# Load the variable seleciton - iPLS variable selection on Savitzky-Golay smoothed raw data
NN_Model <- 'FCNN Model/Hake_2019_CNN_15_Rdm_models_21_Apr_2023.RData'  # Load the NN model - 10 Random Models

# Use Predict_NN_Age() to find the NN predicted ages
Predict_NN_Age(Conda_TF_Eniv, Spectra_Path, NN_Model)

 
 