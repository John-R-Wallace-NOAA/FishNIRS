
########################################################
# Need >= R ver 3.0  
########################################################

setwd("C:/ALL_USR/JRW/SIDT/Hake 2019") # Change this path as needed
# Sys.setenv(GITHUB_PAT = '**********')  # You will need a 'GITHUB_PAT' from GitHub set somewhere in R (Google how to get one, if needed).

#  --- Conda TensorFlow environment ---
Conda_TF_Eniv <- "C:/Users/John.Wallace/AppData/Local/miniconda3/envs/tf"  # Change this path as needed

if (!any(installed.packages()[, 1] %in% "tensorflow")) 
     install.packages("tensorflow")
     
if (!any(installed.packages()[, 1] %in% "keras")) 
     install.packages("keras") 
     
library(tensorflow)
library(keras)   
Sys.setenv("RETICULATE_PYTHON" = Conda_TF_Eniv) 

# --- TensorFlow Math Check  ---
a <- tf$Variable(5.56)
b <- tf$Variable(2.7)
a + b

# --- Pause here when submitting code to R ---

k_clear_session() 




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

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/plotCI.jrw2.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/sort.f.R") 
  
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Predict_NN_Age.R")

# --- Put new spectra scans in a separate folder and put the name of the folder below ---
Spectra_Path <- "New_Scans" 

#  ---- Note if you get this error: < Error in `[.data.frame`(data.frame(prospectr::savitzkyGolay(newScans.RAW, : undefined columns selected > or you know that 
#         the new spectra scan(s) do not have the same freq. as the model expects, then add the file 'FCNN\PACIFIC_HAKE_AAA_Correct_Scan_Freq' to your scans and an interpolation will be done. ---

# --- Load the NN model - 20 Random Models each with 10 full k-fold models - using the 2019 Hake NN model as an example ---
NN_Model <- 'FCNN Model/Hake_2019_FCNN_20_Rdm_models_1_Apr_2023.RData'   

# --- Use Predict_NN_Age() to find the NN predicted ages ---
New_Ages <- Predict_NN_Age(Conda_TF_Eniv, Spectra_Path, NN_Model, plot = TRUE)
(New_Ages <- cbind(Index = 1:nrow(New_Ages), sort.f(New_Ages, 'NN_Pred_Median')))


dev.new(width = 11, height = 8)
plotCI.jrw2(1:nrow(New_Ages), New_Ages$NN_Pred_Median, New_Ages$Upper_Quantile_0.975, New_Ages$Lower_Quantile_0.025, 
    ylim = c(-0.5, max(New_Ages$Upper_Quantile_0.975) + 0.5),
    xlim = c(-0.5, nrow(New_Ages) + 0.5), ylab = "Neural Net Estimated Age", xlab = "Index",
    main = "Quantiles are a reflection of the NN models Precision, not the Accuracy to a TMA Age")
    
abline(h = 1:(floor(max(New_Ages$NN_Pred_Median)) + 1), col ='grey80')
    
plotCI.jrw2(1:nrow(New_Ages), New_Ages$NN_Pred_Median, New_Ages$Upper_Quantile_0.975, New_Ages$Lower_Quantile_0.025, 
    ylim = c(-0.5, max(New_Ages$Upper_Quantile_0.975) + 0.5),
    xlim = c(-0.5, nrow(New_Ages) + 0.5), ylab = "Neural Net Estimated Age", xlab = "Index",
    main = "Quantiles are a reflection of the NN models Precision, not the Accuracy to a TMA Age", add = TRUE)
  
# Show the new ages rounded in green - the rounding Delta for 2019 Hake is zero    
Delta <- 0
points( 1:nrow(New_Ages), round(New_Ages$NN_Pred_Median + Delta), col = 'green', pch = 19)


    
# --- Write out to a CSV file ---   
write.csv(New_Ages, file = 'New Ages for 2019 Hake.csv', row.names = FALSE)


