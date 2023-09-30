
########################################################
# Need >= R ver 3.0  
########################################################

if(interactive()) {
     setwd("C:/ALL_USR/JRW/SIDT/Hake 2019 NN Model/Predict NN Ages")  # Change path as needed
} else { 
    options(width = 140)
} 

# Sys.setenv(GITHUB_PAT = '**********')  # You will need a 'GITHUB_PAT' from GitHub set somewhere in R (Search the Web how to get one from GitHub, if needed).

#  --- Conda TensorFlow environment ---
Conda_TF_Eniv <- "C:/m3/envs/tf"  # Change this path as needed

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


Sys.setenv(RETICULATE_PYTHON = Conda_TF_Eniv) 
Sys.getenv("RETICULATE_PYTHON") 

# --- TensorFlow Load and Math Check  ---
a <- tf$Variable(5.56)
cat("\n\nTensorFlow Math Check\n\na = "); print(a)
b <- tf$Variable(2.7)
cat("\nb = "); print(b)
cat("\na + b = "); print(a + b)
cat("\n\n")

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

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/sort.f.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Date.R") 
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R") 
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/saveHtmlFolder.R") 
 
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Predict_NN_Age.R")


# --- Put new spectra scans in a separate folder and enter the name of the folder below ---
Spectra_Path <- "New_Scans" 

# --- The NN predicted ages will go in the path defined below ---
Predicted_Ages_Path <- "Predicted_Ages"
dir.create(Predicted_Ages_Path, showWarnings = FALSE)

#  ---- Note if you get this error: < Error in `[.data.frame`(data.frame(prospectr::savitzkyGolay(newScans.RAW, : undefined columns selected > or you know that 
#         the new spectra scan(s) do not have the same freq. as the model expects, then add the file 'FCNN\PACIFIC_HAKE_AAA_Correct_Scan_Freq' to your scans and an interpolation will be done. ---

# --- Load the NN model - 10-20 random models each with 10-fold complete 'k-fold' models. ---
NN_Model <- 'FCNN Model/Hake_2019_FCNN_20_Rdm_models_1_Apr_2023.RData'   

# --- Use Predict_NN_Age() to find the NN predicted ages ---
# New_Ages <- Predict_NN_Age(Conda_TF_Eniv, Spectra_Path, NN_Model, plot = TRUE, NumRdmModels = 1, htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure for New Ages')) # One random model is faster for testing
New_Ages <- Predict_NN_Age(Conda_TF_Eniv, Spectra_Path, NN_Model, plot = TRUE, htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure for New Ages'))

  
# --- Write out to a CSV file - Example of file name using Date() function: 'New Ages for 2019 Hake, 26 Sep 2023.csv' 
write.csv(New_Ages, file = paste0(Predicted_Ages_Path, '/NN Predicted Ages, ', Date(" "), '.csv'), row.names = FALSE)


# --- Create plots with age estimates and quantile error bars ---
New_Ages <- data.frame(Index = 1:nrow(New_Ages), New_Ages)  # Add 'Index' as the first column in the data frame
print(New_Ages[1:5, ])

Delta <- 0  # The rounding Delta for 2019 Hake is zero  
New_Ages$Age_Rounded <- round(New_Ages$NN_Pred_Median + Delta)
New_Ages$Rounded_Age <- factor(" ")


# - Plot by order implied by the spectra file names -
g <- ggplotly(ggplot(New_Ages, aes(Index, NN_Pred_Median)) +  
geom_point() +
geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
geom_point(aes(Index, Age_Rounded, color = Rounded_Age)) + scale_color_manual(values = c(" " = "green")), dynamicTicks = TRUE)
print(g)
saveHtmlFolder(paste0(Predicted_Ages_Path, '/Predicted_Ages_Order_by_File_Names'))
Sys.sleep(3)

     
# - Plot by sorted NN predicted ages -
New_Ages_Sorted <- sort.f(New_Ages, 'NN_Pred_Median') # Sort 'New_ages' by 'NN_Pred_Median', except for "Index" (see the next line below)
New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  # Reset Index for graphing

print(New_Ages_Sorted[1:5, ])

g <- ggplotly(ggplot(New_Ages_Sorted, aes(Index, NN_Pred_Median)) +  
geom_point() +
geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
geom_point(aes(Index, Age_Rounded, color = Rounded_Age)) + scale_color_manual(values = c(" " = "green")), dynamicTicks = TRUE)
print(g)
saveHtmlFolder(paste0(Predicted_Ages_Path, '/Predicted_Ages_Sorted'))


