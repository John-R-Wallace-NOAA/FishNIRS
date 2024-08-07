
########################################################
# Need >= R ver 3.0  
########################################################

setwd("C:/ALL_USR/JRW/SIDT/Hake 2019") # Change this path as needed
# Sys.setenv(GITHUB_PAT = '**********')  # You will need a 'GITHUB_PAT' from GitHub set somewhere in R (Google how to get one, if needed).

#  --- Conda TensorFlow environment ---
Conda_TF_Eniv <- "C:/Users/John.Wallace/AppData/Local/miniconda3/envs/tf"  # Change this path as needed

if (!any(installed.packages()[, 1] %in% "ggplot2")) 
     install.packages("ggplot2") 

if (!any(installed.packages()[, 1] %in% "plotly")) 
     install.packages("plotly")      
     
if (!any(installed.packages()[, 1] %in% "tensorflow")) 
     install.packages("tensorflow")
     
if (!any(installed.packages()[, 1] %in% "keras")) 
     install.packages("keras") 
  
library(ggplot2)
library(plotly)        
library(tensorflow)
library(keras)  

Sys.setenv(RETICULATE_PYTHON = Conda_TF_Eniv) 
Sys.getenv("RETICULATE_PYTHON") 

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

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/sort.f.R") 
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Predict_NN_Age.R")


# --- Put new spectra scans in a separate folder and put the name of the folder below ---
Spectra_Path <- "New_Scans" 

#  ---- Note if you get this error: < Error in `[.data.frame`(data.frame(prospectr::savitzkyGolay(newScans.RAW, : undefined columns selected > or you know that 
#         the new spectra scan(s) do not have the same freq. as the model expects, then add the file 'FCNN\PACIFIC_HAKE_AAA_Correct_Scan_Freq' to your scans and an interpolation will be done. ---

# --- Load the NN model - 20 Random Models each with 10 full k-fold models. Below the 2019 Hake NN model is being used as an example ---
NN_Model <- 'FCNN Model/Hake_2019_FCNN_20_Rdm_models_1_Apr_2023.RData'   

# --- Use Predict_NN_Age() to find the NN predicted ages ---
New_Ages <- Predict_NN_Age(Conda_TF_Eniv, Spectra_Path, NN_Model, plot = TRUE)


# --- Create a plot with age estimates and quantile error bars ---
(New_Ages$Index <- data.frame(Index = 1:nrow(New_Ages), New_Ages))[1:5,]  # Add 'Index' as the first column in the data frame
  
# The rounding Delta for 2019 Hake is zero    
Delta <- 0
New_Ages$Age_Rounded <- round(New_Ages$NN_Pred_Median + Delta)
New_Ages$Rounded_Age <- factor(" ")

ggplotly(ggplot(New_Ages, aes(Index, NN_Pred_Median)) +  
geom_point() +
geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
geom_point(aes(Index, Age_Rounded, color = Rounded_Age)) + scale_color_manual(values = c(" " = "green")), dynamicTicks = TRUE)


# --- Sorted Ages ---
New_Ages_Sorted <- sort.f(New_Ages, 'NN_Pred_Median') # Sort 'New_ages' by 'NN_Pred_Median', except for "Index" (see below)
New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  # Reset Index for graphing
New_Ages_Sorted[1:5, ]

ggplotly(ggplot(New_Ages_Sorted, aes(Index, NN_Pred_Median)) +  
geom_point() +
geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
geom_point(aes(Index, Age_Rounded, color = Rounded_Age)) + scale_color_manual(values = c(" " = "green")), dynamicTicks = TRUE)


  
# --- Write out to a CSV file ---   
write.csv(New_Ages, file = 'New Ages for 2019 Hake Sept 25 2023.csv', row.names = FALSE)
