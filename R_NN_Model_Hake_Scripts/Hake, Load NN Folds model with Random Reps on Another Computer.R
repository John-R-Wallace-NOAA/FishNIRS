
# --- Initial setup  (The curly brakets without  indented lines directly below are there to enable the  hiding of code sections using Notepad++.) ---
{
setwd("C:/ALL_USR/JRW/SIDT/Hake Data 2019") # Change this path as needed.

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

# Toolbox functions 
if (!any(installed.packages()[, 1] %in% "JRWToolBox"))  {
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/openwd.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/lib.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/sort.f.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/predicted_observed_plot.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/residuals_plot.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/as.num.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/match.f.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Table.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/renum.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/agg.table.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/r.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/gof.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Date.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/timeStamp.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/dec.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/loess.line.R")
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/plot.loess.R")
}    

# FishNIRS funtions
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/predicted_observed_plot.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/residuals_plot.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Correlation_R_squared_RMSE_MAE_SAD.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Mode.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/agreementFigure.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/FCNN_Model.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/CNN_model_ver_5.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/CNN_model_2D.R")  # Not working yet


lib(tidyverse)
lib(recipes)
lib(rsample)
lib(GGally)
lib(skimr)
lib(e1071)
lib(mdatools)
lib(plotly)
lib(reticulate)
lib(tensorflow)
lib(keras)



# --- Setup for TensorFlow and Keras ---
Sys.setenv("RETICULATE_PYTHON" = "C:/m/envs/tf_cpu_only") # Change this path to where your Conda TensorFlow environment is located.
Sys.getenv("RETICULATE_PYTHON")

# Test to see if  TensorFlow is working in R
a <- tf$Variable(5.56)
b <- tf$Variable(2.7)
a + b

k_clear_session() 

}   

# Load the raw spectra
load("W:\\ALL_USR\\JRW\\SIDT\\Hake Data 2019\\Hake_spectra_2019.sg.iPLS.RData")
load("W:\\ALL_USR\\JRW\\SIDT\\Hake Data 2019\\Hake_TMA_2019.RData")

# Load saved model files (This could be intermediary results from a saved RData file of a model that is still running. )
load('Hake_2019_Rdm_model_20_1_Apr_2023_09_26_40.RData')


# Split the data into folds, spitting the remainder of an un-even division into the first folds, one otie per fold until finished
set.seed(Seed_Data)
num_folds <- 10
index_org <- 1:nrow(Hake_spectra_2019.sg.iPLS)
(fold_size_min <- floor(length(index_org)/num_folds))
(num_extra <- num_folds * dec(length(index_org)/num_folds))
index <- index_org
folds_index <- list()
for(i in 1:(num_folds - 1)) {
print(c(fold_size_min, i, num_extra, i <= num_extra, fold_size_min + ifelse(i <= num_extra, 1, 0), i - num_extra))
   folds_index[[i]] <- sample(index, fold_size_min + ifelse(i < (num_extra + 0.1), 1, 0))  # Finite math - grr!
   index <- index[!index %in% folds_index[[i]]]
}
folds_index[[num_folds]] <- index

lapply(folds_index, length)
c(sum(unlist(lapply(folds_index, length))), length(index_org))  # Check that the number of oties is the same


Rdm_reps <- length(Rdm_folds_index)
y.fold.test.pred_RDM <- NULL
for (j in 1:Rdm_reps) {

   folds_index <- Rdm_folds_index[[j]]
   Fold_models <- Rdm_models[[j]]
   
   y.fold.test.pred.ALL <- NULL
   for (i in 1:length(Fold_models)) {
      x.fold.test <- as.matrix(1000 * Hake_spectra_2019.sg.iPLS[folds_index[[i]], ])
      y.fold.test.pred <- as.vector(predict(unserialize_model(Fold_models[[i]], custom_objects = NULL, compile = TRUE), x.fold.test))
      print(c(length(folds_index[[i]]), length(y.fold.test.pred)))
      y.fold.test.pred.ALL <- rbind(y.fold.test.pred.ALL, cbind(Index = folds_index[[i]], y.test.fold.pred = y.fold.test.pred))
   }
   
   y.test.pred <- sort.f(data.frame(y.fold.test.pred.ALL))[, 2]  # Sort on the Index to match back to the order of the full Hake_TMA_2019 and Hake_spectra_2019.sg.iPLS
   
   y.fold.test.pred_RDM <- rbind(y.fold.test.pred_RDM, y.test.pred)
   
   dev.new(width = 11, height = 8)
   agreementFigure(Hake_TMA_2019, y.test.pred, Delta = -0.05, full = TRUE, main = paste0("Random Rep = ", j)) # Delta is a previous estimate or guess for now
   
   # Full figure only needed for a long-lived species like Sablefish
   # dev.new(width = 11, height = 8)
   # agreementFigure(Hake_TMA_2019, y.test.pred, Delta = -0.25, full = FALSE, main = paste0("Random Rep = ", j))
  
}


# ----------------------- Median over all Rdm_reps Models ------------------------
Delta <- -0.05  # Previous estimate or guess
y.fold.test.pred_RDM_median <- apply(y.fold.test.pred_RDM, 2, median)
c(Delta = Delta, Correlation_R_squared_RMSE_MAE_SAD(Hake_TMA_2019, round(y.fold.test.pred_RDM_median + Delta)))

      Delta Correlation   R_squared        RMSE         MAE         SAD 
  -0.050000    0.959373    0.920396    0.776363    0.370719  866.000000 

  

# What is the best Delta (by SAD, with ties broken by RMSE) on the median over all, Rdm_reps,  full k-folds 
for (Delta. in seq(0, -0.45, by  = -0.05)) {
  cat("\n\n")
  print(c(Delta = Delta., Correlation_R_squared_RMSE_MAE_SAD(Hake_TMA_2019, round(y.fold.test.pred_RDM_median + Delta.))))
  }
  
      Delta Correlation   R_squared        RMSE         MAE         SAD 
   0.000000    0.959358    0.920368    0.772494    0.369007  862.000000 

 
dev.new(width = 11, height = 8)
agreementFigure(Hake_TMA_2019, y.fold.test.pred_RDM_median, Delta = 0.0, full = TRUE, main = paste0("Median over ", Rdm_reps, ' Full k-Fold Models'), cex = 1.25)   
  

# Apply that best Delta to all Rdm_reps models individually
Delta <- 0.0
Stats_RDM_median_by_model <- NULL
for(numRdmModels in 1:Rdm_reps) {

   y.fold.test.pred_RDM_median <- apply(y.fold.test.pred_RDM[numRdmModels, ,drop = FALSE], 2, median)
   Stats_RDM_median_by_model <- rbind(Stats_RDM_median_by_model, data.frame(t(Correlation_R_squared_RMSE_MAE_SAD(Hake_TMA_2019, round(y.fold.test.pred_RDM_median + Delta)))))
}

Stats_RDM_median_by_model 
 
   

# An additional full k-fold added to the total number of models at each step   
dev.new(width = 11, height = 8)
par(mfrow = c(3,2))  
Delta <- 0.0
Stats_RDM_median_by_model_added <- NULL
for(numRdmModels in 1:Rdm_reps) {

   y.fold.test.pred_RDM_median <- apply(y.fold.test.pred_RDM[1:numRdmModels, ,drop = FALSE], 2, median)
   Stats_RDM_median_by_model_added  <- rbind(Stats_RDM_median_by_model_added, data.frame(t(Correlation_R_squared_RMSE_MAE_SAD(Hake_TMA_2019, round(y.fold.test.pred_RDM_median + Delta)))))
}

Stats_RDM_median_by_model_added

min.stats <- apply(Stats_RDM_median_by_model_added[, c(3,5)], 2, min)
minAdj <- sweep(data.matrix(Stats_RDM_median_by_model_added[, c(3,5)]), 2, min.stats)
max.of.Adj <- apply(minAdj, 2, max)
(Stats_0_1_interval <- cbind(Stats_RDM_median_by_model_added[,1:2], t(t(minAdj)/max.of.Adj)))

matplot(1:Rdm_reps, Stats_0_1_interval, type = 'o', col = c(1:3,6), xlab = 'Number of Complete Folds', ylab = 'Various Stats', main = 'Original Order')
 
# Add 5 more Randomized order figures
set.seed(c(Seed_Main, 747)[2]) 
(Seed_reps <- round(runif(6, 0, 1e8)))

for (i in 1:5) {
   set.seed(Seed_reps[i])
   (Rdm_Vec <- sample(1:Rdm_reps)) 
   Stats_RDM_median_by_model_added <- NULL
   for(numRdmModels in 1:Rdm_reps) {
   
      y.fold.test.pred_RDM_median <- apply(y.fold.test.pred_RDM[Rdm_Vec[1:numRdmModels], ,drop = FALSE], 2, median)
      Stats_RDM_median_by_model_added  <- rbind(Stats_RDM_median_by_model_added, data.frame(t(Correlation_R_squared_RMSE_MAE_SAD(Hake_TMA_2019, round(y.fold.test.pred_RDM_median + Delta)))))
   }
     
   min.stats <- apply(Stats_RDM_median_by_model_added[, c(3,5)], 2, min)
   minAdj <- sweep(data.matrix(Stats_RDM_median_by_model_added[, c(3,5)]), 2, min.stats)
   max.of.Adj <- apply(minAdj, 2, max)
   (Stats_0_1_interval <- cbind(Stats_RDM_median_by_model_added[,1:2], t(t(minAdj)/max.of.Adj)))
        
   matplot(1:Rdm_reps, Stats_0_1_interval, type = 'o', col = c(1:3,6), xlab = 'Number of Complete Folds', ylab = 'Various Stats', main = 'Randomized Order')
}
 
  
