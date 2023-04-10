
# Change the path TensorFlow as needed
Sys.setenv("RETICULATE_PYTHON" = "C:/Users/John.Wallace/AppData/Local/miniconda3/envs/tf")
Sys.getenv("RETICULATE_PYTHON")


library(tensorflow)
library(keras)
k_clear_session()

# Test TensorFlow environment
a <- tf$Variable(5.56)
b <- tf$Variable(2.7)
a + b

Seed_Data <- c(777, 747, 727, 787, 797)[3]
set.seed(Seed_Data)

Seed_Model <- c(777, 747, 727, 787, 797)[3]
tensorflow::set_random_seed(Seed_Model,  disable_gpu = c(TRUE, FALSE)[1])

lib(reticulate)
lib(tidyverse)
lib(recipes)
lib(rsample)
lib(GGally)
lib(skimr)
lib(e1071)
lib(plotly)

# lib(openxlsx)


sourceFunctionURL <- function (URL,  type = c("function", "script")[1]) {
       " # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() "
       require(httr)
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

#FishNIRS funtion
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/predicted_observed_plot.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/residuals_plot.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Correlation_R_squared_RMSE_MAE_SAD.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Mode.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/agreementFigure.R")



setwd("W:/ALL_USR/JRW/SIDT/Hake Data 2019")
base::load("W:\\ALL_USR\\JRW\\SIDT\\Hake Data 2019\\Hake_spectra_2019.sg.iPLS.RData")
base::load("W:\\ALL_USR\\JRW\\SIDT\\Hake Data 2019\\Hake_TMA_2019.RData")

# Load saved models
loadName <- 'Hake_2019_Rdm_models_1_Apr_2023_09_26_40' # Use the name from the saved model 
base::load(paste0(loadName, '.RData'))


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


Delta <- -0.05 # Previous estimate or guess
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
   
   dev.new()
   agreementFigure(Hake_TMA_2019, y.test.pred, Delta = -0.25, full = TRUE, main = paste0("Random Rep = ", j)) 
   
   # dev.new()
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


 
dev.new()
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
dev.new()
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
 
   
   
   
   
   
   
   
   
   
   
  