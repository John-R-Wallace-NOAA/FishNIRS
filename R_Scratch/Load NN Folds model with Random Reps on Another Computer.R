
# Change path as needed
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



# Set the working directory and load the iPLS data
setwd("W:/ALL_USR/JRW/SIDT/Sablefish/Keras_CNN_Models/Office PC with GPU")
base::load('Sable_Spectra_2017_2019.sg.iPLS.RData')
base::load('Sable_TMA_2017_2019.RData')

# Removing the crystallized otie
Sable_Spectra_2017_2019.sg.iPLS <- Sable_Spectra_2017_2019.sg.iPLS[-78, ]
Sable_TMA_2017_2019 <- Sable_TMA_2017_2019[-78]


# Split the data into folds, spitting the remainder of an un-even division into the first folds, one otie per fold until finished
set.seed(Seed_Data)
num_folds <- 10
index_org <- 1:nrow(Sable_Spectra_2017_2019.sg.iPLS)
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



# Load saved models
loadName <- 'Sablefish_2017_2019_Rdm_models_14_Mar_2023_01_38_30' # Use the name from the saved model 
load(paste0(loadName, '.RData'))


Delta <- -0.2
Rdm_reps <- length(Rdm_folds_index)
y.fold.test.pred_RDM <- NULL
for (j in 1:Rdm_reps) {

   folds_index <- Rdm_folds_index[[j]]
   Fold_models <- Rdm_models[[j]]
   
   y.fold.test.pred.ALL <- NULL
   for (i in 1:length(Fold_models)) {
      x.fold.test <- as.matrix(1000 * Sable_Spectra_2017_2019.sg.iPLS[folds_index[[i]], ])
      y.fold.test.pred <- as.vector(predict(unserialize_model(Fold_models[[i]], custom_objects = NULL, compile = TRUE), x.fold.test))
      print(c(length(folds_index[[i]]), length(y.fold.test.pred)))
      y.fold.test.pred.ALL <- rbind(y.fold.test.pred.ALL, cbind(Index = folds_index[[i]], y.test.fold.pred = y.fold.test.pred))
   }
   
   y.test.pred <- sort.f(data.frame(y.fold.test.pred.ALL))[, 2]  # Sort on the Index to match back to the order of the full Sable_TMA_2017_2019 and Sable_Spectra_2017_2019.sg.iPLS
   
   y.fold.test.pred_RDM <- rbind(y.fold.test.pred_RDM, y.test.pred)
   
   dev.new()
   agreementFigure(Sable_TMA_2017_2019, y.test.pred, Delta = -0.25, full = TRUE, main = paste0("Random Rep = ", j)) 
   
   # dev.new()
   # agreementFigure(Sable_TMA_2017_2019, y.test.pred, Delta = -0.25, full = FALSE, main = paste0("Random Rep = ", j))
  
}

# ----------------------- Median ------------------------
y.fold.test.pred_RDM_median <- apply(y.fold.test.pred_RDM, 2, median)
Correlation_R_squared_RMSE_MAE_SAD(Sable_TMA_2017_2019, round(y.fold.test.pred_RDM_median + Delta))



for (Delta. in c(0.00, -0.10, -0.20, -0.25, -0.30, -0.40)) {
  cat("\n\n", Delta., "\n")
  print(Correlation_R_squared_RMSE_MAE_SAD(Sable_TMA_2017_2019, round(y.fold.test.pred_RDM_median + Delta.)))
}

 -0.25 
Correlation   R_squared        RMSE         MAE         SAD 
   0.952753    0.907739    3.554810    2.069270 2808.000000 

   
Delta <- -0.25  # Best from above
dev.new()
predicted_observed_plot(Sable_TMA_2017_2019, round(y.fold.test.pred_RDM_median + Delta), xlab = 'Sable_TMA_2017_2019', ylab = 'y.fold.test.pred_RDM_median')

dev.new()
agreementFigure(Sable_TMA_2017_2019, y.fold.test.pred_RDM_median, Delta = Delta, full = TRUE, main = paste0('Random Main Seed = ', Seed_Main, ': Number of Random Reps = ', Rdm_reps),
              ylab = "Median over Random Repeats (then rounded after adding Delta)") 

dev.new()
agreementFigure(Sable_TMA_2017_2019, y.fold.test.pred_RDM_median, Delta = Delta, full = FALSE, main = paste0('Random Main Seed = ', Seed_Main, ': Number of Random Reps = ', Rdm_reps),
              ylab = "Median over Random Repeats (then rounded after adding Delta)") 


# Compare stats with the individual random repeats
for(i in 1:nrow(y.fold.test.pred_RDM)) {

   cat("\n\n", i, "\n")
   for(Delta. in c(-0.20, -0.25, -0.30))
      print(c(Delta = Delta., Correlation_R_squared_RMSE_MAE_SAD(Sable_TMA_2017_2019, round(y.fold.test.pred_RDM[i, ] + Delta.))))
}   


# --------------------------- Mean & sd ---------------------------------------------------------
y.fold.test.pred_RDM_mean <- apply(y.fold.test.pred_RDM, 2, mean)
y.fold.test.pred_RDM_sd <- apply(y.fold.test.pred_RDM, 2, sd)

# SD
dev.new()
plot(Sable_TMA_2017_2019, y.fold.test.pred_RDM_sd )

dev.new(width = 18, height = 12)
plotCI.jrw3(Sable_TMA_2017_2019, y.fold.test.pred_RDM_mean, y.fold.test.pred_RDM_sd, sfrac = 0.005)
abline(0, 1, col = 'grey', lty = 2)


# Mean
for (Delta. in c(0.00, -0.10, -0.20, -0.25, -0.30, -0.35, -0.40)) {
  cat("\n\n", Delta., "\n")
  print(Correlation_R_squared_RMSE_MAE_SAD(Sable_TMA_2017_2019, round(y.fold.test.pred_RDM_mean + Delta.)))
}

Delta <- -0.30 # Best from above
dev.new()
predicted_observed_plot(Sable_TMA_2017_2019, round(y.fold.test.pred_RDM_mean + Delta), xlab = 'Sable_TMA_2017_2019', ylab = 'y.fold.test.pred_RDM_mean')

dev.new()
agreementFigure(Sable_TMA_2017_2019, y.fold.test.pred_RDM_mean, Delta = Delta, full = TRUE, main = paste0('Random Main Seed = ', Seed_Main, ': Number of Random Reps = ', Rdm_reps),
              ylab = "Mean over Random Repeats (then rounded after adding Delta)") 

dev.new()
agreementFigure(Sable_TMA_2017_2019, y.fold.test.pred_RDM_mean, Delta = Delta, full = FALSE, main = paste0('Random Main Seed = ', Seed_Main, ': Number of Random Reps = ', Rdm_reps),
              ylab = "Mean over Random Repeats (then rounded after adding Delta)") 


# Median vs mean
dev.new()
plot(y.fold.test.pred_RDM_mean, y.fold.test.pred_RDM_median)

dev.new(14, 14)
plot(Sable_TMA_2017_2019, y.fold.test.pred_RDM_median, col = 'green')
points(Sable_TMA_2017_2019 + 0.2, y.fold.test.pred_RDM_mean, col = 'red')

dev.new()
plot(Sable_TMA_2017_2019, y.fold.test.pred_RDM_median, col = 'green', ylim = c(0, 15), xlim = c(0, 15))
points(Sable_TMA_2017_2019 + 0.15, y.fold.test.pred_RDM_mean, col = 'red')




# ----------------------- Quantiles ------------------------
y.fold.test.pred_RDM_q_0.25 <- apply(y.fold.test.pred_RDM, 2, quantile, probs = 0.25)
y.fold.test.pred_RDM_q_0.25 <- abs(y.fold.test.pred_RDM_median - y.fold.test.pred_RDM_q_0.25)

y.fold.test.pred_RDM_q_0.75 <- apply(y.fold.test.pred_RDM, 2, quantile, probs = 0.75)
y.fold.test.pred_RDM_q_0.75 <- abs(y.fold.test.pred_RDM_median - y.fold.test.pred_RDM_q_0.75)

dev.new(width = 18, height = 12)
plotCI.jrw3(Sable_TMA_2017_2019, y.fold.test.pred_RDM_median, y.fold.test.pred_RDM_q_0.75, y.fold.test.pred_RDM_q_0.25, sfrac = 0.0025, 
     xlab = 'Sable_TMA_2017_2019', ylab = 'y.fold.test.pred_RDM_median', main = 'Median with Inter-Quartile Range from 25% to 75%')
abline(0, 1, col = 'green', lty = 2)











dev.new(width = 18, height = 12)
N <- 150
plotCI.jrw3(Sable_TMA_2017_2019[1:N], y.fold.test.pred_RDM_median[1:N], y.fold.test.pred_RDM_q_0.75[1:N], y.fold.test.pred_RDM_q_0.25[1:N], sfrac = 0.0025)
abline(0, 1, col = 'grey', lty = 2)



dev.new()
plot(Sable_TMA_2017_2019, y.fold.test.pred_RDM_median)
points(Sable_TMA_2017_2019, y.fold.test.pred_RDM_q_0.025, col= 'green')
points(Sable_TMA_2017_2019, y.fold.test.pred_RDM_q_0.975, col= 'red')


dev.new()
plotCI.jrw3(Sable_TMA_2017_2019[O], y.fold.test.pred_RDM_median[O], y.fold.test.pred_RDM_q_0.975[O], y.fold.test.pred_RDM_q_0.025[O], xlim = c(0, 15), ylim = c(0, 15))



for (Delta. in -(1:4)/10) {
  cat("\n\n", Delta., "\n")
  print(Correlation_R_squared_RMSE_MAE_SAD(Sable_TMA_2017_2019, round(y.fold.test.pred_RDM_median + Delta)))
}


dev.new()
agreementFigure(Sable_TMA_2017_2019, y.fold.test.pred_RDM_median, Delta = -0.2, full = TRUE, main = paste0('Random Main Seed = ', Seed_Main, ': Number of Random Reps = ', Rdm_reps)) 

dev.new()
agreementFigure(Sable_TMA_2017_2019, y.fold.test.pred_RDM_median, Delta = -0.2, full = FALSE, main = paste0('Random Main Seed = ', Seed_Main, ': Number of Random Reps = ', Rdm_reps)) 
 

 
# ---------------------------------- Mode ---------------------------------------
y.fold.test.pred_RDM_Mode <- apply(y.fold.test.pred_RDM, 2, Mode)
Correlation_R_squared_RMSE_MAE_SAD(Sable_TMA_2017_2019, round(y.fold.test.pred_RDM_Mode + Delta))

for (Delta. in c(0.00, -0.10, -0.20, -0.25, -0.30, -0.35, -0.40)) {
  cat("\n\n", Delta., "\n")
  print(Correlation_R_squared_RMSE_MAE_SAD(Sable_TMA_2017_2019, round(y.fold.test.pred_RDM_Mode + Delta)))
}

dev.new()
agreementFigure(Sable_TMA_2017_2019, y.fold.test.pred_RDM_Mode, Delta = -0.2, full = TRUE, main = paste0('Random Main Seed = ', Seed_Main, ': Number of Random Reps = ', Rdm_reps)) 

dev.new()
agreementFigure(Sable_TMA_2017_2019, y.fold.test.pred_RDM_Mode, Delta = -0.2, full = FALSE, main = paste0('Random Main Seed = ', Seed_Main, ': Number of Random Reps = ', Rdm_reps)) 
  
