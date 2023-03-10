
#  ---- WSL -----
# # # FYI, R Packages are here: /R/x86_64-pc-linux-gnu-library/4.2)
# # 
# # # In Ubuntu running on WSL
# # 
# # conda activate tf-py38
# # 
# # 
# # cd /mnt/c/ALL_USR/JRW/SIDT/Sablefish/Keras_CNN_Models # ************ c Drive for work **********
# # 
# # # setwd("C:/ALL_USR/JRW/SIDT/Sablefish/Keras_CNN_Models")
# # 
# # # Use below to end conda session
# # conda deactivate
# # 
# # R # Start R
# # options(width = 160)
# # if (!any(installed.packages()[, 1] %in% "remotes"))  install.packages('remotes')  
# # 

# # Setup TensorFlow in R
# # path_to_python <- "./home/wallacej/miniconda3/envs/tf-py38/bin/python3.8"
# # virtualenv_create("r-reticulate", python = path_to_python)
# # tensorflow::install_tensorflow(envname = "r-reticulate")



#  ---- Native Windows -----
remotes::install_github("John-R-Wallace-NOAA/JRWToolBox")
library(JRWToolBox)

# install.packages('tensorflow')
# install.packages('reticulate')
# install.packages('keras')
# install.packages('tidyverse')
# install.packages('recipes')
# install.packages('rsample')
# install.packages('GGally')
# install.packages('skimr')
# install.packages('e1071')


Sys.setenv("RETICULATE_PYTHON" = "C:/Users/John.Wallace/AppData/Local/miniconda3/envs/tf")
Sys.getenv("RETICULATE_PYTHON")
library(tensorflow)
library(keras)
k_clear_session()


Seed_Data <- c(777, 747, 727, 787, 797)[3]
set.seed(Seed_Data)

Seed_Model <- c(777, 747, 727, 787, 797)[3]
tensorflow::set_random_seed(Seed_Model,  disable_gpu = c(TRUE, FALSE)[1])

library(reticulate)
library(tidyverse)
library(recipes)
library(rsample)
library(GGally)
library(skimr)
library(e1071)

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


#Toolbox functions
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/openwd.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/lib.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/sort.f.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/predicted_observed_plot.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/residuals_plot.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/R_squared_RMSE_MAE.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/as.num.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/match.f.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Table.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/renum.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/agg.table.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/r.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/gof.R")
# # ... this list needs to be updated

#FishNIRS funtion
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/predicted_observed_plot.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/residuals_plot.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Correlation_R_squared_RMSE_MAE_SAD.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Mode.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/agreementFigure.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/sort.f.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/dec.R")


# https://stackoverflow.com/questions/58982467/how-can-i-maximize-the-gpu-usage-of-tensorflow-2-0-from-r-with-keras-library
# (gpu <- tf$config$experimental$get_visible_devices('GPU')[[1]])
# tf$config$experimental$set_memory_growth(device = gpu, enable = TRUE)

# Other functions - hit <Tab> twice
tf$config$experimental$

# tf$config$experimental$ClusterDeviceFilters              tf$config$experimental$disable_mlir_bridge               
# tf$config$experimental$disable_mlir_graph_optimization   tf$config$experimental$enable_mlir_bridge                
# tf$config$experimental$enable_mlir_graph_optimization    tf$config$experimental$enable_op_determinism             
# tf$config$experimental$enable_tensor_float_32_execution  tf$config$experimental$get_device_details                
# tf$config$experimental$get_device_policy                 tf$config$experimental$get_memory_growth                 
# tf$config$experimental$get_memory_info                   tf$config$experimental$get_memory_usage                  
# tf$config$experimental$get_synchronous_execution         tf$config$experimental$get_virtual_device_configuration  
# tf$config$experimental$get_visible_devices               tf$config$experimental$list_logical_devices              
# tf$config$experimental$list_physical_devices             tf$config$experimental$reset_memory_stats                
# tf$config$experimental$set_device_policy                 tf$config$experimental$set_memory_growth         



# Test TensorFlow
hello <- tf$constant('Hello, TensorFlow!')
tf$print(hello)

zeros <- tf$Variable(tf$zeros(shape(1L)))
tf$print(zeros)

a <- tf$constant(10.1)
b <- tf$constant(32.5)
a + b

a <- tf$Variable(5.56)
b <- tf$Variable(2.7)
a + b

unclass(tf$constant) 

# Run below in PowerShell(PS)/cmd to show NVIDIA info
nvidia-smi

# Run below in PS under a conda activated environment to show Tensor info
(base) PS C:\> conda activate tf_new
(tf_new) tensorboard --bind_all --logdir C:\ALL_USR\JRW\SIDT\Sablefish\Keras_CNN_Models\logs
# Fix profile - need admin
# https://stackoverflow.com/questions/71023977/tensorboard-profiler-failed-to-load-libcupti-is-it-installed-and-accessible
 #Tutorial
    https://tensorflow.rstudio.com/tutorials/quickstart/beginner

# setwd('/mnt/w/ALL_USR/JRW/SIDT/Sablefish')   
# base::load('RData')

# Look at raw data
   base::load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish\\Sable_2017_2019 21 Nov 2022.RData")
   options(digits = 11)
   Sable_2017_2019[1:2, c(1:2, 1153:1184)] # Look at first and last columns
   plotly.Spec(Sable_2017_2019, 'all') # Missing TMA (NA) included as grey lines
   
   Sable_2017_2019_noEx <- Sable_2017_2019[Sable_2017_2019[, '4004'] < 0.7, ]    
   Sable_2017_2019_noEx <- Sable_2017_2019_noEx[!is.na(Sable_2017_2019_noEx$TMA), ]  # 1,358 1,184
   plotly.Spec(Sable_2017_2019, 'all') # No more grey lines
   
   # single Crystallized otie is the 78th one
   Sable_2017_2019_noEx <- renum(Sable_2017_2019_noEx)
   Sable_2017_2019_noEx[Sable_2017_2019_noEx$crystallized == 1, c(1:2, 1153:1184)]


# Set working working directory and load data
setwd("C:/ALL_USR/JRW/SIDT/Sablefish/Keras_CNN_Models")
base::load('Sable_Spectra_2017_2019.sg.iPLS.RData')
base::load('Sable_TMA_2017_2019.RData')

# Removing the crystallized otie
Sable_Spectra_2017_2019.sg.iPLS <- Sable_Spectra_2017_2019.sg.iPLS[-78, ]
Sable_TMA_2017_2019 <- Sable_TMA_2017_2019[-78]




# = = = = = = = = = = = = = = = = = Intially run the code between the '= = =' lines = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
   
# Split the data into folds
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
c(sum(unlist(lapply(folds_index, length))), length(index_org))


gof()  # *** Removing all graphics windows ***
dev.set(2) #2
dev.new() # 3
dev.new(width = 10, height = 4) # 4
dev.new(width = 11, height = 8) # 5
dev.new(width = 10, height = 10) # 6
dev.new(width = 10, height = 10) # 7


# = = = = = = = = = = = = = = = = = Run the code between the '= = =' lines = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    
 
# tensorflow::set_random_seed(Seed_Model, disable_gpu = c(TRUE, FALSE)[1]) 


Rdm_reps <- 5
Seed_Main <- 707
set.seed(Seed_Main) 
Seed_reps <- sample(1e7, Rdm_reps)

Rdm_models <- list() 
Rdm_folds_index <- list()

file.create('Run_NN_Model_Flag', showWarnings = TRUE)

for(j in 1:Rdm_reps) {

   if(!file.exists('Run_NN_Model_Flag'))
       break

   Seed_Data <- Seed_reps[j]
   num_folds <- 10
   
   # Split the data into folds based on the current seed which is dictated by Seed_Main (see above)
   set.seed(Seed_Data)
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
   c(sum(unlist(lapply(folds_index, length))), length(index_org))

   
   Fold_models <- list()
   for (i in 1:num_folds) {
   
       Sable_Spectra_2017_2019.sg.iPLS.F <- Sable_Spectra_2017_2019.sg.iPLS[-folds_index[[i]], ]
       Sable_TMA_2017_2019.F <- Sable_TMA_2017_2019[-folds_index[[i]]]
       
       
       # Split the data into training set (2/3) and test set (1/3)
       set.seed(Seed_Data)
       index <- 1:nrow(Sable_Spectra_2017_2019.sg.iPLS.F)
       testindex <- sample(index, trunc(length(index)/3))
       x.test <- 1000 * Sable_Spectra_2017_2019.sg.iPLS.F[testindex, ]
       x.train <- 1000 * Sable_Spectra_2017_2019.sg.iPLS.F[-testindex, ]
       y.test <- Sable_TMA_2017_2019.F[testindex]
       y.train <- Sable_TMA_2017_2019.F[-testindex]
       
       cat(paste0("\n\nDimension of x.train = ", paste(dim(x.train), collapse = ' '), '\n\n')) #  906 380; 905 380 with crystallized otie removed
       
       # Source the models
       source("C:\\ALL_USR\\JRW\\SIDT\\Sablefish\\Keras_CNN_Models\\FCNN_Model.R")
       
       # Same learning rate for all models
       learningRate <- c(0.00088, 0.0009)[2]
       
       layer_dropout_rate <- NULL
       # layer_dropout_rate <- 0.2
       
       model_Name <- c('FCNN_model', 'CNN_model_ver_5', 'CNN_model_2D')[1]
       
       if(model_Name == 'FCNN_model')  model <- FCNN_model(layer_dropout_rate = layer_dropout_rate)
       if(model_Name == 'CNN_model_ver_5')  model <- CNN_model_ver_5()
       if(model_Name == 'CNN_model_2D')  model <- CNN_model_2D()
       
        
       # ------ Run the model on the training set -----
       
       # Stop this run by removing the 'Run_NN_Model_Flag' file in the Windows working directory. The latest run of epochs will finish smoothly.
       # Likewise in Ubuntu, stop this run by putting R in the background (<ctl - z>), remove the flag in the Linux shell (rm Run_NN_Model_Flag) , and put R back in the foreground with 'fg'.  The latest run of epochs will finish smoothly.
       # The number of old tabs created in the browser reflects the number of iterations completed. (The axis in the different tabs may change as the model improves.)
       # Excessive tabs may crash the browser, delete the old tabs as needed
       
       # Create a TensorBoard callback - below is Python code
       # https://www.tensorflow.org/tensorboard/tensorboard_profiling_keras
       # https://cran.r-project.org/web/packages/keras/vignettes/training_callbacks.html
       #   logs = "logs/" + datetime.now().strftime("%Y%m%d-%H%M%S")
       #   tboard_callback = tf.keras.callbacks.TensorBoard(log_dir = logs, histogram_freq = 1, profile_batch = '500,520')
       
       #   model.fit(ds_train, epochs=2, validation_data=ds_test, callbacks = list(callback_tensorboard(histogram_freq = 1, profile_batch = 2))
         
       # Here is the callbacks arg added to the fit inside of R  
       #   fit(..., callbacks = list(callback_tensorboard(histogram_freq = 1, profile_batch = 2))  # Inside R
       
       # Super-convergence
       # https://towardsdatascience.com/https-medium-com-super-convergence-very-fast-training-of-neural-networks-using-large-learning-rates-decb689b9eb0
       
       
       # -- Don't reset Iter, Cor, CA_diag, SAD, or .Random.seed when re-starting the same run ---
       tensorflow::set_random_seed(Seed_Model,  disable_gpu = TRUE); Seed_Model  # Trying to this here and and above (see the help)
       set.seed(Seed_Data); Seed_Data # Re-setting the 'data' seed here to know where the model starts, also the Keras backend needs to cleared and the model reloaded - see above.
       Iter_Num <- 8
       Iter <- 0
       Cor <- RMSE <- CA_diag <- SAD <- saveModels <- NULL
       saveModels_List <- list()
       
       while(file.exists('Run_NN_Model_Flag')) {
       
          (Iter <- Iter + 1)
          cat(paste0("\n\nRandom Replicates = ", j, ": Fold number = ", i, ": Iter = ", Iter,"\n"))
          
          viewMetrics <- c(TRUE, FALSE)[2]
          
          # FCNN model
          if(model_Name == 'FCNN_model') {
             x.train.array <- as.matrix(x.train)
             history <- fit(model, x.train.array, y.train, epochs = 1, batch_size = 32, validation_split = 0.2, verbose = 2, 
                             #  callbacks = list(callback_tensorboard(histogram_freq = 1, profile_batch = 2)),
                             view_metrics = viewMetrics)
             history <- fit(model, x.train.array, y.train, epochs = 198, batch_size = 32, validation_split = 0.2, verbose = 0, view_metrics = viewMetrics)
             history <- fit(model, x.train.array, y.train, epochs =   1, batch_size = 32, validation_split = 0.2, verbose = 2, view_metrics = viewMetrics)
             history <- fit(model, x.train.array, y.train, epochs =  99, batch_size = 32, validation_split = 0.2, verbose = 0, view_metrics = viewMetrics)
             history <- fit(model, x.train.array, y.train, epochs =   1, batch_size = 32, validation_split = 0.2, verbose = 2, view_metrics = viewMetrics)
             history <- fit(model, x.train.array, y.train, epochs = 200, batch_size = 32, validation_split = 0.2, verbose = 0, view_metrics = viewMetrics)
             x.test.array <- as.matrix(x.test)
          }
          
          # CNN_model ver 1,3,4,5
          if(model_Name == 'CNN_model_ver_5') {
             x.train.array <- array(as.matrix(x.train), c(nrow(x.train), ncol(x.train), 1))
             history <- fit(model, x.train.array, y.train, epochs = 50, batch_size = 32, validation_split = 0.2, verbose = 2, 
                  view_metrics = viewMetrics)
               #  view_metrics = viewMetrics, callbacks = list(callback_tensorboard(histogram_freq = 1, profile_batch = 2))) # profile_batch = c(1, 5)
              x.test.array <- array(as.matrix(x.test), c(nrow(x.test), ncol(x.test), 1))
           }
          
          # CNN_model ver 2 
          # x.train.array <- array(as.matrix(x.train), c(nrow(x.train), ncol(x.train), 1))   
          # history <- fit(model, x.train.array, y.train, epochs =   1, batch_size = 32, validation_split = 0.2, verbose = 2, view_metrics = viewMetrics)
          # history <- fit(model, x.train.array, y.train, epochs =  99, batch_size = 32, validation_split = 0.2, verbose = 0, view_metrics = viewMetrics)
          # history <- fit(model, x.train.array, y.train, epochs =   1, batch_size = 32, validation_split = 0.2, verbose = 2, view_metrics = viewMetrics)
          # history <- fit(model, x.train.array, y.train, epochs = 199, batch_size = 32, validation_split = 0.2, verbose = 0, view_metrics = viewMetrics)
          # x.test.array <- array(as.matrix(x.test), c(nrow(x.test), ncol(x.test), 1))
        
          if(model_Name == 'CNN_model_2D') {
              x.train.array <- array(as.matrix(x.train), c(nrow(x.train), ncol(x.train), 1))
              history <- fit(model, x.train.array, y.train * diag(length(y.train)), epochs = 50, batch_size = 32, validation_split = 0.2, verbose = 2, view_metrics = viewMetrics)
              x.test.array <- array(as.matrix(x.test), c(nrow(x.test), ncol(x.test), 1))
          }  
         
          evaluate(model, x.test.array,  y.test, verbose = 0)
          cat("\n")
          
          print(summary(history))
          
          dev.set(3)
          print(plot(history))
              
          # Predict using the test set; plot, create statistics, and create an agreement table
          y.test.pred <- predict(model, x.test.array)
         
          if(model_Name == 'FCNN_model' & is.null(layer_dropout_rate))  Delta <- -0.2
          if(model_Name == 'FCNN_model' & !is.null(layer_dropout_rate))  Delta <- -0.3
          if(model_Name == 'CNN_model_ver_5')  Delta <- -0.2
          if(model_Name == 'CNN_model_2D')  Delta <- 0
            
          y.test.pred.rd <- round(y.test.pred + Delta)  # Rounding with a subtracted delta (tested 1 (one) time which delta worked best)
          
          dev.set(4)
          # plot(y.test, y.test.pred)
          #E abline(0, 1, col = 'green', lty = 2)
          print(predicted_observed_plot(y.test, y.test.pred, xlab = 'y.test', ylab = 'y.test.pred'))
          
          # SAD vector the Sum of absolute differences plot
          # SAD <- c(SAD, sqrt(sum((y.test - y.test.pred.rd)^2)/(length(y.test) - 1)))  # RMSE
          SAD <- c(SAD, sum(abs(y.test - y.test.pred.rd)))
          
          # Correlation, R_squared, RMSE, MAE, SAD (Sum of Absolute Differences)
          cat("\n\n")
          print(Correlation_R_squared_RMSE_MAE_SAD(y.test, y.test.pred.rd))
          cat("(Prediction has been rounded to the nearest integer)\n")
          
          # Correlation vector for the iterations plot
          Cor <- c(Cor, cor(y.test, y.test.pred))
       
          # RMSE vector for the iterations plot
          RMSE <- c(RMSE, sqrt(mean((y.test - y.test.pred)^2, na.rm = TRUE)))
             
          
          # e1071::classAgreement diagonal
          CA_diag <- c(CA_diag, e1071::classAgreement(Table(y.test.pred.rd, y.test), match.names = FALSE)$diag)
          cat("\nclassAgreement Diagonal =", rev(CA_diag)[1], "\n")
          cat("\n\n")
          # print(e1071::classAgreement(Table(y.test.pred.rd, y.test), match.names = TRUE)$diag)  #  match.names = TRUE option
          
          # Correlation Between Sum of Absolute Differences and the classAgreement diagonal 
          if(length(SAD) >= 10)
          #    cat("\nCorrelation between sum of Absolute Differences and the classAgreement Diagonal =", signif(cor(SAD[5:length(SA)], CA_diag[5:length(CA_diag)]), 6), "\n")
               cat("\nCorrelation between sum of Absolute Differences and the classAgreement Diagonal =", signif(cor(SAD, tail(CA_diag, length(SAD))), 6), "\n")
         
          # dev.new(width = 14, height = 10)
          # agreementFigure(y.test, y.test.pred, Delta, full = TRUE)
         
          dev.set(5)
          agreementFigure(y.test, y.test.pred, Delta, main = paste0("Random Reps = ", j, ": Fold Num = ", i, ": Iter = ", Iter))
         
          dev.set(2)
          par(mfrow = c(3, 1))
          # plot(1:length(Cor), sqrt(Cor), col = 'green', ylim = c(-0.03, 1.03), ylab = "Correlation (green)", xlab = "Iteration Number")
          # abline(h = c(0.2, 0.9), lty = 2, col ='grey39', lwd = 1.25)
          plot(1:length(RMSE), RMSE, col = 'green', type = 'b', ylab = "RMSE (green)", xlab = "Iteration Number")
          abline(h = 4, lty = 2, col ='grey39', lwd = 1.25)
          try(plot.loess(1:length(CA_diag), CA_diag, col = 'red', line.col = 'deeppink', type = 'b', ylab = "Diagonal of Class Agreement (red)", xlab = "Iteration Number"))
          abline(h = 0.2, lty = 2, col ='grey39', lwd = 1.25)
         
          # Avoiding high SAD values at the beginning, and rarely during, a run.
          SAD_plot <- SAD
          SAD_plot[SAD_plot > 1400] <- NA  # Extreme model runs can, on a very rare occasion, put the value of SAD above 1,400 beyond the initial runs
          try(plot.loess(1:length(SAD_plot), SAD_plot, col = 'blue', line.col = 'dodgerblue', type = 'b', ylab = "Sum of Absolute Differences (blue)", xlab = "Iteration Number"))
          abline(h = 950, lty = 2, col ='grey39', lwd = 1.25)
          
          print(saveName <- paste0('Sable_', paste(get.subs(model_Name, "_")[-2], collapse = "_"), '_SM_', Seed_Model, '_SD_', Seed_Data, '_LR_', 
             format(learningRate, sci = FALSE), '_LD_', ifelse(is.null(layer_dropout_rate), 0, layer_dropout_rate), '_It_', length(SAD), 
             '_SAD_', rev(SAD)[1], '_', paste(get.subs(sub('  ', ' ', timestamp(quiet = TRUE)), ' ')[c(4, 3, 6)], collapse = "_")))
          assign(saveName, serialize_model(model, include_optimizer = TRUE))
          # save(Iter, Cor, CA_diag, SAD, learningRate, layer_dropout_rate, .Random.seed, list = saveName, file = paste0(saveName, '.RData'))
          
          saveModels <- c(saveModels, saveName)
          saveModels_List[[saveName]] <- serialize_model(model, include_optimizer = TRUE)
         
          if(Iter == Iter_Num)
              break
       }
       
       Iter_Best_Model <- sort.f(data.frame(SAD, RMSE, CA_diag, Iter = 1:Iter_Num), c(1, 3))[1, 4]  # Best model is when SAD is lowest, with ties broken by CA_diag
       # Iter_Best_Model <- sort.f(data.frame(SAD, RMSE, CA_diag, Iter = 1:Iter_Num), c(3, 1))[1, 4]  # Best model is when SAD is lowest, with ties broken by CA_diag
       print(sort.f(data.frame(SAD, RMSE, CA_diag, Iter = 1:Iter_Num), c(1, 3)))
       cat(paste0('\n\nBest_Model Number = ', Iter_Best_Model, '\n\n'))
       
       Fold_models[[i]] <- saveModels_List[[Iter_Best_Model]]
       cat(paste0('\nBest Model Name = ', saveModels[Iter_Best_Model], "\n\n"))
       rm(list = saveModels)
       
       x.fold.test <- as.matrix(1000 * Sable_Spectra_2017_2019.sg.iPLS[folds_index[[i]], ])
       y.fold.test <- Sable_TMA_2017_2019[folds_index[[i]]]
       y.fold.test.pred <- predict(unserialize_model(Fold_models[[i]], custom_objects = NULL, compile = TRUE),  x.fold.test)
       
       dev.set(6)
       agreementFigure(y.fold.test, y.fold.test.pred, Delta = -0.2, full = TRUE, main = paste0("Random Reps = ", j, ": Fold Num = ", i)) 
       
       dev.set(7)
       agreementFigure(y.fold.test, y.fold.test.pred, Delta = -0.2, full = FALSE, main = paste0("Random Reps = ", j, ": Fold Num = ", i)) 
   } 

   Rdm_models[[j]] <- Fold_models # List of lists
   Rdm_folds_index[[j]] <- folds_index # List vectors
}  



# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
 
y.fold.test.pred_RDM <- NULL
for (j in 1:Rdm_reps) {


   folds_index <- Rdm_folds_index[[j]]
   Fold_models <- Rdm_models[[j]]
   
   x.fold.test.ALL <- NULL
   y.fold.test.ALL <- NULL
   y.fold.test.pred.ALL <- NULL
   for ( i in 1:length(Fold_models)) {
      x.fold.test <- as.matrix(1000 * Sable_Spectra_2017_2019.sg.iPLS[folds_index[[i]], ])
      x.fold.test.ALL <- rbind(x.fold.test.ALL, x.fold.test)
      y.fold.test.ALL <- c(y.fold.test.ALL, Sable_TMA_2017_2019[folds_index[[i]]])
      print(len(predict(unserialize_model(Fold_models[[i]], custom_objects = NULL, compile = TRUE), x.fold.test)))
      y.fold.test.pred.ALL <- c(y.fold.test.pred.ALL, predict(unserialize_model(Fold_models[[i]], custom_objects = NULL, compile = TRUE), x.fold.test))
   }

  dev.new()
  agreementFigure(y.fold.test.ALL, y.fold.test.pred.ALL, Delta = -0.25, full = TRUE) 
  
  dev.new()
  agreementFigure(y.fold.test.ALL, y.fold.test.pred.ALL, Delta = -0.25, full = FALSE, Iter = NA) 

  y.fold.test.pred_RDM <- rbind(y.fold.test.pred_RDM, y.fold.test.pred.ALL)
  
}

y.fold.test.pred_RDM_median <- apply(y.fold.test.pred_RDM, 2, median)

dev.new()
agreementFigure(y.fold.test.ALL, y.fold.test.pred_RDM_median, Delta = -0.25, full = TRUE, main = paste0('Random Main Seed = ', Seed_Main, ': Number of Random Reps = ', Rdm_reps)) 

dev.new()
agreementFigure(y.fold.test.ALL, y.fold.test.pred_RDM_median, Delta = -0.25, full = FALSE, main = paste0('Random Main Seed = ', Seed_Main, ': Number of Random Reps = ', Rdm_reps)) 
  


# ------------------------------------------------------------------------------------------------------------------------------------------------------- 
  
  



y.train.pred <- predict(model, x.train.array)

dev.new()
agreementFigure(y.train, y.train.pred, Delta = 0, full = FALSE)

dev.new()
agreementFigure(y.train, y.train.pred, Delta = 0, full = FALSE, axes_zoomed_limits = 0:70, cex = 0.75)

dev.new()
agreementFigure(y.test, y.test.pred, Delta = 0, full = FALSE, axes_zoomed_limits = 0:70, cex = 0.75)


# Check which delta gives the lowest SAD
for (Delta in seq(0, -0.5, by = -0.1)) {


   # dev.new()
   # agreementFigure(y.test, y.test.pred, Delta = Delta, full = F)
   cat(paste0('Delta = ', Delta, "\n"))
   print(Correlation_R_squared_RMSE_MAE_SAD(y.test, round(y.test.pred + Delta)))
   cat("\n\n")
}



# ------------- Find that metadata for an extreme point on the graph ---------------------

testindex[y.test == 0 & y.test.pred.rd == 7] # 1242

Sable_2017_2019_noEx_no_crystal <- Sable_2017_2019_noEx[-78, ]
dim(Sable_2017_2019_noEx_no_crystal) # 1,357 1,184

length(Sable_TMA_2017_2019) # 1,357

# No issues for 1242 in the testindex (Seed = 747)
Sable_2017_2019_noEx_no_crystal[1242, c(1:2, 1153:1184)]

Sable_TMA_2017_2019[1242] # Age 0 

# ----------------------------------------------------------------------------



# Bias vs variance
# https://curiousily.com/posts/hackers-guide-to-fixing-underfitting-and-overfitting-models/


# Outside the while() interation the same analysis of the model can be run, optionally after the model is restored with unserialize() (see below)

# Predict using the test set, plot, create statistics, and create an agreement table
y.test.pred <- predict(model, as.matrix(x.test))
dev.new(width = 20, height = 8)
plot(y.test, y.test.pred)
abline(0, 1, col = 'green', lty = 2)

# Correlation, R_squared, RMSE, MAE
Correlation_R_squared_RMSE_MAE(y.test, y.test.pred) # Correlation  0.9292393 (2 loops of 1000 with 777 seed),  0.9602671 (3 loops of 1000 with 747 seed)

# Sum of absolute difference
sum(abs(y.test - round(y.test.pred))), "\n") # 1,088

# classAgreement
e1071::classAgreement(Table(round(y.test.pred), y.test), match.names = FALSE) #  $diag 0.3053097, 0.283, 0.3384956
e1071::classAgreement(Table(round(y.test.pred), y.test), match.names = TRUE) # $diag 0.300885, 0.283, 0.3362832

# Missing not included
# options(width = 300)
# Table(round(y.test.pred), y.test)

# Look at and write out an agreement table with missing  ages included
Agreement_Agg <-aggregate(list(N = rep(1, length(y.test))), list(y.test = y.test, y.test.pred = round(y.test.pred)), length)
maxAge <- max(c(y.test, y.test.pred))
Agreement_Table <- expand.grid( y.test.pred = 0:maxAge, y.test = 0:maxAge)
Agreement_Table <- renum(match.f(Agreement_Table, Agreement_Agg, c('y.test', 'y.test.pred'), c('y.test', 'y.test.pred'), 'N'))
Agreement_Table$N[is.na(Agreement_Table$N)] <- 0
options(width = 300)
# agg.table(Agreement_Table)
write.csv(agg.table(Agreement_Table), file = 'Agreement_Table.csv')  # See the table below

dev.new(); predicted_observed_plot(y.test, y.test.pred, zoomMax = 15) 

dev.new(); residuals_plot(y.test, y.test.pred)

summary(lm(y.test.pred ~y.test))


# Save the entire NN models with serialize()/unserialize()
# https://cran.r-project.org/web/packages/keras/vignettes/guide_keras.html
saveName <- 'Sable_Ns1_Nd2_1_Seed_797_LR_00088_1997_21_Feb_2023'
assign(saveName, serialize_model(model, include_optimizer = TRUE))
save(list = saveName, file = paste0(saveName, '.RData'))
save(Iter, Cor, CA_diag, SAD, file = paste0(saveName, '_MD.RData'))


# loadName <- saveName
loadName <- 'Sable_ANN_Sd_797_LR_0.00088_It_1998_21_Feb_2023' 
load(paste0(loadName, '.RData'))
load(paste0(loadName, '_MD.RData'))
model <- unserialize_model(eval(parse(text = loadName,)), custom_objects = NULL, compile = TRUE)
summary(model)
evaluate(model, as.matrix(x.test),  y.test, verbose = 2)


# -----------------------------------------------------------------------------------------------------------------------

# Results for kernel_regularizer only on input
Correlation = 0.9591366

Sum of absolute difference = 970
classAgreement diag 0.3384956


   y.test
      0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 34 35 36 38 39 41 44 47 49 50 51 55 56 57 58 61 63 65 66 67 71
  0   9  3                                                                                                                                                         
  1     63  2                                                                                                                                                      
  2      7  4 11  3                                                                                                                                                
  3      1    42 12  2  4  2     1                                                                                                                                 
  4      1    17 10  6  7  2  2        1                                                                                                                           
  5            4 13  4  9  3  1     4     1                                                                                                                        
  6            1  8  4  7  1  4  3  2  4        1                                                                                                                  
  7               2  2  8  4  4  4  2                                                                                                                              
  8               1  2  7     1  1     1     1                                                                                                                     
  9                  1  1     2  4     2                          1                                                                                                
  10              1     3  2  1  2        1        1     1     1                                                                                                   
  11              1  1     1  1     1     1     1  2        1           1                                                                                          
  12                       1     1           1  1  2  1  1  1     1                                                                                                
  13                          2                             1              1  1                                                                                    
  14                    1  2        1     1                       1                                                                                                
  15                                            1     1  1     2                                                                                                   
  16                                   1                             1                                                                                             
  17                                                                 1                                                                                             
  18                                                              1                                                                                                
  19                                         1        1     2  1  1        2  1  1                                                                                 
  20                                               1  1                                            1                                                               
  21                                                        1           1                 1                                                                        
  22                                                                    1              1                                                                           
  23                                                                                1  1                                                                           
  24                                                                    1  1                                      1                                                
  25                                                              1  1                    1                                                                        
  26                                                  1                                                                                                            
  27                                                                                   1  1  1                                                                     
  28                                                                                            1                                                                  
  30                                                                                                                    1                                          
  31                                                                                                     1                                                         
  32                                                                                                  1        1                                                   
  33                                                                                                                 1                                             
  35                                                                                                                       1                                       
  36                                                                             1                          1                                                      
  37                                                              1                                                                                                
  38                                                     1                                                                                                         
  39                                                                                                     1                    1                                    
  45                                                                                                                                            1                  
  47                                                                                                                                         1                     
  48                                                                                                                       1                                       
  52                                                                                                                             1                                 
  54                                                                                                                                                  1            
  55                                                                                                                                1                             1
  56                                                                                                                                                        1      
  57                                                                                                                                               1           1   
  58                                                                                                                                   1     1                    1
  60                                                                                                                       1                                       
  61                                                                                                                                                     1         
  62                                                                                                                                      1  1                     
  63                                                                                                                                   1                           






















