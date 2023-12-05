

# ./cpulimit --exe /more_home/h_jwallace/Python/tf_cpu_only/bin/python --limit 1000 &
# ./cpulimit --exe /opt/R/64-bit/R-4.1.2_MKL/lib64/R/bin/exec/R --limit 1000 > /dev/null 2>&1 &

# nice -19 /opt/R/64-bit/R-4.1.2_MKL/bin/R
# <ctrl-z>
# $ jobs -l
# [1]+ ##### Stopped                 R
# $ ./cpulimit --pid 37921 --limit 1000 & 

# echo "source('NN Model.R')" | /opt/R/64-bit/R-4.1.2_MKL/bin/R --no-save


# setwd(ifelse(.Platform$OS.type == 'windows', "C:/ALL_USR/JRW/SIDT/Train_NN_Model", "/more_home/h_jwallace/SIDT/Train_NN_Model")) 
# source('NN Model.R')



# --- Initial setup  (The curly brakets without indented lines directly below are there to enable the hiding of code sections using Notepad++.) ---
#      (For those who dislike Rstudio and others,  Notepad++ is here: https://notepad-plus-plus.org/  and NppToR that passes R code from Notepad++ to R is here: https://sourceforge.net/projects/npptor/)


if(interactive()) 
      setwd(ifelse(.Platform$OS.type == 'windows', "C:/ALL_USR/JRW/SIDT/Predict_NN_Ages", "/more_home/h_jwallace/SIDT/Predict_NN_Ages"))   # Change path to the Spectra Set's .GlobalEnv as needed
if(!interactive())   options(width = 120)      
Spectra_Set <- c("Hake_2019", "Sable_2017_2019", "Sable_Combo_2022")[2]
Spectra_Path <- "New_Scans"    # Put new spectra scans in a separate folder and enter the name of the folder below
Predicted_Ages_Path <- "Predicted_Ages" # The NN predicted ages will go in the path defined below
dir.create(Predicted_Ages_Path, showWarnings = FALSE)
TMA_Ages <- c(TRUE, FALSE)[2] # Are TMA ages available and are they to be used?
verbose <- c(TRUE, FALSE)[1]
plot <- c(TRUE, FALSE)[1]
Max_N_Spectra <- list(50, 200, 'All')[[3]]  # Max number of new spectra to be plotted in the spectra figure. (All spectra in the 'New_Scans' folder will be assigned an age regardless of the number plotted in the figure.)
spectraInterp = c('stats_splinefun_lowess', 'prospectr_resample')[1]
opusReader = c('pierreroudier_opusreader', 'philippbaumann_opusreader2')[2]


# (1) Hake 2019, BMS
if(Spectra_Set == "Hake_2019") {
   shortNameSegments <- c(2, 4) # Segments 2 and 4 of the spectra file name, e.g.: (PACIFIC, HAKE, BMS201906206C, 1191, OD1) => (HAKE, 1191)
   shortNameSuffix <- 'BMS'
   opusReader <- 'pierreroudier_opusreader'
   fineFreqAdj <- 150
}
 
# (2) Sablefish 2017 & 2019, Combo survey
if(Spectra_Set == "Sable_2017_2019") { 
   shortNameSegments <- c(1, 3) # Segments 1 and 3 of the spectra file name, e.g.: (SABLEFISH, COMBO201701203A, 28, OD1) => (SABLEFISH, 28)
   shortNameSuffix <- 'Year'
   yearPosition <- c(6, 9) # e.g. COMBO201701203A => 2017 (Segment used (see above) is: shortNameSegments[1] + 1)
   fineFreqAdj <- 0
   opusReader <- 'pierreroudier_opusreader'
   if(TMA_Ages)
     TMA_Meta <- "C:/ALL_USR/JRW/SIDT/Sablefish/Keras_CNN_Models/Sable_2017_2019 21 Nov 2022.RData"  # If used, change path to the main sepectra/metadata save()'d data frame which contains TMA ages.  Matching done via 'filenames'.
}  

# (3) Sablefish 2022, Combo survey
if(Spectra_Set == "Sable_Combo_2022") {
   shortNameSegments <- c(1,5) # Segments 1 and 3 of the spectra file name, e.g.: (SABLEFISH, COMBO201701203A, 28, OD1) => (SABLEFISH, 28)
   shortNameSuffix <- 'Year'
   yearPosition <- c(6, 9) # e.g. COMBO201701203A => 2017 (Segment used (see above) is: shortNameSegments[1] + 1)
   fineFreqAdj <- 0
   if(TMA_Ages)
     TMA_Meta <- "C:/ALL_USR/JRW/SIDT/Sablefish/Keras_CNN_Models/Sable_2017_2019 21 Nov 2022.RData"  # If used, change path to the main sepectra/metadata save()'d data frame which contains TMA ages.  Matching done via 'filenames'.
}  



# Load functions and packages
{  ###
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
if (any(installed.packages()[, 1] %in% "JRWToolBox"))  {
       library(JRWToolBox)
} else {
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Ls.R")
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
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/browserPlot.R")
}    

# FishNIRS funtion
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly_spectra.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/predicted_observed_plot.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/residuals_plot.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Correlation_R_squared_RMSE_MAE_SAD.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Mode.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/agreementFigure.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/FCNN_Model.R");  FCNN_model_ver_1 <- FCNN_model
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/CNN_model_ver_5.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Read_OPUS_Spectra.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/CNN_model_2D.R")  # Not working yet


lib(lattice)
lib(dplyr)
# remotes::install_github("r-lib/ragg@bc501c9951c5934afa55da6d36cdf03c2705d99f")
# lib(tidyverse)
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
lib(prospectr)
lib(openxlsx)
# install.packages('RcppArmadillo')  # Need to use 4.0 version
lib(RcppArmadillo)



# Configuration failed to find one of freetype2 libpng libtiff-4. Try installing:
#  * deb: libfreetype6-dev, libpng-dev libtiff5-dev libjpeg-dev (Debian, Ubuntu, etc)
#  * rpm: freetype-devel libpng-devel libtiff-devel libjpeg-turbo-devel (Fedora, CentOS, RHEL)
#  * csw: libfreetype_dev libpng16_dev libtiff_dev libjpeg_dev (Solaris)
# If freetype2 libpng libtiff-4 is already installed, check that 'pkg-config' is in your
# PATH and PKG_CONFIG_PATH contains a freetype2 libpng libtiff-4.pc file. If pkg-config
# is unavailable you can set INCLUDE_DIR and LIB_DIR manually via:
# R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
# -------------------------- [ERROR MESSAGE] ---------------------------
# <stdin>:1:22: fatal error: ft2build.h: No such file or directory
# compilation terminated.




# --- Setup for TensorFlow and Keras ---

# You will need a 'GITHUB_PAT' from GitHub set somewhere in R (If you need help, search the Web how to get one from GitHub.)
# Sys.setenv(GITHUB_PAT = "**************")   # If you set GITHUB_PAT here, uncomment this line. Note, do not share your GITHUB_PAT, nor load it onto GitHib.
Sys.getenv("GITHUB_PAT") 
 
#  --- Conda TensorFlow environment ---
Conda_TF_Eniv <- ifelse(.Platform$OS.type == 'windows', "C:/m3/envs/tf", "/more_home/h_jwallace/Python/tf_cpu_only/bin")  # Change this path as needed
Sys.setenv(RETICULATE_PYTHON = Conda_TF_Eniv) 
Sys.getenv("RETICULATE_PYTHON") 


# https://github.com/rstudio/tensorflow/issues/412
# tf$config$threading$set_intra_op_parallelism_threads(1L)
# tf$config$threading$set_intra_op_parallelism_threads(11L)
# cat("\nintra_op_parallelism_threads =", tf$config$threading$get_intra_op_parallelism_threads(), "\n")
# tf$config$threading$set_inter_op_parallelism_threads(11L)
# cat("\nintra_op_parallelism_threads =", tf$config$threading$get_inter_op_parallelism_threads(), "\n\n")
# tf$executing_eagerly()


# https://github.com/rstudio/tensorflow/issues/412
# config <- tf$compat$v1$ConfigProto(intra_op_parallelism_threads = 2L, inter_op_parallelism_threads = 2L)
# session = tf$compat$v1$Session(config=config)
# tf$compat$v1$keras$backend$set_session(session)



# Test to see if  TensorFlow is working in R
a <- tf$Variable(5.56)
b <- tf$Variable(2.7)
a + b

k_clear_session() 

Seed_Fold <- c(777, 747, 727, 787, 797)[3]
Seed_Model <- c(777, 747, 727, 787, 797)[3]


# Pick the NN model to use (CNN_model_2D currently not working.)
model_Name <- c('FCNN_model_ver_1', 'CNN_model_ver_5', 'CNN_model_2D')[1]
 
Disable_GPU <- model_Name == 'FCNN_model_ver_1' # Only using the CPU is faster for the FCNN model but slower for CNN_model_ver_5, at least on Sablefish with data from 2017 and 2019.
tensorflow::set_random_seed(Seed_Model, disable_gpu = Disable_GPU)
} ### 

# ------ NN Model -------
{   ###
# Load the data
load(paste0(Spectra_Set, '_Model_Spectra.sg.iPLS.RData'))
load(paste0(Spectra_Set, '_TMA_Vector.RData')

set.seed(Seed_Fold)
TMA_Tab <- Table(TMA_Vector)
(Low_strata <- sum(TMA_Tab[1:3]))
(Mid_strata <- sum(TMA_Tab[4:20]))
(High_strata <- sum(TMA_Tab[21:length(TMA_Tab)]))
(SaveOutOties <- c(sample(order(TMA_Vector)[1:Low_strata], 5), sample(order(TMA_Vector)[1:Mid_strata], 5), sample(order(TMA_Vector)[1:High_strata], 5)) )  # Save out 15 oties for final model check.

Model_Spectra.sg.iPLS_SaveOutOties <- Model_Spectra.sg.iPLS[SaveOutOties, ]
save(Model_Spectra.sg.iPLS_SaveOutOties, file = paste0(Spectra_Set, '_Model_Spectra.sg.iPLS_SaveOutOties.RData'))

TMA_Vector_SaveOutOties <- TMA_Vector[SaveOutOties]
save(TMA_Vector_SaveOutOties, file = paste0(Spectra_Set, '_TMA_Vector_SaveOutOties.RData'))

dim(Model_Spectra.sg.iPLS)
Model_Spectra.sg.iPLS <- Model_Spectra.sg.iPLS[-(SaveOutOties), ]
dim(Model_Spectra.sg.iPLS)
TMA_Vector <- TMA_Vector[-(SaveOutOties)]

# Use full file names (with '.0' at the end)
fileNames <- dir(path = Spectra_Path)
# fileNames <- get.subs(fileNames, sep = ".")[1, ]
fileNames[1:5]

# These are the oties that are not used in the NN model and are saved out for testing
sort(fileNames[SaveOutOties])
dir.create(paste0(Spectra_Set, '/Sable_Combo_2022_Saved_Out'), showWarnings = FALSE)
file.copy(paste0(Spectra_Path, "/", fileNames[SaveOutOties]), paste0(Spectra_Set, '_Saved_Out'), recursive = TRUE, copy.date = TRUE)


# = = = = = = = = = = = = = = = = = Intial setup to run the NN code between the '= = =' lines = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
   
# Split the data into folds, spitting the remainder of an un-even division into the first folds, one otie per fold until finished
set.seed(Seed_Fold)
num_folds <- 10
index_org <- 1:nrow(Model_Spectra.sg.iPLS)
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

lapply(folds_index, length) # Check the binning result
c(sum(unlist(lapply(folds_index, length))), length(index_org))  # Check that the number of oties is the same


graphics.off()  
dev.new(width = 14, height = 6) #2
dev.new() # 3
dev.new(width = 11, height = 8) # 4
dev.new(width = 11, height = 8) # 5
dev.new(width = 10, height = 10) # 6
dev.new(width = 10, height = 10) # 7



# = = = = = = = = = = = = = = = = = Run the NN code between the '= = =' lines and expect long run times = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    
(Rdm_reps <- ifelse(model_Name == 'FCNN_model_ver_1', 20, 10))
Seed_Main <- 707   # Reducing the number of seeds will be considered later
set.seed(Seed_Main) 
Seed_reps <- sample(1e7, Rdm_reps)

# Start fresh or continue by loading a file with model iterations already finished (see the commented line with an example model file). 
Rdm_models <- list() 
Rdm_folds_index <- list()
# load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish\\Keras_CNN_Models\\Hake_2019_FCNN_10_Rdm_model_1_May_2023_13_34_20.RData") 

file.create('Run_NN_Model_Flag', showWarnings = TRUE) # Stopping the model with this flag is broken by the nested loops, but left for now.

# Note that errors from plot.loess() are trapped by try() and are normal early in the iteration loop since there are not enough data to smooth.
for(j in (length(Rdm_folds_index) + 1):Rdm_reps) {
 
   cat(paste0("\n\nStart of Random Rep = ", j , "\n\n"))

   Seed_Data <- Seed_reps[j]
   num_folds <- 10
   
   # Split the data into folds based on the current seed which is dictated by Seed_Main (see above)
   set.seed(Seed_Data)
   index_org <- 1:nrow(Model_Spectra.sg.iPLS)
   (fold_size_min <- floor(length(index_org)/num_folds))
   (num_extra <- num_folds * dec(length(index_org)/num_folds))
   index <- index_org
   folds_index <- list()
   for(i in 1:(num_folds - 1)) {
      print(c(fold_size_min, i, num_extra, i <= num_extra, fold_size_min + ifelse(i <= num_extra, 1, 0), i - num_extra))
      folds_index[[i]] <- sample(index, fold_size_min + ifelse(i < (num_extra + 0.1), 1, 0))  # Finite math - grr!
      index <- index[!index %in% folds_index[[i]]]
   }
   folds_index[[num_folds]] <- index  # Remainder from the above for() loop goes into the last fold index
   
   lapply(folds_index, length)
   c(sum(unlist(lapply(folds_index, length))), length(index_org))

   
   Fold_models <- list()
   for (i in 1:num_folds) {
   
       Model_Spectra.sg.iPLS.F <- Model_Spectra.sg.iPLS[-folds_index[[i]], ]
       TMA_Vector.F <- TMA_Vector[-folds_index[[i]]]
       
       
       # Split the data into training set (2/3) and test set (1/3)
       set.seed(Seed_Data)
       index <- 1:nrow(Model_Spectra.sg.iPLS.F)
       testindex <- sample(index, trunc(length(index)/3))
       x.test <- 1000 * Model_Spectra.sg.iPLS.F[testindex, ]
       x.train <- 1000 * Model_Spectra.sg.iPLS.F[-testindex, ]
       y.test <- TMA_Vector.F[testindex]
       y.train <- TMA_Vector.F[-testindex]
       
       cat(paste0("\n\nDimension of x.train = ", paste(dim(x.train), collapse = ' '), '\n\n')) #  906 380; 905 380 with crystallized otie removed
         
       # Same learning rate for all models
       learningRate <- c(0.00088, 0.0009)[2]
       
       layer_dropout_rate <- NULL
       # layer_dropout_rate <- 0.2
       
       if(model_Name == 'FCNN_model_ver_1')  model <- FCNN_model_ver_1(layer_dropout_rate = layer_dropout_rate)
       if(model_Name == 'CNN_model_ver_5')  model <- CNN_model_ver_5()
       if(model_Name == 'CNN_model_2D')  model <- CNN_model_2D()
             
       # -- Don't reset Iter, Cor, CA_diag, SAD, or .Random.seed when re-starting the same run ---
       tensorflow::set_random_seed(Seed_Model, disable_gpu = Disable_GPU); Seed_Model  # Trying to this here and above (see the help for: tensorflow::set_random_seed)
       set.seed(Seed_Data); Seed_Data # Re-setting the 'data' seed here to know where the model starts, also the Keras backend needs to cleared and the model reloaded - see above.
       Iter_Num <- 8
       Iter <- 0
       Cor <- RMSE <- CA_diag <- SAD <- saveModels <- NULL
       saveModels_List <- list()
       
       while(file.exists('Run_NN_Model_Flag')) {    # The multiple full fold version breaks the stop by removing the file flag, but it remains for now
       
          # R memory garbage collection
          gc()
          
          # Clear TensorFlow's session
          k_clear_session()
       
          (Iter <- Iter + 1)
          cat(paste0("\n\nRandom Replicates = ", j, ": Fold number = ", i, ": Iter = ", Iter,"\n"))
          
          viewMetrics <- c(TRUE, FALSE)[2]
          
          # config <- tf$compat.v1.ConfigProto(intra_op_parallelism_threads = 2L, inter_op_parallelism_threads = 2L)
          # session <-  tf$Session(config = config)
          # k_set_session(session)
          
          # blas_set_num_threads(4)
          # blas_get_num_procs()
          
          # FCNN model
          if(model_Name == 'FCNN_model_ver_1') {
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
         
          if(model_Name == 'FCNN_model_ver_1' & is.null(layer_dropout_rate))  Delta <- -0.05 # Delta is a previous estimate or guess for now, which varies by species.
          if(model_Name == 'FCNN_model_ver_1' & !is.null(layer_dropout_rate))  Delta <- -0.3
          if(model_Name == 'CNN_model_ver_5')  Delta <- -0.2
          if(model_Name == 'CNN_model_2D')  Delta <- 0
            
          y.test.pred.rd <- round(y.test.pred + Delta)  # Rounding with a added delta  (which is a negative number)
          
          dev.set(4)
          # plot(y.test, y.test.pred)
          # abline(0, 1, col = 'green', lty = 2)
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
          
          print(saveName <- paste0(Spectra_Set, '_', paste(get.subs(model_Name, "_")[-2], collapse = "_"), '_SM_', Seed_Model, '_RI_', j, '_LR_', 
             format(learningRate, sci = FALSE), '_LD_', ifelse(is.null(layer_dropout_rate), 0, layer_dropout_rate), '_It_', length(SAD), 
             '_SAD_', rev(SAD)[1], '_', timeStamp()))
          assign(saveName, serialize_model(model, include_optimizer = TRUE))
          # save(Iter, Cor, CA_diag, SAD, learningRate, layer_dropout_rate, .Random.seed, list = saveName, file = paste0(saveName, '.RData'))
          
          saveModels <- c(saveModels, saveName)
          saveModels_List[[saveName]] <- serialize_model(model, include_optimizer = TRUE)
         
          if(Iter == Iter_Num)
              break
       } # Iter while() loop
       
       if(!file.exists('Run_NN_Model_Flag'))
          break
       
       Iter_Best_Model <- sort.f(data.frame(SAD, RMSE, CA_diag, Iter = 1:Iter_Num), c(1, 3))[1, 4]  # Best model is when SAD is lowest, with ties broken by CA_diag
       # Iter_Best_Model <- sort.f(data.frame(SAD, RMSE, CA_diag, Iter = 1:Iter_Num), c(3, 1))[1, 4]  # Best model is when SAD is lowest, with ties broken by CA_diag
       print(sort.f(data.frame(SAD, RMSE, CA_diag, Iter = 1:Iter_Num), c(1, 3)))
       cat(paste0('\n\nBest_Model Number = ', Iter_Best_Model, '\n\n'))
       
       Fold_models[[i]] <- saveModels_List[[Iter_Best_Model]]
       cat(paste0('\nBest Model Name = ', saveModels[Iter_Best_Model], "\n\n"))
       rm(list = saveModels)
       
       x.fold.test <- as.matrix(1000 * Model_Spectra.sg.iPLS[folds_index[[i]], ])
       y.fold.test <- TMA_Vector[folds_index[[i]]]
       y.fold.test.pred <- predict(unserialize_model(Fold_models[[i]], custom_objects = NULL, compile = TRUE), x.fold.test)
       
       dev.set(6)
       agreementFigure(y.fold.test, y.fold.test.pred, Delta = Delta, full = TRUE, main = paste0("Random Rep = ", j, ": Fold Num = ", i)) 
       
       dev.set(7)
       agreementFigure(y.fold.test, y.fold.test.pred, Delta = Delta, full = FALSE, main = paste0("Random Rep = ", j, ": Fold Num = ", i)) 
   } # j Fold loop
   
   if(!file.exists('Run_NN_Model_Flag'))
          break

   Rdm_models[[j]] <- Fold_models # List of lists being assigned to an element of a list - the best model for each fold (10 or other used) within the jth random rep
   Rdm_folds_index[[j]] <- folds_index # List of vectors being assigned to an element of a list - the index for each fold (10 or other used) within the jth random rep
   
   save(Iter, i, j, Cor, CA_diag, SAD, learningRate, layer_dropout_rate, Seed_Fold, Seed_Model, Seed_Main, Rdm_models, 
         Rdm_folds_index, file = paste0(Spectra_Set, '_', model_Name, '_', length(Rdm_folds_index), '_Rdm_model_', timeStamp(), '.RData'))
   
   x.fold.test.ALL <- NULL
   y.fold.test.ALL <- NULL
   y.fold.test.pred.ALL <- NULL
   for (k in 1:length(Fold_models)) {
      x.fold.test <- as.matrix(1000 * Model_Spectra.sg.iPLS[folds_index[[k]], ])
      x.fold.test.ALL <- rbind(x.fold.test.ALL, x.fold.test)
      y.fold.test.ALL <- c(y.fold.test.ALL, TMA_Vector[folds_index[[k]]])
      print(len(predict(unserialize_model(Fold_models[[k]], custom_objects = NULL, compile = TRUE), x.fold.test)))
      y.fold.test.pred.ALL <- c(y.fold.test.pred.ALL, predict(unserialize_model(Fold_models[[k]], custom_objects = NULL, compile = TRUE), x.fold.test))
   }

   browserPlot('agreementFigure(y.fold.test.ALL, y.fold.test.pred.ALL, Delta = Delta, full = TRUE, main = paste0("Random Rep = ", j)) ')
   
}  # k Random Replicate loop

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

} ###


# Find Median over all Rdm_reps Models and create figures
{ ###
(Rdm_reps <- length(Rdm_folds_index))

y.fold.test.pred_RDM <- NULL
for (j in 1:Rdm_reps) {

   folds_index <- Rdm_folds_index[[j]]
   Fold_models <- Rdm_models[[j]]
   
   y.fold.test.pred.ALL <- NULL
   for (i in 1:length(Fold_models)) {
      x.fold.test <- as.matrix(1000 * Model_Spectra.sg.iPLS[folds_index[[i]], ])
      y.fold.test.pred <- as.vector(predict(unserialize_model(Fold_models[[i]], custom_objects = NULL, compile = TRUE), x.fold.test))
      print(c(length(folds_index[[i]]), length(y.fold.test.pred)))
      y.fold.test.pred.ALL <- rbind(y.fold.test.pred.ALL, cbind(Index = folds_index[[i]], y.test.fold.pred = y.fold.test.pred))
   }
   
   y.test.pred <- sort.f(data.frame(y.fold.test.pred.ALL))[, 2]  # Sort on the Index to match back to the order of the full TMA_Vector and Model_Spectra.sg.iPLS
   
   y.fold.test.pred_RDM <- rbind(y.fold.test.pred_RDM, y.test.pred)
   
   browserPlot('agreementFigure(TMA_Vector, y.test.pred, Delta = -0.05, full = TRUE, main = paste0("Random Rep = ", j))') # Delta is a previous estimate or guess for now
   
   # Full figure only needed for a long-lived species like Sablefish
   # dev.new(width = 11, height = 8)
   # agreementFigure(TMA_Vector, y.test.pred, Delta = -0.25, full = FALSE, main = paste0("Random Rep = ", j))
}

 
} ###
   
