

# --- Initial settings setup ---
{ ### 

if(interactive()) 
      setwd(ifelse(.Platform$OS.type == 'windows', "C:/ALL_USR/JRW/SIDT/Train_NN_Model", "/more_home/h_jwallace/SIDT/Train_NN_Models"))   # Change path to the Spectra Set's .GlobalEnv as needed
if(!interactive())   options(width = 120)      
Spectra_Set <- c("Hake_2019", "Sable_2017_2019", "Sable_Combo_2022", "Sable_Combo_2021", "Sable_Combo_2019", "Sable_Combo_2018", "Sable_Combo_2017", "Sable_Combo_2023")[3] # Defaults for reading in the spectra sets are in the Read_OPUS_Spectra() function.
Spectra_Path <- "Model_Scans"    # Put new spectra scans in a separate folder and enter the name of the folder below
dir.create('Figures', showWarnings = FALSE)
verbose <- c(TRUE, FALSE)[1]
plot <- c(TRUE, FALSE)[1]
# Default number of new spectra to be plotted in spectra figures. (The plot within Read_OPUS_Spectra() is given a different default below). 
# All spectra in the Spectra_Path folder will be assigned an age regardless of the number plotted in the figure.
Max_N_Spectra <- list(50, 200, 'All')[[3]] 
 
print(getwd())
print(Spectra_Set)

}

# --- Load functions and packages ---
{ ###

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
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Ls.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/openwd.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/lib.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/sort.f.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/predicted_observed_plot.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/residuals_plot.R")
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
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/browsePlot.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/gPlot.R")   
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/recode.simple.R")  
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/headTail.R")  
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/factor.f.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Correlation_R_squared_RMSE_MAE_SAD.R")

# FishNIRS funtion
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly_spectra.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/predicted_observed_plot.R")
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Mode.R")
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


# --- Setup for TensorFlow and Keras ---

#  --- Conda TensorFlow environment ---
Conda_TF_Eniv <- ifelse(.Platform$OS.type == 'windows', "C:/m3/envs/tf", "/more_home/h_jwallace/Python/tf_cpu_only/bin")  # Change these paths as needed
Sys.setenv(RETICULATE_PYTHON = Conda_TF_Eniv) 
Sys.getenv("RETICULATE_PYTHON") 

if(.Platform$OS.type != 'windows') {
   # https://github.com/rstudio/tensorflow/issues/412
   config <- tf$compat$v1$ConfigProto(intra_op_parallelism_threads = 2L, inter_op_parallelism_threads = 2L)
   session = tf$compat$v1$Session(config=config)
   tf$compat$v1$keras$backend$set_session(session)
}

# Set pseudo random number seeds
Seed_Fold <- c(777, 747, 727, 787, 797)[3]
set.seed(Seed_Fold)

Seed_Model <- c(777, 747, 727, 787, 797)[3] 

# Pick the NN model to use (CNN_model_2D currently not working.)
model_Name <- c('FCNN_model_ver_1', 'CNN_model_ver_5', 'CNN_model_2D')[1]
 
Disable_GPU <- model_Name == 'FCNN_model_ver_1' # Only using the CPU is faster for the FCNN model but slower for CNN_model_ver_5, at least on Sablefish with data from 2017 and 2019.
# Disable_GPU <- FALSE
cat("\nDisable_GPU =", Disable_GPU, "\n\n")

# !!! Disabling the GPU has to come first for it to work. !!! (Grrrr, finally found this on the Web applied to Python and it is true here also!)
# Use < nvidia-smi -l 5 > within e.g. < conda activate C:/m3/envs/tf > inside of Powershell or Command Prompt to see a more correct percent of GPU utilization then with Task Manager.
tensorflow::set_random_seed(Seed_Model, disable_gpu = Disable_GPU) 

# Test to see if TensorFlow is working in R
 cat("\n\n"); print(tf_config()); cat("\n")

a <- tf$Variable(5.56)
b <- tf$Variable(2.7)
print(a + b)

k_clear_session()

}  ###

# --- NN Model Setup ---
{ ###

# Load the data if needed
base::load(paste0(Spectra_Set, '_Model_Spectra.sg.iPLS.RData')); headTail(Model_Spectra.sg.iPLS, 2, 2, 3, 5)
base::load(paste0(Spectra_Set, '_Model_Spectra_Meta_ALL_GOOD_DATA.RData')); headTail(Model_Spectra_Meta, 2, 2, 3, 46)

# base::load(paste0(Spectra_Set, '_SaveOutOties_Seed_727.RData')); print(length(SaveOutOties))
# Model_Spectra_Meta <- Model_Spectra_Meta[-SaveOutOties, ]; print(dim(Model_Spectra_Meta))

# print(Model_Spectra_Meta[1:3, c(1, (grep('project', names(Model_Spectra_Meta))):ncol(Model_Spectra_Meta))])

TMA_Vector <- Model_Spectra_Meta$TMA; print(length(TMA_Vector))
fileNames = Model_Spectra_Meta$filenames; print(length(fileNames))


# ----- Remove both metadata columns for testing  -----
# Model_Spectra.sg.iPLS$length_prop_max <- Model_Spectra.sg.iPLS$structure_weight_dg <- NULL

# ----- Leave only the fish length for testing  -----
# Model_Spectra.sg.iPLS$structure_weight_dg <- NULL

# ----- Leave only the otie weight for testing  -----
# Model_Spectra.sg.iPLS$length_prop_max <- NULL


#   # --------- Special code to test 'Month_Scaled', 'Depth_m', 'Sex', 'Weight_kg', 'Days_into_Year', and reduced model size in the NN Model with the same scans in Model_Spectra.sg.iPLS ------------------------------
#   base::load("C:\\ALL_USR\\JRW\\SIDT\\Get Otie Info from Data Warehouse\\selectSpAgesFramFeb2024.RData")  # From NWFSC Data Warehouse
#   
#   
#   #     ===>                                                                                                                                                                                              Fish length and Otie Wgt: # SAD: 2050; RMSE: 2.7280
#   # Model_Spectra.sg.iPLS <- match.f(data.frame(Model_Spectra.sg.iPLS, specimen_id = as.character(Model_Spectra_Meta$specimen_id)), selectSpAgesFramFeb2024, "specimen_id", "AgeStr_id", c('Month_Scaled', 'Depth_m', 'Sex', 'Weight_kg')) # Very poor results
#   # Model_Spectra.sg.iPLS <- match.f(data.frame(Model_Spectra.sg.iPLS, specimen_id = as.character(Model_Spectra_Meta$specimen_id)), selectSpAgesFramFeb2024, "specimen_id", "AgeStr_id", c('Month_Scaled', 'Weight_kg', 'Depth_m'))  # SAD: 2029; RMSE: 2.7678
#   Model_Spectra.sg.iPLS <- match.f(data.frame(Model_Spectra.sg.iPLS, specimen_id = as.character(Model_Spectra_Meta$specimen_id)), selectSpAgesFramFeb2024, "specimen_id", "AgeStr_id", c('Weight_kg', 'Depth_m')) # SAD: 2002; RMSE: 2.6742
#   #    Model_Spectra.sg.iPLS <- match.f(data.frame(Model_Spectra.sg.iPLS, filenames = Model_Spectra_Meta$filenames, specimen_id = as.character(Model_Spectra_Meta$specimen_id)), selectSpAgesFramFeb2024, "specimen_id", "AgeStr_id", c('Weight_kg', 'Depth_m', 'Length_cm', 'Age')) # Stratified Random
#   # Model_Spectra.sg.iPLS <- match.f(data.frame(Model_Spectra.sg.iPLS, specimen_id = as.character(Model_Spectra_Meta$specimen_id)), selectSpAgesFramFeb2024, "specimen_id", "AgeStr_id", 'Weight_kg') # SAD: 2088; RMSE: 2.7389
#   # Model_Spectra.sg.iPLS <- match.f(data.frame(Model_Spectra.sg.iPLS, specimen_id = as.character(Model_Spectra_Meta$specimen_id)), selectSpAgesFramFeb2024, "specimen_id", "AgeStr_id", 'Depth_m')  # SAD: 2042; RMSE: 2.7404
#   # Model_Spectra.sg.iPLS <- match.f(data.frame(Model_Spectra.sg.iPLS, specimen_id = as.character(Model_Spectra_Meta$specimen_id)), selectSpAgesFramFeb2024, "specimen_id", "AgeStr_id", 'Month_Scaled')  # SAD: ????; RMSE: ????
#   # Model_Spectra.sg.iPLS <- match.f(data.frame(Model_Spectra.sg.iPLS, specimen_id = as.character(Model_Spectra_Meta$specimen_id)), selectSpAgesFramFeb2024, "specimen_id", "AgeStr_id", c('Weight_kg', 'Depth_m', 'Days_into_Year')) # SAD: 2090; RMSE: 2.8047

#   Model_Spectra.sg.iPLS$specimen_id <- NULL  # specimen_id only needed for the matching above


# Use the best metadata found with the above testing for the production code is saved in the prior section


#  ==== Metadata only model run - no need to load Model_Spectra.sg.iPLS above, has it is created below ====
     # Commented out the random selection code below so that the number oties is not reduced
#  Model_Spectra.sg.iPLS <- Model_Spectra_Meta[, c('structure_weight_dg', 'Length_prop_max', 'Weight_prop_max', 'Depth_prop_max', 'Latitude_prop_max')] # dg = decigram
#  headTail(Model_Spectra.sg.iPLS, 3, 2, 3, 2)


# Check again for missing data
print(dim(Model_Spectra.sg.iPLS))
print(dim(na.omit(Model_Spectra.sg.iPLS)))


################ OLD ###############
# These 3 oties in the metadata were missing from the Data WareHouse for Sablefish 2022: AgeStr_id %in% 102133144:102133146  ????????????????

#  Model_Spectra.sg.iPLS$Month_Scaled[is.na(Model_Spectra.sg.iPLS$Month_Scaled)] <- 6:8/12
#  Model_Spectra.sg.iPLS$Depth_m[is.na(Model_Spectra.sg.iPLS$Depth_m)] <- mean(Model_Spectra.sg.iPLS$Depth_m, na.rm = TRUE)
#  Model_Spectra.sg.iPLS$Weight_kg[is.na(Model_Spectra.sg.iPLS$Weight_kg)] <- mean(Model_Spectra.sg.iPLS$Weight_kg, na.rm = TRUE) 
#  Model_Spectra.sg.iPLS$Sex[is.na(Model_Spectra.sg.iPLS$Sex)] <- c('M','F', 'M')

#  print(dim(na.omit(Model_Spectra.sg.iPLS)))
#  print(headTail(Model_Spectra.sg.iPLS, 3, 2, 3, 5))
################################

#  # --------- Special code cont. -----------
#  if(!is.null(Model_Spectra.sg.iPLS$Sex)) {
#     Model_Spectra.sg.iPLS$Sex_prop_max <- as.numeric(recode.simple(Model_Spectra.sg.iPLS$Sex, data.frame(c('F','M', 'U'), 0:2)))/2  # ** All variables have to be numeric ** 
#     Model_Spectra.sg.iPLS$Sex <- NULL
#  }   
#  
#  if(!is.null(Model_Spectra.sg.iPLS$Depth_m)) {
#     Model_Spectra.sg.iPLS$Depth_prop_max <- (Model_Spectra.sg.iPLS$Depth_m - min(Model_Spectra.sg.iPLS$Depth_m))/(max(Model_Spectra.sg.iPLS$Depth_m) - min(Model_Spectra.sg.iPLS$Depth_m))
#     Model_Spectra.sg.iPLS$Depth_m <- NULL
#  }   
#  
#  if(!is.null(Model_Spectra.sg.iPLS$Weight_kg)) {
#     Model_Spectra.sg.iPLS$Weight_prop_max <- (Model_Spectra.sg.iPLS$Weight_kg - min(Model_Spectra.sg.iPLS$Weight_kg))/(max(Model_Spectra.sg.iPLS$Weight_kg) - min(Model_Spectra.sg.iPLS$Weight_kg))
#     Model_Spectra.sg.iPLS$Weight_kg <- NULL
#  }   
#  
#  if(!is.null(Model_Spectra.sg.iPLS$Days_into_Year)) {
#     Model_Spectra.sg.iPLS$Days_into_Year_prop_max <- (Model_Spectra.sg.iPLS$Days_into_Year - min(Model_Spectra.sg.iPLS$Days_into_Year))/(max(Model_Spectra.sg.iPLS$Days_into_Year) - min(Model_Spectra.sg.iPLS$Days_into_Year))
#      Model_Spectra.sg.iPLS$Days_into_Year <- NULL
#  }   

#  headTail(Model_Spectra.sg.iPLS, 3, 2, 3, 5)

# = = = = = = = = = = = = = = = = = Intial setup = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
   
# Special seed setting for model testing   
Seed_Fold <- 787 # Seed_Fold = 787 for Run 3.  Seed 747 used for Fish_Len_Otie_Wgt_Run_2 .  Using a different seed starting here, to test main run of Sable_2022 with fish length and otie weight (and other metadata runs)
                 #      Seed_Fold = 727 used in the code above and for previous runs (Fish_Len_Otie_Wgt Run 1) of Sable_2022 before 28 Dec 2023

# ------- Reduce model size to see the change in prediction ability ----------------------
set.seed(Seed_Fold) 

# --- Random selection of a reduced number of oties ---
Rdm_Oties <- sample(1:nrow(Model_Spectra.sg.iPLS), 750)  

Model_Spectra.sg.iPLS <- Model_Spectra.sg.iPLS[Rdm_Oties, ]
print(dim(Model_Spectra.sg.iPLS))
headTail(Model_Spectra.sg.iPLS, 3, 2, 3, 5)

# Model_Spectra_Meta <- Model_Spectra_Meta[Rdm_Oties, ]
# print(Model_Spectra_Meta[1:3, c(1, (grep('project', names(Model_Spectra_Meta))):ncol(Model_Spectra_Meta))])

TMA_Vector <- TMA_Vector[Rdm_Oties]; print(length(TMA_Vector))
fileNames = Model_Spectra_Meta$filenames[Rdm_Oties]; print(length(fileNames))
 

# --- Stratified random selection of a reduced number of oties---
#   print(Bin_Num <- Table(factor.f(Model_Spectra.sg.iPLS$Length_cm, breaks = c(0, 25, 45, 65, Inf))))
#   print(Mid_Split <- (500 - Bin_Num[1] - Bin_Num[4])/2)
#   
#   Model_Spectra.sg.iPLS <- rbind(Model_Spectra.sg.iPLS[Model_Spectra.sg.iPLS$Length_cm <= 25 | Model_Spectra.sg.iPLS$Length_cm > 65, ], 
#                                  Model_Spectra.sg.iPLS[Model_Spectra.sg.iPLS$Length_cm > 25 & Model_Spectra.sg.iPLS$Length_cm <= 45, ][sample(1:Bin_Num[2], floor(Mid_Split)), ], 
#                                  Model_Spectra.sg.iPLS[Model_Spectra.sg.iPLS$Length_cm > 45 & Model_Spectra.sg.iPLS$Length_cm <= 65, ][sample(1:Bin_Num[2], ceiling(Mid_Split)), ])
#   print(dim(Model_Spectra.sg.iPLS))
#   
#   TMA_Vector <- Model_Spectra.sg.iPLS$Age
#   fileNames <- Model_Spectra.sg.iPLS$filenames
#   
#   Model_Spectra.sg.iPLS$Length_cm <- NULL
#   Model_Spectra.sg.iPLS$Age <- NULL
#   Model_Spectra.sg.iPLS$filenames <- NULL
#   
#   print(headTail(Model_Spectra.sg.iPLS, 3, 2, 3, 5))

} ###



# --- Setup graphic windows ---
graphics.off()  
dev.new(width = 14, height = 6) #2
dev.new() # 3
dev.new(width = 11, height = 8) # 4
dev.new(width = 11, height = 8) # 5
dev.new(width = 10, height = 10) # 6
dev.new(width = 10, height = 10) # 7


# = = = = = = Pick number of random reps (Rdm_reps), number of folds (num_folds), and iteration number (Iter_Num), then run the NN code to the next '= = =' line and expect long run times = = = = = = = = =
    
load('Rdm_reps_Iter.RData')	
cat("\n\nRdm_reps_Iter =", Rdm_reps_Iter, "\n\n")

Rdm_reps <- Rdm_reps_Iter + 1  # j loop
num_folds <- 10 # i loop    # How many folds work best for metadata only models was checked
Iter_Num <- 8  # Iter while() loop

# (Rdm_reps <- ifelse(model_Name == 'FCNN_model_ver_1', 20, 10))
Seed_Main <- Seed_Fold + 20 # Seed_Fold 747 used for Fish_Len_Otie_Wgt_Run_2. Seed_Main <- 707 used for previous runs of Sable_2022 before 28 Dec 2023  # Reducing the number of seeds will be considered later
set.seed(Seed_Main) 
Seed_reps <- sample(1e7, Rdm_reps)

# Start fresh or continue by loading a file with model iterations already finished (see the commented line with an example model file). 
if(Rdm_reps_Iter == 1) {
   Rdm_models <- list() 
   Rdm_folds_index <- list()
} else {
     base::load(paste0("Rdm_model_", Rdm_reps_Iter - 1, ".RData"))     
}

file.create('Run_NN_Model_Flag', showWarnings = TRUE) # Stopping the model with this flag is broken by the nested loops, but left for now in a hope that it can prehaps be fixed.


# Note that errors from plot.loess() are trapped by try() and are normal early in the iteration loop since there is not enough data to smooth.
for(j in (length(Rdm_folds_index) + 1):Rdm_reps) {
 
   cat(paste0("\n\nStart of Random Rep = ", j , "\n\n"))
  
   Seed_Data <- Seed_reps[j]
   
   # Split the data into folds, spitting the remainder of an un-even division into the first folds, one otie per fold until finished.
   #   The split is based on the current seed which is dictated by Seed_Main (see above).
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
   
   print(lapply(folds_index, length)) # Check the binning result
   print(c(sum(unlist(lapply(folds_index, length))), length(index_org)))

   
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
	   
       Iter <- 0
       Cor <- RMSE <- CA_diag <- SAD <- saveModels <- NULL
       saveModels_List <- list()
       
       while(file.exists('Run_NN_Model_Flag')) {    # The multiple full fold version breaks the "stop by removing the file flag", but it remains for now
       
          # R memory garbage collection
          gc()
          
          # Clear TensorFlow's session
          k_clear_session()
       
          (Iter <- Iter + 1)
		  
          cat(paste0("\n\nRandom Replicates = ", j, ": Fold number = ", i, ": Iter = ", Iter,"\n"))
		  
		  if(Iter > 1) {
		      Iter_Loop_Time_Min <- as.numeric(difftime(Sys.time(), Loop_Start_Time, units = "mins"))
		      cat("\nThe last 500 epochs took", format(Iter_Loop_Time_Min, digits = 4), "minutes.  ")  # 500 epochs is hardwired - see below
              Time_Left_Min <- ((Rdm_reps - j) * num_folds * Iter_Num + (num_folds - i) * Iter_Num + (Iter_Num - Iter + 1)) * Iter_Loop_Time_Min
			  if(Time_Left_Min < 60)  cat("Around", format(Time_Left_Min, digits = 4), "minutes left.\n\n")
			  if(Time_Left_Min >= 60 & Time_Left_Min < 60 * 24)  cat("Around", format(Time_Left_Min/60, digits = 4), "hours left.\n\n")
		      if(Time_Left_Min >= 60 * 24)  cat("Around", format(Time_Left_Min/60/24, digits = 4), "days left.\n\n")
		  } 
		  
		  Loop_Start_Time <- Sys.time()
		  
     
          # config <- tf$compat.v1.ConfigProto(intra_op_parallelism_threads = 2L, inter_op_parallelism_threads = 2L)
          # session <-  tf$Session(config = config)
          # k_set_session(session)
          
          # blas_set_num_threads(4)
          # blas_get_num_procs()
          
          # FCNN model
          if(model_Name == 'FCNN_model_ver_1') {
             x.train.array <- as.matrix(x.train)
			 # callback_tensorboard() writes a log for TensorBoard, which allows you to visualize dynamic graphs of your training and test metrics.
             history <- fit(model, x.train.array, y.train, epochs = 1, batch_size = 32, validation_split = 0.2, verbose = 2, 
                             callbacks = if(file.exists('NN_Verbose_Flag.txt')) list(callback_tensorboard(histogram_freq = 1, profile_batch = 2)) else NULL, view_metrics = FALSE)  
             history <- fit(model, x.train.array, y.train, epochs = 198, batch_size = 32, validation_split = 0.2, verbose = ifelse(file.exists('NN_Verbose_Flag.txt'), 2, 0), view_metrics = ifelse(file.exists('NN_Verbose_Flag.txt'), TRUE, FALSE))
             history <- fit(model, x.train.array, y.train, epochs =   1, batch_size = 32, validation_split = 0.2, verbose = 2, view_metrics = FALSE)
             history <- fit(model, x.train.array, y.train, epochs =  99, batch_size = 32, validation_split = 0.2, verbose = 0, view_metrics = FALSE)
             history <- fit(model, x.train.array, y.train, epochs =   1, batch_size = 32, validation_split = 0.2, verbose = 2, view_metrics = FALSE)
             history <- fit(model, x.train.array, y.train, epochs = 200, batch_size = 32, validation_split = 0.2, verbose = 0, view_metrics = FALSE)
             x.test.array <- as.matrix(x.test)
          }
          
		  viewMetrics <- c(TRUE, FALSE)[2]
		  
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
		  if(file.exists('NN_Verbose_Flag.txt'))
		     browsePlot('print(plot(history))', file = paste0("NN_History_Iter_", Iter, ".png")) # Save NN History figures
              
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
         
          dev.set(5)  # agreementFigure() also prints out the Correlation, R_squared, RMSE, MAE, SAD (Sum of Absolute Differences) 
          agreementFigure(y.test, y.test.pred, Delta, full = FALSE, main = paste0("Random Reps = ", j, ": Fold Num = ", i, ": Iter = ", Iter))
         
          dev.set(2)
          par(mfrow = c(3, 1))
          
          plot(1:length(RMSE), RMSE, col = 'green', type = 'b', ylab = "RMSE (green)", xlab = "Iteration Number")
          abline(h = 4, lty = 2, col ='grey39', lwd = 1.25)
		  
		  if(Iter < 5) 
		     try(plot(1:length(CA_diag), CA_diag, col = 'red', type = 'b', ylab = "Diagonal of Class Agreement (red)", xlab = "Iteration Number"))
		  else
		     try(plot.loess(1:length(CA_diag), CA_diag, col = 'red', line.col = 'deeppink', type = 'b', ylab = "Diagonal of Class Agreement (red)", xlab = "Iteration Number"))
          abline(h = 0.2, lty = 2, col ='grey39', lwd = 1.25)
         
          # Avoiding high SAD values at the beginning, and rarely, during a run.
          SAD_plot <- SAD
          SAD_plot[SAD_plot > 1400] <- NA  # Extreme model runs can, on a very rare occasion, put the value of SAD above 1,400 beyond the initial runs
		  
		  if(Iter < 5) 
		     try(plot(1:length(SAD_plot), SAD_plot, col = 'blue', type = 'b', ylab = "Sum of Absolute Differences (blue)", xlab = "Iteration Number"))
		  else
             try(plot.loess(1:length(SAD_plot), SAD_plot, col = 'blue', line.col = 'dodgerblue', type = 'b', ylab = "Sum of Absolute Differences (blue)", xlab = "Iteration Number"))
          abline(h = 950, lty = 2, col ='grey39', lwd = 1.25)
          
		  # Save all the iteration models until the best one is found
          print(saveName <- paste0(Spectra_Set, '_', paste(get.subs(model_Name, "_")[-2], collapse = "_"), '_SM_', Seed_Model, '_RI_', j, '_LR_', 
             format(learningRate, sci = FALSE), '_LD_', ifelse(is.null(layer_dropout_rate), 0, layer_dropout_rate), '_It_', length(SAD), 
             '_SAD_', rev(SAD)[1], '_', timeStamp()))
          assign(saveName, keras::serialize_model(model, include_optimizer = TRUE))
          # save(Iter, Cor, CA_diag, SAD, learningRate, layer_dropout_rate, .Random.seed, list = saveName, file = paste0(saveName, '.RData')) # For debugging
          
          saveModels <- c(saveModels, saveName)
          saveModels_List[[saveName]] <- keras::serialize_model(model, include_optimizer = TRUE)
         
          if(Iter == Iter_Num)
              break
       } # Iter while() loop
       
       if(!file.exists('Run_NN_Model_Flag'))
          break
       
       Iter_Best_Model <- sort.f(data.frame(SAD, RMSE, CA_diag, Iter = 1:Iter_Num), c(1, 2))[1, 4]  # Best model is when SAD is lowest, with ties broken by RMSE
       print(sort.f(data.frame(SAD, RMSE, CA_diag, Iter = 1:Iter_Num), c(1, 3)))
       cat(paste0('\n\nBest_Model Number = ', Iter_Best_Model, '\n\n'))
       
       Fold_models[[i]] <- saveModels_List[[Iter_Best_Model]]
       cat(paste0('\nBest Model Name = ', saveModels[Iter_Best_Model], "\n\n"))
       rm(list = saveModels)
       
       x.fold.test <- as.matrix(1000 * Model_Spectra.sg.iPLS[folds_index[[i]], ])
       y.fold.test <- TMA_Vector[folds_index[[i]]]
       y.fold.test.pred <- predict(keras::unserialize_model(Fold_models[[i]], custom_objects = NULL, compile = TRUE), x.fold.test)
       
       dev.set(6)
       agreementFigure(y.fold.test, y.fold.test.pred, Delta = Delta, full = TRUE, main = paste0("Random Rep = ", j, ": Fold Num = ", i)) 
       
       dev.set(7)
       agreementFigure(y.fold.test, y.fold.test.pred, Delta = Delta, full = FALSE, main = paste0("Random Rep = ", j, ": Fold Num = ", i)) 
   } # j Fold loop
   
   if(!file.exists('Run_NN_Model_Flag'))
          break

   Rdm_models[[j]] <- Fold_models # List of lists being assigned to an element of a list - the best model for each fold (10 or other used) within the jth random rep
   Rdm_folds_index[[j]] <- folds_index # List of vectors being assigned to an element of a list - the index for each fold (10 or other used) within the jth random rep
  
  
   x.fold.test.ALL <- NULL
   y.fold.test.ALL <- NULL
   y.fold.test.pred.ALL <- NULL
   for (k in 1:length(Fold_models)) {
      x.fold.test <- as.matrix(1000 * Model_Spectra.sg.iPLS[folds_index[[k]], ])
      x.fold.test.ALL <- rbind(x.fold.test.ALL, x.fold.test)
      y.fold.test.ALL <- c(y.fold.test.ALL, TMA_Vector[folds_index[[k]]])
      print(length(predict(keras::unserialize_model(Fold_models[[k]], custom_objects = NULL, compile = TRUE), x.fold.test)))
      y.fold.test.pred.ALL <- c(y.fold.test.pred.ALL, predict(keras::unserialize_model(Fold_models[[k]], custom_objects = NULL, compile = TRUE), x.fold.test))
   }

   browsePlot('agreementFigure(y.fold.test.ALL, y.fold.test.pred.ALL, Delta = Delta, full = TRUE, main = paste0("Random Rep = ", j))')   
   
   
   SG_Variables_Selected <- names(Model_Spectra.sg.iPLS)
   roundingDelta <- Delta  # This Delta is only the previous estimate or guess for now (see above). The best rounding Delta is again tested for in the predict script.
   
   save(Iter, i, j, Cor, CA_diag, SAD, learningRate, layer_dropout_rate, Seed_Fold, Seed_Model, Seed_Main, Rdm_models, 
            Rdm_folds_index, SG_Variables_Selected, roundingDelta, file = paste0("Rdm_model_", length(Rdm_folds_index), ".RData"))
   
   
}  # k Random Replicate loop

# 