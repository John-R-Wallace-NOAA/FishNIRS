####################################################################
# --- Initial setup  (The curly brakets with a '###' comment and without indented lines directly below them, are there to enable the hiding of code sections using Notepad++.) ---
#            (For those who dislike Rstudio,  Notepad++ is here: https://notepad-plus-plus.org/  and NppToR that passes R code from Notepad++ to R is here: https://sourceforge.net/projects/npptor/)

# You will need a 'GITHUB_PAT' from GitHub set somewhere in R (If you need help, search the Web how to get one from GitHub.)
# Sys.setenv(GITHUB_PAT = "**************")   # If you set GITHUB_PAT here, uncomment this line. Note, do not share your GITHUB_PAT, nor load it onto GitHib.
Sys.getenv("GITHUB_PAT") 
####################################################################

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
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Cor_R_squared_RMSE_MAE_SAD_APE.R")

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

# Test to see if TensorFlow is working in R and has the correct path
cat("\n\nCheck that Tensorflow as the correct path:", Conda_TF_Eniv, "\n\n"); print(tf_config()); cat("\n")

a <- tf$Variable(5.56)
b <- tf$Variable(2.7)
print(a + b)

k_clear_session()

}  ###

# --- If Model_Spectra.sg.iPLS and TMA_Vector are missing for the current spectra set, create them now ---
{ ###

if(!file.exists(paste0(Spectra_Set, '_Model_Spectra.sg.iPLS.RData'))) {

   Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set, Spectra_Path = Spectra_Path, Max_N_Spectra = 300, Meta_Path = paste0(Spectra_Set, "_NIRS_Scanning_Session_Report.xlsx"),
                                               verbose = verbose, plot = plot, htmlPlotFolder = paste0('Figures/', Spectra_Set, '_Spectra_Sample_of_300'))
											   
  # For testing Read_OPUS_Spectra(): Meta_Path <- Spectra_Path <- NULL; Meta_Add <- TRUE; spectraInterp = 'stats_splinefun_lowess'; excelSheet <- 3; opusReader = 'philippbaumann_opusreader2'; (htmlPlotFolder <- paste0('Figures/', Spectra_Set, '_Spectra_Sample_of_300'))

	
   save(Model_Spectra_Meta, file = paste0(Spectra_Set, '_Model_Spectra_Meta_ALL_GOOD_DATA.RData')) 
   # load(file = paste0(Spectra_Set, '_Model_Spectra_Meta_ALL_GOOD_DATA.RData'))
   
   
   # Print out metadata with missing values - some oties may have been removed by Read_OPUS_Spectra() function if there were majors issues (e.g. too much chipping or tissue).
   metadata <- Model_Spectra_Meta[, -(2:((1:ncol(Model_Spectra_Meta))[names(Model_Spectra_Meta) %in% 'project'] - 1))]
   headTail(metadata, 2)
   
   print(renum(metadata[(is.na(metadata$TMA) | is.na(metadata$structure_weight_dg) | is.na(metadata$Length_prop_max) | is.na(metadata$Weight_prop_max) | is.na(metadata$Depth_prop_max) | is.na(metadata$Latitude_prop_max)), 
                                    c('filenames', 'TMA', 'structure_weight_g', 'Length_cm', 'Weight_kg', 'Sex', 'Depth_m', 'Month', 'Days_into_Year', 'Latitude_dd')]))
	
   #  Remove missing values from (Model_Spectra_Meta
   headTail(Model_Spectra_Meta, 2, 2, 3, 45)
   
   Model_Spectra_Meta <- Model_Spectra_Meta[!(is.na(Model_Spectra_Meta$TMA) | is.na(Model_Spectra_Meta$structure_weight_dg) | is.na(Model_Spectra_Meta$Length_prop_max) 
                              | is.na(Model_Spectra_Meta$Weight_prop_max) | is.na(Model_Spectra_Meta$Depth_prop_max) | is.na(Model_Spectra_Meta$Latitude_prop_max)), ]
   
   headTail(Model_Spectra_Meta, 2, 2, 3, 45)
   
   # Create the spectra only file 
   Model_Spectra <- Model_Spectra_Meta[, 2:((1:ncol(Model_Spectra_Meta))[names(Model_Spectra_Meta) %in% 'project'] - 1)]
   headTail(Model_Spectra)
  
   # Double check for missing data
   print(dim(Model_Spectra))
   print(dim(na.omit(Model_Spectra)))

   
   (TMA_Vector <- Model_Spectra_Meta$TMA)[1:10]
   length(TMA_Vector)
   sum(is.na(Model_Spectra_Meta$TMA))
   
   save(Model_Spectra_Meta, file = paste0(Spectra_Set, '_Model_Spectra_Meta_ALL_GOOD_DATA.RData')) 
   # load(file = paste0(Spectra_Set, '_Model_Spectra_Meta_ALL_GOOD_DATA.RData'))
 
   
   # Savitzky-Golay smoothing    
   { ##
   
   ###################################################################################################################
   ### Perform Savitzky-Golay 1st derivative with 17 point window smoothing 2rd order polynomial fit and visualize ###
   ### Intro: http://127.0.0.1:30354/library/prospectr/doc/prospectr.html
   ###################################################################################################################
   ### NOTE ### If there are multiple years of data, all subsequent transformations should be applied to the whole data set, then re-subset  
      
   Model_Spectra.sg <- data.frame(prospectr::savitzkyGolay(Model_Spectra, m = 1, p = 2, w = 15))
   
   
   ####################################################
   ###  iPLS algorithm in mdatools  ### 
   ####################################################
   
   # Maximum number of components to calculate.
   nComp <- c(10, 15)[2]
   Model_Spectra.iPLS.F <- mdatools::ipls(Model_Spectra.sg, TMA_Vector, glob.ncomp = nComp, center = TRUE, scale = TRUE, cv = 100,
                     int.ncomp = nComp, int.num = nComp, ncomp.selcrit = "min", method = "forward", silent = FALSE)
   
   summary(Model_Spectra.iPLS.F) 
   
   # plot the newly selected spectra regions 
   browsePlot('plot(Model_Spectra.iPLS.F)')
   
   Model_Spectra.iPLS.F$int.selected
   sort(Model_Spectra.iPLS.F$var.selected)
   
   # dev.new()  - With a main title
   # plot(Model_Spectra.iPLS.F, main = NULL)          
   
   # plot predictions before and after selection
   browsePlot('
     par(mfrow = c(2, 1))
     mdatools::plotPredictions(Model_Spectra.iPLS.F$gm) # gm = global PLS model with all variables included
     mdatools::plotPredictions(Model_Spectra.iPLS.F$om) # om = optimized PLS model with selected variables
   ')
   
   browsePlot('mdatools::plotRMSE(Model_Spectra.iPLS.F)')
   
   # RMSE  before and after selection
   
   # Find the ylim outside limits and apply to both figures 
    par(mfrow = c(2, 1))
    mdatools::plotRMSE(Model_Spectra.iPLS.F$gm)
	yMin <- par()$usr[3]
	yMax <- par()$usr[4]
	
    mdatools::plotRMSE(Model_Spectra.iPLS.F$om)
    yMin <- min(par()$usr[3], yMin)
	yMax <- max(par()$usr[4], yMax)
   
   # Use the same ylim for both plots
   browsePlot('
     par(mfrow = c(2, 1))
     mdatools::plotRMSE(Model_Spectra.iPLS.F$gm, ylim = c(yMin, yMax))
     mdatools::plotRMSE(Model_Spectra.iPLS.F$om, ylim = c(yMin, yMax))
   ')
   
   # Select iPLS vars and add metadata wanted
   # (p <- length(Model_Spectra.iPLS.F$var.selected)) # 380 freq selected out of a total of 1140
   
   # Model_Spectra.sg.iPLS <- data.frame(Model_Spectra.sg[, sort(Model_Spectra.iPLS.F$var.selected)], length_prop_max = Model_Spectra_Meta$length_cm/max(Model_Spectra_Meta$length_cm), 
   #                                        structure_weight_dg = 10 * Model_Spectra_Meta$structure_weight_g) # dg = decigram
   
   # Order of columns in Model_Spectra.sg.iPLS needs to be kept consistent for the NN model
   # Commented out the random selection code below so that the number oties is not reduced
   
   # ---- Scans and metadata run----
   Model_Spectra.sg.iPLS <- data.frame(Model_Spectra.sg[, sort(Model_Spectra.iPLS.F$var.selected)], Model_Spectra_Meta[, c('structure_weight_dg', 'Length_prop_max', 'Weight_prop_max', 'Depth_prop_max', 'Latitude_prop_max')]) # dg = decigram
   								   
   save(Model_Spectra.sg.iPLS, file = paste0(Spectra_Set, '_Model_Spectra.sg.iPLS.RData'))
   # save(TMA_Vector, file = paste0(Spectra_Set, '_TMA_Vector.RData'))    
} ##
 
}
 
} ###

# --- NN Model ---
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

# Two Random Model steps for each call to the R sub-process (Calling R from R)
for(Rdm_reps_Iter in  seq(1, 19, by = 2)) {  

   cat("\n\nRdm_reps_Iter =", Rdm_reps_Iter, "\n\n")
   save(Rdm_reps_Iter, file = 'C:/ALL_USR/JRW/SIDT/Train_NN_Model/Rdm_reps_Iter.RData')	

   # Create the local initial file '.Rprofile' which Rgui.exe will run when started
   # The listed default packages do not immediately load. 
   #   They are loaded by the time a prompt is given, but that doesn't happen here, so these packages are loaded explictly.
   shell("echo library(stats); library(graphics); library(grDevices); library(utils); library(datasets) > .Rprofile")
   shell("echo source('NN_Model_TensorFlow_Loop.R') >> .Rprofile")
   shell("echo quit('no',,FALSE) >> .Rprofile")

   # Create run.bat and start it with the 'wait' argument (FYI remove the 'wait' argument for parallel  processing.)
   shell("echo cd C:\\ALL_USR\\JRW\\SIDT\\Train_NN_Model > run.bat")
   shell("echo C:\\R\\R\\bin\\x64\\Rgui.exe --no-save --no-restore --no-site-file --no-environ >> run.bat")
   shell("echo exit >> run.bat")
   shell("start /wait run.bat")
   shell("del run.bat")
}

load("C:\\ALL_USR\\JRW\\SIDT\\Train_NN_Model\\Rdm_model_20.RData")

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

} ###

# --- Find Median over all Rdm_reps Models and create figures ---
{ ###

# Only 2 loads needed to redo this section with new data - the Model_Spectra.sg.iPLS has to, of course, match the Rdm_model and Rdm_folds_index and the Conda eniv needs to be loaded properly using the code above.
# base::load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt_GPU_Machine\\Sable_Combo_2022_FCNN_model_ver_1_5_Rdm_model_21_Dec_2023_08_14_19.RData")
# base::load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt\\Sable_Combo_2022_Model_Spectra.sg.iPLS.RData") 
#    A generic Model_Spectra.sg.iPLS may need metadata added to the scans - compare to the end of SG_Variables_Selected (found within e.g. ...FCNN_model_ver_1_20_Pred_Median_TMA.RData).
#    If the model has a reduced number of otoliths, then the correct Run number's pseudo random number seed has to set.
#    cor(Sable_Combo_2022_NN_Pred_Median_TMA$TMA, TMA_Vector) needs to be 1 (one).


# ----------------------- Put the fitted results for each random reps (Rdm_reps) full fold model into a data frame:  y.fold.test.pred_RDM ------------------------
(Rdm_reps <- length(Rdm_folds_index))

y.fold.test.pred_RDM <- NULL
for (j in 1:Rdm_reps) {

   folds_index <- Rdm_folds_index[[j]]
   Fold_models <- Rdm_models[[j]]
   
   y.fold.test.pred.ALL <- NULL
   for (i in 1:length(Fold_models)) {
      if(verbose)
         cat(paste0("\nRdm_reps ", j, ": Fold_model ", i, "\n"))
      x.fold.test <- as.matrix(1000 * Model_Spectra.sg.iPLS[folds_index[[i]], ])
      y.fold.test.pred <- as.vector(predict(keras::unserialize_model(Fold_models[[i]], custom_objects = NULL, compile = TRUE), x.fold.test))
	  if(verbose)
         print(c(length(folds_index[[i]]), length(y.fold.test.pred)))
      y.fold.test.pred.ALL <- rbind(y.fold.test.pred.ALL, cbind(Index = folds_index[[i]], y.test.fold.pred = y.fold.test.pred))
   }
   
   cat(paste0("\nFold_model ", j, "\n"))
   print(dim(y.fold.test.pred.ALL ))
   
   y.test.pred <- sort.f(data.frame(y.fold.test.pred.ALL))[, 2]  # Sort on the Index to match back to the order of the full TMA_Vector and Model_Spectra.sg.iPLS
   
   y.fold.test.pred_RDM <- rbind(y.fold.test.pred_RDM, y.test.pred)
   
   cat(paste0("\nRdm_reps ", j, "\n"))
   print(dim(y.fold.test.pred_RDM))
   
   # dev.new(width = 11, height = 8)
   # agreementFigure(TMA_Vector, y.test.pred, Delta = -0.05, full = TRUE, main = paste0("Random Rep = ", j))
   
   # if(verbose)
   #   browsePlot('agreementFigure(TMA_Vector, y.test.pred, Delta = -0.05, full = TRUE, main = paste0("Random Rep = ", j))') # Delta is a previous estimate or guess for now
  
   # Full figure only needed for a long-lived species like Sablefish
   # dev.new(width = 11, height = 8)
   # agreementFigure(TMA_Vector, y.test.pred, Delta = -0.25, full = FALSE, main = paste0("Random Rep = ", j))
}


# ----------------------- Median over all Rdm_reps Models ------------------------

# Look to see if all random reps fit well and didn't have an issue with good fitting (only seen in the metadata only models). Also compare the agreement figures.
renum(t(apply(y.fold.test.pred_RDM, 1, function(x) unlist(Cor_R_squared_RMSE_MAE_SAD_APE(x, TMA_Vector)))))

y.fold.test.pred_RDM_median <- apply(y.fold.test.pred_RDM, 2, median)
# y.fold.test.pred_RDM_median <- apply(y.fold.test.pred_RDM[1, , drop = FALSE], 2, median)  # Select only those random runs which show good fits 

if(!exists('Delta'))
   Delta <- -0.05  # Previous estimate or guess
data.frame(Delta = Delta, Cor_R_squared_RMSE_MAE_SAD_APE(TMA_Vector, round(y.fold.test.pred_RDM_median + Delta)))

  
# What is the best Delta (by SAD, with ties broken by RMSE) on the median over all, Rdm_reps, full k-folds 
Delta_Table <- NULL
for (Delta. in seq(0, -0.45, by  = -0.05)) {
  # cat("\n\n")
  # print(c(Delta = Delta., Cor_R_squared_RMSE_MAE_SAD_APE(TMA_Vector, round(y.fold.test.pred_RDM_median + Delta.))))
  Delta_Table <- rbind(Delta_Table, c(Delta = Delta., unlist(Cor_R_squared_RMSE_MAE_SAD_APE(TMA_Vector, round(y.fold.test.pred_RDM_median + Delta.)))))
}
  
print(Delta_Table <- data.frame(Delta_Table)) 
  
# Best Delta from table above
(Delta <- Delta_Table$Delta[order(Delta_Table$SAD, Delta_Table$RMSE)[1]])

SG_Variables_Selected <- names(Model_Spectra.sg.iPLS)
roundingDelta <- Delta

# Do a final save after the best rounding Delta is official found for this spectra set - the best rounding Delta is again tested for in the predict script.
save(Iter, i, j, Cor, CA_diag, SAD, learningRate, layer_dropout_rate, Seed_Fold, Seed_Model, Seed_Main, Rdm_models, 
         Rdm_folds_index, SG_Variables_Selected, roundingDelta, file = paste0(Spectra_Set, '_', model_Name, '_', length(Rdm_folds_index), '_Rdm_model_', timeStamp(), '.RData'))


# Agreement Figures (standard and zoomed) using the best delta from above
# dev.new(width = 11, height = 8) # R plot window version
# agreementFigure(TMA_Vector, y.fold.test.pred_RDM_median, Delta = Delta, full = TRUE, main = paste0("Median over ", Rdm_reps, ' Full k-Fold Models'), cex = 1.25) # R plot window version
# browsePlot('agreementFigure(TMA_Vector, y.fold.test.pred_RDM_median, Delta = Delta, full = TRUE, main = paste0("Median over ", Rdm_reps, " Full k-Fold Models"), cex = 1.25)', file = 'Figures/Sable_2022_Combo_20_Rdm_Final.pdf', pdf = TRUE) # PDF version
browsePlot('agreementFigure(TMA_Vector, y.fold.test.pred_RDM_median, Delta = Delta, full = TRUE, main = paste0("Median over ", Rdm_reps, " Full k-Fold Models"))', file = paste0('Figures/', Spectra_Set, '_', length(Rdm_folds_index), '_Rdm_Final.png'))
browsePlot('agreementFigure(TMA_Vector, y.fold.test.pred_RDM_median, Delta = Delta, full = FALSE, main = paste0("Median over ", Rdm_reps, " Full k-Fold Models"))', file = paste0('Figures/', Spectra_Set, '_', length(Rdm_folds_index), '_Rdm_Final_Zoomed.png'))
                                                                                                                                               

# Apply that best Delta (from above) to all Rdm_reps models individually
Stats_RDM_median_by_model <- NULL
for(numRdmModels in 1:Rdm_reps) {

   y.fold.test.pred_RDM_median_one_rep <- apply(y.fold.test.pred_RDM[numRdmModels, ,drop = FALSE], 2, median)
   Stats_RDM_median_by_model <- rbind(Stats_RDM_median_by_model, data.frame(t(unlist(Cor_R_squared_RMSE_MAE_SAD_APE(TMA_Vector, round(y.fold.test.pred_RDM_median_one_rep + Delta))))))
}

print(Stats_RDM_median_by_model)
 

# An additional full k-fold added to the total number of models at each step in turn
# dev.new(width = 11, height = 8)

{ # matplot of Various stats vs nunber of complete Folds
browsePlot("
par(mfrow = c(3,2))  
# Delta <- -0.05  # Reset Delta if recreating this figure as an old Delta may linger.
Stats_RDM_median_by_model_added <- NULL
for(numRdmModels in 1:Rdm_reps) {

   y.fold.test.pred_RDM_median <- apply(y.fold.test.pred_RDM[1:numRdmModels, ,drop = FALSE], 2, median)
   Stats_RDM_median_by_model_added  <- rbind(Stats_RDM_median_by_model_added, data.frame(t(unlist(Cor_R_squared_RMSE_MAE_SAD_APE(TMA_Vector, round(y.fold.test.pred_RDM_median + Delta))))))
}

print(Stats_RDM_median_by_model_added)

min.stats <- apply(Stats_RDM_median_by_model_added[, c(3,5)], 2, min)
minAdj <- sweep(data.matrix(Stats_RDM_median_by_model_added[, c(3,5)]), 2, min.stats)
max.of.Adj <- apply(minAdj, 2, max)
print(Stats_0_1_interval <- cbind(Stats_RDM_median_by_model_added[, 1:2], t(t(minAdj)/max.of.Adj)))


matplot(1:Rdm_reps, Stats_0_1_interval, type = 'o', col = c(1:3, 6), xlab = 'Number of Complete Folds', ylab = 'Various Stats', main = 'Original Order')
 
# Add 5 more Randomized order figures
set.seed(Seed_Main) 
(Seed_reps <- round(runif(6, 0, 1e8)))

for (i in 1:5) { 
   set.seed(Seed_reps[i])
   (Rdm_Vec <- sample(1:Rdm_reps)) 
   Stats_RDM_median_by_model_added <- NULL
   for(numRdmModels in 1:Rdm_reps) {
   
      y.fold.test.pred_RDM_median <- apply(y.fold.test.pred_RDM[Rdm_Vec[1:numRdmModels], , drop = FALSE], 2, median)
      Stats_RDM_median_by_model_added  <- rbind(Stats_RDM_median_by_model_added, data.frame(t(unlist(Cor_R_squared_RMSE_MAE_SAD_APE(TMA_Vector, round(y.fold.test.pred_RDM_median + Delta))))))
   }
     
   min.stats <- apply(Stats_RDM_median_by_model_added[, c(3,5)], 2, min)
   minAdj <- sweep(data.matrix(Stats_RDM_median_by_model_added[, c(3,5)]), 2, min.stats)
   max.of.Adj <- apply(minAdj, 2, max)
   (Stats_0_1_interval <- cbind(Stats_RDM_median_by_model_added[, 1:2], t(t(minAdj)/max.of.Adj)))
        
   matplot(1:Rdm_reps, Stats_0_1_interval, type = 'o', col = c(1:3, 6), xlab = 'Number of Complete Folds', ylab = 'Various Stats', main = 'Randomized Order')
}
", width = 11, height = 8, file = paste0('Figures/Full_k-fold_models_added_sequentially.png'))
}

# --- NN prediction for each otie in the NN model ---
Pred_median <- r(data.frame(NN_Pred_Median = apply(y.fold.test.pred_RDM, 2, median), 
                            Lower_Quantile_0.025 = apply(y.fold.test.pred_RDM, 2, quantile, probs = 0.025, na.rm = TRUE),
                            Upper_Quantile_0.975 = apply(y.fold.test.pred_RDM, 2, quantile, probs = 0.975, na.rm = TRUE)), 4) 
cat(paste0("\n\n--- Note: The quantiles are a reflection of the NN models precision based on ", length(Rdm_models), " full 10-fold randomized models, not the accuracy to a TMA Age ---\n\n"))   
 
assign(paste0(Spectra_Set, '_NN_Pred_Median_TMA'), data.frame(filenames = fileNames, Pred_median, TMA = TMA_Vector), pos = 1)
save(list = paste0(Spectra_Set, '_NN_Pred_Median_TMA'), file = paste0(Spectra_Set, '_', model_Name, '_', length(Rdm_folds_index), '_Pred_Median_TMA_', timeStamp(), '.RData'))


# This agreementFigure() already produced above
# (y.fold.test.pred_RDM_median <- apply(y.fold.test.pred_RDM, 2, median))[1:10]
# browsePlot('agreementFigure(TMA_Vector, y.fold.test.pred_RDM_median, Delta = Delta, full = TRUE, main = paste0("Median over ", Rdm_reps, " Full k-Fold Models"), cex = 1.25)')


# Copy the spectra set prediction to a generic name and add a rounded prediction by adding the best delta found above
Model_Ages <- get(paste0(Spectra_Set, '_NN_Pred_Median_TMA'))
Model_Ages$Pred_Age_Rounded <- round(Model_Ages$NN_Pred_Median + Delta)
Model_Ages[1:5,]

# -- Plot by sorted difference --
# g <- ggplot(Model_Ages_Sub, aes(jitter(TMA, 1.25), TMA - NN_Pred_Median)) +  
# geom_point() 
# browsePlot('print(g)', file = paste0('Figures/TMA_minus_NN_Pred_vs_TMA.png'))

# Jitter TMA; vertical line for each unique TMA - without standard grid - all the data used for the training (not a subset like below).
xlim <- c(min(Model_Ages$TMA) - 1.25, max(Model_Ages$TMA) + 1.25)   
Model_Ages$TMA_Minus_Pred_Age_Rounded <- Model_Ages$TMA - Model_Ages$Pred_Age_Rounded
browsePlot('set.seed(707); gPlot(Model_Ages, "TMA", "TMA_Minus_Pred_Age_Rounded", ylab = "TMA - Pred_Age_Rounded", xFunc = jitter, ylim = c(-xlim[2], xlim[2]), xlim = xlim,
               grid = FALSE, vertLineEachPoint = TRUE)', file = paste0('Figures/TMA_minus_round_NN_Pred_vs_TMA_Jitter.png')) 

               
# ====== Using a SAMPLE of 100 ages so the figures are not too crowded =====
set.seed(Seed_Fold)
Model_Ages_Sub <- Model_Ages[sample(1:nrow(Model_Ages), 100),  ]  
Model_Ages_Sub$Index <- 1:nrow(Model_Ages_Sub)

# - Plot by order implied by the spectra file names - ggplotly() changes how scale_color_manual() works ?????????????????
cols <- c('green', 'red')
g <- ggplot(Model_Ages_Sub, aes(Index, NN_Pred_Median)) +  
geom_point() +
geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
geom_point(aes(Index, Pred_Age_Rounded, col = cols[1])) + 
geom_point(aes(Index + 0.1, TMA, col = cols[2])) + 
scale_color_manual(labels = c('Rounded Age', 'TMA'), values = cols, name = ' ')
browsePlot('print(g)', file = paste0('Figures/Predicted_Ages_Order_by_File_Names_Subset.png'))

   
# -- Plot by sorted NN predicted ages --
Model_Ages_Sub_Sorted <- sort.f(Model_Ages_Sub, 'NN_Pred_Median') # Sort Model_Ages_Sub by NN_Pred_Median, except for "Index" (see the next line below)
Model_Ages_Sub_Sorted$Index <- sort(Model_Ages_Sub_Sorted$Index)  # Reset Index for graphing
if(verbose) head(Model_Ages_Sub_Sorted, 10)

cols <- c('green', 'red')
g <- ggplot(Model_Ages_Sub_Sorted, aes(Index, NN_Pred_Median)) +  
# xlim(0, 65) + ylim(0, 20) +
geom_point() +
geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
geom_point(aes(Index, Pred_Age_Rounded, col = cols[1])) + 
geom_point(aes(Index + 0.1, TMA, col = cols[2])) + 
scale_color_manual(labels = c('Rounded Age', 'TMA'), values = cols, name = ' ')
browsePlot('print(g)', file = paste0('Figures/Predicted_Ages_Sorted_Subset.png'))


# -- Plot by sorted TMA --
Model_Ages_Sub_Sorted <- sort.f(Model_Ages_Sub, 'TMA') # Sort Model_Ages_Sub by TMA, except for "Index" (see the next line below)
Model_Ages_Sub_Sorted$Index <- sort(Model_Ages_Sub_Sorted$Index)  # Reset Index for graphing
if(verbose) head(Model_Ages_Sub_Sorted, 10)

cols <- c('green', 'red')
g <- ggplot(Model_Ages_Sub_Sorted, aes(Index, NN_Pred_Median)) +  
# xlim(0, 65) + ylim(0, 20) +
geom_point() +
geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
geom_point(aes(Index, TMA, col = cols[2])) + 
geom_point(aes(Index + 0.1, Pred_Age_Rounded, col = cols[1])) + 
scale_color_manual(labels = c('Rounded Age', 'TMA'), values = cols, name = ' ')
browsePlot('print(g)', file = paste0('Figures/TMA_Sorted_Subset.png'))


# How many TMA ages are 3 or under
print(Table(TMA_Vector <= 3)/length(TMA_Vector))

# How many TMA ages are 15 or under
print(Table(TMA_Vector <= 15)/length(TMA_Vector))

# How many TMA ages are 20 or under
print(Table(TMA_Vector <= 20)/length(TMA_Vector))

} 
   
