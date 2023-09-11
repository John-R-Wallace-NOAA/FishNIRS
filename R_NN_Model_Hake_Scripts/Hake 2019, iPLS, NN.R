
# --- Initial setup  (The curly brakets without  indented lines directly below are there to enable the  hiding of code sections using Notepad++.) ---
#            (For those who dislike Rstudio,  Notepad++ is here: https://notepad-plus-plus.org/   and NppToR that passes R code from Notepad++ to R is here: https://sourceforge.net/projects/npptor/)

{
setwd("C:/SIDT/Hake Data 2019") # Change this path as needed.

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
}    

# FishNIRS funtion
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/predicted_observed_plot.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/residuals_plot.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Correlation_R_squared_RMSE_MAE_SAD.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Mode.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/agreementFigure.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/FCNN_model_ver_1.R")
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
lib(prospectr)
# lib(openxlsx)


# --- Setup for TensorFlow and Keras ---

# Sys.setenv("RETICULATE_PYTHON" = "C:/m/envs/tf_cpu_only") # Change this path to where your Conda TensorFlow environment is located.
Sys.setenv("RETICULATE_PYTHON" = "C:/m/envs/tf") 
Sys.getenv("RETICULATE_PYTHON")

# Test to see if  TensorFlow is working in R
a <- tf$Variable(5.56)
b <- tf$Variable(2.7)
a + b

k_clear_session() 

Seed_Fold <- c(777, 747, 727, 787, 797)[3]
set.seed(Seed_Fold)

Seed_Model <- c(777, 747, 727, 787, 797)[3]


# Pick the NN model to use (CNN_model_2D currently not working.)
model_Name <- c('FCNN_model_ver_1', 'CNN_model_ver_5', 'CNN_model_2D')[1]
 
Disable_GPU <- model_Name == 'FCNN_model_ver_1' # Only using the CPU is faster for the FCNN model but slower for CNN_model_ver_5, at least on Sablefish with data from 2017 and 2019.
tensorflow::set_random_seed(Seed_Model, disable_gpu = Disable_GPU)
}   

# --- Load and look at the raw spectra and metadata  ---
{
# See 'Read OPUS Hake 2019 Spectra into R.R' in this repo for how to read OPUS files into R using Pierre Roudier's 'opusreader' R package.
load("hake_all_2019.8.10 ORG.RData") 

# Look at the data and metadata
hake_all_2019.8.10[1:4, c(1:3, 1110:1158)]

hake_all_2019.8.10$crystallized <- as.logical(hake_all_2019.8.10$crystallized)
hake_all_2019.8.10$unscannable <- as.logical(hake_all_2019.8.10$unscannable)
names(hake_all_2019.8.10)[names(hake_all_2019.8.10) %in% 'Age'] <- "TMA"
hake_all_2019.8.10$shortName <- apply(hake_all_2019.8.10[, 'filenames', drop = FALSE], 1, function(x) paste(get.subs(x, sep = "_")[c(2,4)], collapse = "_"))

hake_all_2019.8.10[1:4, c(1:3, 1110:1159)]

dim(hake_all_2019.8.10)
hake_all_2019.8.10 <- hake_all_2019.8.10[!(hake_all_2019.8.10$crystallized | hake_all_2019.8.10$unscannable), ]
dim(hake_all_2019.8.10)

# Look at the data with plotly and remove rogue otie
plotly.Spec(hake_all_2019.8.10, 'all')
hake_all_2019.8.10 <- hake_all_2019.8.10[!hake_all_2019.8.10$shortName %in% 'HAKE_48', ]
plotly.Spec(hake_all_2019.8.10, 'all')



# Spectra only 
Hake_spectra_2019 <- hake_all_2019.8.10[, 2:1113]

# TMA only
Hake_TMA_2019 <- hake_all_2019.8.10$TMA

}

# Savitzky-Golay smoothing    
{

###################################################################################################################
### Perform Savitzky-Golay 1st derivative with 17 point window smoothing 2rd order polynomial fit and visualize ###
### Intro: http://127.0.0.1:30354/library/prospectr/doc/prospectr.html
###################################################################################################################
### NOTE ### If there are multiple years of data, all subsequent transformations should be applied to the whole data set, then re-subset  
   
Hake_spectra_2019.sg <- data.frame(prospectr::savitzkyGolay(Hake_spectra_2019, m = 1, p = 2, w = 15))


####################################################
###  iPLS algorithm in mdatools  ### 
####################################################


# Maximum number of components to calculate.
nComp <- c(10, 15)[2]
 
Hake_spectra_2019.iPLS.F <- mdatools::ipls(Hake_spectra_2019.sg, Hake_TMA_2019, glob.ncomp = nComp, center = TRUE, scale = TRUE, cv = 100,
                  int.ncomp = nComp, int.num = nComp, ncomp.selcrit = "min", method = "forward", silent = FALSE)

summary(Hake_spectra_2019.iPLS.F) 

iPLS variable selection results
  Method: forward
  Validation: random with 100 segments
  Number of intervals: 15
  Number of selected intervals: 4
  RMSECV for global model: 0.801053 (15 LVs)
  RMSECV for optimized model: 0.753910 (15 LVs)

Summary for selection procedure:
   n start  end selected nComp      RMSE    R2
1  0     1 1098    FALSE    15 0.8010530 0.914
2 14   953 1025     TRUE    15 0.8902166 0.894
3 11   734  806     TRUE    15 0.7908851 0.916
4 15  1026 1098     TRUE    15 0.7627235 0.922
5 10   661  733     TRUE    15 0.7542682 0.924


# plot the newly selected spectra regions 
dev.new()
plot(Hake_spectra_2019.iPLS.F)     
Hake_spectra_2019.iPLS.F$int.selected
sort(Hake_spectra_2019.iPLS.F$var.selected)

# dev.new()  - With a main title
# plot(Hake_spectra_2019.iPLS.F, main = NULL)          

# plot predictions before and after selection
dev.new()
par(mfrow = c(2, 1))
mdatools::plotPredictions(Hake_spectra_2019.iPLS.F$gm) # gm = global PLS model with all variables included
mdatools::plotPredictions(Hake_spectra_2019.iPLS.F$om) # om = optimized PLS model with selected variables

dev.new()
mdatools::plotRMSE(Hake_spectra_2019.iPLS.F)


# RMSE  before and after selection

# Visually find the ylim to apply to both figures  and over all areas and WB
dev.new()
par(mfrow = c(2, 1))
mdatools::plotRMSE(Hake_spectra_2019.iPLS.F$gm)
mdatools::plotRMSE(Hake_spectra_2019.iPLS.F$om)

# Use the ylim for both plots
dev.new()
par(mfrow = c(2, 1))
mdatools::plotRMSE(Hake_spectra_2019.iPLS.F$gm, ylim = c(3.4, 11))
mdatools::plotRMSE(Hake_spectra_2019.iPLS.F$om, ylim = c(3.4, 11))



# Select out vars
# (p <- length(Hake_spectra_2019.iPLS.F$var.selected)) # 380 freq selected out of a total of 1140

Hake_spectra_2019.sg.iPLS <- data.frame(Hake_spectra_2019.sg[, sort(Hake_spectra_2019.iPLS.F$var.selected)])

Hake_spectra_2019.Age.sg.iPLS <- data.frame(Age = Hake_TMA_2019, Hake_spectra_2019.sg.iPLS)
dim(Hake_spectra_2019.Age.sg.iPLS) # 

# 2D plot
Hake_spectra_2019.sg.iPLS.PLOT <- cbind(hake_all_2019.8.10[, 1, drop = FALSE], Hake_spectra_2019.sg.iPLS, hake_all_2019.8.10[, 1156:1184])
plotly.Spec(Hake_spectra_2019.sg.iPLS.PLOT, 'all') # 2336  293


# Plot the transformed spectra by age using only variables selected using iPLS
(Hake_spectra_2019.Age.sg.iPLS.Long <- reshape2::melt(Hake_spectra_2019.Age.sg.iPLS, id = 'Age', variable.name = 'Freq', value.name = 'Absorbance'))[1:4, ]
Hake_spectra_2019.Age.sg.iPLS.Long$Freq <- as.numeric(substring(Hake_spectra_2019.Age.sg.iPLS.Long$Freq, 2))
Hake_spectra_2019.Age.sg.iPLS.Long <- sort.f(Hake_spectra_2019.Age.sg.iPLS.Long, 'Freq')
Hake_spectra_2019.Age.sg.iPLS.Agg <- aggregate(list(Absorbance = Hake_spectra_2019.Age.sg.iPLS.Long$Absorbance), 
     list(Freq = Hake_spectra_2019.Age.sg.iPLS.Long$Freq, Age = Hake_spectra_2019.Age.sg.iPLS.Long$Age), mean, na.rm = TRUE)
Hake_spectra_2019.Age.sg.iPLS.Agg$Age <- ordered(Hake_spectra_2019.Age.sg.iPLS.Agg$Age, sort(unique(Hake_spectra_2019.Age.sg.iPLS.Agg$Age)))

plotly::ggplotly(ggplot2::ggplot(data = Hake_spectra_2019.Age.sg.iPLS.Agg, aes(x = Freq, y = Absorbance, z = Age)) + geom_line(aes(colour = Age), size = 0.2) + 
                    scale_color_manual(values=rainbow(length(unique(Hake_spectra_2019.Age.sg.iPLS.Agg$Age)), alpha = 1)))
                 
save(Hake_spectra_2019.sg.iPLS, file = 'Hake_spectra_2019.sg.iPLS.RData')
save('Hake_TMA_2019', file = 'Hake_TMA_2019.RData')                 

                 
                 
# --------------- Try ipls() with smoothed spectra data and metadata  - NO METADATA WAS SELECTED ------------------------

# Remove NA's with predictors and response together - then resplit
Hake_spectra_2019.sg.META <- na.omit(cbind(Hake_spectra_2019.sg, hake_all_2019.8.10[, c("latitude", "longitude", "length", "weight", "sex")], TMA = Hake_TMA_2019))
Ncol <- ncol(Hake_spectra_2019.sg.META)
Hake_spectra_2019.sg.META[1:3, c(1:2, (Ncol - 4):Ncol)]

TMA.META <- Hake_spectra_2019.sg.META[, Ncol]
Hake_spectra_2019.sg.META <- Hake_spectra_2019.sg.META[, -Ncol]
   
Hake_spectra_2019.iPLS.META.F <- mdatools::ipls(Hake_spectra_2019.sg.META, TMA.META, glob.ncomp = nComp, center = TRUE, scale = TRUE, cv = 100,
                  int.ncomp = nComp, int.num = nComp, ncomp.selcrit = "min", method = "forward", silent = FALSE)

summary(Hake_spectra_2019.iPLS.META.F)

# Plot the newly selected spectra regions 
dev.new()
plot(Hake_spectra_2019.iPLS.META.F)     

Hake_spectra_2019.iPLS.META.F$int.selected
sort(Hake_spectra_2019.iPLS.META.F$var.selected)

Hake_spectra_2019.sg.META[, Hake_spectra_2019.iPLS.F$var.selected][1:3, c(1:3, 373:380)]
names(Hake_spectra_2019.sg.META[, Hake_spectra_2019.iPLS.F$var.selected])
    
}

# ------ NN Model -------
{
# Load the data
load('Hake_spectra_2019.sg.iPLS.RData')
load('Hake_TMA_2019.RData')

# = = = = = = = = = = = = = = = = = Intial setup to run the NN code between the '= = =' lines = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
   
# Split the data into folds, spitting the remainder of an un-even division into the first folds, one otie per fold until finished
set.seed(Seed_Fold)
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
   folds_index[[num_folds]] <- index  # Remainder from the above for() loop goes into the last fold index
   
   lapply(folds_index, length)
   c(sum(unlist(lapply(folds_index, length))), length(index_org))

   
   Fold_models <- list()
   for (i in 1:num_folds) {
   
       Hake_spectra_2019.sg.iPLS.F <- Hake_spectra_2019.sg.iPLS[-folds_index[[i]], ]
       Hake_TMA_2019.F <- Hake_TMA_2019[-folds_index[[i]]]
       
       
       # Split the data into training set (2/3) and test set (1/3)
       set.seed(Seed_Data)
       index <- 1:nrow(Hake_spectra_2019.sg.iPLS.F)
       testindex <- sample(index, trunc(length(index)/3))
       x.test <- 1000 * Hake_spectra_2019.sg.iPLS.F[testindex, ]
       x.train <- 1000 * Hake_spectra_2019.sg.iPLS.F[-testindex, ]
       y.test <- Hake_TMA_2019.F[testindex]
       y.train <- Hake_TMA_2019.F[-testindex]
       
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
          
          print(saveName <- paste0('Hake_', paste(get.subs(model_Name, "_")[-2], collapse = "_"), '_SM_', Seed_Model, '_RI_', j, '_LR_', 
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
       
       x.fold.test <- as.matrix(1000 * Hake_spectra_2019.sg.iPLS[folds_index[[i]], ])
       y.fold.test <- Hake_TMA_2019[folds_index[[i]]]
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
         Rdm_folds_index, file = paste0('Hake_2019_', model_Name, '_', k, '_Rdm_model_', timeStamp(), '.RData'))
   
   x.fold.test.ALL <- NULL
   y.fold.test.ALL <- NULL
   y.fold.test.pred.ALL <- NULL
   for (k in 1:length(Fold_models)) {
      x.fold.test <- as.matrix(1000 * Hake_spectra_2019.sg.iPLS[folds_index[[k]], ])
      x.fold.test.ALL <- rbind(x.fold.test.ALL, x.fold.test)
      y.fold.test.ALL <- c(y.fold.test.ALL, Hake_TMA_2019[folds_index[[k]]])
      print(len(predict(unserialize_model(Fold_models[[k]], custom_objects = NULL, compile = TRUE), x.fold.test)))
      y.fold.test.pred.ALL <- c(y.fold.test.pred.ALL, predict(unserialize_model(Fold_models[[k]], custom_objects = NULL, compile = TRUE), x.fold.test))
   }

   dev.new()
   agreementFigure(y.fold.test.ALL, y.fold.test.pred.ALL, Delta = Delta, full = TRUE, main = paste0("Random Rep = ", j)) 
   
   dev.new()
   agreementFigure(y.fold.test.ALL, y.fold.test.pred.ALL, Delta = Delta, full = FALSE, main = paste0("Random Rep = ", j))
   
}  # k Random Replicate loop

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

}


# Find Median over all Rdm_reps Models and create figures
{
(Rdm_reps <- length(Rdm_folds_index))

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
   ...

# Hand entered best Delta from above 
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
Delta <- 0.0 # Redundant, but the previous setting of Delta may have been skipped to recreate this figure only, and an old Delta may linger.
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
 
 
} 
   
