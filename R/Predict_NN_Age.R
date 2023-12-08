
###################################
#       Need >= R ver 3.0         #
###################################

# ------ 'Predict_N_Age_Script' is a wrapper for this function -----

# -- If needed, add 'SG_Variables_Selected' and 'roundingDelta' to the NN Model '.RData' file (2019 Hake, 2017/2019 Sablefish given as examples) --

# # Get the variables selected from the column names of the data used in the NN model building
#   SG_Variables_Selected <- names(Hake_spectra_2019.sg.iPLS)
#   SG_Variables_Selected <- names(Sable_Spectra_2017_2019.sg.iPLS)  # Sablefish

# # Save the rounding Delta for the current NN model 
# roundingDelta <- 0
# roundingDelta <- -0.2 # Sablefish
# roundingDelta <- -0.05 # Sablefish 2022 Combo

# # Add 'SG_Variables_Selected' and 'roundingDelta' to the NN Model '.RData' file
# JRWToolBox::resave(SG_Variables_Selected, roundingDelta, file = 'FCNN Model/Hake_2019_FCNN_20_Rdm_models_1_Apr_2023.RData')
# JRWToolBox::resave(SG_Variables_Selected, roundingDelta, file = 'FCNN Model/Sablefish_2017_2019_Rdm_models_22_Mar_2023_14_57_26.RData') # Sablefish 


# --- Test this Predict_NN_Age() function as a standalone function using 2019 Hake ---
# - Conda TensorFlow environment -
# Conda_TF_Eniv <- "C:/Users/John.Wallace/AppData/Local/miniconda3/envs/tf" # Work desktop and laptop  
# # Conda_TF_Eniv <- "C:/m/envs/tf_cpu_only" # NWFSC GPU machine - CPU only   
# # Conda_TF_Eniv <- "C:/m/envs/tf" # NWFSC GPU machine - GPU version breaks in R, but works in Python  
# # Conda_TF_Eniv <- "/more_home/h_jwallace/SIDT/Predict_NN_Ages" # Tantalus

# library(tensorflow)
# library(keras)   
# Sys.setenv("RETICULATE_PYTHON" = Conda_TF_Eniv) 

# - TensorFlow Math Check  -
# a <- tf$Variable(5.56)
# b <- tf$Variable(2.7)
# k
# Spectra_Path <- "New_Scans" # Put new spectra scans in a separate folder and put the name of the folder here
# NN_Model <- 'FCNN Model/Hake_2019_FCNN_20_Rdm_models_1_Apr_2023.RData'  # 20 Random Models

# Predict_NN_Age(Conda_TF_Eniv, Spectra_Path, NN_Model)



Predict_NN_Age <- function(Conda_TF_Eniv, Spectra_Path, NN_Model, plot = TRUE, htmlPlotFolder = NULL, NumRdmModels = NULL, shortNameSegments = c(2,4), shortNameSuffix = NULL, 
     opusReader = c('pierreroudier_opusreader', 'philippbaumann_opusreader2')[2], spectraInterp = c('stats_splinefun_lowess', 'prospectr_resample')[1], fineFreqAdj = 150, Predicted_Ages_Path = NULL, verbose = FALSE,  ...) {
   
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
   
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/r.R") 
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/ll.R")  
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/bar.R")    
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/renum.R")   
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R")   
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/predict.lowess.R")  
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/saveHtmlFolder.R") 
   
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Correlation_R_squared_RMSE_MAE_SAD.R")
   
   '%r1%' <- function (e1, e2) 
   {
      ifelse(e1%%e2 == 0, e2, e1%%e2)
   }
   
   if (!any(installed.packages()[, 1] %in% "remotes")) 
     install.packages("remotes") 
   
   if(opusReader == 'pierreroudier_opusreader') {
      if (!any(installed.packages()[, 1] %in% "opusreader")) 
         remotes::install_github("pierreroudier/opusreader")   # https://github.com/pierreroudier/opusreader
   }   
  
   if(opusReader == 'philippbaumann_opusreader2') {  
      if (!any(installed.packages()[, 1] %in% "opusreader2")) 
        remotes::install_github("spectral-cockpit/opusreader2")   #  https://github.com/spectral-cockpit/opusreader2
   }
   
   if (!any(installed.packages()[, 1] %in% "tensorflow")) 
     install.packages("tensorflow")
     
   if (!any(installed.packages()[, 1] %in% "keras")) 
     install.packages("keras") 

   if (!any(installed.packages()[, 1] %in% "prospectr") & spectraInterp == 'prospectr_resample') 
     install.packages("prospectr") 
   
   # --- Setup for TensorFlow and Keras ---
   require(tensorflow)
   require(keras) 
   if(spectraInterp == 'prospectr_resample')
        require(prospectr)   
   
   # --- Change this path to where your Conda TensorFlow environment is located. ---
   Sys.setenv("RETICULATE_PYTHON" = Conda_TF_Eniv) # If this is function is called in the normal way from a species script, then this is line is redundant, otherwise it may be needed.
   
   # # --- Test TensorFlow environment ---
   # a <- tf$Variable(5.56)
   # b <- tf$Variable(2.7)
   # cat("\nTensorFlow Math Check (5.56 + 2.70):\n")
   # print(a + b)
   # cat("\n\n")
   # k_clear_session()
   
   # --- Load the NN model for SG_Variables_Selected and Rdm_models objects ---
   if(!verbose)
      base::load(NN_Model)
      
   if(verbose) {
      '  ###  Just using envir = parent.frame() (the default) adds an environment it seems. Listing an envir arg to save locally is: base::load(file, envir = environment()), or use base::load(file) as done above ###  '
      base::load(NN_Model, envir = parent.frame()) # Save to [[.GlobalEnv]] 
      print(ll()); cat("\n\n")
      if(!interactive())  Sys.sleep(3)
   }
   
   # --- Create a character vector of all spectral files within 'Spectra_Path'---   
   (fileNames <- dir(path = Spectra_Path))[1:10]
   cat(paste0("\nNumber of spectral files to be read in: ", length(fileNames), "\n\n"))
   
   shortName <- apply(matrix(fileNames, ncol = 1), 1, function(x) paste(get.subs(x, sep = "_")[shortNameSegments], collapse = "_"))
   if(!is.null(shortNameSuffix))
       shortName <- paste0(shortName, "_", shortNameSuffix)
      
      
   # newScans.ADJ <- opusreader::opus_read(paste(Spectra_Path, i , sep = "/"), simplify = TRUE, wns_digits = 0)[[2]]
      
      
   # Reading in spectra and doing interpolation if needed. For both methods, the wavebands used are based on very first OPUS spectra read-in. 
   if(spectraInterp %in% c('stats_splinefun_lowess', 'prospectr_resample')) {     # Below simplify = FALSE, so interpolation is not done by Roudier's opusreader::opus_read(). 
   
      # ----------- Re-sampling wavebands using stats::splinefun() with lowess() (inside of the JRWToolBox::predict.lowess() function) or prospectr::resample() --------------
      newScans.ADJ <- list()
      for (i in fileNames)  {
         print(i)
         if(opusReader == 'pierreroudier_opusreader')
            try(newScans.ADJ[[i]] <- opusreader::opus_read(paste(Spectra_Path, i , sep = "/"), simplify = TRUE, wns_digits = 0)[[2]] )
            
         if(opusReader == 'philippbaumann_opusreader2')
            try(newScans.ADJ[[i]] <- opusreader2::read_opus_single(paste(Spectra_Path, i , sep = "/"))[[3]]$data ) 
      }
      
      wavebandsNum <- as.numeric(colnames(newScans.ADJ[[1]]))
      # wavebandsNum <- trunc(wavebandsNum * 10)/10
      # wavebandsToUse <- round(wavebandsNum)
      # wavebandsToUse.tt <- ifelse(dec(wavebandsNum) > 0.5 - 0.00001 & dec(wavebandsNum) < 0.5 + 0.00001 & dec(trunc(wavebandsNum)/2) > 0 - 0.00001 & dec(trunc(wavebandsNum)/2) < 0 + 0.00001 , wavebandsToUse + 1, wavebandsToUse)
      # 
      # tmp <- cbind( wavebandsNum, decimal = wavebandsNum - trunc( wavebandsNum ), wavebandsToUse, wavebandsToUse.tt )
      # tmp[tmp[,3] != tmp[,4], ]
      # 
      
      wavebandsToUse <- round(wavebandsNum)
      # For Sablefish 2017.2019 a handful of SG_Variables_Selected spectra (consecutive numbers) whose trunc() is an even number rounded up and not down when the decimal was 0.5. ????????????????
      # c(length(SG_Variables_Selected), sum(as.numeric(substring(SG_Variables_Selected, 2)) %in% wavebandsToUse))
      (adjNum <- as.numeric(substring(SG_Variables_Selected, 2))[!as.numeric(substring(SG_Variables_Selected, 2)) %in% wavebandsToUse])
      
      if(length(adjNum) >= 1) {
        for (i in adjNum)
          wavebandsToUse[wavebandsToUse == i - 1] <- i
      }
      
      # c(length(SG_Variables_Selected), sum(as.numeric(substring(SG_Variables_Selected, 2)) %in% wavebandsToUse))
      
      colnames(newScans.ADJ[[1]]) <- wavebandsToUse
      
      if(verbose & plot) {          # Extra adjustments added here - THIS IS EXPERIMENTAL
         png(width = 16, height = 10, units = 'in', res = 600, file = paste0(Predicted_Ages_Path, '/Spline_Function_Raw.png'))
         plot(wavebandsToUse - 270, newScans.ADJ[[1]] + 0.10, type = 'l', ylim = c(0, 1.2), xlim = c(3500, 8000))
         
         for(j in 2:length(fileNames)) {
            wavebandsOld <- as.numeric(colnames(newScans.ADJ[[j]]))
            # adjFreq <- c(0, min(wavebandsToUse) - min(wavebandsOld) + fineFreqAdj)[2]
            adjFreq <- ifelse(j > 89, 0, -270)
            # adjAsorb <- c(0, mean(newScans.ADJ[[1]]) - mean(newScans.ADJ[[j]]))[1]
            adjAsorb <- ifelse(j > 89, 0, 0.10)
            lines(wavebandsOld + adjFreq, newScans.ADJ[[j]] + adjAsorb, col = j)
         }
         abline(v = as.numeric(substring(SG_Variables_Selected, 2)), col = 'grey')
         dev.off()
         browseURL(paste0(getwd(), "/", Predicted_Ages_Path, '/Spline_Function_Raw.png'), browser = ifelse(.Platform$OS.type == 'windows', "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe", getOption("browser")))
         # browseURL(paste0(getwd(), "/", Predicted_Ages_Path, '/Spline_Function_Raw.png'))
      } 
      
      
      
      # newScans.ADJ[[1]] <- newScans.ADJ[[1]]
      # c(length(SG_Variables_Selected), sum(as.numeric(substring(SG_Variables_Selected, 2)) %in% wavebandsToUse))
      newScans.ADJ_int <- matrix(data = NA, nrow = length(newScans.ADJ), ncol = length(wavebandsToUse)) #make empty matrix for loop
      newScans.ADJ_int[1, ] <- newScans.ADJ[[1]]
      
      for (j in 2:length(newScans.ADJ)) { 
        bar(j, length(newScans.ADJ))
        wavebandsOld <- as.numeric(colnames(newScans.ADJ[[j]]))
        if(all(wavebandsOld %in% wavebandsToUse))
           newScans.ADJ_int[j,] <- newScans.ADJ[[j]]
        else {
           cat("\n\n*** Number", j, "waveband is not equal to the first waveband read-in ***\n\n")
           adjFreq <- c(0, min(wavebandsToUse) - min(wavebandsOld) + fineFreqAdj)[1]
           # print(c(j, adjFreq,  min(wavebandsToUse), min(wavebandsOld), min(wavebandsOld) + adjFreq))
           adjAsorb <- c(0, mean(newScans.ADJ[[1]]) - mean(newScans.ADJ[[j]]))[1]
           if(spectraInterp == 'stats_splinefun_lowess') 
              newScans.ADJ_int[j,] <- predict.lowess(lowess(wavebandsOld + adjFreq, newScans.ADJ[[j]] + adjAsorb, f = 0.001), newdata = wavebandsToUse)
           if(spectraInterp == 'prospectr_resample') 
              newScans.ADJ_int[j,] <- prospectr::resample(X = newScans.ADJ[[j]] + adjAsorb, wav = wavebandsOld + adjFreq, new.wav = wavebandsToUse)   
            
           if(any(wavebandsToUse < min(wavebandsOld + adjFreq)))  # Hack for extrapolating beyond the end of the old wavebands.
              newScans.ADJ_int[j,][wavebandsToUse < min(wavebandsOld + adjFreq)] <- newScans.ADJ_int[j,][wavebandsToUse < min(wavebandsOld + adjFreq)][1]
           
           # dev.new()   
           # plot(wavebandsToUse, newScans.ADJ_int[j,], col = 'green')
           # points(wavebandsOld + adjFreq, newScans.ADJ[[j]] + adjAsorb)
                        
        }
      }
      
      # colnames(newScans.ADJ_int) <- colnames(newScans.ADJ[[1]])
      colnames(newScans.ADJ_int) <- wavebandsToUse
      # sum(as.numeric(substring(SG_Variables_Selected, 2)) %in% wavebandsToUse)
      newScans.RAW <- as.data.frame(newScans.ADJ_int)
      # dim(newScans.RAW)
      print(head(newScans.RAW[, c(1:5, (ncol(newScans.RAW) - 25):(ncol(newScans.RAW) - 20))])); cat("\n\n")
  }
      
      
# run <- function(...) {     # Use when debugging interactively to avoid error with dots (...)
  if(plot) {
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")
     rowNums <- 1:nrow(newScans.RAW)
     if(length(rowNums <= 26^2))
        plotly.Spec(data.frame(filenames = fileNames, htmlPlotFolder = htmlPlotFolder, newScans.RAW, Otie = paste0(LETTERS[floor((rowNums - 0.001)/26) + 1], LETTERS[rowNums %r1% 26], '_', rowNums), shortName = shortName), colorGroup = 'Otie', ...)
     else
       plotly.Spec(data.frame(filenames = fileNames, htmlPlotFolder = htmlPlotFolder, newScans.RAW, Otie = factor(rowNums), shortName = shortName), colorGroup = 'Otie', ...) 
   }    
# }; run() # Use when debugging interactively

   cat("\nDimension of Spectral File Matrix Read In:", dim(newScans.RAW), "\n\n")
   
   if(verbose) 
       cat("\nStarting the estimation of NN ages based the spectra scans provided:\n\n")
       
   
   # all(SG_Variables_Selected %in% names(data.frame(prospectr::savitzkyGolay(newScans.RAW, m = 1, p = 2, w = 15))))

   trySgVarSel <- try(newScans <- data.frame(prospectr::savitzkyGolay(newScans.RAW, m = 1, p = 2, w = 15))[, SG_Variables_Selected], silent = TRUE)   # SG_Variables_Selected is part of the NN_Model .RData file.
   if(inherits(trySgVarSel, "try-error") & !interactive() & .Platform$OS.type == 'windows') {
        shell(paste0("echo.  > ", Predicted_Ages_Path, "\\ERROR_READ_ME.txt"))
        shell(paste0("echo The wavebands selected using the Savitzky Golay function and used in the current NN model are not the same as in the current spectra nor have the current spectra been interpolated to those wavebands. >> ", Predicted_Ages_Path, "\\ERROR.txt"))
        stop(paste0("\nThe wavebands selected using the Savitzky Golay function and used in the current NN model are not\nthe same as in the current spectra nor have the current spectra been interpolated to those wavebands.\n\n"))
   }
   
   # c(length(SG_Variables_Selected), sum(SG_Variables_Selected %in% names(data.frame(prospectr::savitzkyGolay(newScans.RAW, m = 1, p = 2, w = 15)))))
   
  save(newScans, newScans.RAW, file = 'newScans.RData' )
    
  if(is.null(NumRdmModels))
      N <- length(Rdm_models)
  else
      N <- NumRdmModels
 
   newScans.pred.ALL <- NULL
   for(j in 1:N) {
      Fold_models <- Rdm_models[[j]]
      for (i in 1:length(Fold_models)) {      
            newScans.pred <- as.vector(predict(keras::unserialize_model(Fold_models[[i]], custom_objects = NULL, compile = TRUE), as.matrix(1000 * newScans)))
            newScans.pred.ALL <- rbind(newScans.pred.ALL, data.frame(Index = 1:nrow(newScans), newScans.pred = newScans.pred))
     }
   }  
      
   Pred_median <- r(data.frame(NN_Pred_Median = aggregate(list(NN_Pred_Median = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), median, na.rm = TRUE)[,2], 
      Lower_Quantile_0.025 = aggregate(list(Quantile_0.025 = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), quantile, probs = 0.025, na.rm = TRUE)[,2],
      Upper_Quantile_0.975 = aggregate(list(Quantile_0.975 = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), quantile, probs = 0.975, na.rm = TRUE)[,2]), 4)
    
   cat(paste0("\n\n--- Note: The quantiles are a reflection of the NN models precision based on ", N, " full 10-fold randomized models, not the accuracy to a TMA Age ---\n\n"))    
   data.frame(filenames = fileNames, Pred_median)
}

