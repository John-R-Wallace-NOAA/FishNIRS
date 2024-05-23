
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
# FYI: JRWToolBox::extractRData() extracts out a single R object from a *.RData save() and ls.RData() lists the R objects within a *.RData.

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



Predict_NN_Age <- function(Conda_TF_Eniv, Spectra_Path, Model_Spectra_Meta, NN_Model, plot = TRUE, htmlPlotFolder = NULL, NumRdmModels = NULL, 
                            Predicted_Ages_Path = NULL, opusReader = c('pierreroudier_opusreader', 'philippbaumann_opusreader2')[2], verbose = FALSE, Folds_Num = 10, ...) {
   
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
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/match.f.R")  
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/ls.RData.R") 
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R")  
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/headTail.R")   
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/predict.lowess.R")  
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/saveHtmlFolder.R") 
    
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Cor_R_squared_RMSE_MAE_SAD_APE.R")
    
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
    
    # -- Setup for TensorFlow and Keras --
    require(tensorflow)
    require(keras) 
    
    # -- Change this path to where your Conda TensorFlow environment is located. --
    Sys.setenv("RETICULATE_PYTHON" = Conda_TF_Eniv) # If this is function is called in the normal way from a species script, then this is line is redundant, otherwise it may be needed.
    print(Sys.getenv("RETICULATE_PYTHON"))
    
    # # -- Test TensorFlow environment --
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
        cat("\n\nNN_Model Path:" , NN_Model, "\n\n")
        
     '  ###  Just using envir = parent.frame() (the default) adds an environment it seems. Listing an envir arg to save locally is: base::load(file, envir = environment()), or use base::load(file) as done above ###  '
     '  ### parent.frame(sys.nframe() + 1) should work to always get back to .GlobalEnv regardless of how deep the frames are!!!   '
        base::load(NN_Model, envir = parent.frame(sys.nframe() + 1)) # Save to [[.GlobalEnv]] just so ll() will work... grr!
        print(ll()); cat("\n\n")
        Sys.sleep(2)
    }
 
    # cat("\n\nNN_Model Path:" , NN_Model, "\n\n")
    # base::load(NN_Model)
    
    # if(verbose) {
    #    ls.RData(NN_Model)
    #    Sys.sleep(2)
    # }
    
    
    # -- Find the metadata variables that are in the NN Model --
    SG_Variables_Selected[grep("Length_cm", SG_Variables_Selected)] <- "Length_prop_max"
    SG_Variables_Selected[grep("length_prop_max", SG_Variables_Selected)] <- "Length_prop_max"
    SG_Variables_Selected[grep("Weight_kg", SG_Variables_Selected)] <- "Weight_prop_max"
    SG_Variables_Selected[grep("Depth_m", SG_Variables_Selected)] <- "Depth_prop_max"
    
    metaDataVar <- (1:length(SG_Variables_Selected))[is.na(as.numeric(substring(SG_Variables_Selected, 2)))]
    print(SG_Variables_Selected[metaDataVar])
    
   
    # -- Extract newScans.RAW, fileNames, and shortName from Model_Spectra_Meta --
    newScans.RAW <- Model_Spectra_Meta[, 2:(grep('project', names(Model_Spectra_Meta)) - 1)]
    if(verbose) {
       print(headTail(newScans.RAW, 5))
       cat("\nDimension of Spectral File Matrix Read In:", dim(newScans.RAW), "\n\n")    
    }
    
    fileNames <- Model_Spectra_Meta$filenames
    shortName <- apply(Model_Spectra_Meta[, 'filenames', drop = FALSE], 1, function(x) paste(get.subs(x, sep = "_")[c(1, 5)], collapse = "_"))
   
    if(plot) { 
      png(width = 16, height = 10, units = 'in', res = 600, file = paste0(Predicted_Ages_Path, '/Savitzky_Golay_Variables_Selected.png'))
      wavebandsToUse <- as.numeric(substring(colnames(newScans.RAW), 2))    
      plot(wavebandsToUse, newScans.RAW[1, ], type = 'l', ylim = c(0, 1.2), xlim = c(3500, 8000), xlab = 'Wavebands Used (1/cm) (Wavelengths of light from 2,500 - 1,250 nm)', ylab = 'Absorbance')
      
      for(j in 2:100)  # Just the first 100 wavebands - the point is to look at the variables selected via Savitzky Golay.
          lines(wavebandsToUse, newScans.RAW[j, ], col = j)
      
      abline(v = as.numeric(substring(SG_Variables_Selected, 2)), col = 'grey')
      dev.off()
      browseURL(paste0(getwd(), "/", Predicted_Ages_Path, '/Savitzky_Golay_Variables_Selected.png'), browser = ifelse(file.exists("C:/Program Files/Google/Chrome/Application/chrome.exe"), 
                "C:/Program Files/Google/Chrome/Application/chrome.exe", "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"))
                
      # browseURL(paste0(getwd(), "/", Predicted_Ages_Path, '/Savitzky_Golay_Variables_Selected.png'))  # Try the default browser.  If getOption("browser") is NULL, I get the Windows Photo Viewer.
    }    
  
    # -- Estimation of NN ages based the spectra scans provided --   
    if(verbose) 
        cat("\nStarting the estimation of NN ages based the spectra scans provided:\n\n")
    
    # all(SG_Variables_Selected %in% names(data.frame(prospectr::savitzkyGolay(newScans.RAW, m = 1, p = 2, w = 15))))
  
    dim(newScans.RAW) # All wavebands
    
    if(length(SG_Variables_Selected) > length(metaDataVar)) {  # Wavebands used (vs metadata only model)
    
        length(SG_Variables_Selected[1:(length(SG_Variables_Selected) - length(metaDataVar))])  # Those wavebands selected via SG
        trySgVarSel <- try(newScans <- data.frame(prospectr::savitzkyGolay(newScans.RAW, m = 1, p = 2, w = 15))[, SG_Variables_Selected[1:(length(SG_Variables_Selected) - length(metaDataVar))]], silent = TRUE)   # SG_Variables_Selected is part of the NN_Model .RData file.
        
        if(inherits(trySgVarSel, "try-error") & !interactive() & .Platform$OS.type == 'windows') {
           shell(paste0("echo.  > ", Predicted_Ages_Path, "/ERROR_READ_ME.txt"))
           shell(paste0("echo The wavebands selected using the Savitzky Golay function and used in the current NN model are not the same as in the current spectra nor have the current spectra been interpolated to those wavebands. >> ", Predicted_Ages_Path, "/ERROR.txt"))
           stop(paste0("\nThe wavebands selected using the Savitzky Golay function and used in the current NN model are not\nthe same as in the current spectra nor have the current spectra been interpolated to those wavebands.\n\n"))
        }
        
        # dim(trySgVarSel)
        # c(length(SG_Variables_Selected), sum(SG_Variables_Selected %in% names(data.frame(prospectr::savitzkyGolay(newScans.RAW, m = 1, p = 2, w = 15)))), length(metaDataVar))
        
        newScans <- match.f(data.frame(fileNames, newScans), Model_Spectra_Meta, 'fileNames', 'filenames', SG_Variables_Selected[metaDataVar])[, -1]  
    }
    
    if(length(SG_Variables_Selected) == length(metaDataVar)) # Metadata only model
       newScans <- Model_Spectra_Meta[ ,SG_Variables_Selected[metaDataVar]]
    
	TF <- rowSums(is.na(newScans)) == 0  # Remove rows with NA's for legacy models and save which rows were removed for below
	newScans <- newScans[TF, ]
	Model_Spectra_Meta <- Model_Spectra_Meta[TF, ]
	fileNames <- Model_Spectra_Meta$filenames
		
    if(verbose) {
	   cat("\n\'newScans' data frame with metadata (if any) and columns with NA's removed saved to the .GlobalEnv\n\n")
       dim(newScans)
       headTail(newScans, 3, 3, 3, 5)
    }
    
    assign("newScans", newScans , pos = 1) # Save for Cor_R_squared_RMSE_MAE_SAD_APE_Table for various values of N
    assign("Model_Spectra_Meta", Model_Spectra_Meta , pos = 1)
    
    if(is.null(NumRdmModels))
        (N <- length(Rdm_models))
    else
        (N <- NumRdmModels)
  
    newScans.pred.ALL <- NULL
    for(j in 1:N) {
       Fold_models <- Rdm_models[[j]]
       for (i in 1:length(Fold_models)) {      
             newScans.pred <- as.vector(predict(keras::unserialize_model(Fold_models[[i]], custom_objects = NULL, compile = TRUE), as.matrix(1000 * newScans)))
             Corr <- cor(newScans.pred, Model_Spectra_Meta$TMA)
             if(all(newScans.pred == 0) || Corr < 0.85) {
                if(verbose)
                   cat(paste0("\nRandom Model ", j, "; Fold ", i, " NOT accepted with a correlation of ", round(Corr, 4), "\n\n\n"))
                next
             } else {
                 if(verbose)
                   cat(paste0("\nRandom Model ", j, "; Fold ", i, " accepted with a correlation of ", round(Corr, 4), "\n\n\n"))
             }  
             newScans.pred.ALL <- rbind(newScans.pred.ALL, data.frame(Index = 1:nrow(newScans), newScans.pred = newScans.pred))
      }
    } 
    
    Pred_median <- r(data.frame(NN_Pred_Median = aggregate(list(NN_Pred_Median = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), median, na.rm = TRUE)[,2], 
          Lower_Quantile_0.025 = aggregate(list(Quantile_0.025 = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), quantile, probs = 0.025, na.rm = TRUE)[,2],
          Upper_Quantile_0.975 = aggregate(list(Quantile_0.975 = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), quantile, probs = 0.975, na.rm = TRUE)[,2]), 4)
          
    Pred_median[paste0('Num_of_Full_', Folds_Num, '_Fold_Models')] <- aggregate(list(N = newScans.pred.ALL$newScans.pred), list(Index = newScans.pred.ALL$Index), function(x) length(x)/Folds_Num)[,2]
    
    
    New_Ages <- data.frame(filenames = fileNames, Pred_median)          
 
    if(verbose) {     
       cat("\n\nPred_median:\n\n")  
       headTail(Pred_median, 3, 3)
       cat(paste0("\n\n--- Note: The quantiles are a reflection of the NN models precision based on ", nrow(newScans.pred.ALL)/nrow(New_Ages)/Folds_Num, " full ", Folds_Num, "-fold randomized models, not the accuracy to a TMA Age ---\n\n"))   
    }
    
    list(New_Ages = New_Ages, newScans.pred.ALL = newScans.pred.ALL)
}

