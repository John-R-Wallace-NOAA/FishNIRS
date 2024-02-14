 
 
# Sable_2020_Scans <- Read_OPUS_Spectra("Sable_Combo_2022", Spectra_Path = 'Sable_2022_Scans')
# plotly_spectra(Sable_2020_Scans)
# plotly_spectra(Sable_2020_Scans, N_Samp = 300, htmlPlotFolder = 'Figures/Sablefish_2022_Spectra_Sample_of_300')


Read_OPUS_Spectra <- function(Spectra_Set = c("Hake_2019", "Sable_2017_2019", "Sable_Combo_2022")[3], Spectra_Path = NULL, 
                         Meta_Add = c(TRUE, FALSE)[1], TMA_Ages = c(TRUE, FALSE)[2], Meta_Path = NULL, 
                         excelSheet = 3, Max_N_Spectra = list(50, 200, 'All')[[3]], 
                         verbose = c(TRUE, FALSE)[1], plot = c(TRUE, FALSE)[2], htmlPlotFolder = "Figures",
                         spectraInterp = c('stats_splinefun_lowess', 'prospectr_resample')[1], 
                         opusReader = c('pierreroudier_opusreader', 'philippbaumann_opusreader2')[2]) { 
 
#  Max_N_Spectra is the max number of new spectra to be plotted in the spectra figure. 
 
# ------------------------------------ Main User Setup ------------------------------------------------------------
   
    # (1) Hake 2019, BMS
    if(Spectra_Set == "Hake_2019") {
       if(interactive())
          setwd("C:/ALL_USR/JRW/SIDT/Predict_NN_Ages")  # Change path to the Spectra Set's .GlobalEnv as needed
       shortNameSegments <- c(2, 4) # Segments 2 and 4 of the spectra file name, e.g.: (PACIFIC, HAKE, BMS201906206C, 1191, OD1) => (HAKE, 1191)
       shortNameSuffix <- 'BMS'
       opusReader <- 'pierreroudier_opusreader'
       fineFreqAdj <- 150
    }
     
    # (2) Sablefish 2017 & 2019, Combo survey
    if(Spectra_Set == "Sable_2017_2019") { 
       if(interactive()) 
          setwd("C:/ALL_USR/JRW/SIDT/Predict_NN_Ages")  
       shortNameSegments <- c(1, 3) # Segments 1 and 3 of the spectra file name, e.g.: (SABLEFISH, COMBO201701203A, 28, OD1) => (SABLEFISH, 28)
       shortNameSuffix <- 'Year'
       yearPosition <- c(6, 9) # e.g. COMBO201701203A => 2017 (Segment used (see above) is: shortNameSegments[1] + 1)
       fineFreqAdj <- 0
       opusReader <- 'pierreroudier_opusreader'
     }  
    
    # (3) Sablefish 2022, Combo survey
    if(Spectra_Set == "Sable_Combo_2022") { 
       if(is.null(Spectra_Path)) 
          Spectra_Path = 'C:/ALL_USR/JRW/SIDT/Sablefish 2022 Combo/Sable_2022_Scans'
       if(Meta_Add & is.null(Meta_Path)) 
          Meta_Path = paste0('C:/ALL_USR/JRW/SIDT/Sablefish 2022 Combo/', Spectra_Set, '_NIRS_Scanning_Session_Report.xlsx')
       shortNameSegments <- c(1,5) # Segments 1 and 3 of the spectra file name, e.g.: (SABLEFISH, COMBO201701203A, 28, OD1) => (SABLEFISH, 28)
       shortNameSuffix <- 'Combo'
       yearPosition <- c(6, 9) # e.g. COMBO201701203A => 2017 (Segment used (see above) is: shortNameSegments[1] + 1)
       fineFreqAdj <- 0
    }  
	
	# (4) Sablefish 2021, Combo survey
    if(Spectra_Set == "Sable_Combo_2021") { 
       if(is.null(Spectra_Path)) 
          Spectra_Path = 'C:/ALL_USR/JRW/SIDT/Sablefish 2021 Combo/Sable_2021_Scans'
       if(Meta_Add & is.null(Meta_Path)) 
          Meta_Path = paste0('C:/ALL_USR/JRW/SIDT/Sablefish 2021 Combo/', Spectra_Set, '_NIRS_Scanning_Session_Report.xlsx')
       shortNameSegments <- c(1,5) # Segments 1 and 3 of the spectra file name, e.g.: (SABLEFISH, COMBO201701203A, 28, OD1) => (SABLEFISH, 28)
       shortNameSuffix <- 'Combo'
       yearPosition <- c(6, 9) # e.g. COMBO201701203A => 2017 (Segment used (see above) is: shortNameSegments[1] + 1)
       fineFreqAdj <- 0
    }  
# -----------------------------------------------------------------------------------------------------------------------------------
 
    if(!any(installed.packages()[, 1] %in% "remotes")) 
       install.packages("remotes") 
  
    if(!any(installed.packages()[, 1] %in% "openxlsx")) 
       install.packages("openxlsx")       
     
    if(!any(installed.packages()[, 1] %in% "dplyr")) 
       install.packages("dplyr")     
    
    if(opusReader == 'pierreroudier_opusreader') {
       if (!any(installed.packages()[, 1] %in% "opusreader")) 
          remotes::install_github("pierreroudier/opusreader")   # https://github.com/pierreroudier/opusreader
    }   
    
    if(opusReader == 'philippbaumann_opusreader2') {  
       if (!any(installed.packages()[, 1] %in% "opusreader2")) 
         remotes::install_github("spectral-cockpit/opusreader2")   #  https://github.com/spectral-cockpit/opusreader2
    }
  
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
    
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/bar.R")   
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R")   
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/predict.lowess.R")  
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/saveHtmlFolder.R") 
    
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Correlation_R_squared_RMSE_MAE_SAD.R")
    
    '%r1%' <- function (e1, e2) 
    {
       ifelse(e1%%e2 == 0, e2, e1%%e2)
    }
     
# -----------------------------------------------------------------------------------------------------------------------------------

# --- Create a character vector of all spectral files within 'Spectra_Path'---   
    if(is.null(Spectra_Path))
       fileNames.0 <- dir()
    else   
       fileNames.0 <- dir(path = Spectra_Path)
       
    if(verbose) {
       cat(paste0("\nNumber of spectral files to be read in: ", length(fileNames.0), "\n\n"))
       print(fileNames.0[1:10])
       cat("\n\n")
    }
    
    shortName <- apply(matrix(fileNames.0, ncol = 1), 1, function(x) paste(get.subs(x, sep = "_")[shortNameSegments], collapse = "_"))
    if(!is.null(shortNameSuffix))
        shortName <- paste0(shortName, "_", shortNameSuffix)
       
    # Reading in spectra and doing interpolation if needed. For both methods, the wavebands used are based on very first OPUS spectra read-in. 
    if(spectraInterp %in% c('stats_splinefun_lowess', 'prospectr_resample')) {     # Below simplify = FALSE, so interpolation is not done by Roudier's opusreader::opus_read(). 
    
        # ----------- Re-sampling wavebands using stats::splinefun() with lowess() (inside of the JRWToolBox::predict.lowess() function) or prospectr::resample() --------------
        newScans.ADJ <- list()
        for (i in fileNames.0)  {
           print(i)
           if(opusReader == 'pierreroudier_opusreader')
              try(newScans.ADJ[[i]] <- opusreader::opus_read(paste(Spectra_Path, i , sep = "/"), simplify = FALSE, wns_digits = 0)[[2]] )
              
           if(opusReader == 'philippbaumann_opusreader2')
              try(newScans.ADJ[[i]] <- opusreader2::read_opus_single(paste(Spectra_Path, i , sep = "/"))[[3]]$data ) 
        }
        
        wavebandsToUse <- as.numeric(colnames(newScans.ADJ[[1]]))
        if(verbose) {
            cat("\n\nWavebands being used:",  head(wavebandsToUse, 4), "...", tail(wavebandsToUse, 4))
            cat("\nDifference in wavebands:", diff(head(wavebandsToUse, 5)), "...", diff(tail(wavebandsToUse, 5)), "\n\n")
        }
               
        colnames(newScans.ADJ[[1]]) <- wavebandsToUse
        wavebandsToUse.8k <- wavebandsToUse[wavebandsToUse <= 8000]
        newScans.ADJ_int <- matrix(data = NA, nrow = length(newScans.ADJ), ncol = length(wavebandsToUse.8k)) #make empty matrix for loop
        newScans.ADJ_int[1, ] <- newScans.ADJ[[1]][wavebandsToUse <= 8000]
        
        for (j in 2:length(newScans.ADJ)) { 
          bar(j, length(newScans.ADJ))
          wavebandsOld <- as.numeric(colnames(newScans.ADJ[[j]]))
          wavebandsOld.8k <- wavebandsOld[wavebandsOld <= 8000]
          if(all(wavebandsOld.8k %in% wavebandsToUse.8k))
             newScans.ADJ_int[j,] <- newScans.ADJ[[j]][wavebandsOld <= 8000]
          else {    
             adjFreq <- min(wavebandsToUse.8k) - min(wavebandsOld.8k) + fineFreqAdj
             # print(c(j, adjFreq,  min(wavebandsToUse.8k), min(wavebandsOld.8k), min(wavebandsOld.8k) + adjFreq))
             adjAsorb <- mean(newScans.ADJ[[1]]) - mean(newScans.ADJ[[j]])
             
             if(spectraInterp == 'stats_splinefun_lowess') 
                newScans.ADJ_int[j,] <- predict.lowess(lowess(wavebandsOld.8k + adjFreq, newScans.ADJ[[j]] + adjAsorb, f = 0.001), newdata = wavebandsToUse.8k)
                
             if(spectraInterp == 'prospectr_resample') 
                newScans.ADJ_int[j,] <- prospectr::resample(X = newScans.ADJ[[j]] + adjAsorb, wav = wavebandsOld.8k + adjFreq, new.wav = wavebandsToUse.8k)   
              
             if(any(wavebandsToUse.8k < min(wavebandsOld.8k + adjFreq)))  # Hack for extrapolating beyond the end of the old wavebands.
                newScans.ADJ_int[j,][wavebandsToUse.8k < min(wavebandsOld.8k + adjFreq)] <- newScans.ADJ_int[j,][wavebandsToUse.8k < min(wavebandsOld.8k + adjFreq)][1]
             
             # dev.new()   
             # plot(wavebandsToUse.8k, newScans.ADJ_int[j,], col = 'green')
             # points(wavebandsOld.8k + adjFreq, newScans.ADJ[[j]] + adjAsorb)
                          
          }
        }
        
        # colnames(newScans.ADJ_int) <- colnames(newScans.ADJ[[1]])
        colnames(newScans.ADJ_int) <- wavebandsToUse.8k
        newScans.RAW <- as.data.frame(newScans.ADJ_int)
        # dim(newScans.RAW)
        print(head(newScans.RAW[, c(1:5, (ncol(newScans.RAW) - 25):(ncol(newScans.RAW) - 20))])); cat("\n\n")
    }
       
    if(plot) {
       sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")
       rowNums <- 1:nrow(newScans.RAW)
       if(length(rowNums <= 26^2))
          plotly.Spec(data.frame(filenames = fileNames.0, newScans.RAW, Otie = paste0(LETTERS[floor((rowNums - 0.001)/26) + 1], LETTERS[rowNums %r1% 26], '_', rowNums), shortName = shortName), N_Samp = min(c(length(fileNames.0), Max_N_Spectra)), colorGroup = 'Otie')
       else
         plotly.Spec(data.frame(filenames = fileNames.0, newScans.RAW, Otie = factor(rowNums), shortName = shortName), N_Samp = min(c(length(fileNames.0), Max_N_Spectra)), colorGroup = 'Otie') 
       
       if(!is.null(htmlPlotFolder)) {
         dir.create(htmlPlotFolder, showWarnings = FALSE)
         saveHtmlFolder(htmlPlotFolder, view = !interactive())
       }  
    }    
 
    if(verbose)
       cat("\nDimension of Spectral File Matrix Read In:", dim(newScans.RAW), "\n\n")
       
    if(Meta_Add) {
       fileNames <- get.subs(fileNames.0, sep = ".")[1, ]  # No '.0' in the metadata xlsx
       metadata <- openxlsx::read.xlsx(Meta_Path, sheet = excelSheet) # Load in ancillary data 
       Model_Spectra_Meta <- dplyr::left_join(data.frame(filenames = fileNames, newScans.RAW), metadata, dplyr::join_by("filenames" == "NWFSC_NIR_Filename")) # Match by filenames and look at the data/metadata
       if(verbose)
          print(Model_Spectra_Meta[1:5, c(1:3, (ncol(Model_Spectra_Meta) - 36):ncol(Model_Spectra_Meta))])
       Model_Spectra_Meta$length_prop_max <- Model_Spectra_Meta$length_cm/max(Model_Spectra_Meta$length_cm, na.rm = TRUE)
       Model_Spectra_Meta$structure_weight_dg = 10 * Model_Spectra_Meta$structure_weight_g # dg = decigram
       Model_Spectra_Meta$percent_crystallized_scan[is.na(Model_Spectra_Meta$percent_crystallized_scan)] <- 0 # Change NA to zero so that a numerical test can be done.
       Model_Spectra_Meta$percent_missing_scan[is.na(Model_Spectra_Meta$percent_missing_scan)] <- 0
       Model_Spectra_Meta$tissue_level_scan[is.na(Model_Spectra_Meta$tissue_level_scan)] <- 0
       Model_Spectra_Meta <- data.frame(Model_Spectra_Meta, shortName = shortName) 
       
       TF <- Model_Spectra_Meta$percent_crystallized_scan <= 15 & Model_Spectra_Meta$percent_crystallized_scan <= 10 & Model_Spectra_Meta$tissue_level_scan <= 10 & 
                     !is.na(Model_Spectra_Meta$length_cm) & !is.na(Model_Spectra_Meta$structure_weight_g)
       names(Model_Spectra_Meta)[names(Model_Spectra_Meta) %in% 'age_best'] <- "TMA"    
       if(TMA_Ages)        
           TF <- TF & !is.na(Model_Spectra_Meta$TMA)
       Model_Spectra_Meta <- Model_Spectra_Meta[TF, ]
       
       if(verbose)
          cat(paste0('\n\nTotal number of oties read in: ', sum(TF) + sum(!TF), '.  Number rejected based on metadata (including missing TMA, when asked for): ', sum(!TF), '.  Number kept: ', sum(TF), '.\n'), quote = FALSE)
       
       invisible(Model_Spectra_Meta) 
    } else  
      invisible(newScans.RAW)
}   
      
     
      
      
      
      
      
      
      
      
      
      
      
