 
 
# Sable_2020_Scans <- Read_OPUS_Spectra("Sable_Combo_2022", Spectra_Path = 'Sable_2022_Scans')
# plotly_spectra(Sable_2020_Scans)
# plotly_spectra(Sable_2020_Scans, N_Samp = 300, htmlPlotFolder = 'Figures/Sablefish_2022_Spectra_Sample_of_300')


Read_OPUS_Spectra <- function(Spectra_Set = c("PWHT_Acoustic2019", "PWHT_Acoustic2023", "Sable_2017_2019", "Sable_Combo_2022")[2], Spectra_Path = NULL, 
                         shortNameSegments = NULL, shortNameSuffix = 'COMBO', yearPosition = 6, fineFreqAdj = 0,
                         Meta_Path = NULL, Extra_Meta_Path = NULL, TMA_Ages_Only = c(TRUE, FALSE)[2], 
                         excelSheet = 3, Max_N_Spectra = list(50, 200, 'All')[[3]], OR_2008_2011 = FALSE,
                         verbose = c(TRUE, FALSE)[1], plot = c(TRUE, FALSE)[1], Static_Figure = NULL, htmlPlotFolder = NULL, 
                         spectraInterp = c('stats_splinefun_lowess', 'prospectr_resample')[1], 
                         opusReader = c('pierreroudier_opusreader', 'philippbaumann_opusreader2')[2], Debug = FALSE) { 
 
       #  Max_N_Spectra is the max number of new spectra to be plotted in the spectra figure. 
 
 # ------------------------------------ Download or define functions ------------------------------------------------
 
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
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/renum.R")  
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Table.R") 
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/match.f.R")      
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R")   
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/headTail.R")      
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/recode.simple.R")  
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Months.POSIXt.R") 
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/predict.lowess.R")  
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/saveHtmlFolder.R") 
    
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Correlation_R_squared_RMSE_MAE_SAD.R")
    
    '%r1%' <- function (e1, e2) 
    {
       ifelse(e1%%e2 == 0, e2, e1%%e2)
    }
     

# ------------------------------------ Main User Setup ------------------------------------------------------------
   
    # Hake 2019, BMS
    if(Spectra_Set == "Hake_2019") {
       shortNameSegments <- c(2, 4) # Segments 2 and 4 of the spectra file name, e.g.: PWHT_Acoustic2019_NIR0023A_PRD_1_100620381_O1.0 => (PWHT, 100620381)
       shortNameSuffix <- 'BMS'
       fineFreqAdj <- 150
    }
    
    # Hake 2019, Acoustic Survey
    if(Spectra_Set == "PWHT_Acoustic2019") {
       shortNameSegments <- c(1, 6) # Segments 2 and 4 of the spectra file name, e.g.: (PACIFIC, HAKE, BMS201906206C, 1191, OD1) => (HAKE, 1191)
       shortNameSuffix <- 'Acoustic'
       Extra_Meta_Path <- "C:/SIDT/PWHT_Acoustic2019/PWHT_Acoustic2019_Extra_Metadata.Rdata"
    }
     
    # Hake 2023, Acoustic Survey
    if(Spectra_Set == "PWHT_Acoustic2023") {
       shortNameSegments <- c(1, 6) # Segments 2 and 4 of the spectra file name, e.g.: (PACIFIC, HAKE, BMS201906206C, 1191, OD1) => (HAKE, 1191)
       shortNameSuffix <- 'Acoustic'
       Meta_Path <- "C:/SIDT/PWHT_Acoustic_2023/Acoustic_2023_PWHT_NIR0069_Scanning_Session_Report_For_NWC.xlsx"
       # Extra_Meta_Path <- "C:/SIDT/PWHT_Acoustic2023/PWHT_Acoustic2023_Extra_Metadata.Rdata"
    }
      
      
    # Sablefish 2017 & 2019, Combo survey
    if(Spectra_Set == "Sable_2017_2019") { 
       shortNameSegments <- c(1, 3) # Segments 1 and 3 of the spectra file name, e.g.: (SABLEFISH, COMBO201701203A, 28, OD1) => (SABLEFISH, 28)
       shortNameSuffix <- 'Year'
       yearPosition <- c(6, 6 + 3) # e.g. COMBO201701203A => 2017 (Segment used (see above) is: shortNameSegments[1] + 1)
       opusReader <- 'pierreroudier_opusreader'
     }  
    
    # Sablefish Combo survey
    if(Spectra_Set %in% c("Sable_Combo_2017", "Sable_Combo_2018", "Sable_Combo_2019", "Sable_Combo_2021", "Sable_Combo_2022", "Sable_Combo_2023")) { 
        Year <- get.subs(Spectra_Set, sep = "_")[3]
        if(is.null(Spectra_Path)) 
          Spectra_Path <- paste0('C:/SIDT/Sable_Combo_', Year, '/Sable_Combo_', Year, '_Scans')
       
        if(is.null(Meta_Path))
            Meta_Path <- paste0('C:/SIDT/Sable_Combo_', Year, '/', Spectra_Set, '_NIRS_Scanning_Session_Report_For_NWFSC.xlsx')
        
        if(!is.null(Extra_Meta_Path) && Extra_Meta_Path == "No_Default") # Special execption e.g. reference scans 
           Extra_Meta_Path <- NULL
        else   
           Extra_Meta_Path <- "C:/SIDT/Get Otie Info from Data Warehouse/selectSpAgesFramFeb2024.RData"    
        
        shortNameSegments <- c(1, 5) 
        shortNameSuffix <- 'Combo'
        # yearPosition <- c(6, 6 + 3) # e.g. COMBO201701203A => 2017 (Segment used (see above) is: shortNameSegments[1] + 1)
    }  
    
    # REYE_RougheyeRF_2015-2017 and BLSP_COMBO_2006-2018 Combo Survey
    if(Spectra_Set %in% c("BLSP_COMBO_2006-2018", "REYE_RougheyeRF_2015-2017")) {
       shortNameSegments <- c(1, 6) # Segments 1 and 6 of the spectra file name, e.g.: REYE_COMBO2015_NIR0033A_PRD_11_102010851_O1.0 => (REYE, 102010851)
       shortNameSuffix <- 'Combo'
       yearPosition <- 6 # Segment used (see above) is: shortNameSegments[1] + 1
       # Extra_Meta_Path <- 
    }
    
    # REYE_OR_Comm_2008_2011
    if(Spectra_Set %in% "REYE_OR_Comm_2008_2011") {
       shortNameSegments <- c(1, 5) 
       shortNameSuffix <- 'Comm'
       yearPosition <- 3 # Segment used (see above) is: shortNameSegments[1] + 1
       # Extra_Meta_Path <- 
    }
   
    # REYE_OR_Comm_2022
    if(Spectra_Set %in% "REYE_OR_Comm_2022") {
       shortNameSegments <- c(1, 6, 7) 
       shortNameSuffix <- 'Comm'
       yearPosition <- 7 # Segment used (see above) is: shortNameSegments[1] + 1
       # Extra_Meta_Path <- 
    }
   
    # REYE_OR_Comm_2023
    if(Spectra_Set %in% "REYE_OR_Comm_2023") {
       shortNameSegments <- c(1, 6, 7) 
       shortNameSuffix <- 'Comm'
       yearPosition <- 7 # Segment used (see above) is: shortNameSegments[1] + 1
       # Extra_Meta_Path <- 
    }
    
    # REYE_WA_Comm_2012_2013
    if(Spectra_Set %in% "REYE_WA_Comm_2012_2013") {
       # Spectra_Path <- paste0("REYE_Comm_2012_2023/", Spectra_Set, "_Scans")  # dir.create(Spectra_Path, showWarnings = FALSE, recursive = TRUE)
       # Meta_Path <- "REYE_Comm_2012_2023/REYE_WACOMM_WAREC_2012_2013_NIRS_Scanning_Session_Report_For_NWFSC.xlsx"
       # Extra_Meta_Path <- 
       # htmlPlotFolder = paste0('Predicted_Ages/', Spectra_Set, '_Spectra_Sample_of_', Max_N_Spectra)
       shortNameSegments <- c(1, 6) 
       shortNameSuffix <- 'CommRec'
       yearPosition <- 6 # e.g. WAREC2012 => 2012 (Segment used (see above) is: shortNameSegments[1] + 1)
    }
   
    # REYE_WA_Comm_2011
    if(Spectra_Set %in% "REYE_WA_Comm_2011") {
       # Spectra_Path <- paste0("REYE_Comm_2012_2023/", Spectra_Set, "_Scans")  # dir.create(Spectra_Path, showWarnings = FALSE, recursive = TRUE)
       # Meta_Path <- "REYE_Comm_2012_2023/REYE_WACOMM_2011_NIRS0042_Scanning_Report_For_NWC.xlsx"
       # Extra_Meta_Path <- 
       # htmlPlotFolder = paste0('Predicted_Ages/', Spectra_Set, '_Spectra_Sample_of_', Max_N_Spectra)
       shortNameSegments <- c(1, 6) 
       shortNameSuffix <- 'Comm'
       yearPosition <- 7 # e.g. WACOMM2011 => 2011 (Segment used (see above) is: shortNameSegments[1] + 1)
    }
    
    # Chilipepper SW
    if(grepl("CLPR_SWFSC", Spectra_Set)) {   
       shortNameSegments <- c(3, 2)
       shortNameSuffix <- 'Combo_Survey'     
    }
    
    # Chilipepper NW
    if(grepl("CLPR_NWFSC", Spectra_Set)) {   
       shortNameSegments <- c(1, 6)
       shortNameSuffix <- 'Combo_Survey'     
    }
    
    # Chilipepper CACOMM
    if(grepl("CLPR_CACOMM_2019_2020", Spectra_Set)) {   
       shortNameSegments <- c(1, 6)
       shortNameSuffix <- 'CA_Comm'     
    }
    
    if(Debug) {
        assign('Spectra_Set', Spectra_Set, pos = 1)
        assign('Spectra_Path', Spectra_Path, pos = 1)
        assign('shortNameSegments', shortNameSegments, pos = 1)
        assign('shortNameSuffix', shortNameSuffix, pos = 1)
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
    
    # -----------------------------------------------------------------------------------------------------------------------------------

  
    # --- Create a character vector of all spectral files within 'Spectra_Path'---   
    if(verbose)
       cat(paste("\nSpectra_Path =", Spectra_Path, "\n"))
    
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
        
    fileNames <- get.subs(fileNames.0, sep = ".")[1, ]  # No '.0' in the metadata xlsx
    
    # Read in metadata
    if(verbose)
       cat(paste("\nMeta_Path =", Meta_Path, "\n\n"))
    metadata <- openxlsx::read.xlsx(Meta_Path, sheet = excelSheet, detectDates = TRUE) # Load in ancillary data 
    
    if(Debug) {
       assign('shortName', shortName, pos = 1) 
       assign('fileNames', fileNames, pos = 1)
       assign('metadata', metadata, pos = 1) 
    }
    
    # Reading in spectra and doing interpolation if needed. For both methods, the wavebands used are based on very first OPUS spectra read-in. 
    if(spectraInterp %in% c('stats_splinefun_lowess', 'prospectr_resample')) {     # Below simplify = FALSE, so interpolation is not done by Roudier's opusreader::opus_read(). 
    
        # ----------- Re-sampling wavebands using stats::splinefun() with lowess() (inside of the JRWToolBox::predict.lowess() function) or prospectr::resample() --------------
        newScans.ADJ <- list()
        newScans_meta <- NULL
        for (i in 1:length(fileNames.0))  {
           print(fileNames.0[i])
           if(opusReader == 'pierreroudier_opusreader')
              try(newScans.ADJ[[i]] <- opusreader::opus_read(paste(Spectra_Path, fileNames.0[i] , sep = "/"), simplify = FALSE, wns_digits = 0)[[2]] )
              
           if(opusReader == 'philippbaumann_opusreader2') {
              try(Opus_Single <- opusreader2::read_opus_single(paste(Spectra_Path, fileNames.0[i] , sep = "/")))
              newScans.ADJ[[i]] <- Opus_Single[[3]]$data
              newScans_meta <- rbind(newScans_meta, data.frame(filenames = fileNames[i], 
                                      TSC_ref = ifelse(is.null(Opus_Single[["instrument_ref"]]$parameters$TSC$parameter_value), NA, Opus_Single[["instrument_ref"]]$parameters$TSC$parameter_value), # Scanner Reference Temperature
                                          TSC = ifelse(is.null(Opus_Single[["instrument"]]$parameters$TSC$parameter_value), NA, Opus_Single[["instrument"]]$parameters$TSC$parameter_value),      # Scanner Temperature
                                          TSM = ifelse(is.null(Opus_Single[["instrument_ref"]]$parameters$TSM$parameter_value), NA, Opus_Single[["instrument_ref"]]$parameters$TSM$parameter_value),  # TBD what TSM stands for
                                          HUM = ifelse(is.null(Opus_Single[["instrument_ref"]]$parameters$HUM$parameter_value), NA, Opus_Single[["instrument_ref"]]$parameters$HUM$parameter_value),  # Relative Humidity Interferometer
                                          DUR = ifelse(is.null(Opus_Single[["instrument_ref"]]$parameters$DUR$parameter_value), NA, Opus_Single[["instrument_ref"]]$parameters$DUR$parameter_value))) # Scan time (sec)
           }   
        }
        
        
        wavebandsToUse <- as.numeric(colnames(newScans.ADJ[[1]]))
        
        if(Debug) {
           assign('newScans_meta', newScans_meta, pos = 1)
           assign('newScans.ADJ', newScans.ADJ, pos = 1)
           assign("wavebandsToUse", wavebandsToUse, pos = 1)
        }
        
        if(verbose) {
            cat("\n\nWavebands being used:",  head(wavebandsToUse, 4), "...", tail(wavebandsToUse, 4))
            cat("\nDifference in wavebands:", diff(head(wavebandsToUse, 5)), "...", diff(tail(wavebandsToUse, 5)), "\n\n")
            cat("Tabulation of differences:"); Table(diff(wavebandsToUse)); cat("\n\n")
        }
               
        colnames(newScans.ADJ[[1]]) <- wavebandsToUse
        wavebandsToUse.8k <- wavebandsToUse[wavebandsToUse <= 8000]
        newScans.ADJ_int <- matrix(data = NA, nrow = length(newScans.ADJ), ncol = length(wavebandsToUse.8k)) # make empty matrix for loop
        newScans.ADJ_int[1, ] <- newScans.ADJ[[1]][wavebandsToUse <= 8000]
        
        for (j in 2:length(newScans.ADJ)) { 
          # bar(j, length(newScans.ADJ))
          print(j)
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
        newScans.RAW <- renum(as.data.frame(newScans.ADJ_int))
        # dim(newScans.RAW)
        # print(head(newScans.RAW[, c(1:5, (ncol(newScans.RAW) - 25):(ncol(newScans.RAW) - 20))])); cat("\n\n")
    }
    
    if(Debug)
       assign('newScans.RAW', newScans.RAW, pos = 1)
    
    if(verbose)
       cat("\nDimension of Spectral File Matrix Read In:", dim(newScans.RAW), "\n\n")
    
    
    # Model_Spectra_Meta <- dplyr::left_join(data.frame(filenames = fileNames, newScans.RAW), metadata, dplyr::join_by("filenames" == "NWFSC_NIR_Filename")) # Match by filenames and look at the data/metadata
    
    if(!OR_2008_2011) {
        
        metadata$specimen_id <- as.character(metadata$specimen_id)
        cat("\n\nmetadata$specimen_id = ", metadata$specimen_id[1:4], "\n\n")
    
        if(grepl("CLPR_SWFSC", Spectra_Set)) { 
        
           # metadata$specimen_id <- get.subs(metadata$specimen_id, sep = "_")[1, ]
           cat("\n\nModel_Spectra_Meta$specimen_id = ", get.subs(fileNames, '_')[2,][1:4], "\n\n")
           Model_Spectra_Meta <- dplyr::left_join(data.frame(filenames = fileNames, newScans.RAW, specimen_id = get.subs(fileNames, '_')[2,]), 
              metadata, dplyr::join_by("specimen_id" == "specimen_id")) # Match by specimen_id
              
        } else if(grepl("CLPR_CACOMM_1985", Spectra_Set) | grepl("CLPR_CACOMM_1986", Spectra_Set)) { 
        
           # metadata$specimen_id <- get.subs(metadata$specimen_id, sep = "_")[1, ]
           cat("\n\nModel_Spectra_Meta$specimen_id = ", substring(fileNames, 13)[1:4], "\n\n")
           # Model_Spectra_Meta <- dplyr::left_join(data.frame(filenames = fileNames, newScans.RAW, specimen_id = substring(fileNames, 13)), 
           #   metadata, dplyr::join_by("specimen_id" == "specimen_id")) # Match by specimen_id    
           
           Model_Spectra_Meta <- match.f(data.frame(filenames = fileNames, newScans.RAW, specimen_id = substring(fileNames, 13)), metadata,  "specimen_id",  "specimen_id")    
              
        } else if(grepl("CLPR_NWFSC", Spectra_Set) | grepl("CLPR_CACOMM_2019_2020", Spectra_Set) | grepl("CLPR_ORCOMM_2023_2024", Spectra_Set)) {  
           
           cat("\n\nModel_Spectra_Meta$specimen_id = ", get.subs(fileNames, '_')[6,][1:4], "\n\n")
           Model_Spectra_Meta <- dplyr::left_join(data.frame(filenames = fileNames, newScans.RAW, specimen_id = get.subs(fileNames, '_')[6,]), 
              metadata, dplyr::join_by("specimen_id" == "specimen_id")) # Match by specimen_id      
              
        } else {
        
            cat("\n\nmetadata$specimen_id = ", metadata$specimen_id[1:4], "\n\n")
            
            if(length(get.subs(metadata$specimen_id[1], sep = "_")) == 1) {
               cat("\n\nModel_Spectra_Meta$specimen_id = ", get.subs(fileNames, '_')[6,][1:4], "\n\n")
               Model_Spectra_Meta <- dplyr::left_join(data.frame(filenames = fileNames, newScans.RAW, specimen_id = get.subs(fileNames, '_')[6,]), 
                  metadata, dplyr::join_by("specimen_id" == "specimen_id")) # Match by specimen_id
            
            } else {
               cat("\n\nModel_Spectra_Meta$specimen_id = ", paste(get.subs(fileNames, '_')[6,], get.subs(fileNames, '_')[7,], sep = "_")[1:4], "\n\n")
               Model_Spectra_Meta <- dplyr::left_join(data.frame(filenames = fileNames, newScans.RAW, specimen_id = paste(get.subs(fileNames, '_')[6,], get.subs(fileNames, '_')[7,], sep = "_")), 
                    metadata, dplyr::join_by("specimen_id" == "specimen_id")) # Match by specimen_id
            }        
        }    
    }     
    
    if(OR_2008_2011) {
        metadata$specimen_id <- paste(get.subs(metadata$specimen_id, sep = "-")[1, ], metadata$sequence_number, sep = "_")
        
        Model_Spectra_Meta <- dplyr::left_join(data.frame(filenames = fileNames, newScans.RAW, specimen_id = paste(get.subs(fileNames, '_')[5,], get.subs(fileNames, '_')[6,], sep = "_")), 
               metadata, dplyr::join_by("specimen_id" == "specimen_id")) # Match by specimen_id
    }     
    
    SPECIMEN_ID <- Model_Spectra_Meta$specimen_id
    Model_Spectra_Meta$specimen_id <- NULL; Model_Spectra_Meta$specimen_id <- SPECIMEN_ID # Code expects 'project' to be the first column in the metadata - so move 'specimen_id' to the back    
    headTail(Model_Spectra_Meta, 3, 3, 3, 80)
    names(Model_Spectra_Meta)[names(Model_Spectra_Meta) %in% 'age_best'] <- "TMA" 
    names(Model_Spectra_Meta)[names(Model_Spectra_Meta) %in% 'WA_age_best'] <- "TMA"
    
    if(!(grepl("CLPR_SWFSC", Spectra_Set) | grepl("CLPR_CACOMM_1985", Spectra_Set) | grepl("CLPR_CACOMM_1986", Spectra_Set))) {
       Model_Spectra_Meta$percent_crystallized_scan[is.na(Model_Spectra_Meta$percent_crystallized_scan)] <- 0 # Change NA to zero so that a numerical test can be done.
       Model_Spectra_Meta$percent_missing_scan <- as.numeric(Model_Spectra_Meta$percent_missing_scan)
       Model_Spectra_Meta$percent_missing_scan[is.na(Model_Spectra_Meta$percent_missing_scan)] <- 0  # Change NA to zero so that a numerical test can be done.
       Model_Spectra_Meta$tissue_level_scan[is.na(Model_Spectra_Meta$tissue_level_scan)] <- 'none' # Change NA to 'none' tissue level.
    }     
     
    Model_Spectra_Meta <- data.frame(Model_Spectra_Meta, shortName = shortName)  
     
    #  if(verbose) {
    #     print(Model_Spectra_Meta[1:3, c(1, (grep('project', names(Model_Spectra_Meta))):ncol(Model_Spectra_Meta))])     
    #     CAT("\N\N")
    #  }
    
    # -- structure_weight_dg --
    if(!is.null(Model_Spectra_Meta$structure_weight_g))  {  # line 290 of 2019 has "N/A" in "CLPR_NWFSC"
           Model_Spectra_Meta$structure_weight_g <- as.numeric(Model_Spectra_Meta$structure_weight_g)
           Model_Spectra_Meta$structure_weight_dg = 10 * Model_Spectra_Meta$structure_weight_g # dg = decigram
           Model_Spectra_Meta <- Model_Spectra_Meta[!is.na(Model_Spectra_Meta$structure_weight_dg), ]
	}
    
    if(Debug)
       assign('Model_Spectra_Meta_D1', Model_Spectra_Meta, pos = 1)

    if(!is.null(Extra_Meta_Path)) {    

        if(verbose) 
           cat("\n\n Extra_Meta_Path = ", Extra_Meta_Path, "\n\n")        
    
        if(Spectra_Set == "PWHT_Acoustic2019")
        
            Model_Spectra_Meta <- match.f(Model_Spectra_Meta,  PWHT_Acoustic2019_Extra_Metadata, "specimen_id", "Specimen_Number", c('Length_cm', 'Weight_kg', 'Sex'))
        
        else  {
        
            Object_Name <- JRWToolBox::load(Extra_Meta_Path) # 'DW' is NWFSC Data Warehouse
            assign('metadata_DW', eval(parse(text = Object_Name[grep("Metadata", Object_Name)])))
            
            if(Debug)
               assign('metadata_DW', metadata_DW, pos = 1)
            
            Model_Spectra_Meta$length_cm <- Model_Spectra_Meta$Length_cm <- Model_Spectra_Meta$weight_kg <- Model_Spectra_Meta$Sex <- Model_Spectra_Meta$Latitude <- Model_Spectra_Meta$Longitude <- NULL   
            Model_Spectra_Meta <- match.f(Model_Spectra_Meta, metadata_DW, "specimen_id", "AgeStr_id", c('Length_cm', 'Weight_kg', 'Sex', 'Depth_m', 'Latitude_dd', 'Month', 'Days_into_Year'))
        }
     
        
        if(Debug)        
            assign('Model_Spectra_Meta_D2', Model_Spectra_Meta, pos = 1)
                    
        if(!is.null(Model_Spectra_Meta$Depth_m)) {
           if(sum(abs(diff(Model_Spectra_Meta$Depth_m[!is.na(Model_Spectra_Meta$Depth_m)]))) == 0) # Are all the values the same?
             Model_Spectra_Meta$Depth_prop_max <- 0.5
           else
             Model_Spectra_Meta$Depth_prop_max <- (Model_Spectra_Meta$Depth_m - ifelse(exists("Sable_Depth_m_Range"), Sable_Depth_m_Range[1], min(Model_Spectra_Meta$Depth_m, na.rm = TRUE)))/
                            (ifelse(exists("Sable_Depth_m_Range"), Sable_Depth_m_Range[2], max(Model_Spectra_Meta$Depth_m, na.rm = TRUE)) - 
                                ifelse(exists("Sable_Depth_m_Range"), Sable_Depth_m_Range[1], min(Model_Spectra_Meta$Depth_m, na.rm = TRUE)))
        }
         
        if(!is.null(Model_Spectra_Meta$Latitude_dd))
           Model_Spectra_Meta$Latitude_prop_max <- (Model_Spectra_Meta$Latitude_dd - 30.5)/(49.1 - 30.5)
           
           
        if(!is.null(Model_Spectra_Meta$Days_into_Year)) {
           if(sum(abs(diff(Model_Spectra_Meta$Days_into_Year[!is.na(Model_Spectra_Meta$Days_into_Year)]))) == 0)  # Are all the values the same?
             Model_Spectra_Meta$Days_into_Year_prop_max <- 0.5
           else
             Model_Spectra_Meta$Days_into_Year_prop_max <- (Model_Spectra_Meta$Days_into_Year - min(Model_Spectra_Meta$Days_into_Year, na.rm = TRUE))/(max(Model_Spectra_Meta$Days_into_Year, na.rm = TRUE) - min(Model_Spectra_Meta$Days_into_Year, na.rm = TRUE))
         }  
         
        # For consistennce, don't use the 'light' (or other level) of tissue level until a study is done
        # TF <- Model_Spectra_Meta$percent_crystallized_scan <= 15 & Model_Spectra_Meta$percent_missing_scan <= 10 & !is.na(Model_Spectra_Meta$Month_Scaled) & 
        #         (Model_Spectra_Meta$tissue_level_scan == "light" | is.na(Model_Spectra_Meta$tissue_level_scan)) & !is.na(Model_Spectra_Meta$Length_cm) & !is.na(Model_Spectra_Meta$structure_weight_g)
            
        if(!is.null(Model_Spectra_Meta$percent_crystallized_scan))
           TF <- Model_Spectra_Meta$percent_crystallized_scan <= 15 & Model_Spectra_Meta$percent_missing_scan <= 10 & Model_Spectra_Meta$tissue_level_scan == 'none' 
		else  
           TF <- rep(TRUE, nrow(Model_Spectra_Meta))   
        
    } else {
        if(grepl("CLPR_CACOMM", Spectra_Set) | grepl("CLPR_ORCOMM", Spectra_Set)) {   
           Model_Spectra_Meta$Sex <- recode.simple(Model_Spectra_Meta$Sex, cbind(c(1, 2, NA), c('M', 'F', 'U')))
           Model_Spectra_Meta$Month <- Months.POSIXt(Model_Spectra_Meta$catch_date)
        }   
        TF <- rep(TRUE, nrow(Model_Spectra_Meta))
    }
    
    if(!is.null(Model_Spectra_Meta$Length_cm))
        Model_Spectra_Meta$Length_prop_max <- Model_Spectra_Meta$Length_cm/ifelse(exists("Sable_Len_cm_Range"), Sable_Len_cm_Range[2], max(Model_Spectra_Meta$Length_cm, na.rm = TRUE))
        
    if(!is.null(Model_Spectra_Meta$Weight_kg) & !all(is.na(Model_Spectra_Meta$Weight_kg)))
        Model_Spectra_Meta$Weight_prop_max <- (Model_Spectra_Meta$Weight_kg - ifelse(exists("Sable_Wght_kg_Range"), Sable_Wght_kg_Range[1], min(Model_Spectra_Meta$Weight_kg, na.rm = TRUE)))/
                            (ifelse(exists("Sable_Wght_kg_Range"), Sable_Wght_kg_Range[2], max(Model_Spectra_Meta$Weight_kg, na.rm = TRUE)) - 
                                ifelse(exists("Sable_Wght_kg_Range"), Sable_Wght_kg_Range[1], min(Model_Spectra_Meta$Weight_kg, na.rm = TRUE)))
        
    
    # -- Sex, One hot encoding --
    if(!is.null(Model_Spectra_Meta$Sex)) {
        Model_Spectra_Meta$Sex <- as.character(Model_Spectra_Meta$Sex)
        Model_Spectra_Meta$Sex[is.na(Model_Spectra_Meta$Sex)] <- "U"
        Sex_ <- Model_Spectra_Meta$Sex
        Model_Spectra_Meta <- cbind(Model_Spectra_Meta, as.data.frame(model.matrix(formula(~ -1 + Sex_))))  # Three indicator columns added: Sex_F, Sex_M, Sex_U
        if(is.null(Model_Spectra_Meta$Sex_U))  Model_Spectra_Meta$Sex_U <- 0
    }   
    
    #  -- Month, One hot encoding -- 
    if(!is.null(Model_Spectra_Meta$Month) & !all(is.na(Model_Spectra_Meta$Month))) {
	    Month_ <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')[Model_Spectra_Meta$Month]
	    if(length(unique(Model_Spectra_Meta$Month)) > 2) {
                Model_Spectra_Meta <- cbind(Model_Spectra_Meta[!is.na(Model_Spectra_Meta$Month), ], as.data.frame(model.matrix(formula(~ -1 + Month_)))) # Indicator columns added: Month_May, Month_Jun, Month_Jul, ...
	    } else {
	        Model_Spectra_Meta <- Model_Spectra_Meta[!is.na(Model_Spectra_Meta$Month), ]
	    	Model_Spectra_Meta[paste0("Month_", unique(Month_))] <- 1
        }		
    }
    
    
    if(TMA_Ages_Only) {     
        TF <- TF & !is.na(Model_Spectra_Meta$TMA)
        if(verbose)
           cat('\n\nNumber of oties with TMA age:', sum(!is.na(Model_Spectra_Meta$TMA)), "\n\n")
    }
    
    Model_Spectra_Meta <- renum(Model_Spectra_Meta[TF, ])
    
    if(Debug)
       assign("Model_Spectra_Meta", Model_Spectra_Meta, pos = 1)
    
    if(verbose) {
        print(Model_Spectra_Meta[1:3, c(1, (grep('project', names(Model_Spectra_Meta))):ncol(Model_Spectra_Meta))])
        
        cat("\n\nRange of standardized metadata variables:\n\n")
        print(apply(Model_Spectra_Meta[, grep('prop_max', names(Model_Spectra_Meta)), drop = FALSE], 2, range, na.rm = T))
        
        cat("\n\nNumber of missing values in the standardized metadata variables within the final result:\n\n")
        print(apply(Model_Spectra_Meta[, grep('prop_max', names(Model_Spectra_Meta)), drop = FALSE], 2, function(x) sum(is.na(x))))
        
        cat(paste0('\n\nTotal number of oties read in: ', sum(TF) + sum(!TF), '.  Number rejected based on metadata (including missing TMA, when asked for): ', sum(!TF), '.  Number kept: ', sum(TF), '.\n'))
        cat("\nAfter the particular metadata is selected for in a model run, remove those oties which contain any missing values for those applications, like NN modeling, that cannot handle them.\n\n")
    }
    
    if(plot) {
    
        sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")
        
        if(is.null(Model_Spectra_Meta$TMA)) {
        
            rowNums <- 1:nrow(newScans.RAW)
            if(length(rowNums <= 26^2))
               plotly.Spec(data.frame(filenames = fileNames.0, newScans.RAW, Otie = paste0(LETTERS[floor((rowNums - 0.001)/26) + 1], LETTERS[rowNums %r1% 26], '_', rowNums), shortName = shortName), 
                           N_Samp = min(c(length(fileNames.0), Max_N_Spectra)), colorGroup = 'Otie')
            else
              plotly.Spec(data.frame(filenames = fileNames.0, newScans.RAW, Otie = factor(rowNums), shortName = shortName), N_Samp = min(c(length(fileNames.0), Max_N_Spectra)), colorGroup = 'Otie') 
            
            # if(!is.null(Static_Figure)) {
            #    dir.create("Figures", showWarnings = FALSE)
            #    ggsave(Static_Figure, path = "Figures", width = 14, height = 10, units = "in", dpi = 600)
            # }
               
        
        } else 
        
            plotly.Spec(Model_Spectra_Meta, N_Samp = min(c(nrow(Model_Spectra_Meta), Max_N_Spectra)), colorGroup = 'TMA')  

        if(!is.null(htmlPlotFolder)) {
              dir.create("Figures", showWarnings = FALSE)
              dir.create(paste0("Figures/", htmlPlotFolder), showWarnings = FALSE)
              saveHtmlFolder(paste0("Figures/", htmlPlotFolder), view = !interactive())
        }        
    }
     
    invisible(Model_Spectra_Meta)
}   
      
     
      
      
      
      
      
      


