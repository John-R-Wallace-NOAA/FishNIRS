

Predict_NN_Age_Wrapper <- function(Spectra_Set = c("Hake_2019", "Sable_2017_2019", "Sable_Combo_2022", "Sable_Combo_2021", "Sable_Combo_2019", "Sable_Combo_Multi_17_21")[3], 
                           Train_Result_Path = "C:/SIDT/Train_NN_Model", Model_Spectra_Meta_Path = NULL, Use_Session_Report_Meta = !grepl('Multi', Spectra_Set),
                           opusReader = c('pierreroudier_opusreader', 'philippbaumann_opusreader2')[2], Rdm_Reps_Main = 20, Folds_Num = 10, 
                           Max_N_Spectra = list(50, 200, 'All')[[2]], Seed_Plot = 707, Spectra_Path = "New_Scans", 
                           Predicted_Ages_Path = "Predicted_Ages", Meta_Add  = TRUE, TMA_Ages = TRUE, verbose = TRUE, plot = TRUE) {

    '  ################################################################################################################################################################                             '
    '  #       Need >= R ver 3.0                                                                                                                                      #                             '
    '  #                                                                                                                                                              #                             '
    '  # You may need a "GITHUB_PAT" from GitHub set somewhere in R (If you need help, search the Web how to get one from GitHub.)                                    #                             '
    '  # Sys.setenv(GITHUB_PAT = "**************")   # If you set GITHUB_PAT here, uncomment this line. Note, do not share your GITHUB_PAT, nor load it onto GitHib.  #                             '
    '  # Sys.getenv("GITHUB_PAT")                                                                                                                                     #                             '
    '  ################################################################################################################################################################                             '
    '                                                                                                                                                                                               '
    '  #  Spectra_Path  # Put new spectra scans in a separate folder and enter the name of the folder in this argument                                                                              '
    '  #  Predicted_Ages_Path # The NN predicted ages will go in the path defined by this argument                                                                                                  '
    '  #  Meta_Add  #  Will metadata be used                                                                                                                                                        '
    '  #  TMA_Ages  # Are TMA ages available and are they to be used?                                                                                                                               '
    '                                                                                                                                                                                               '
    '  #  Max_N_Spectra  # Default number of new spectra to be plotted in spectra figures. (The plot within Read_OPUS_Spectra() is given a different default below).                                '
    '  #                 # All spectra in the Spectra_Path folder will be assigned an age regardless of the number plotted in the figure.                                                           '
    '                                                                                                                                                                                                  '
    '  #  Model_Spectra_Meta_Path    # Example paths                                                                                                                                                        '
    '  #     "C:/SIDT/Train_NN_Model/Sable_Combo_2022_Model_Spectra_Meta_ALL_GOOD_DATA.RData"                                                                                                       '
    '  #     "C:/SIDT/Predict_NN_Ages/Sable_Combo_2022_Model_Spectra_Meta_ALL_GOOD_DATA_1556N.RData"                                                                                                '
    '  #     "C:/SIDT/Sablefish 2022 Combo/Sable_Combo_2022_NN_Sex/Sable_Combo_2022_NN_Fish_Len_Otie_Wgt_Weight_Depth_No_Lat_Male_Run_1/Sable_Combo_2022_Model_Spectra_Meta_ALL_GOOD_DATA.RData")    '        

     
     # ------------------------------------ Main User Setup ------------------------------------------------------------
      
     if(interactive()) {
           dir.create("C:/SIDT/Predict_NN_Ages", recursive = TRUE, showWarnings = FALSE)
           setwd(ifelse(.Platform$OS.type == 'windows', "C:/SIDT/Predict_NN_Ages", "/more_home/h_jwallace/SIDT/Predict_NN_Ages"))   # Change path to the Spectra Set's .GlobalEnv as needed
           getwd()
     }    
     
     if(!interactive())   
           options(width = 120)   
            
     dir.create(Predicted_Ages_Path, showWarnings = FALSE)
     
        
     #  ----------------- Packages ------------------------
     if (!any(installed.packages()[, 1] %in% "lattice")) 
          install.packages("lattice") 
     
     if (!any(installed.packages()[, 1] %in% "R.utils")) 
          install.packages("R.utils") 
          
     if(!any(installed.packages()[, 1] %in% "openxlsx")) 
            install.packages("openxlsx")     
     
     if (!any(installed.packages()[, 1] %in% "ggplot2")) 
          install.packages("ggplot2") 
     
     if (!any(installed.packages()[, 1] %in% "plotly")) 
          install.packages("plotly")    

     if (!any(installed.packages()[, 1] %in% "FSA")) 
          install.packages("FSA")              
          
     if (!any(installed.packages()[, 1] %in% "tensorflow")) 
          install.packages("tensorflow")
          
     if (!any(installed.packages()[, 1] %in% "keras")) 
          install.packages("keras") 
     
     library(lattice)
     library(R.utils)     
     library(ggplot2)
     library(plotly) 
     library(FSA)       
     library(tensorflow)
     library(keras)  
     
     
     # --- Download functions from GitHub ---
     sourceFunctionURL <- function (URL,  type = c("function", "script")[1]) {
               '   # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git()   '
               '   # Example to save a function to the working directory:   '
               '   # sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/rgit/master/R/gitAFile.R")   '
               '   # gitAFile("John-R-Wallace-NOAA/JRWToolBox/master/R/browsePlot.R", File = "browsePlot.R")   '
               
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
     ###
     
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/load.R") # For interactive use - not currently used below
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Date.R") 
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/gPlot.R")
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/sort.f.R")
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/match.f.R")
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/headTail.R")
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R")
	 sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/lowess.line.R")
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/switchSlash.R")   # Switch "\" to "/" for copied Windows paths [Copy path and run switchSlash() in R. Utilized by JRWToolBox::setWd()]
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/extractRData.R")  
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/saveHtmlFolder.R")
     
    
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly_spectra.R")
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Predict_NN_Age.R")
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Read_OPUS_Spectra.R")
     sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Cor_R_squared_RMSE_MAE_SAD_APE.R")
     
     getwd()
     Spectra_Set
     
     # =================================================================================================================================================================== 
        
     if(FALSE) {   
         # (1) Hake 2019, BMS
         if(Spectra_Set == "Hake_2019") {
            NN_Model <- 'FCNN Model/Hake_2019_FCNN_20_Rdm_models_1_Apr_2023.RData'   # Change path to the Spectra Set's NN model as needed - 10-20 random models each with 10-fold complete 'k-fold' models.
            shortNameSegments <- c(2, 4) # Segments 2 and 4 of the spectra file name, e.g.: (PACIFIC, HAKE, BMS201906206C, 1191, OD1) => (HAKE, 1191)
            shortNameSuffix <- 'BMS'
            opusReader <- 'pierreroudier_opusreader'
            fineFreqAdj <- 150
         }
          
         # (2) Sablefish 2017 & 2019, Combo survey
         if(Spectra_Set == "Sable_2017_2019") { 
            NN_Model <- 'FCNN Model/Sablefish_2017_2019_Rdm_models_22_Mar_2023_14_57_26.RData'
            shortNameSegments <- c(1, 3) # Segments 1 and 3 of the spectra file name, e.g.: (SABLEFISH, COMBO201701203A, 28, OD1) => (SABLEFISH, 28)
            shortNameSuffix <- 'Year'
            yearPosition <- c(6, 9) # e.g. COMBO201701203A => 2017 (Segment used (see above) is: shortNameSegments[1] + 1)
            fineFreqAdj <- 0
            opusReader <- 'pierreroudier_opusreader'
            Meta_Path <- "C:/SIDT/Sablefish/Keras_CNN_Models/Sable_2017_2019 21 Nov 2022.RData"  # If used, change path to the main sepectra/metadata save()'d data frame which contains TMA ages.  Matching done via 'filenames'.
         }  
         
         # (3) Sablefish 2022, Combo survey
         if(Spectra_Set == "Sable_Combo_2022") { 
         
             print(NN_Model <- paste0(Train_Result_Path, "/", list.files(Train_Result_Path, "FCNN_model..........Rdm_model")))
             NN_Pred_Median_TMA <- extractRData('Sable_Combo_2022_NN_Pred_Median_TMA', paste0(Train_Result_Path, "/", list.files(Train_Result_Path, "Pred_Median_TMA")))
             print(dim(NN_Pred_Median_TMA))
             
            print(Meta_Path <- paste0('C:/SIDT/Sablefish 2022 Combo/', Spectra_Set, '_NIRS_Scanning_Session_Report_For_NWFSC.xlsx'))
            print(Meta_Path_Save <- paste0(Predicted_Ages_Path, '/', Spectra_Set, '_NIRS_Scanning_Session_Report_with_NN_Ages_For_NWFSC.xlsx'))
            opusReader <- c('pierreroudier_opusreader', 'philippbaumann_opusreader2')[2]
             Sys.sleep(2)
         }  
         
         # (4) Sablefish 2021, Combo survey predicted with Sable 2022 Model
         if(Spectra_Set == "Sable_Combo_2021") { 
             # NN_Model <- "Sable_Combo_2022_FCNN_model_ver_1_20_Rdm_model_8_Apr_2024_11_06_09.RData"  # Sable_Combo_2022_NN_Fish_Len_Otie_Wgt_Weight_Depth_Lat_Run_3_BEST
             # NN_Pred_Median_TMA <- extractRData('Sable_Combo_2022_NN_Pred_Median_TMA', 
             #            "C:/SIDT/Sablefish 2022 Combo/Sable_Combo_2022_NN_Fish_Len_Otie_Wgt/Sable_Combo_2022_FCNN_model_ver_1_20_Pred_Median_TMA_15_Dec_2023_12_23_01.RData")
             
             Train_Result_Path <- "C:/SIDT/Sablefish 2022 Combo/Sable_Combo_2022_NN_FIND_BEST_METADATA/Sable_Combo_2022_NN_Fish_Len_Otie_Wgt_Weight_Depth_Lat_Run_3_BEST"
                      
             (NN_Model <- paste0(Train_Result_Path, "/", list.files(Train_Result_Path, "FCNN_model..........Rdm_model")))
             NN_Pred_Median_TMA <- extractRData('Sable_Combo_2022_NN_Pred_Median_TMA', paste0(Train_Result_Path, "/", list.files(Train_Result_Path, "Pred_Median_TMA")))
             dim(NN_Pred_Median_TMA)
             
             Meta_Path <- paste0('C:/SIDT/Sablefish 2021 Combo/', Spectra_Set, '_NIRS_Scanning_Session_Report_For_NWFSC.xlsx')  # !!!!! Change the original name of the Session Report to match this name. !!!!!
             # base::load("C:/SIDT/Sablefish/Sable_Combo_Ages_DW.RData")  # 'DW' is NWFSC Data Warehouse
             # metadata_DW <- Sable_Combo_Ages_DW; rm(Sable_Combo_Ages_DW)
             Meta_Path_Save <- paste0(Predicted_Ages_Path, '/', Spectra_Set, '_NIRS_Scanning_Session_Report_with_NN_Ages.xlsx')               
             opusReader <- c('pierreroudier_opusreader', 'philippbaumann_opusreader2')[2]
             Seed_Plot <- 707
         }  
         
         # (5) Sablefish 2019, Combo survey predicted with Sable 2022 Model
         if(Spectra_Set == "Sable_Combo_2019") { 
             NN_Model <- "Sable_Combo_2022_FCNN_model_40_Rdm_models_Runs_1_3.RData"  # Combine 20X Rdm Models/Created by Combine 20X Rdm Models.R
             NN_Pred_Median_TMA <- extractRData('Sable_Combo_2022_NN_Pred_Median_TMA', 
                         "C:/SIDT/Sablefish 2022 Combo/Sable_Combo_2022_NN_Fish_Len_Otie_Wgt/Sable_Combo_2022_FCNN_model_ver_1_20_Pred_Median_TMA_15_Dec_2023_12_23_01.RData")
             Meta_Path <- paste0('C:/SIDT/Sablefish 2019 Combo/', Spectra_Set, '_NIRS_Scanning_Session_Report.xlsx')  # !!!!! Change the original name of the Session Report to match this name. !!!!!
             # base::load("C:/SIDT/Sablefish/Sable_Combo_Ages_DW.RData")  # 'DW' is NWFSC Data Warehouse
             # metadata_DW <- Sable_Combo_Ages_DW; rm(Sable_Combo_Ages_DW)
             Meta_Path_Save <- paste0(Predicted_Ages_Path, '/', Spectra_Set, '_NIRS_Scanning_Session_Report_with_NN_Ages.xlsx')               
             opusReader <- c('pierreroudier_opusreader', 'philippbaumann_opusreader2')[2]
             Seed_Plot <- 707
         }  
    }
    
    print(NN_Model <- paste0(Train_Result_Path, "/", list.files(Train_Result_Path, "FCNN_model..........Rdm_model")))
    NN_Pred_Median_TMA <- extractRData(paste0(Spectra_Set, '_NN_Pred_Median_TMA'), paste0(Train_Result_Path, "/", list.files(Train_Result_Path, "Pred_Median_TMA")))
    headTail(NN_Pred_Median_TMA)
    Sys.sleep(2)
    
    if(Use_Session_Report_Meta) {  #  Meta_Path cannot be FALSE if Read_OPUS_Spectra() is used below. Read_OPUS_Spectra() in this function currently only works for single year predictions.
       print(Meta_Path <- paste0('C:/SIDT/', Spectra_Set, '/', Spectra_Set, '_NIRS_Scanning_Session_Report_For_NWFSC.xlsx'))
       print(Meta_Path_Save <- paste0(Predicted_Ages_Path, '/', Spectra_Set, '_NIRS_Scanning_Session_Report_with_NN_Ages_For_NWFSC.xlsx'))
    }
      
      
     #  --- Conda TensorFlow environment ---
     Conda_TF_Eniv <- ifelse(.Platform$OS.type == 'windows', "C:/m3/envs/tf", "/more_home/h_jwallace/Python/tf_cpu_only/bin")  # Change this path as needed
     Sys.setenv(RETICULATE_PYTHON = Conda_TF_Eniv) 
     Sys.getenv("RETICULATE_PYTHON") 
      
     #  ---- Note if you get this error: < Error in `[.data.frame`(data.frame(prospectr::savitzkyGolay(newScans.RAW, : undefined columns selected > or you know that 
     #         the new spectra scan(s) do not have the same freq. as the model expects, then add the file 'FCNN\AAA_********_Correct_Scan_Freq' to your scans and an interpolation will be done. ---
     #         '********' is based on the spectra set you are currently working with, e.g. AAA_PACIFIC_HAKE_2019_Correct_Scan_Freq. If you add this file, the first NN age reported will be from this file, and can be ignored/removed.
     #         If the batch run is crashing, you can first try to adding this file to your scans to see if that fixes the issue.
      
     
     # --- TensorFlow Load and Math Check  ---
     tf_config()
     
     a <- tf$Variable(5.56)
     cat("\n\nTensorFlow Math Check\n\na = "); print(a)
     b <- tf$Variable(2.7)
     cat("\nb = "); print(b)
     cat("\na + b = "); print(a + b)
     cat("\n\n")
     
     k_clear_session() 
     
     print(getwd())
     print(Spectra_Set)
     
     # ============= Pause here when interactively submitting code to R =================
     
     # --- Use Predict_NN_Age() to find the NN predicted ages ---  
     
     (fileNames <- dir(path = Spectra_Path))[1:10]
     if(exists('shortNameSuffix') && shortNameSuffix == 'Year')
        shortNameSuffix. <- apply(matrix(fileNames, ncol = 1), 1, function(x) substr(get.subs(x, sep = "_")[shortNameSegments[1] + 1], yearPosition[1], yearPosition[2]))
     
     if(exists('shortNameSuffix') && shortNameSuffix != 'Year')
        shortNameSuffix. <- shortNameSuffix
        
     if(!exists('shortNameSuffix'))
         shortNameSuffix. <- NULL
     
     # Maximum number of wavebands to show in the spectra figure
     if(length(fileNames > 0))
        N_Samp <- ifelse(is.numeric(Max_N_Spectra), min(c(length(fileNames), Max_N_Spectra)), 'All')
     else
       N_Samp <- ifelse(is.numeric(Max_N_Spectra), Max_N_Spectra, 'All')
     
     cat("\n\nN_Samp =", N_Samp, "\n\n")
     
     if(is.null(Model_Spectra_Meta_Path))
        Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set, Spectra_Path = Spectra_Path, TMA_Ages = TMA_Ages, Max_N_Spectra = N_Samp, verbose = verbose, Meta_Add = Meta_Add,
                                      Meta_Path = Meta_Path, plot = plot, htmlPlotFolder = paste0(Predicted_Ages_Path, '/', Spectra_Set, '_Spectra_Sample_of_', N_Samp))
     else
       load(Model_Spectra_Meta_Path)     
     
     headTail(Model_Spectra_Meta, 2, 2, 3, 46)
     
     #   # Change 'Length_prop_max' to 'length_prop_max' if flag is set above
     #   if(lower_case_length_prop_max)
     #       names(Model_Spectra_Meta)[grep("Length_prop_max", names(Model_Spectra_Meta))] <- "length_prop_max"
     #   headTail(Model_Spectra_Meta, 2, 2, 3, 46)
     
     # Extract the metadata
     metadata <- Model_Spectra_Meta[, c(1, (grep('project', names(Model_Spectra_Meta))):ncol(Model_Spectra_Meta))]
     headTail(metadata, 2)
     
     # For testing Read_OPUS_Spectra():  Meta_Add <- TRUE; spectraInterp = 'stats_splinefun_lowess'; excelSheet <- 3; opusReader = 'philippbaumann_opusreader2'; (htmlPlotFolder <- paste0(Predicted_Ages_Path, '/', Spectra_Set, '_Spectra_Sample_of_', N_Samp))
     
     
     ##### This is the MAIN CALL to Predict_NN_Age() function #####
     
     # New_Ages <- Predict_NN_Age(Conda_TF_Eniv, Spectra_Path, NN_Model, plot = plot, NumRdmModels = 1,  htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure for New Ages'), spectraInterp = spectraInterp, fineFreqAdj = fineFreqAdj,
     #      Predicted_Ages_Path = Predicted_Ages_Path,  shortNameSegments = shortNameSegments, shortNameSuffix = shortNameSuffix., N_Samp = N_Samp, verbose = verbose) # One random model for faster testing
     
     # New_Ages <- Predict_NN_Age(Conda_TF_Eniv, Spectra_Path, Model_Spectra_Meta, NN_Model, plot = plot, htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure for New Ages'), spectraInterp = spectraInterp, fineFreqAdj = fineFreqAdj, opusReader = opusReader, 
     #    Predicted_Ages_Path = Predicted_Ages_Path,  shortNameSegments = shortNameSegments, shortNameSuffix = shortNameSuffix., N_Samp = N_Samp, verbose = verbose) # Use the max number of random model replicates available
     
     New_Ages_Pred <- Predict_NN_Age(Conda_TF_Eniv, Spectra_Path, Model_Spectra_Meta, NN_Model, plot = plot, htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure for New Ages'),  
                                         Predicted_Ages_Path = Predicted_Ages_Path, opusReader = opusReader, N_Samp = N_Samp, verbose = verbose, Folds_Num = Folds_Num) # Use the max number of random model replicates available (the default for arg 'NumRdmModels')
     
     New_Ages <- New_Ages_Pred[['New_Ages']]
     
     cat("\n\nNew_Ages  Missing Predictions\n")
     print(New_Ages[is.na(New_Ages$NN_Pred_Median), ])  # Missing predictions
     
     cat("\n\nNew_Ages\n")
     New_Ages <- New_Ages[!is.na(New_Ages$NN_Pred_Median), ]  # Non-missing predictions
     headTail(New_Ages)
     
     newScans.pred.ALL <- New_Ages_Pred[['newScans.pred.ALL']]
     headTail(newScans.pred.ALL, 3, 3)
     
     
     # For testing Predict_NN_Age(): plot = TRUE; NumRdmModels = c(1, 20)[2];  htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure for New Ages'); N_Samp = N_Samp                                    
          
     
     #  -- Look length and weight vs TMA and each other -- 
     # sum(is.na(metadata$weight_kg))
     # browsePlot('plot.lowess(metadata$TMA, metadata$length_cm)', file = 'Sablefish Combo 2024 Length vs TMA.png')
     # browsePlot('plot.lowess(metadata$TMA, metadata$weight_kg)', file = 'Sablefish Combo 2024 Weight vs TMA.png')
     # browsePlot('plot.lowess(metadata$length_cm, metadata$weight_kg, 0.15)', file = 'Sablefish Combo 2024 Length vs Weight.png')
     
     
     # --- If TMA ages are not available ---
     if(!TMA_Ages) {
     
         sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/browsePlot.R") 
         
         # ----- Extract the rounding Delta -----
         Delta <- extractRData('roundingDelta', file = NN_Model) # e.g. the rounding Delta for 2019 Hake is zero.  
         New_Ages$Age_Rounded <- round(New_Ages$NN_Pred_Median + Delta)
         cat(paste0("\n\nUsing a rounding Delta of ", Delta, "\n\n"))
        
         # --- Save ages ---
         save(New_Ages, file = paste0(Predicted_Ages_Path, '/NN Predicted Ages, ', Date(" "), '.RData'))
         
          # --- Save metadata to a new NIRS_Scanning_Session_Report
         # metadata$length_cm <- metadata$weight_kg <- NULL   
         # metadata <- match.f(metadata, metadata_DW, "specimen_id", "AgeStr_id", c('Length_cm', 'Weight_kg'))
         metadata <- match.f(metadata, New_Ages, "filenames", "filenames", c("NN_Pred_Median", "Lower_Quantile_0.025", "Upper_Quantile_0.975"))
         
         if(Use_Session_Report_Meta) {
            metadata.wb <- openxlsx::loadWorkbook(Meta_Path) # Load in ancillary data 
            # metadata.wb # View WorkBook object
            openxlsx::addWorksheet(metadata.wb, paste0('Metadata + NN Ages, ', Date(" ")))
            openxlsx::writeData(metadata.wb, paste0('Metadata + NN Ages, ', Date(" ")), metadata)
            # metadata.wb # View WorkBook object
            # Meta_Data_RAW <- read.xlsx(metadata.wb, "Sample_List_Data")
            openxlsx::saveWorkbook(metadata.wb, Meta_Path_Save, overwrite = TRUE)
         }
         
         # --- Add Index to New_ages and set cols for the figures below ---
         New_Ages <- data.frame(Index = 1:nrow(New_Ages), New_Ages)  # Add 'Index' as the first column in the data frame
         headTail(New_Ages, 3)
         cols <- 'green'
          
         # -- Plot by order implied by the spectra file names --
         g <- ggplot(New_Ages, aes(Index, NN_Pred_Median)) +  
         geom_point() +
         geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
         geom_point(aes(Index, Age_Rounded, col = cols)) + 
         scale_color_manual(labels = 'Rounded Age', values = cols, name = ' ')
         browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/Predicted_Ages_Order_by_File_Names.png'))
          
         
         # -- Plot by sorted NN predicted ages --
         New_Ages_Sorted <- sort.f(New_Ages, 'NN_Pred_Median') # Sort 'New_ages' by 'NN_Pred_Median', except for "Index" (see the next line below)
         New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  # Reset Index for graphing
         if(verbose) head(New_Ages_Sorted, 10)
         
         g <- ggplot(New_Ages_Sorted, aes(Index, NN_Pred_Median)) +  
         geom_point() +
         geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
         geom_point(aes(Index, Age_Rounded, col = cols)) + 
         scale_color_manual(labels = 'Rounded Age', values = cols, name = ' ')
         browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/Predicted_Ages_Sorted.png'))
     }
     
     
     # --- Use TMA ages, if available ---
     if(TMA_Ages) {
     
         sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Table.R") 
         sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/match.f.R") 
         sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/browsePlot.R") 
         sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/agreementFigure.R")
         
         # -- Download functions from GitHub into the working directory to look at and/or edit --
         # sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/GitHub_File_Download.R")
         # GitHub_File_Download("John-R-Wallace-NOAA/FishNIRS/master/R/agreementFigure.R")
         # GitHub_File_Download("John-R-Wallace-NOAA/FishNIRS/master/R/Read_OPUS_Spectra.R")
           
         New_Ages$TMA <- NULL # Clear old TMA before updating
         if(length(get.subs(get.subs(New_Ages$filenames[1], sep = "."))) == 2)
             New_Ages$filenames <- get.subs(New_Ages$filenames, sep = ".")[1,]
         New_Ages <- match.f(New_Ages, Model_Spectra_Meta, 'filenames', 'filenames', 'TMA')  
         headTail(New_Ages)
         cat("\n\nLooking for a Delta that gives an improved fit based on SAD with ties broken by APE:\n")  # R Squared)
         
         
          if(nrow(New_Ages) > nrow(NN_Pred_Median_TMA)) {
              # What is the best Delta (by SAD, with ties broken by APE) on the median over all, Rdm_reps, full k-folds. A new Delta (perhaps the same value) can be found here since TMA ages are available.
              Delta_Table <- NULL
              for (Delta. in seq(0, -0.45, by  = -0.05)) {
                cat("\n\n")
                print(data.frame(Delta = Delta., Cor_R_squared_RMSE_MAE_SAD_APE(New_Ages$TMA, round(New_Ages$NN_Pred_Median + Delta.))))
                Delta_Table <- rbind(Delta_Table, c(Delta = Delta., Cor_R_squared_RMSE_MAE_SAD_APE(New_Ages$TMA, round(New_Ages$NN_Pred_Median + Delta.))))
              }
              
              print(Delta_Table <- data.frame(Delta_Table)) 
                
              # Best Delta from table above
               (Delta <- as.numeric(Delta_Table$Delta)[order(as.numeric(Delta_Table$SAD), as.numeric(Delta_Table$APE))[1]])
               cat("\nBest Delta from the table above", Delta, "\n\n")
          
          } else {
             # ----- Extract the rounding Delta -----
             Delta <- extractRData('roundingDelta', file = NN_Model) # e.g. the rounding Delta for 2019 Hake is zero.  
         }
         
         New_Ages$Age_Rounded <- round(New_Ages$NN_Pred_Median + Delta)
         cat(paste0("\n\nUsing a rounding Delta of ", Delta, "\n\n"))
         nrow(newScans.pred.ALL)/max(newScans.pred.ALL$Index)
     
         
         # --- Save ages ---
         save(New_Ages, file = paste0(Predicted_Ages_Path, '/NN Predicted Ages, ', Date(" "), '.RData'))
          save(newScans.pred.ALL, file = paste0(Predicted_Ages_Path, '/newScans.pred.ALL, ', Date(" "), '.RData'))
         
          # --- Save metadata to a new NIRS_Scanning_Session_Report
         # metadata$length_cm <- metadata$weight_kg <- NULL   
         # metadata <- match.f(metadata, metadata_DW, "specimen_id", "AgeStr_id", c('Length_cm', 'Weight_kg'))
         metadata <- match.f(metadata, New_Ages, "filenames", "filenames", c("NN_Pred_Median", "Lower_Quantile_0.025", "Upper_Quantile_0.975", "TMA"))
         
         if(Use_Session_Report_Meta)  {
            metadata.wb <- openxlsx::loadWorkbook(Meta_Path) # Load in ancillary data 
            # metadata.wb # View WorkBook object
            openxlsx::addWorksheet(metadata.wb, paste0('Metadata + NN Ages, ', Date(" ")))
            openxlsx::writeData(metadata.wb, paste0('Metadata + NN Ages, ', Date(" ")), metadata)
            # metadata.wb # View WorkBook object
            # Meta_Data_RAW <- read.xlsx(metadata.wb, "Sample_List_Data")
            openxlsx::saveWorkbook(metadata.wb, Meta_Path_Save, overwrite = TRUE)
          }
         
         # --- Add Index to New_ages and set colors and pchs for the figures below ---
         New_Ages <- data.frame(Index = 1:nrow(New_Ages), Year = as.numeric(substring(get.subs(New_Ages$filenames, sep = "_")[2, ], 6)), New_Ages, TMA_Minus_Age_Rounded = New_Ages$TMA - New_Ages$Age_Rounded)  # Add 'Index' as the first column in the data frame and Year the second
         headTail(New_Ages, 3)
		 Table(New_Ages$Year)
		  
         assign('New_Ages', New_Ages, pos = 1)
         assign('Delta', Delta, pos = 1) 
         assign('Seed_Plot', Seed_Plot, pos = 1) 
         assign('Rdm_Reps_Main', Rdm_Reps_Main, pos = 1)
         assign('Folds_Num', Folds_Num, pos = 1)
         cols <- c('green', 'red')
         pchs <- c(16, 1)
         
         
         # -- Spectra Figure with TMA for New Ages --
         plotly.Spec(Model_Spectra_Meta, N_Samp = N_Samp, htmlPlotFolder = paste0(Predicted_Ages_Path, '/Spectra Figure with TMA for New Ages'))
        
         
         # -- Agreement Figures --
         # browsePlot('agreementFigure(New_Ages$TMA, New_Ages$NN_Pred_Median, Rdm_Reps = Rdm_Reps_Main, Folds = Folds_Num, Delta = Delta, full = TRUE)', file = paste0(Predicted_Ages_Path, '/Agreement_Figure.pdf'), pdf = TRUE)   
         browsePlot('agreementFigure(New_Ages$TMA, New_Ages$NN_Pred_Median, Rdm_Reps = Rdm_Reps_Main, Folds = Folds_Num, Delta = Delta, full = TRUE)', file = paste0(Predicted_Ages_Path, '/Agreement_Figure.png'))
         browsePlot('agreementFigure(New_Ages$TMA, New_Ages$NN_Pred_Median, Rdm_Reps = Rdm_Reps_Main, Folds = Folds_Num, Delta = Delta, full = FALSE)', file = paste0(Predicted_Ages_Path, '/Agreement_Figure_Zoomed.png'))
		 
		 if(length(unique(New_Ages$Year)) > 1) {
            for(Year in unique(New_Ages$Year)) {
			   assign('Year', Year, pos = 1)
			   assign('New_Ages_Year', New_Ages[New_Ages$Year %in% Year, ], pos = 1)
	           browsePlot('agreementFigure(New_Ages_Year$TMA, New_Ages_Year$NN_Pred_Median, Rdm_Reps = Rdm_Reps_Main, Folds = Folds_Num, Delta = Delta, full = TRUE, main = Year)', file = paste0(Predicted_Ages_Path, '/Agreement_Figure_', Year, '.png'))
            }
         }
		 
         if(verbose & !interactive())  Sys.sleep(5)
        
        
         # -- Plot by order implied by the spectra file names -  ???? ggplotly() changes how scale_color_manual() works ???? --
         g <- ggplot(New_Ages, aes(Index, NN_Pred_Median)) +  
         geom_point() +
         geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
         geom_point(aes(Index, Age_Rounded, col = cols[1])) + 
         geom_point(aes(Index + 0.1, TMA, col = cols[2])) + 
         scale_color_manual(labels = c('Rounded Age', 'TMA'), values = cols, name = ' ')
         assign('g', g, pos = 1)
         browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/Predicted_Ages_Order_by_File_Names.png'), width = 18, height = 10,)
         
         # New_Ages$Rounded_Age <- factor(" ") # This is needed for ggplotly plotting below
         # g <- ggplotly(ggplot(New_Ages, aes(TMA, NN_Pred_Median)) +  
         # geom_point() +
         # geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
         # geom_point(aes(TMA, Age_Rounded, color = Rounded_Age)) + scale_color_manual(values = c(" " = "green")), dynamicTicks = TRUE)
         # print(g)
         # unlink(paste0(Predicted_Ages_Path, '/Predicted_Ages_Order_by_File_Names'), recursive = TRUE)
         # saveHtmlFolder(paste0(Predicted_Ages_Path, '/TMA vs Predicted_Ages'), view = !interactive())
        
        
         # -- Relative error by TMA age --
         g <- xyplot((NN_Pred_Median - TMA)/ifelse(TMA == 0, 1, TMA) ~ TMA, group = TMA, data = New_Ages, ylab = "(NN_Pred_Median - TMA)/TMA (TMA in denominator set to 1 if TMA = 0)",
              panel = function(...) { panel.xyplot(...); panel.abline(h = 0, col = 'grey') })
         assign('g', g, pos = 1)
         browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/Relative_error_by_TMA_age.png'))
         
         # -- Plot of relative error by sorted TMA age --   
         New_Ages_Sorted <- sort.f(New_Ages, 'TMA')  # Sort 'New_ages' by TMA, except for "Index" (see the next line below)
         New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  # Reset Index for graphing
         if(verbose) headTail(New_Ages_Sorted, 5)
         g <- xyplot((NN_Pred_Median - TMA)/ifelse(TMA == 0, 1, TMA) ~ Index, group = TMA, data = New_Ages_Sorted, ylab = "(NN_Pred_Median - TMA)/TMA (TMA in denominator set to 1 if TMA = 0)",
               panel = function(...) { panel.xyplot(...); panel.abline(h = 0, col = 'grey') })
         assign('g', g, pos = 1)
         browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/Relative_error_by_sorted_TMA.png'))
         
         
         # -- Plot by sorted NN predicted ages --
         New_Ages_Sorted <- sort.f(New_Ages, 'NN_Pred_Median') # Sort 'New_ages' by 'NN_Pred_Median', except for "Index" (see the next line below)
            #  New_Ages_Sorted <- sort.f(New_Ages[sample(1:nrow(New_Ages_Sorted), 200), ], 'NN_Pred_Median') 
            #  New_Ages_Sorted <- New_Ages_Sorted[New_Ages_Sorted$NN_Pred_Median <= 10, ]
         New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  # Reset Index for graphing
         if(verbose) head(New_Ages_Sorted, 20)
         g <- ggplot(New_Ages_Sorted, aes(Index, NN_Pred_Median)) + 
         geom_point() +
         geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
         geom_point(aes(Index, Age_Rounded, col = cols[1]), pch = pchs[1]) + 
         geom_point(aes(Index, TMA, col = cols[2]), pch = pchs[2]) +  
         scale_color_manual(labels = c('Rounded Age', 'TMA'), values = cols, name = ' ') 
         assign('g', g, pos = 1)         
         browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/Predicted_Ages_Sorted.png'))
         
         # https://r-graphics.org/recipe-scatter-shapes   
         # scale_color_manual(labels = c('Rounded Age', 'TMA'), values = list(colour = cols, pch = pchs), aesthetics = c('colour', 'shape'), name = ' ') 
         # scale_shape_manual(values = pchs)
         # scale_fill_manual(values = c(cols[1], NA), guide = guide_legend(override.aes = list(shape = pchs[2])))
         # guides(fill=guide_legend(override.aes=list(shape=16))) +
         # scale_shape_manual(values = pchs, guide = guide_legend(override.aes = list(alpha = 1, size = 10)))
        
         
         # -- Plot SUBSET of data by sorted NN predicted ages --
         set.seed(Seed_Plot)
         New_Ages_Sorted <- sort.f(New_Ages[sample(1:nrow(New_Ages), 150), ], 'NN_Pred_Median')  # Sort 'New_ages' by 'NN_Pred_Median', except for "Index" (see the next line below)
         New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  # Reset Index for graphing
         if(verbose) head(New_Ages_Sorted, 20)
         g <- ggplot(New_Ages_Sorted, aes(Index, NN_Pred_Median)) + 
         geom_point() +
         geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
         geom_point(aes(Index, Age_Rounded, col = cols[1]), pch = pchs[1]) + 
         geom_point(aes(Index, TMA, col = cols[2]), pch = pchs[2]) +  
         scale_color_manual(labels = c('Rounded Age', 'TMA'), values = cols, name = ' ') 
         assign('g', g, pos = 1)
         browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/Predicted_Ages_Sorted_Subset.png'))
        
         
         # -- Plot by sorted TMA --
         New_Ages_Sorted <- na.omit(New_Ages_Sorted)
         if(verbose) head(New_Ages_Sorted, 20)
         g <- ggplot(New_Ages_Sorted, aes(Index, NN_Pred_Median)) +  
         geom_point() +
         geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
         geom_point(aes(Index, TMA, col = cols[2])) + 
         geom_point(aes(Index, Age_Rounded, col = cols[1])) + 
         scale_color_manual(labels = c('Rounded Age', 'TMA'), values = cols, name = ' ')
         assign('g', g, pos = 1)
         browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/TMA_Sorted.png'))
         
         
         # -- Plot SUBSET of data by sorted by TMA --
         set.seed(Seed_Plot)
         New_Ages_Sorted <- sort.f(New_Ages[sample(1:nrow(New_Ages), 150), ], 'TMA') 
         New_Ages_Sorted$Index <- sort(New_Ages_Sorted$Index)  
         New_Ages_Sorted <- na.omit(New_Ages_Sorted)
         if(verbose) head(New_Ages_Sorted, 20)
         g <- ggplot(New_Ages_Sorted, aes(Index, NN_Pred_Median)) +  
         geom_point() +
         geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
         geom_point(aes(Index, TMA, col = cols[2])) + 
         geom_point(aes(Index, Age_Rounded, col = cols[1])) + 
         scale_color_manual(labels = c('Rounded Age', 'TMA'), values = cols, name = ' ')
         assign('g', g, pos = 1)
         browsePlot('print(g)', file = paste0(Predicted_Ages_Path, '/TMA_Sorted_Subset.png'))
       
       
                         
         # -- Plot, using ALL THE DATA, TMA minus rounded age vs TMA  --
         dim(New_Ages)
         
         assign('xlim', c(min(c(New_Ages$TMA, New_Ages$Age_Rounded)) - 1.25, max(c(New_Ages$TMA, New_Ages$Age_Rounded)) + 1.25), pos = 1)         
         New_Ages$TMA_Minus_Age_Rounded <- New_Ages$TMA - New_Ages$Age_Rounded
         assign('New_Ages', New_Ages, pos = 1)
		 # Superceded ny the highlighted version below
         # browsePlot('set.seed(Seed_Plot); gPlot(New_Ages, "TMA", "TMA_Minus_Age_Rounded", ylab = "TMA - Age_Rounded", xFunc = jitter, ylim = c(-xlim[2], xlim[2]), xlim = xlim,
         #                 grid = FALSE, vertLineEachPoint = TRUE)', file = paste0(Predicted_Ages_Path, '/TMA_minus_NN_Age_Rounded_vs_TMA_Jittered.png'))
                            
         
         # -- Plot, using ALL THE DATA, TMA minus rounded age vs TMA, highlighting those oties that were left out of the NN model - if any --
         
         # Look at the oties the were used in the NN model
         headTail(NN_Pred_Median_TMA, 2)
               
         # Restrict new ages to those that have predictions from the NN model         
         New_Ages_Good <- New_Ages[!is.na(New_Ages$NN_Pred_Median), ]
         dim(New_Ages_Good)
     
         # Find those oties that were left out of the NN model for testing - if any.
         NN_Pred_Median_TMA$Used_NN_Model <- TRUE # Used in the NN model
         New_Ages_Good <- match.f(New_Ages_Good, NN_Pred_Median_TMA, 'filenames', 'filenames', 'Used_NN_Model')
         New_Ages_Good$Used_NN_Model[is.na(New_Ages_Good$Used_NN_Model)] <- FALSE 
         Table(New_Ages_Good$Used_NN_Model)
		 
		 # Plot TMA minus rounded age vs TMA with oties left out of the NN model highlighted (if present)
         if(!all(New_Ages_Good$Used_NN_Model)) {
            
            # assign('xlim', c(min(New_Ages_Good$Age_Rounded) - 1.25, max(New_Ages_Good$Age_Rounded) + 1.25) , pos = 1)  # Using TMA xlim for consistence         
            New_Ages_Good$TMA_Minus_Age_Rounded <- New_Ages_Good$TMA - New_Ages_Good$Age_Rounded
            assign('New_Ages_Good', New_Ages_Good, pos = 1)
            browsePlot('
                set.seed(Seed_Plot)
                gPlot(New_Ages_Good, "TMA", "TMA_Minus_Age_Rounded", ylab = paste0("TMA - round(NN Predicted Age + Delta), Delta = ", Delta), xFunc = jitter, ylim = c(-xlim[2], xlim[2]), xlim = xlim,
                           grid = FALSE, vertLineEachPoint = TRUE, col = "white")
                set.seed(Seed_Plot)
                points(jitter(New_Ages_Good$TMA[New_Ages_Good$Used_NN_Model]), New_Ages_Good$TMA_Minus_Age_Rounded[New_Ages_Good$Used_NN_Model])
                points(jitter(New_Ages_Good$TMA[!New_Ages_Good$Used_NN_Model]), New_Ages_Good$TMA_Minus_Age_Rounded[!New_Ages_Good$Used_NN_Model], col = "red", pch = ifelse(sum(!New_Ages_Good$Used_NN_Model) > 100, 1, 19))
                          
            ', file = paste0(Predicted_Ages_Path, '/TMA_minus_NN_Age_Rd_vs_Age_Rd_Jitter_Left_Out_Oties_Highlighted.png')) 
         }
         
         # Plot TMA minus rounded age vs the predicted "Age_Rounded" with oties left out of the NN model highlighted (if present)
         if(!all(New_Ages_Good$Used_NN_Model)) {
            
            assign('xlim', c(min(New_Ages_Good$TMA) - 1.25, max(New_Ages_Good$TMA) + 1.25) , pos = 1)            
            New_Ages_Good$TMA_Minus_Age_Rounded <- New_Ages_Good$TMA - New_Ages_Good$Age_Rounded
            assign('New_Ages_Good', New_Ages_Good, pos = 1)
            browsePlot('
                set.seed(Seed_Plot)
                gPlot(New_Ages_Good, "Age_Rounded", "TMA_Minus_Age_Rounded", ylab = paste0("TMA - round(NN Predicted Age + Delta), Delta = ", Delta), xFunc = jitter, ylim = c(-xlim[2], xlim[2]), xlim = xlim,
                           grid = FALSE, vertLineEachPoint = TRUE, col = "white")
                set.seed(Seed_Plot)
                points(jitter(New_Ages_Good$Age_Rounded[New_Ages_Good$Used_NN_Model]), New_Ages_Good$TMA_Minus_Age_Rounded[New_Ages_Good$Used_NN_Model])
                points(jitter(New_Ages_Good$Age_Rounded[!New_Ages_Good$Used_NN_Model]), New_Ages_Good$TMA_Minus_Age_Rounded[!New_Ages_Good$Used_NN_Model], col = "red", pch = ifelse(sum(!New_Ages_Good$Used_NN_Model) > 100, 1, 19))
                          
            ', file = paste0(Predicted_Ages_Path, '/TMA_minus_NN_Age_Rounded_vs_Age_Rounded_Jittered_Left_Out_Oties_Highlighted.png')) 
         }
		 
		 # The same by year, if there is more than one year
         if(length(unique(New_Ages$Year)) > 1) {
            browsePlot('
               par(mfrow = c(3, 2))
               for(Year in unique(New_Ages$Year)) {
                         New_Ages_Year <- New_Ages_Good[New_Ages_Good$Year %in% Year, ]
                         gPlot(New_Ages_Year, "TMA", "TMA_Minus_Age_Rounded", ylab = paste0("TMA - round(NN Predicted Age + Delta), Delta = ", Delta), xFunc = jitter, ylim = c(-xlim[2], xlim[2]), xlim = xlim,
                                    main = Year, grid = FALSE, vertLineEachPoint = TRUE, col = "white")
                         points(jitter(New_Ages_Year$TMA[New_Ages_Year$Used_NN_Model]), New_Ages_Year$TMA_Minus_Age_Rounded[New_Ages_Year$Used_NN_Model])
                         points(jitter(New_Ages_Year$TMA[!New_Ages_Year$Used_NN_Model]), New_Ages_Year$TMA_Minus_Age_Rounded[!New_Ages_Year$Used_NN_Model], col = "red", pch = ifelse(sum(!New_Ages_Year$Used_NN_Model) > 100, 1, 19))
                              
              }
            ', file = paste0(Predicted_Ages_Path, '/TMA_minus_NN_Age_Rd_vs_TMA_Jitter_Left_Out_Oties_Highlight_by_Year.png'), width = 10, height = 10, res = 600)
         }
		 
         # By year, with TMA minus rounded age vs the predicted "Age_Rounded" 
         if(length(unique(New_Ages$Year)) > 1) {
            browsePlot('
               par(mfrow = c(3, 2))
               for(Year in unique(New_Ages$Year)) {
                   New_Ages_Year <- New_Ages_Good[New_Ages_Good$Year %in% Year, ]
                   gPlot(New_Ages_Year, "Age_Rounded", "TMA_Minus_Age_Rounded", ylab = paste0("TMA - round(NN Predicted Age + Delta), Delta = ", Delta), xFunc = jitter, ylim = c(-xlim[2], xlim[2]), xlim = xlim,
                              main = Year, grid = FALSE, vertLineEachPoint = TRUE, col = "white")
                   points(jitter(New_Ages_Year$Age_Rounded[New_Ages_Year$Used_NN_Model]), New_Ages_Year$TMA_Minus_Age_Rounded[New_Ages_Year$Used_NN_Model])
                   points(jitter(New_Ages_Year$Age_Rounded[!New_Ages_Year$Used_NN_Model]), New_Ages_Year$TMA_Minus_Age_Rounded[!New_Ages_Year$Used_NN_Model], col = "red", pch = ifelse(sum(!New_Ages_Year$Used_NN_Model) > 100, 1, 19))
                   lowess.line(New_Ages_Year$Age_Rounded, New_Ages_Year$TMA_Minus_Age_Rounded, smoothing.param = 0.05, col = "green")
                   abline(lsfit(New_Ages_Year$Age_Rounded, New_Ages_Year$TMA_Minus_Age_Rounded), col = "blue")
    
              }
            ', file = paste0(Predicted_Ages_Path, '/TMA_minus_NN_Age_Rd_vs_Age_Rd_Jitter_lsfit_lowess.png'), width = 10, height = 10, res = 600)
         }
     
         sink(paste0(Predicted_Ages_Path, "/", Spectra_Set, "_Stats.txt"), split = TRUE)
         {
           cat("\n\n")
           print(Cor_R_squared_RMSE_MAE_SAD_APE(New_Ages$TMA, round(New_Ages$NN_Pred_Median + Delta)))
           
           cat("\n\nFSA (Simple Fisheries Stock Assessment Methods) package's agePrecision() stats:\n\n")
           summary(agePrecision(~ TMA + round(NN_Pred_Median + Delta), data = New_Ages), what="precision")
         }
         sink()
         
     }
}     
     
     
     
     