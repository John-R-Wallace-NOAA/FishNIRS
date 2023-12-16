
# --- Initial setup  (The curly brakets with a '###' comment and without indented lines directly below them, are there to enable the hiding of code sections using Notepad++.) ---
#            (For those who dislike Rstudio,  Notepad++ is here: https://notepad-plus-plus.org/  and NppToR that passes R code from Notepad++ to R is here: https://sourceforge.net/projects/npptor/)
{ ###
if(interactive()) 
      setwd(ifelse(.Platform$OS.type == 'windows', "C:/ALL_USR/JRW/SIDT/Train_NN_Model", "/more_home/h_jwallace/SIDT/Train_NN_Models"))   # Change path to the Spectra Set's .GlobalEnv as needed
if(!interactive())   options(width = 120)      
Spectra_Set <- c("Hake_2019", "Sable_2017_2019", "Sable_Combo_2022")[3]
Spectra_Path <- "Model_Scans"    # Put new spectra scans in a separate folder and enter the name of the folder below
Predicted_Ages_Path <- "Predicted_Ages" # The NN predicted ages will go in the path defined below
dir.create(Predicted_Ages_Path, showWarnings = FALSE)
verbose <- c(TRUE, FALSE)[1]
plot <- c(TRUE, FALSE)[1]
Max_N_Spectra <- list(50, 200, 'All')[[3]]  # Max number of new spectra to be plotted in the spectra figure. (All spectra in the Spectra_Path folder will be assigned an age regardless of the number plotted in the figure.)
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
}  

# (3) Sablefish 2022, Combo survey
if(Spectra_Set == "Sable_Combo_2022") {
   shortNameSegments <- c(1,5) # Segments 1 and 3 of the spectra file name, e.g.: (SABLEFISH, COMBO201701203A, 28, OD1) => (SABLEFISH, 28)
   shortNameSuffix <- 'Year'
   yearPosition <- c(6, 9) # e.g. COMBO201701203A => 2017 (Segment used (see above) is: shortNameSegments[1] + 1)
   fineFreqAdj <- 0
}  

getwd()
Spectra_Set

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
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/gPlot.R")
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



#    Configuration failed to find one of freetype2 libpng libtiff-4. Try installing:
#     * deb: libfreetype6-dev, libpng-dev libtiff5-dev libjpeg-dev (Debian, Ubuntu, etc)
#     * rpm: freetype-devel libpng-devel libtiff-devel libjpeg-turbo-devel (Fedora, CentOS, RHEL)
#     * csw: libfreetype_dev libpng16_dev libtiff_dev libjpeg_dev (Solaris)
#    If freetype2 libpng libtiff-4 is already installed, check that 'pkg-config' is in your
#    PATH and PKG_CONFIG_PATH contains a freetype2 libpng libtiff-4.pc file. If pkg-config
#    is unavailable you can set INCLUDE_DIR and LIB_DIR manually via:
#    R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
#    -------------------------- [ERROR MESSAGE] ---------------------------
#    <stdin>:1:22: fatal error: ft2build.h: No such file or directory
#    compilation terminated.
#    



# --- Setup for TensorFlow and Keras ---

# You will need a 'GITHUB_PAT' from GitHub set somewhere in R (If you need help, search the Web how to get one from GitHub.)
# Sys.setenv(GITHUB_PAT = "**************")   # If you set GITHUB_PAT here, uncomment this line. Note, do not share your GITHUB_PAT, nor load it onto GitHib.
Sys.getenv("GITHUB_PAT") 
 
#  --- Conda TensorFlow environment ---
Conda_TF_Eniv <- ifelse(.Platform$OS.type == 'windows', "C:/m3/envs/tf", "/more_home/h_jwallace/Python/tf_cpu_only/bin")  # Change this path as needed
Sys.setenv(RETICULATE_PYTHON = Conda_TF_Eniv) 
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

}  ###

# --- Create Model_Spectra.sg.iPLS and TMA_Vector for a spectra set ---
{ ###

if(!exists(paste0(Spectra_Set, '_Model_Spectra.sg.iPLS.RData')) {

   # --- Load and look at the raw spectra and metadata  ---
   { ###
   
   fileNames.0 <- dir(path = Spectra_Path)
   fileNames <- get.subs(fileNames.0, sep = ".")[1, ]  # No '.0' in the metadata xlsx
   fileNames[1:10]
   
   cat(paste0("\nNumber of spectral files to be read in: ", length(fileNames), "\n\n")) # Sable_Combo_2022: 1557
   
   Model_Spectra <- Read_OPUS_Spectra(Spectra_Set, Spectra_Path = Spectra_Path)
   # plotly.Spectra.Only(Model_Spectra)
   plotly_spectra(Model_Spectra, N_Samp = 300, htmlPlotFolder = paste0('Figures/', Spectra_Set, '_Spectra_Sample_of_300'))
    
   
   # ADD VESSEL, CRUISE, REGION, LOCATION, AND BIO METADATA
   metadata <- openxlsx::read.xlsx(paste0(Spectra_Set, "_NIRS_Scanning_Session_Report.xlsx"), sheet = 3) #load in ancillary data
   
   # Match by filenames and look at the data/metadata
   (Model_Spectra_Meta <- dplyr::left_join(data.frame(filenames = fileNames, Model_Spectra), metadata, join_by("filenames" == "NWFSC_NIR_Filename")))[1:5, c(1:3, 504:540)]
   
   
  #                                      filenames     X8000     X7992     X3984     X3976     X3968     X3960     X3952 project sample_year pacfin_code_id sequence_number age_structure_id specimen_id age_best length_cm weight_kg sex structure_weight_g NWFSC_NIR_Project
  #  1   SABL_COMBO2022_NIR0022A_PRD_1_102157421_O1 0.2202285 0.2201835 0.5210375 0.5241458 0.5254004 0.5238540 0.5198408   COMBO        2022           SABL               1 102157421-SABL-O   102157421       14      51.5      1.40   2             0.0228               PRD
  #  2  SABL_COMBO2022_NIR0022A_PRD_10_102157430_O1 0.2037776 0.2037497 0.5053568 0.5085597 0.5098205 0.5081757 0.5039832   COMBO        2022           SABL              10 102157430-SABL-O   102157430        6      48.5      1.04   2             0.0173               PRD
  #  3 SABL_COMBO2022_NIR0022A_PRD_100_102157520_O1 0.1761716 0.1760852 0.4939839 0.4972751 0.4985640 0.4968473 0.4924470   COMBO        2022           SABL             100 102157520-SABL-O   102157520        6      56.0      1.96   2             0.0196               PRD
  #  4  SABL_COMBO2022_NIR0022A_PRD_11_102157431_O1 0.1911490 0.1911127 0.5702074 0.5739523 0.5753983 0.5733758 0.5683330   COMBO        2022           SABL              11 102157431-SABL-O   102157431       16      55.0      1.66   1             0.0265               PRD
  #  5  SABL_COMBO2022_NIR0022A_PRD_12_102157432_O1 0.1880359 0.1880160 0.5069040 0.5105314 0.5119459 0.5100320 0.5052297   COMBO        2022           SABL              12 102157432-SABL-O   102157432        6      50.5      1.20   1             0.0200               PRD
  #    NWFSC_NIR_Scan_Session age_structure_side_scan crystallized_scan percent_crystallized_scan broken_scan tip_only_scan anterior_tip_missing posterior_tip_missing percent_missing_scan tissue_present_scan tissue_level_scan oil_clay_contamination_scan stained_scan
  #  1               NIR0022A                       L              <NA>                        NA        <NA>          <NA>                 <NA>                  <NA>                   NA                <NA>              <NA>                        <NA>           NA
  #  2               NIR0022A                       L              <NA>                        NA        <NA>          <NA>                 <NA>                  <NA>                   NA                <NA>              <NA>                        <NA>           NA
  #  3               NIR0022A                       L              <NA>                        NA        <NA>          <NA>                 <NA>                  <NA>                   NA                <NA>              <NA>                        <NA>           NA
  #  4               NIR0022A                       R              <NA>                        NA        <NA>          <NA>                 <NA>                  <NA>                   NA                <NA>              <NA>                        <NA>           NA
  #  5               NIR0022A                       L              <NA>                        NA        <NA>          <NA>                 <NA>                  <NA>                   NA                <NA>              <NA>                        <NA>           NA
  #    contamination_other_scan notes_scan unscannable_BB unscannable_Broken_MissingPieces unscannable_Crystalized unscannable_sample_mixed unscannable_no_otolith
  #  1                       NA       <NA>           <NA>                             <NA>                    <NA>                     <NA>                   <NA>
  #  2                       NA       <NA>           <NA>                             <NA>                    <NA>                     <NA>                   <NA>
  #  3                       NA       <NA>           <NA>                             <NA>                    <NA>                     <NA>                   <NA>
  #  4                       NA       <NA>           <NA>                             <NA>                    <NA>                     <NA>                   <NA>
  #  5                       NA       <NA>           <NA>                             <NA>                    <NA>                     <NA>                   <NA>
  #  
  #  
  #                                                           (L) only one otolith in the cell                                                        (L) Only one otolith in the cell .                                                         (L) only one otolith in the cell. 
  #                                                                                          1                                                                                         1                                                                                         3 
  #                                                          (L) Only one otolith in the cell.                                                        (L) only one otolith in the cell.                                                         (L) Only one otolith in the cell.  
  #                                                                                          4                                                                                         5                                                                                         2 
  #                                                       10% missing from the posterior end.                                                        5% missing from the posterior end.                                                 Anterior portion missing from the otolith  
  #                                                                                          1                                                                                         1                                                                                         1 
  #                                                 anterior portion missing from the otolith.                                                  Anterior portion missing of the otolith                                                                       Anterior tip missing 
  #                                                                                          1                                                                                         1                                                                                         1 
  #  Both otoliths missing >30%, so did not weigh. Did scan, otolith missing anterior portion.                                                         L: Only one otolith in the cell.                            Left ololith missing anterior portion. Right otolith BB for TMA 
  #                                                                                          1                                                                                         2                                                                                         1 
  #                               Left otolith was scanned, might had some clay residue on it.                                                             Otolith broken in two pieces.                                                           Otolith missing posterior end.  
  #                                                                                          2                                                                                         1                                                                                         1 
  #                                                                                       tiny                                                                                      <NA> 
  #                                                                                          2                                                                                      1525 
 
   
   # These are not matched to the scans, since they are unscannable
   Model_Spectra_Meta$unscannable_BB <- Model_Spectra_Meta$unscannable_Broken_MissingPieces <- Model_Spectra_Meta$unscannable_Crystalized <- Model_Spectra_Meta$unscannable_sample_mixed <- Model_Spectra_Meta$unscannable_no_otolith <- NULL
   
   Model_Spectra_Meta$sex <- recode.simple(Model_Spectra_Meta$sex, cbind(c(1, 2, 3), c('M', 'F', 'U')))  # Wow, sex still in numbers!!
   # xyplot(length_cm ~ age_best, group = sex, data = Model_Spectra_Meta, auto = TRUE)
         
   Model_Spectra_Meta$crystallized_scan <- !is.na(Model_Spectra_Meta$crystallized_scan)
   Table(Model_Spectra_Meta$crystallized_scan)
   
   Table(Model_Spectra_Meta$percent_crystallized_scan, Model_Spectra_Meta$crystallized_scan)  # All NA percent_crystallized_scan's are zeros
   Model_Spectra_Meta$percent_crystallized_scan[is.na(Model_Spectra_Meta$percent_crystallized_scan)] <- 0 # Change NA to zero so that a numerical test can be done (see below).
   Table(Model_Spectra_Meta$percent_crystallized_scan, Model_Spectra_Meta$crystallized_scan) 
   
   Table(Model_Spectra_Meta$broken_scan)  #  Do all of these broken pieces fit together well? What about entering the number of pieces the otie is broken into - zero for an unbroken ototlith.
                                          # Are the oties ever broken during scanning or the handling of the otie for scanning?
                                          
   Table(Model_Spectra_Meta$tip_only_scan)  # is there a difference between 'no' and an NA for this one?
     
   Table(Model_Spectra_Meta$anterior_tip_missing)
   
   Table(Model_Spectra_Meta$posterior_tip_missing)
   
   Table(Model_Spectra_Meta$percent_missing_scan)   
   Model_Spectra_Meta$percent_missing_scan[is.na(Model_Spectra_Meta$percent_missing_scan)] <- 0 # Now a numerical test can be done (see below).
   Table(Model_Spectra_Meta$percent_missing_scan)  
   
   Table(Model_Spectra_Meta$tissue_present_scan)  
   
   Table(Model_Spectra_Meta$tissue_level_scan) # Change this to 'percent_tissue_level_scan'?
   Model_Spectra_Meta$tissue_level_scan[is.na(Model_Spectra_Meta$tissue_level_scan)] <- 0 # Now a numerical test can be done (see below).
   Table(Model_Spectra_Meta$tissue_level_scan)
   
   Table(Model_Spectra_Meta$oil_clay_contamination_scan) # No percentage for this one
   
   Table(Model_Spectra_Meta$stained_scan) # All NA's. Is this 'yes'/'no' or percentage?
   
   Table(Model_Spectra_Meta$contamination_other_scan) # All NA's. Is this 'yes'/'no' or percentage?
     
   
   names(Model_Spectra_Meta)[names(Model_Spectra_Meta) %in% 'age_best'] <- "TMA"
   Table(is.finite(Model_Spectra_Meta$TMA))
         
   rev(sort(Model_Spectra_Meta$TMA))[1:20]
   
   dim(Model_Spectra_Meta) # Sable Combo 2022: 1557 rows 535 cols
   
   TF <- !is.na(Model_Spectra_Meta$TMA) & Model_Spectra_Meta$percent_crystallized_scan <= 15 & Model_Spectra_Meta$percent_crystallized_scan <= 10 &
          Model_Spectra_Meta$tissue_level_scan <= 10 & !is.na(Model_Spectra_Meta$length_cm) & !is.na(Model_Spectra_Meta$structure_weight_g)
   c(sum(TF), sum(!TF), sum(TF) + sum(!TF))
   
   sort(fileNamesRemove <- Model_Spectra_Meta$filenames[!TF])
   
   file.remove(paste0(Spectra_Path, "/", fileNamesRemove, ".0"), recursive = TRUE)  # Raw scans removed here
   
   Model_Spectra_Meta <- Model_Spectra_Meta[TF, ] # --- Doing an overwrite here!! --- Gettin rid of bad scans or ones with no metadata that is wanted
   dim(Model_Spectra_Meta) # Sable Combo 2022: 1528 rows

   Model_Spectra_Meta$shortName <- apply(Model_Spectra_Meta[, 'filenames', drop = FALSE], 1, function(x) paste(get.subs(x, sep = "_")[c(1, 5)], collapse = "_"))
   
   
   # Look at the data with plotly and remove rogue oties if needed# 
   plotly.Spec(Model_Spectra_Meta, 'all', htmlPlotFolder = paste0(Figures, '/Spectra Figure by TMA'))
   # Model_Spectra_Meta <- Model_Spectra_Meta[!Model_Spectra_Meta$shortName %in% 'HAKE_48', ] # Example of removing a rogue otie for Hake 2019
   # plotly.Spec(Model_Spectra_Meta, 'all') # Decide to save figure with rogue otie or the figure without the rogue otie, or both.
   
   
   save(Model_Spectra_Meta, file = paste0(Spectra_Set, '_Model_Spectra_Meta_ALL_GOOD_DATA.RData')) # 1528
   # load(file = paste0(Spectra_Set, '_Model_Spectra_Meta_ALL_GOOD_DATA.RData'))
   
   # TMA only
   TMA_Vector <- Model_Spectra_Meta$TMA
   length(TMA_Vector) # 1528 After bad scans or missing dat - before saving out oties for final model check
   
   # Save out 15 oties for final model check.
   set.seed(Seed_Fold)
   TMA_Tab <- Table(TMA_Vector)
   (Low_strata <- sum(TMA_Tab[1:3]))
   (Mid_strata <- sum(TMA_Tab[4:20]))
   (High_strata <- sum(TMA_Tab[21:length(TMA_Tab)]))
   (SaveOutOties <- c(sample(order(TMA_Vector)[1:Low_strata], 5), sample(order(TMA_Vector)[Low_strata + 1:Mid_strata], 5), sample(order(TMA_Vector)[Low_strata + Mid_strata + 1:High_strata], 5)) )
   sort(TMA_Vector[SaveOutOties]) # Check the results
   save(SaveOutOties, file = paste0(Spectra_Set, '_SaveOutOties_Seed_', Seed_Fold, '.RData'))
   
   # TMA_Vector_SaveOutOties <- TMA_Vector[SaveOutOties]
   # save(TMA_Vector_SaveOutOties, file = paste0(Spectra_Set, '_TMA_Vector_SaveOutOties.RData'))
   # 
   # Model_Spectra_Meta_SaveOutOties <- Model_Spectra_Meta[SaveOutOties, ]
   # save(Model_Spectra_Meta_SaveOutOties, file = paste0(Spectra_Set, '_Model_Spectra_Meta_SaveOutOties.RData'))
   
   dim(Model_Spectra_Meta) # 1528  536
   Model_Spectra_Meta <- Model_Spectra_Meta[-SaveOutOties, ] # --- Doing an overwrite here!! ---
   dim(Model_Spectra_Meta) #  1513  536
   
   # Now recreate the spectra only file Model_Spectra so that bad rows and saved out oties are removed
   Model_Spectra <- cbind(Model_Spectra_Meta[, 2:((1:ncol(Model_Spectra_Meta))[names(Model_Spectra_Meta) %in% 'project'] - 1)]) # --- Overwrite of the spectra only file: Model_Spectra ---
   dim(Model_Spectra_Meta) # 1513  536
   dim(Model_Spectra) # 1513  507
   Model_Spectra[1:3, c(1:2, 505:507)]

   # TMA only vector that matches the recreated Model_Spectra above
   TMA_Vector <- Model_Spectra_Meta$TMA # --- Doing another overwrite here!! ---
   length(TMA_Vector) # 1513
   
   
   # Use full file names - with '.0' at the end
   fileNames.0 <- dir(path = Spectra_Path)  # Need new list of filenames after removals above
   length(fileNames.0)
   fileNames.0[1:5]
   
   # These are the oties that are not used in the NN model and are saved out for testing
   sort(fileNames.0[SaveOutOties])
   dir.create(paste0(Spectra_Set, '_Saved_Out'), showWarnings = FALSE)
   file.copy(paste0(Spectra_Path, "/", fileNames.0[SaveOutOties]), paste0(Spectra_Set, '_Saved_Out'), recursive = TRUE, copy.date = TRUE)
   file.remove(paste0(Spectra_Path, "/", fileNames.0[SaveOutOties]), recursive = TRUE) # More raw scans removed here - number of scans left should match Model_Spectra_Meta, Model_Spectra, and TMA_Vector
   }
   
   # Savitzky-Golay smoothing    
   { ###
   
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
   
   #  iPLS variable selection results
   #   Method: forward
   #   Validation: random with 100 segments
   #   Number of intervals: 15
   #   Number of selected intervals: 9
   #   RMSECV for global model: 2.964359 (15 LVs)
   #   RMSECV for optimized model: 2.880144 (14 LVs)
   # 
   # Summary for selection procedure:
   #     n start end selected nComp     RMSE    R2
   # 1   0     1 493    FALSE    15 2.964359 0.920
   # 2  15   462 493     TRUE    12 3.437861 0.893
   # 3   8   232 264     TRUE    15 3.066425 0.915
   # 4   6   166 198     TRUE    14 3.037084 0.916
   # 5   5   133 165     TRUE    12 3.017657 0.917
   # 6   3    67  99     TRUE    14 2.941591 0.921
   # 7  13   397 429     TRUE    15 2.927269 0.922
   # 8  12   364 396     TRUE    14 2.907818 0.923
   # 9   4   100 132     TRUE    15 2.897542 0.924
   # 10 11   331 363     TRUE    14 2.883531 0.925
   
   
   # plot the newly selected spectra regions 
   dev.new()
   plot(Model_Spectra.iPLS.F)     
   
   Model_Spectra.iPLS.F$int.selected
   sort(Model_Spectra.iPLS.F$var.selected)
   
   # dev.new()  - With a main title
   # plot(Model_Spectra.iPLS.F, main = NULL)          
   
   # plot predictions before and after selection
   dev.new()
   par(mfrow = c(2, 1))
   mdatools::plotPredictions(Model_Spectra.iPLS.F$gm) # gm = global PLS model with all variables included
   mdatools::plotPredictions(Model_Spectra.iPLS.F$om) # om = optimized PLS model with selected variables
   
   dev.new()
   mdatools::plotRMSE(Model_Spectra.iPLS.F)
   
   # RMSE  before and after selection
   
   # Visually find the ylim to apply to both figures  and over all areas and WB
   dev.new()
   par(mfrow = c(2, 1))
   mdatools::plotRMSE(Model_Spectra.iPLS.F$gm)
   mdatools::plotRMSE(Model_Spectra.iPLS.F$om)
   
   # Use the ylim for both plots
   dev.new()
   par(mfrow = c(2, 1))
   mdatools::plotRMSE(Model_Spectra.iPLS.F$gm, ylim = c(2.4, 6.5))
   mdatools::plotRMSE(Model_Spectra.iPLS.F$om, ylim = c(2.4, 6.5))
   
   
   # Select iPLS vars and add metadata wanted
   # (p <- length(Model_Spectra.iPLS.F$var.selected)) # 380 freq selected out of a total of 1140
   
   Model_Spectra.sg.iPLS <- data.frame(Model_Spectra.sg[, sort(Model_Spectra.iPLS.F$var.selected)], length_prop_max = Model_Spectra_Meta$length_cm/max(Model_Spectra_Meta$length_cm), 
                                           structure_weight_dg = 10* Model_Spectra_Meta$structure_weight_g) # dg = decigram
                                           
   save(Model_Spectra.sg.iPLS, file = paste0(Spectra_Set, '_Model_Spectra.sg.iPLS.RData'))
   save(TMA_Vector, file = paste0(Spectra_Set, '_TMA_Vector.RData'))                
   
   if(FALSE) {
      
       Model_Spectra.Age.sg.iPLS <- data.frame(Age = TMA_Vector, Model_Spectra.sg.iPLS)
       dim(Model_Spectra.Age.sg.iPLS) 
       
       
       # 2D plot
       # Model_Spectra.sg.iPLS.PLOT <- cbind(Model_Spectra_Meta[, 1, drop = FALSE], Model_Spectra.sg.iPLS, Model_Spectra_Meta[, ncol(Model_Spectra):ncol(Model_Spectra_Meta)])
       # plotly.Spec(Model_Spectra.sg.iPLS.PLOT, 'all') 
       
       
       # Plot the transformed spectra by age using only variables selected using iPLS
       (Model_Spectra.Age.sg.iPLS.Long <- reshape2::melt(Model_Spectra.Age.sg.iPLS, id = 'Age', variable.name = 'Freq', value.name = 'Absorbance'))[1:4, ]
       Model_Spectra.Age.sg.iPLS.Long$Freq <- as.numeric(substring(Model_Spectra.Age.sg.iPLS.Long$Freq, 2))
       Model_Spectra.Age.sg.iPLS.Long <- sort.f(Model_Spectra.Age.sg.iPLS.Long, 'Freq')
       Model_Spectra.Age.sg.iPLS.Agg <- aggregate(list(Absorbance = Model_Spectra.Age.sg.iPLS.Long$Absorbance), 
            list(Freq = Model_Spectra.Age.sg.iPLS.Long$Freq, Age = Model_Spectra.Age.sg.iPLS.Long$Age), mean, na.rm = TRUE)
       Model_Spectra.Age.sg.iPLS.Agg$Age <- ordered(Model_Spectra.Age.sg.iPLS.Agg$Age, sort(unique(Model_Spectra.Age.sg.iPLS.Agg$Age)))
       
       plotly::ggplotly(ggplot2::ggplot(data = Model_Spectra.Age.sg.iPLS.Agg, aes(x = Freq, y = Absorbance, z = Age)) + geom_line(aes(colour = Age), size = 0.2) + 
                             scale_color_manual(values=rainbow(length(unique(Model_Spectra.Age.sg.iPLS.Agg$Age)), alpha = 1)))
       
                     
                     
       # --------------- Try ipls() with smoothed spectra data and metadata  - NO METADATA WAS SELECTED ------------------------
       
       # Remove NA's with predictors and response together - then re-split
       Model_Spectra.sg.META <- na.omit(cbind(Model_Spectra.sg, Model_Spectra_Meta[, c("latitude", "longitude", "length", "weight", "sex")], TMA = TMA_Vector))
       Ncol <- ncol(Model_Spectra.sg.META)
       Model_Spectra.sg.META[1:3, c(1:2, (Ncol - 4):Ncol)]
       
       TMA.META <- Model_Spectra.sg.META[, Ncol]
       Model_Spectra.sg.META <- Model_Spectra.sg.META[, -Ncol]
          
       Model_Spectra.iPLS.META.F <- mdatools::ipls(Model_Spectra.sg.META, TMA.META, glob.ncomp = nComp, center = TRUE, scale = TRUE, cv = 100,
                         int.ncomp = nComp, int.num = nComp, ncomp.selcrit = "min", method = "forward", silent = FALSE)
       
       summary(Model_Spectra.iPLS.META.F)
       
       # Plot the newly selected spectra regions 
       dev.new()
       plot(Model_Spectra.iPLS.META.F)     
       
       Model_Spectra.iPLS.META.F$int.selected
       sort(Model_Spectra.iPLS.META.F$var.selected)
       
       Model_Spectra.sg.META[, Model_Spectra.iPLS.F$var.selected][1:3, c(1:3, 373:380)]
       names(Model_Spectra.sg.META[, Model_Spectra.iPLS.F$var.selected])
   }   
       
   }
   
} ###
} ###

# ------ NN Model -------
{ ###
# Load the data if needed
base::load(paste0(Spectra_Set, '_Model_Spectra.sg.iPLS.RData'))
base::load(paste0(Spectra_Set, '_TMA_Vector.RData'))
base::load(paste0(Spectra_Set, '_Model_Spectra_Meta_ALL_GOOD_DATA.RData')); load('SaveOutOties.RData'); Model_Spectra_Meta <-  Model_Spectra_Meta[-SaveOutOties, ] # 

# = = = = = = = = = = = = = = = = = Intial setup - run the NN code between the '= = =' lines = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
   
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


# = = = = = = = = = = = = = = = = = Pick number of random reps (Rdm_reps) and run the NN code between the next '= = =' lines and expect long run times = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    
Rdm_reps <- 20    
# (Rdm_reps <- ifelse(model_Name == 'FCNN_model_ver_1', 20, 10))
Seed_Main <- 707   # Reducing the number of seeds will be considered later
set.seed(Seed_Main) 
Seed_reps <- sample(1e7, Rdm_reps)

# Start fresh or continue by loading a file with model iterations already finished (see the commented line with an example model file). 
Rdm_models <- list() 
Rdm_folds_index <- list()
# base::load("Sable_Combo_2022_FCNN_model_ver_1_6_Rdm_model_14_Dec_2023_05_34_33.RData") 

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
   
   SG_Variables_Selected <- names(Model_Spectra.sg.iPLS)
   roundingDelta <- Delta

   save(Iter, i, j, Cor, CA_diag, SAD, learningRate, layer_dropout_rate, Seed_Fold, Seed_Model, Seed_Main, Rdm_models, 
         Rdm_folds_index, SG_Variables_Selected, roundingDelta, file = paste0(Spectra_Set, '_', model_Name, '_', length(Rdm_folds_index), '_Rdm_model_', timeStamp(), '.RData'))
   
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

   
   browserPlot('agreementFigure(y.fold.test.ALL, y.fold.test.pred.ALL, Delta = Delta, full = TRUE, main = paste0("Random Rep = ", j))')   
   
}  # k Random Replicate loop

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

}

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
      y.fold.test.pred <- as.vector(predict(keras::unserialize_model(Fold_models[[i]], custom_objects = NULL, compile = TRUE), x.fold.test))
      print(c(length(folds_index[[i]]), length(y.fold.test.pred)))
      y.fold.test.pred.ALL <- rbind(y.fold.test.pred.ALL, cbind(Index = folds_index[[i]], y.test.fold.pred = y.fold.test.pred))
   }
   
   y.test.pred <- sort.f(data.frame(y.fold.test.pred.ALL))[, 2]  # Sort on the Index to match back to the order of the full TMA_Vector and Model_Spectra.sg.iPLS
   
   y.fold.test.pred_RDM <- rbind(y.fold.test.pred_RDM, y.test.pred)
   
   # dev.new(width = 11, height = 8)
   # agreementFigure(TMA_Vector, y.test.pred, Delta = -0.05, full = TRUE, main = paste0("Random Rep = ", j))
   browserPlot('agreementFigure(TMA_Vector, y.test.pred, Delta = -0.05, full = TRUE, main = paste0("Random Rep = ", j))') # Delta is a previous estimate or guess for now
   
   # Full figure only needed for a long-lived species like Sablefish
   # dev.new(width = 11, height = 8)
   # agreementFigure(TMA_Vector, y.test.pred, Delta = -0.25, full = FALSE, main = paste0("Random Rep = ", j))
}


# ----------------------- Median over all Rdm_reps Models ------------------------

y.fold.test.pred_RDM_median <- apply(y.fold.test.pred_RDM, 2, median)

# Delta <- -0.05  # Previous estimate or guess
c(Delta = Delta, Correlation_R_squared_RMSE_MAE_SAD(TMA_Vector, round(y.fold.test.pred_RDM_median + Delta)))
#      Delta Correlation   R_squared        RMSE         MAE         SAD 
#  -0.050000    0.965315    0.931834    2.744340    1.374090 2079.000000 


  

# What is the best Delta (by SAD, with ties broken by RMSE) on the median over all, Rdm_reps, full k-folds 
Delta_Table <- NULL
for (Delta. in seq(0, -0.45, by  = -0.05)) {
  # cat("\n\n")
  # print(c(Delta = Delta., Correlation_R_squared_RMSE_MAE_SAD(TMA_Vector, round(y.fold.test.pred_RDM_median + Delta.))))
  Delta_Table <- rbind(Delta_Table, c(Delta = Delta., Correlation_R_squared_RMSE_MAE_SAD(TMA_Vector, round(y.fold.test.pred_RDM_median + Delta.))))
  }
  
print(Delta_Table <- data.frame(Delta_Table)) 
  
# Best Delta from table above
(Delta <- Delta_Table$Delta[order(Delta_Table$SAD, Delta_Table$RMSE)[1]])

# Best Delta from above
# dev.new(width = 11, height = 8) # R plot window version
# agreementFigure(TMA_Vector, y.fold.test.pred_RDM_median, Delta = Delta, full = TRUE, main = paste0("Median over ", Rdm_reps, ' Full k-Fold Models'), cex = 1.25) # R plot window version
# browserPlot('agreementFigure(TMA_Vector, y.fold.test.pred_RDM_median, Delta = Delta, full = TRUE, main = paste0("Median over ", Rdm_reps, " Full k-Fold Models"), cex = 1.25)', file = 'Figures/Sable_2022_Combo_20_Rdm_Final.pdf', pdf = TRUE) # PDF version
browserPlot('agreementFigure(TMA_Vector, y.fold.test.pred_RDM_median, Delta = Delta, full = TRUE, main = paste0("Median over ", Rdm_reps, " Full k-Fold Models"), cex = 1.25)', file = paste0('Figures/', Spectra_Set, '_', length(Rdm_folds_index), '_Rdm_Final.png'))
browserPlot('agreementFigure(TMA_Vector, y.fold.test.pred_RDM_median, Delta = Delta, full = FALSE, main = paste0("Median over ", Rdm_reps, " Full k-Fold Models"), cex = 1.25)', file = paste0('Figures/', Spectra_Set, '_', length(Rdm_folds_index), '_Rdm_Final_Zoomed.png'))
                                                                                                                                               

# Apply that best Delta (from above) to all Rdm_reps models individually
Stats_RDM_median_by_model <- NULL
for(numRdmModels in 1:Rdm_reps) {

   y.fold.test.pred_RDM_median <- apply(y.fold.test.pred_RDM[numRdmModels, ,drop = FALSE], 2, median)
   Stats_RDM_median_by_model <- rbind(Stats_RDM_median_by_model, data.frame(t(Correlation_R_squared_RMSE_MAE_SAD(TMA_Vector, round(y.fold.test.pred_RDM_median + Delta)))))
}

print(Stats_RDM_median_by_model)
 

# An additional full k-fold added to the total number of models at each step in turn
# dev.new(width = 11, height = 8)

browserPlot("
par(mfrow = c(3,2))  
# Delta <- -0.05  # Reset Delta if recreating this figure as an old Delta may linger.
Stats_RDM_median_by_model_added <- NULL
for(numRdmModels in 1:Rdm_reps) {

   y.fold.test.pred_RDM_median <- apply(y.fold.test.pred_RDM[1:numRdmModels, ,drop = FALSE], 2, median)
   Stats_RDM_median_by_model_added  <- rbind(Stats_RDM_median_by_model_added, data.frame(t(Correlation_R_squared_RMSE_MAE_SAD(TMA_Vector, round(y.fold.test.pred_RDM_median + Delta)))))
}

print(Stats_RDM_median_by_model_added)

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
   
      y.fold.test.pred_RDM_median <- apply(y.fold.test.pred_RDM[Rdm_Vec[1:numRdmModels], , drop = FALSE], 2, median)
      Stats_RDM_median_by_model_added  <- rbind(Stats_RDM_median_by_model_added, data.frame(t(Correlation_R_squared_RMSE_MAE_SAD(TMA_Vector, round(y.fold.test.pred_RDM_median + Delta)))))
   }
     
   min.stats <- apply(Stats_RDM_median_by_model_added[, c(3,5)], 2, min)
   minAdj <- sweep(data.matrix(Stats_RDM_median_by_model_added[, c(3,5)]), 2, min.stats)
   max.of.Adj <- apply(minAdj, 2, max)
   (Stats_0_1_interval <- cbind(Stats_RDM_median_by_model_added[,1:2], t(t(minAdj)/max.of.Adj)))
        
   matplot(1:Rdm_reps, Stats_0_1_interval, type = 'o', col = c(1:3,6), xlab = 'Number of Complete Folds', ylab = 'Various Stats', main = 'Randomized Order')
}
", width = 11, height = 8, file = paste0('Figures/Full_k-fold_models_added_sequentially.png'))


Pred_median <- r(data.frame(NN_Pred_Median = apply(y.fold.test.pred_RDM, 2, median), 
                            Lower_Quantile_0.025 = apply(y.fold.test.pred_RDM, 2, quantile, probs = 0.025, na.rm = TRUE),
                            Upper_Quantile_0.975 = apply(y.fold.test.pred_RDM, 2, quantile, probs = 0.975, na.rm = TRUE)), 4) 
cat(paste0("\n\n--- Note: The quantiles are a reflection of the NN models precision based on ", length(Rdm_models), " full 10-fold randomized models, not the accuracy to a TMA Age ---\n\n"))   
 
assign(paste0(Spectra_Set, '_NN_Pred_Median_TMA'), data.frame(filenames = Model_Spectra_Meta$filenames, Pred_median, TMA = TMA_Vector), pos = 1)
save(list = paste0(Spectra_Set, '_NN_Pred_Median_TMA'), file = paste0(Spectra_Set, '_', model_Name, '_', length(Rdm_folds_index), '_Pred_Median_TMA_', timeStamp(), '.RData'))

# This agreementFigure() already produced above
# (y.fold.test.pred_RDM_median <- apply(y.fold.test.pred_RDM, 2, median))[1:10]
# browserPlot('agreementFigure(TMA_Vector, y.fold.test.pred_RDM_median, Delta = Delta, full = TRUE, main = paste0("Median over ", Rdm_reps, " Full k-Fold Models"), cex = 1.25)')

Model_Ages <- get(paste0(Spectra_Set, '_NN_Pred_Median_TMA'))
set.seed(Seed_Fold)
Model_Ages <- Model_Ages[sample(1:nrow(Model_Ages), 100),  ]  # Using a sample of 100 ages so the figures are not too crowded
Model_Ages$Age_Rounded <- round(Model_Ages$NN_Pred_Median + Delta)
Model_Ages$Index <- 1:nrow(Model_Ages)

# - Plot by order implied by the spectra file names - ggplotly() changes how scale_color_manual() works ?????????????????
cols <- c('green', 'red')
g <- ggplot(Model_Ages, aes(Index, NN_Pred_Median)) +  
geom_point() +
geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
geom_point(aes(Index, Age_Rounded, col = cols[1])) + 
geom_point(aes(Index + 0.1, TMA, col = cols[2])) + 
scale_color_manual(labels = c('Rounded Age', 'TMA'), values = cols, name = ' ')
browserPlot('print(g)', file = paste0('Figures/Predicted_Ages_Order_by_File_Names.png'))

   
# -- Plot by sorted NN predicted ages --
Model_Ages_Sorted <- sort.f(Model_Ages, 'NN_Pred_Median') # Sort Model_Ages by NN_Pred_Median, except for "Index" (see the next line below)
Model_Ages_Sorted$Index <- sort(Model_Ages_Sorted$Index)  # Reset Index for graphing
if(verbose) head(Model_Ages_Sorted, 20)

cols <- c('green', 'red')
g <- ggplot(Model_Ages_Sorted, aes(Index, NN_Pred_Median)) +  
# xlim(0, 65) + ylim(0, 20) +
geom_point() +
geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
geom_point(aes(Index, Age_Rounded, col = cols[1])) + 
geom_point(aes(Index + 0.1, TMA, col = cols[2])) + 
scale_color_manual(labels = c('Rounded Age', 'TMA'), values = cols, name = ' ')
browserPlot('print(g)', file = paste0('Figures/Predicted_Ages_Sorted.png'))



# -- Plot by sorted TMA --
Model_Ages_Sorted <- sort.f(Model_Ages, 'TMA') # Sort Model_Ages by TMA, except for "Index" (see the next line below)
Model_Ages_Sorted$Index <- sort(Model_Ages_Sorted$Index)  # Reset Index for graphing
if(verbose) head(Model_Ages_Sorted, 20)

cols <- c('green', 'red')
g <- ggplot(Model_Ages_Sorted, aes(Index, NN_Pred_Median)) +  
# xlim(0, 65) + ylim(0, 20) +
geom_point() +
geom_errorbar(aes(ymin = Lower_Quantile_0.025, ymax = Upper_Quantile_0.975)) + 
geom_point(aes(Index, TMA, col = cols[2])) + 
geom_point(aes(Index + 0.1, Age_Rounded, col = cols[1])) + 
scale_color_manual(labels = c('Rounded Age', 'TMA'), values = cols, name = ' ')
browserPlot('print(g)', file = paste0('Figures/TMA_Sorted.png'))



# -- Plot by sorted difference --
# g <- ggplot(Model_Ages, aes(jitter(TMA, 1.25), TMA - NN_Pred_Median)) +  
# geom_point() 
# browserPlot('print(g)', file = paste0('Figures/TMA_minus_NN_Pred_vs_TMA.png'))
  
# Vertical line for each unique TMA - without standard grid            
xlim <- c(min(TMA) - 1.25, max(TMA) + 1.25)       
browserPlot('set.seed(707); gPlot(Model_Ages, jitter(TMA), TMA - NN_Pred_Median, ylim = c(-xlim[2], xlim[2]), xlim = xlim, grid = FALSE); abline(v = unique(TMA), col = "grey"); 
             set.seed(707); points(jitter(TMA), TMA - NN_Pred_Median)', file = paste0('Figures/TMA_minus_NN_Pred_vs_TMA.png'))    
             
             

Table(TMA_Vector <= 3)/length(TMA_Vector)

} 
   
