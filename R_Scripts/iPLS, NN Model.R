
# --- Initial setup  (The curly brakets with a '###' comment and without indented lines directly below them, are there to enable the hiding of code sections using Notepad++.) ---
#            (For those who dislike Rstudio,  Notepad++ is here: https://notepad-plus-plus.org/  and NppToR that passes R code from Notepad++ to R is here: https://sourceforge.net/projects/npptor/)

# You will need a 'GITHUB_PAT' from GitHub set somewhere in R (If you need help, search the Web how to get one from GitHub.)
# Sys.setenv(GITHUB_PAT = "**************")   # If you set GITHUB_PAT here, uncomment this line. Note, do not share your GITHUB_PAT, nor load it onto GitHib.
Sys.getenv("GITHUB_PAT") 

{ ###

if(interactive()) 
      setwd(ifelse(.Platform$OS.type == 'windows', "C:/ALL_USR/JRW/SIDT/Train_NN_Model", "/more_home/h_jwallace/SIDT/Train_NN_Models"))   # Change path to the Spectra Set's .GlobalEnv as needed
if(!interactive())   options(width = 120)      
Spectra_Set <- c("Hake_2019", "Sable_2017_2019", "Sable_Combo_2022")[3] # Defaults for reading in the spectra sets are in the Read_OPUS_Spectra() function.
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
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/browsePlot.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/gPlot.R")   
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/recode.simple.R")  
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/headTail.R")  
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/factor.f.R")

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

# --- If missing create Model_Spectra.sg.iPLS and TMA_Vector for a spectra set ---
{ ###

if(!file.exists(paste0(Spectra_Set, '_Model_Spectra.sg.iPLS.RData'))) {

   #   # --- Load and look at the raw spectra and metadata  ---
   #   { ###
   #   
   #   fileNames.0 <- dir(path = Spectra_Path)
   #   fileNames <- get.subs(fileNames.0, sep = ".")[1, ]  # No '.0' in the metadata xlsx
   #   fileNames[1:10]
   #   
   #   cat(paste0("\nNumber of spectral files to be read in: ", length(fileNames), "\n\n")) # Sable_Combo_2022: 1557
   
   #   Model_Spectra <- Read_OPUS_Spectra(Spectra_Set, Spectra_Path = Spectra_Path)
   #   # plotly.Spectra.Only(Model_Spectra)
   #   plotly_spectra(Model_Spectra, N_Samp = 300, htmlPlotFolder = paste0('Figures/', Spectra_Set, '_Spectra_Sample_of_300'))
   #   
   #   # ADD VESSEL, CRUISE, REGION, LOCATION, AND BIO METADATA
   #   metadata <- openxlsx::read.xlsx(paste0(Spectra_Set, "_NIRS_Scanning_Session_Report.xlsx"), sheet = 3) # Load in ancillary data
   #   
   #   # Match by filenames and look at the data/metadata
   #   (Model_Spectra_Meta <- dplyr::left_join(data.frame(filenames = fileNames, Model_Spectra), metadata, join_by("filenames" == "NWFSC_NIR_Filename")))[1:5, c(1:3, 504:540)]
   
   # Example of using Read_OPUS_Spectra() and not adding metadata
   # Model_Spectra <- Read_OPUS_Spectra(Spectra_Set, Spectra_Path = Spectra_Path, Max_N_Spectra = 300, Meta_Add = FALSE, htmlPlotFolder = paste0('Figures/', Spectra_Set, '_Spectra_Sample_of_300'))
   
   # Grabbing the metadata where it lives at: C:\ALL_USR\JRW\SIDT\Sablefish 2022 Combo 
   
   Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set, Spectra_Path = Spectra_Path, Max_N_Spectra = 300, Meta_Path = paste0(Spectra_Set, "_NIRS_Scanning_Session_Report.xlsx"),
                                               verbose = verbose, plot = plot, htmlPlotFolder = paste0('Figures/', Spectra_Set, '_Spectra_Sample_of_300'))
											   
  #                                       filenames     X8000     X7992     X3952 project sample_year pacfin_code_id sequence_number age_structure_id specimen_id TMA sex structure_weight_g NWFSC_NIR_Project
  # 1   SABL_COMBO2022_NIR0022A_PRD_1_102157421_O1 0.2202285 0.2201835 0.5198408   COMBO        2022           SABL               1 102157421-SABL-O   102157421  14   2             0.0228               PRD
  # 2  SABL_COMBO2022_NIR0022A_PRD_10_102157430_O1 0.2037776 0.2037497 0.5039832   COMBO        2022           SABL              10 102157430-SABL-O   102157430   6   2             0.0173               PRD
  # 3 SABL_COMBO2022_NIR0022A_PRD_100_102157520_O1 0.1761716 0.1760852 0.4924470   COMBO        2022           SABL             100 102157520-SABL-O   102157520   6   2             0.0196               PRD
  # 4  SABL_COMBO2022_NIR0022A_PRD_11_102157431_O1 0.1911490 0.1911127 0.5683330   COMBO        2022           SABL              11 102157431-SABL-O   102157431  16   1             0.0265               PRD
  # 5  SABL_COMBO2022_NIR0022A_PRD_12_102157432_O1 0.1880359 0.1880160 0.5052297   COMBO        2022           SABL              12 102157432-SABL-O   102157432   6   1             0.0200               PRD
  #   NWFSC_NIR_Scan_Session age_structure_side_scan crystallized_scan percent_crystallized_scan broken_scan tip_only_scan anterior_tip_missing posterior_tip_missing percent_missing_scan tissue_present_scan
  # 1               NIR0022A                       L              <NA>                         0        <NA>          <NA>                 <NA>                  <NA>                    0                <NA>
  # 2               NIR0022A                       L              <NA>                         0        <NA>          <NA>                 <NA>                  <NA>                    0                <NA>
  # 3               NIR0022A                       L              <NA>                         0        <NA>          <NA>                 <NA>                  <NA>                    0                <NA>
  # 4               NIR0022A                       R              <NA>                         0        <NA>          <NA>                 <NA>                  <NA>                    0                <NA>
  # 5               NIR0022A                       L              <NA>                         0        <NA>          <NA>                 <NA>                  <NA>                    0                <NA>
  #   tissue_level_scan oil_clay_contamination_scan stained_scan contamination_other_scan notes_scan unscannable_BB unscannable_Broken_MissingPieces unscannable_Crystalized unscannable_sample_mixed unscannable_no_otolith
  # 1              <NA>                        <NA>           NA                       NA       <NA>           <NA>                             <NA>                    <NA>                     <NA>                   <NA>
  # 2              <NA>                        <NA>           NA                       NA       <NA>           <NA>                             <NA>                    <NA>                     <NA>                   <NA>
  # 3              <NA>                        <NA>           NA                       NA       <NA>           <NA>                             <NA>                    <NA>                     <NA>                   <NA>
  # 4              <NA>                        <NA>           NA                       NA       <NA>           <NA>                             <NA>                    <NA>                     <NA>                   <NA>
  # 5              <NA>                        <NA>           NA                       NA       <NA>           <NA>                             <NA>                    <NA>                     <NA>                   <NA>
  #   Length_cm Weight_kg Month_Scaled length_prop_max structure_weight_dg      shortName
  # 1      51.5      1.40    0.4166667       0.5885714               0.228   SABL_1_Combo
  # 2      48.5      1.04    0.4166667       0.5542857               0.173  SABL_10_Combo
  # 3      56.0      1.96    0.4166667       0.6400000               0.196 SABL_100_Combo
  # 4      55.0      1.66    0.4166667       0.6285714               0.265  SABL_11_Combo
  # 5      50.5      1.20    0.4166667       0.5771429               0.200  SABL_12_Combo
  # 
  # Total number of oties read in: 1557.  Number rejected based on metadata (including missing TMA, when asked for): 8.  Number kept: 1549.
  # 
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
 
   
  #    # These are not matched to the scans, since they are unscannable
  #    Model_Spectra_Meta$unscannable_BB <- Model_Spectra_Meta$unscannable_Broken_MissingPieces <- Model_Spectra_Meta$unscannable_Crystalized <- Model_Spectra_Meta$unscannable_sample_mixed <- Model_Spectra_Meta$unscannable_no_otolith <- NULL
  #    
  #    Model_Spectra_Meta$sex <- recode.simple(Model_Spectra_Meta$sex, cbind(c(1, 2, 3), c('M', 'F', 'U')))  # Wow, sex still in numbers!!
  #    # xyplot(length_cm ~ age_best, group = sex, data = Model_Spectra_Meta, auto = TRUE)
  #          
  #    Model_Spectra_Meta$crystallized_scan <- !is.na(Model_Spectra_Meta$crystallized_scan)
  #    Table(Model_Spectra_Meta$crystallized_scan)
  #    
  #    Table(Model_Spectra_Meta$percent_crystallized_scan, Model_Spectra_Meta$crystallized_scan)  # All NA percent_crystallized_scan's are zeros
  #    Model_Spectra_Meta$percent_crystallized_scan[is.na(Model_Spectra_Meta$percent_crystallized_scan)] <- 0 # Change NA to zero so that a numerical test can be done (see below).
  #    Table(Model_Spectra_Meta$percent_crystallized_scan, Model_Spectra_Meta$crystallized_scan) 
  #    
  #    Table(Model_Spectra_Meta$broken_scan)  #  Do all of these broken pieces fit together well? What about entering the number of pieces the otie is broken into - zero for an unbroken ototlith.
  #                                           # Are the oties ever broken during scanning or the handling of the otie for scanning?
  #                                           
  #    Table(Model_Spectra_Meta$tip_only_scan)  # is there a difference between 'no' and an NA for this one?
  #      
  #    Table(Model_Spectra_Meta$anterior_tip_missing)
  #    
  #    Table(Model_Spectra_Meta$posterior_tip_missing)
  #    
  #    Table(Model_Spectra_Meta$percent_missing_scan)   
  #    Model_Spectra_Meta$percent_missing_scan[is.na(Model_Spectra_Meta$percent_missing_scan)] <- 0 # Now a numerical test can be done (see below).
  #    Table(Model_Spectra_Meta$percent_missing_scan)  
  #    
  #    Table(Model_Spectra_Meta$tissue_present_scan)  
  #    
  #    Table(Model_Spectra_Meta$tissue_level_scan) # Change this to 'percent_tissue_level_scan'?
  #    Model_Spectra_Meta$tissue_level_scan[is.na(Model_Spectra_Meta$tissue_level_scan)] <- 0 # Now a numerical test can be done (see below).
  #    Table(Model_Spectra_Meta$tissue_level_scan)
  #    
  #    Table(Model_Spectra_Meta$oil_clay_contamination_scan) # No percentage for this one
  #    
  #    Table(Model_Spectra_Meta$stained_scan) # All NA's. Is this 'yes'/'no' or percentage?
  #    
  #    Table(Model_Spectra_Meta$contamination_other_scan) # All NA's. Is this 'yes'/'no' or percentage?
  #      
  #    
  #    names(Model_Spectra_Meta)[names(Model_Spectra_Meta) %in% 'age_best'] <- "TMA"
  #    Table(is.finite(Model_Spectra_Meta$TMA))
  #          
  #    rev(sort(Model_Spectra_Meta$TMA))[1:20]
  #    
  #    dim(Model_Spectra_Meta) # Sable Combo 2022: 1557 rows 535 cols
  #    
  #    TF <- !is.na(Model_Spectra_Meta$TMA) & Model_Spectra_Meta$percent_crystallized_scan <= 15 & Model_Spectra_Meta$percent_crystallized_scan <= 10 &
  #           Model_Spectra_Meta$tissue_level_scan <= 10 & !is.na(Model_Spectra_Meta$length_cm) & !is.na(Model_Spectra_Meta$structure_weight_g)
  #    c(sum(TF), sum(!TF), sum(TF) + sum(!TF))
  #    
  #    sort(fileNamesRemove <- Model_Spectra_Meta$filenames[!TF])
  #    
  #    file.remove(paste0(Spectra_Path, "/", fileNamesRemove, ".0"), recursive = TRUE)  # Bad raw scans removed here
  #    
  #    Model_Spectra_Meta <- Model_Spectra_Meta[TF, ] # --- Doing an overwrite here!! --- Gettin rid of bad scans or ones with no metadata that is wanted
  #    dim(Model_Spectra_Meta) # Sable Combo 2022: 1528 rows
  #    
  #    Model_Spectra_Meta$shortName <- apply(Model_Spectra_Meta[, 'filenames', drop = FALSE], 1, function(x) paste(get.subs(x, sep = "_")[c(1, 5)], collapse = "_"))
  #    
  #    
  #    # Look at the data with plotly and remove rogue oties if needed# 
  #    plotly.Spec(Model_Spectra_Meta, 'all', htmlPlotFolder = paste0(Figures, '/Spectra Figure by TMA'))
  #    # Model_Spectra_Meta <- Model_Spectra_Meta[!Model_Spectra_Meta$shortName %in% 'HAKE_48', ] # Example of removing a rogue otie for Hake 2019
  #    # plotly.Spec(Model_Spectra_Meta, 'all') # Decide to save figure with rogue otie or the figure without the rogue otie, or both.
  #    
   
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
   file.remove(paste0(Spectra_Path, "/", fileNames.0[SaveOutOties]), recursive = TRUE) # Leave out raw scans removed here - number of scans left should match Model_Spectra_Meta, Model_Spectra, and TMA_Vector
   
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
   
   # Visually find the ylim to apply to both figures and over all areas and WB
   browsePlot('
      par(mfrow = c(2, 1))
      mdatools::plotRMSE(Model_Spectra.iPLS.F$gm)
      mdatools::plotRMSE(Model_Spectra.iPLS.F$om)
   ')
   
   # Use the ylim for both plots
   browsePlot('
     par(mfrow = c(2, 1))
     mdatools::plotRMSE(Model_Spectra.iPLS.F$gm, ylim = c(2.4, 6.5))
     mdatools::plotRMSE(Model_Spectra.iPLS.F$om, ylim = c(2.4, 6.5))
   ')
   
   # Select iPLS vars and add metadata wanted
   # (p <- length(Model_Spectra.iPLS.F$var.selected)) # 380 freq selected out of a total of 1140
   
   # Model_Spectra.sg.iPLS <- data.frame(Model_Spectra.sg[, sort(Model_Spectra.iPLS.F$var.selected)], length_prop_max = Model_Spectra_Meta$length_cm/max(Model_Spectra_Meta$length_cm), 
   #                                        structure_weight_dg = 10 * Model_Spectra_Meta$structure_weight_g) # dg = decigram
   
   Model_Spectra.sg.iPLS <- data.frame(Model_Spectra.sg[, sort(Model_Spectra.iPLS.F$var.selected)], Model_Spectra_Meta[, c('length_prop_max', 'structure_weight_dg', 'Month_Scaled')]) # dg = decigram
                                           
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
       
                     
                     
       # --------------- Try ipls() with smoothed spectra data and metadata - NO METADATA WAS SELECTED HERE ------------------------
       
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

# --- NN Model ---
{ ###

# Load the data if needed
base::load(paste0(Spectra_Set, '_Model_Spectra.sg.iPLS.RData')); print(dim(Model_Spectra.sg.iPLS))
# base::load(paste0(Spectra_Set, '_TMA_Vector.RData')); print(length(TMA_Vector))
base::load(paste0(Spectra_Set, '_Model_Spectra_Meta_ALL_GOOD_DATA.RData')) # Model_Spectra_Meta with SaveOutOties removed is only needed for TMA_Vector and the 'filenames' below
base::load(paste0(Spectra_Set, '_SaveOutOties_Seed_727.RData')); print(length(SaveOutOties))

Model_Spectra_Meta <- Model_Spectra_Meta[-SaveOutOties, ]; print(dim(Model_Spectra_Meta))
print(Model_Spectra_Meta[1:3, c(1, (grep('project', names(Model_Spectra_Meta))):ncol(Model_Spectra_Meta))])
TMA_Vector <- Model_Spectra_Meta$TMA 

# ----- Remove both metadata columns for testing  -----
# Model_Spectra.sg.iPLS$length_prop_max <- Model_Spectra.sg.iPLS$structure_weight_dg <- NULL

# ----- Leave only the fish length for testing  -----
# Model_Spectra.sg.iPLS$structure_weight_dg <- NULL

# ----- Leave only the otie weight for testing  -----
# Model_Spectra.sg.iPLS$length_prop_max <- NULL

print(dim(Model_Spectra.sg.iPLS))
# print(Model_Spectra.sg.iPLS$length_prop_max[1:4])
# print(Model_Spectra.sg.iPLS$structure_weight_dg[1:4])
print(headTail(Model_Spectra.sg.iPLS, 3, 2, 3, 5))


# --------- Special code to test 'Month_Scaled', 'Depth_m', 'Sex', 'Weight_kg', 'Days_into_Year', and reduced model size in the NN Model with the same scans in Model_Spectra.sg.iPLS ------------------------------
base::load("C:\\ALL_USR\\JRW\\SIDT\\Get Otie Info from Data Warehouse\\selectSpAgesFramFeb2024.RData")  # From NWFSC Data Warehouse


#     ===>                                                                                                                                                                                              Fish length and Otie Wgt: # SAD: 2050; RMSE: 2.7280
# Model_Spectra.sg.iPLS <- match.f(data.frame(Model_Spectra.sg.iPLS, specimen_id = as.character(Model_Spectra_Meta$specimen_id)), selectSpAgesFramFeb2024, "specimen_id", "AgeStr_id", c('Month_Scaled', 'Depth_m', 'Sex', 'Weight_kg')) # Very poor results
# Model_Spectra.sg.iPLS <- match.f(data.frame(Model_Spectra.sg.iPLS, specimen_id = as.character(Model_Spectra_Meta$specimen_id)), selectSpAgesFramFeb2024, "specimen_id", "AgeStr_id", c('Month_Scaled', 'Weight_kg', 'Depth_m'))  # SAD: 2029; RMSE: 2.7678
Model_Spectra.sg.iPLS <- match.f(data.frame(Model_Spectra.sg.iPLS, specimen_id = as.character(Model_Spectra_Meta$specimen_id)), selectSpAgesFramFeb2024, "specimen_id", "AgeStr_id", c('Weight_kg', 'Depth_m')) # SAD: 2002; RMSE: 2.6742
#    Model_Spectra.sg.iPLS <- match.f(data.frame(Model_Spectra.sg.iPLS, filenames = Model_Spectra_Meta$filenames, specimen_id = as.character(Model_Spectra_Meta$specimen_id)), selectSpAgesFramFeb2024, "specimen_id", "AgeStr_id", c('Weight_kg', 'Depth_m', 'Length_cm', 'Age'))
# Model_Spectra.sg.iPLS <- match.f(data.frame(Model_Spectra.sg.iPLS, specimen_id = as.character(Model_Spectra_Meta$specimen_id)), selectSpAgesFramFeb2024, "specimen_id", "AgeStr_id", 'Weight_kg') # SAD: 2088; RMSE: 2.7389
# Model_Spectra.sg.iPLS <- match.f(data.frame(Model_Spectra.sg.iPLS, specimen_id = as.character(Model_Spectra_Meta$specimen_id)), selectSpAgesFramFeb2024, "specimen_id", "AgeStr_id", 'Depth_m')  # SAD: 2042; RMSE: 2.7404
# Model_Spectra.sg.iPLS <- match.f(data.frame(Model_Spectra.sg.iPLS, specimen_id = as.character(Model_Spectra_Meta$specimen_id)), selectSpAgesFramFeb2024, "specimen_id", "AgeStr_id", 'Month_Scaled')  # SAD: ????; RMSE: ????
# Model_Spectra.sg.iPLS <- match.f(data.frame(Model_Spectra.sg.iPLS, specimen_id = as.character(Model_Spectra_Meta$specimen_id)), selectSpAgesFramFeb2024, "specimen_id", "AgeStr_id", c('Weight_kg', 'Depth_m', 'Days_into_Year')) # SAD: 2090; RMSE: 2.8047

Model_Spectra.sg.iPLS$specimen_id <- NULL  # specimen_id only needed for the matching above

print(headTail(Model_Spectra.sg.iPLS, 3, 2, 3, 5))

# Check for missing data
print(dim(Model_Spectra.sg.iPLS))
print(dim(na.omit(Model_Spectra.sg.iPLS)))


################ OLD ###############
# These 3 oties in the metadata were missing from the Data WareHouse: AgeStr_id %in% 102133144:102133146  ????????????????

#  Model_Spectra.sg.iPLS$Month_Scaled[is.na(Model_Spectra.sg.iPLS$Month_Scaled)] <- 6:8/12
#  Model_Spectra.sg.iPLS$Depth_m[is.na(Model_Spectra.sg.iPLS$Depth_m)] <- mean(Model_Spectra.sg.iPLS$Depth_m, na.rm = TRUE)
#  Model_Spectra.sg.iPLS$Weight_kg[is.na(Model_Spectra.sg.iPLS$Weight_kg)] <- mean(Model_Spectra.sg.iPLS$Weight_kg, na.rm = TRUE) 
#  Model_Spectra.sg.iPLS$Sex[is.na(Model_Spectra.sg.iPLS$Sex)] <- c('M','F', 'M')

#  print(dim(na.omit(Model_Spectra.sg.iPLS)))
#  print(headTail(Model_Spectra.sg.iPLS, 3, 2, 3, 5))
################################


if(!is.null(Model_Spectra.sg.iPLS$Sex)) {
   Model_Spectra.sg.iPLS$Sex_prop_max <- as.numeric(recode.simple(Model_Spectra.sg.iPLS$Sex, data.frame(c('F','M', 'U'), 0:2)))/2  # ** All variables have to be numeric ** 
   Model_Spectra.sg.iPLS$Sex <- NULL
}   

if(!is.null(Model_Spectra.sg.iPLS$Depth_m)) {
   Model_Spectra.sg.iPLS$Depth_prop_max <- (Model_Spectra.sg.iPLS$Depth_m - min(Model_Spectra.sg.iPLS$Depth_m))/(max(Model_Spectra.sg.iPLS$Depth_m) - min(Model_Spectra.sg.iPLS$Depth_m))
   Model_Spectra.sg.iPLS$Depth_m <- NULL
}   

if(!is.null(Model_Spectra.sg.iPLS$Weight_kg)) {
   Model_Spectra.sg.iPLS$Weight_prop_max <- (Model_Spectra.sg.iPLS$Weight_kg - min(Model_Spectra.sg.iPLS$Weight_kg))/(max(Model_Spectra.sg.iPLS$Weight_kg) - min(Model_Spectra.sg.iPLS$Weight_kg))
   Model_Spectra.sg.iPLS$Weight_kg <- NULL
}   

if(!is.null(Model_Spectra.sg.iPLS$Days_into_Year)) {
   Model_Spectra.sg.iPLS$Days_into_Year_prop_max <- (Model_Spectra.sg.iPLS$Days_into_Year - min(Model_Spectra.sg.iPLS$Days_into_Year))/(max(Model_Spectra.sg.iPLS$Days_into_Year) - min(Model_Spectra.sg.iPLS$Days_into_Year))
    Model_Spectra.sg.iPLS$Days_into_Year <- NULL
}   

print(headTail(Model_Spectra.sg.iPLS, 3, 2, 3, 5))

# = = = = = = = = = = = = = = = = = Intial setup = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
   
Seed_Fold <- 727 # Seed_Fold = 787 for Run 3.  Seed 747 used for Fish_Len_Otie_Wgt_Run_2 .  Using a different seed starting here, to test main run of Sable_2022 with fish length and otie weight (and other metadata runs)
                 #      Seed_Fold = 727 used in the code above and for previous runs (Fish_Len_Otie_Wgt Run 1) of Sable_2022 before 28 Dec 2023

# ------- Reduce model size to see the change in prediction ability ----------------------
set.seed(Seed_Fold) 

# Random selection
Rdm_Oties <- sample(1:nrow(Model_Spectra.sg.iPLS), 750)  # nrow(Model_Spectra.sg.iPLS) for Sablefish 2022 is 1,513 

Model_Spectra.sg.iPLS <- Model_Spectra.sg.iPLS[Rdm_Oties, ]
print(dim(Model_Spectra.sg.iPLS))
print(headTail(Model_Spectra.sg.iPLS, 3, 2, 3, 5))

# Model_Spectra_Meta <- Model_Spectra_Meta[Rdm_Oties, ]
# print(Model_Spectra_Meta[1:3, c(1, (grep('project', names(Model_Spectra_Meta))):ncol(Model_Spectra_Meta))])

TMA_Vector <- TMA_Vector[Rdm_Oties]
print(length(TMA_Vector))


# Stratified random selection
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


# --- Setup graphic windows ---
graphics.off()  
dev.new(width = 14, height = 6) #2
dev.new() # 3
dev.new(width = 11, height = 8) # 4
dev.new(width = 11, height = 8) # 5
dev.new(width = 10, height = 10) # 6
dev.new(width = 10, height = 10) # 7


# = = = = = = Pick number of random reps (Rdm_reps), number of folds (num_folds), and iteration number (Iter_Num), then run the NN code to the next '= = =' line and expect long run times = = = = = = = = =
    
Rdm_reps <- 20
num_folds <- 10
Iter_Num <- 8

# (Rdm_reps <- ifelse(model_Name == 'FCNN_model_ver_1', 20, 10))
Seed_Main <- Seed_Fold + 20 # Seed 747 used for Fish_Len_Otie_Wgt_Run_2. Seed_Main <- 707 used for previous runs of Sable_2022 before 28 Dec 2023  # Reducing the number of seeds will be considered later
set.seed(Seed_Main) 
Seed_reps <- sample(1e7, Rdm_reps)

# Start fresh or continue by loading a file with model iterations already finished (see the commented line with an example model file). 
Rdm_models <- list() 
Rdm_folds_index <- list()
# base::load("Sable_Combo_2022_FCNN_model_ver_1_13_Rdm_model_8_Mar_2024_11_24_15.RData") 

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
       
       while(file.exists('Run_NN_Model_Flag')) {    # The multiple full fold version breaks the stop by removing the file flag, but it remains for now
       
          # R memory garbage collection
          gc()
          
          # Clear TensorFlow's session
          k_clear_session()
       
          (Iter <- Iter + 1)
          cat(paste0("\n\nRandom Replicates = ", j, ": Fold number = ", i, ": Iter = ", Iter,"\n"))
          
          
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
                             view_metrics = FALSE)
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
		     browsePlot('print(plot(history))', file = paste0("NN_History_Iter_", Iter)) # Save NN History figures
              
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
          agreementFigure(y.test, y.test.pred, Delta, main = paste0("Random Reps = ", j, ": Fold Num = ", i, ": Iter = ", Iter))
         
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
          
          print(saveName <- paste0(Spectra_Set, '_', paste(get.subs(model_Name, "_")[-2], collapse = "_"), '_SM_', Seed_Model, '_RI_', j, '_LR_', 
             format(learningRate, sci = FALSE), '_LD_', ifelse(is.null(layer_dropout_rate), 0, layer_dropout_rate), '_It_', length(SAD), 
             '_SAD_', rev(SAD)[1], '_', timeStamp()))
          assign(saveName, keras::serialize_model(model, include_optimizer = TRUE))
          # save(Iter, Cor, CA_diag, SAD, learningRate, layer_dropout_rate, .Random.seed, list = saveName, file = paste0(saveName, '.RData'))
          
          saveModels <- c(saveModels, saveName)
          saveModels_List[[saveName]] <- keras::serialize_model(model, include_optimizer = TRUE)
         
          if(Iter == Iter_Num)
              break
       } # Iter while() loop
       
       if(!file.exists('Run_NN_Model_Flag'))
          break
       
       Iter_Best_Model <- sort.f(data.frame(SAD, RMSE, CA_diag, Iter = 1:Iter_Num), c(1, 3))[1, 4]  # Best model is when SAD is lowest, with ties broken by CA_diag
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

   
   browsePlot('agreementFigure(y.fold.test.ALL, y.fold.test.pred.ALL, Delta = Delta, full = TRUE, main = paste0("Random Rep = ", j))')   
   
}  # k Random Replicate loop

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

} ###

# --- Find Median over all Rdm_reps Models and create figures ---
{ ###

# Only 2 loads needed to redo this section with new data - the Model_Spectra.sg.iPLS has to, of course, match the Rdm_model and Rdm_folds_index 
# base::load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt_GPU_Machine\\Sable_Combo_2022_FCNN_model_ver_1_5_Rdm_model_21_Dec_2023_08_14_19.RData")
# base::load("C:\\ALL_USR\\JRW\\SIDT\\Sablefish 2022 Combo\\Sable_Combo_2022_NN_Fish_Len_Otie_Wgt\\Sable_Combo_2022_Model_Spectra.sg.iPLS.RData")

# ----------------------- Put the fitted results for each random reps (Rdm_reps) full fold model into a data frame:  y.fold.test.pred_RDM ------------------------
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
   browsePlot('agreementFigure(TMA_Vector, y.test.pred, Delta = -0.05, full = TRUE, main = paste0("Random Rep = ", j))') # Delta is a previous estimate or guess for now
   
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

# Agreement Figures (standard and zoomed) using the best delta from above
# dev.new(width = 11, height = 8) # R plot window version
# agreementFigure(TMA_Vector, y.fold.test.pred_RDM_median, Delta = Delta, full = TRUE, main = paste0("Median over ", Rdm_reps, ' Full k-Fold Models'), cex = 1.25) # R plot window version
# browsePlot('agreementFigure(TMA_Vector, y.fold.test.pred_RDM_median, Delta = Delta, full = TRUE, main = paste0("Median over ", Rdm_reps, " Full k-Fold Models"), cex = 1.25)', file = 'Figures/Sable_2022_Combo_20_Rdm_Final.pdf', pdf = TRUE) # PDF version
browsePlot('agreementFigure(TMA_Vector, y.fold.test.pred_RDM_median, Delta = Delta, full = TRUE, main = paste0("Median over ", Rdm_reps, " Full k-Fold Models"), cex = 1.25)', file = paste0('Figures/', Spectra_Set, '_', length(Rdm_folds_index), '_Rdm_Final.png'))
browsePlot('agreementFigure(TMA_Vector, y.fold.test.pred_RDM_median, Delta = Delta, full = FALSE, main = paste0("Median over ", Rdm_reps, " Full k-Fold Models"), cex = 1.25)', file = paste0('Figures/', Spectra_Set, '_', length(Rdm_folds_index), '_Rdm_Final_Zoomed.png'))
                                                                                                                                               

# Apply that best Delta (from above) to all Rdm_reps models individually
Stats_RDM_median_by_model <- NULL
for(numRdmModels in 1:Rdm_reps) {

   y.fold.test.pred_RDM_median <- apply(y.fold.test.pred_RDM[numRdmModels, ,drop = FALSE], 2, median)
   Stats_RDM_median_by_model <- rbind(Stats_RDM_median_by_model, data.frame(t(Correlation_R_squared_RMSE_MAE_SAD(TMA_Vector, round(y.fold.test.pred_RDM_median + Delta)))))
}

print(Stats_RDM_median_by_model)
 

# An additional full k-fold added to the total number of models at each step in turn
# dev.new(width = 11, height = 8)

browsePlot("
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
print(Stats_0_1_interval <- cbind(Stats_RDM_median_by_model_added[,1:2], t(t(minAdj)/max.of.Adj)))


matplot(1:Rdm_reps, Stats_0_1_interval, type = 'o', col = c(1:3,6), xlab = 'Number of Complete Folds', ylab = 'Various Stats', main = 'Original Order')
 
# Add 5 more Randomized order figures
set.seed(Seed_Main) 
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

# Jitter TMA; vertical line for each unique TMA - without standard grid        
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
   




