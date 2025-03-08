

setwd("C:/SIDT/Chilipepper")
# library(JRWToolBox)

  
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

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/headTail.R")    
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Read_OPUS_Spectra.R")

source("C:\\SIDT\\Train_NN_Model\\Read_OPUS_Spectra.R")



sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Import_Species_Metadata_from_NWFSC_Warehouse.R")
# source("C:\\SIDT\\Chilipepper\\Import_Species_Metadata_from_NWFSC_Warehouse.R")

Chilipepper_Combo_Metadata <- Import_Species_Metadata_from_NWFSC_Warehouse("chilipepper")
headTail(Chilipepper_Combo_Metadata)
save(Chilipepper_Combo_Metadata, file = "C:/SIDT/Get Otie Info from Data Warehouse/Chilipepper_Combo_Metadata.RData") # R object name needs to have "Metadata" in it


# For testing Read_OPUS_Spectra():  Spectra_Set = "PWHT_Acoustic2023"; Spectra_Path = "PWHT_Acoustic2023_Scans"; fineFreqAdj = 0;
#                   Meta_Path <- "C:/SIDT/PWHT_Acoustic_2023/Acoustic_2023_PWHT_NIR0069_Scanning_Session_Report_For_NWC.xlsx";
#                   plot <- TRUE; Meta_Add <- TRUE; spectraInterp = 'stats_splinefun_lowess'; excelSheet <- 3; opusReader = 'philippbaumann_opusreader2'; 
#                                  Predicted_Ages_Path = "Predicted_Ages"; (htmlPlotFolder <- paste0(Predicted_Ages_Path, '/', Spectra_Set, '_Spectra_Sample_of_', 20))
#      


for (i in (2010:2016)[7]) {

    Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set = paste0("CLPR_SWFSC_", i), Spectra_Path = paste0(i, "_scans"), htmlPlotFolder = paste0("Figures_", i), Static_Figure = paste0("CLPR_SWFSC_", i, ".png"),
                                        Meta_Path = "C:/SIDT/Chilipepper/Chilipepper_Otolith_Weights_SWFSC_with_Scans.xlsx", excelSheet = i - 2009,
										Extra_Meta_Path = "C:/SIDT/Get Otie Info from Data Warehouse/Chilipepper_Combo_Metadata.RData", Debug = TRUE)
										
	if(i == 2010)  
        Model_Spectra_Meta <- Model_Spectra_Meta[!Model_Spectra_Meta$shortName %in% "CLPR_100165282_Combo_Survey", ]
		
    headTail(Model_Spectra_Meta, 3, 3, 3, 40)
	     
    save(Model_Spectra_Meta, file = paste0("CLPR_SWFSC_", i, "_Model_Spectra_Meta_ALL_GOOD_DATA_TEST.RData"))
}


for (i in c(2019, 2021:2024)) {

    Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set = paste0("CLPR_NWFSC_", i), Spectra_Path = paste0(i, "_scans"), htmlPlotFolder = paste0("Figures_", i), Static_Figure = paste0("CLPR_SWFSC_", i, ".png"),
                                        Meta_Path = paste0("C:/SIDT/Chilipepper/CLPR_COMBO_", i, "_Scanning_Session_Report_For_NWC.xlsx"),
										Extra_Meta_Path = "C:/SIDT/Get Otie Info from Data Warehouse/Chilipepper_Combo_Metadata.RData", Debug = FALSE)
		
    headTail(Model_Spectra_Meta, 3, 3, 3, 40)
	     
    save(Model_Spectra_Meta, file = paste0("CLPR_SWFSC_", i, "_Model_Spectra_Meta_ALL_GOOD_DATA.RData"))
}


# --- CA Commercial - The 2018 oties were missing... ---

#  !!!!!! Capitalize the first letter of "length_cm", weight_kg", and "sex" in the Excel file !!!!!!

Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set = "CLPR_CACOMM_2019_2020", Spectra_Path = "2019_2020_CACOMM_Scans", htmlPlotFolder = "Figures_CLPR_CACOMM_2019_2020", 
                                        shortNameSegments <- c(1, 6), shortNameSuffix <- 'CA_Comm', Static_Figure = "CLPR_CACOMM_2019_2020.png",
                                        Meta_Path = "C:/SIDT/Chilipepper/CLPR_CACOMM_2019_2020_Scanning_Session_Report_For_NWC.xlsx", Debug = TRUE)
		
headTail(Model_Spectra_Meta, 3, 3, 3, 40)
	     
save(Model_Spectra_Meta, file = paste0("CLPR_CACOMM_2019_2020_Model_Spectra_Meta_ALL_GOOD_DATA.RData"))


# --- OR Commercial ---

#  !!!!!! Capitalize the first letter of "length_cm", weight_kg", and "sex" in the Excel file !!!!!!

Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set = "CLPR_ORCOMM_2023_2024", Spectra_Path = "2023_2024_ORCOMM_Scans", htmlPlotFolder = "Figures_CLPR_ORCOMM_2023_2024", 
                                        shortNameSegments <- c(1, 6), shortNameSuffix <- 'CA_Comm', Static_Figure = "CLPR_ORCOMM_2023_2024.png",
                                        Meta_Path = "C:/SIDT/Chilipepper/CLPR_ORCOMM_2023_2024_Scanning_Session_Report_For_NWC.xlsx", Debug = TRUE)
		
headTail(Model_Spectra_Meta, 3, 3, 3, 40)
	     
save(Model_Spectra_Meta, file = paste0("CLPR_CLPR_ORCOMM_2023_2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData"))

 
       

# ============ Combine years ==============

library(JRWToolBox)
setwd("C:/SIDT/Chilipepper")


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
	
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")

# Remove "CLPR_100165282_Combo_Survey otie
load("C:\\SIDT\\Chilipepper\\CLPR_SWFSC_2010_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta <- Model_Spectra_Meta[!Model_Spectra_Meta$shortName %in% "CLPR_100165282_Combo_Survey", ]
headTail(Model_Spectra_Meta, 3, 3, 3, 40)
save(Model_Spectra_Meta, file = "C:\\SIDT\\Chilipepper\\CLPR_SWFSC_2010_Model_Spectra_Meta_ALL_GOOD_DATA.RData")

# Create a vector with the column names wanted
load("C:\\SIDT\\Chilipepper\\CLPR_SWFSC_2014_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta$NOTES <- NULL
Columns <- names(Model_Spectra_Meta)
matrix(Columns, ncol = 1)
Columns <- Columns[c(1:511, 517, 518, 513, 520:526, 519, 527:533, 538)]
matrix(Columns, ncol = 1) # Length is now 530
Columns <- c(Columns[1:529], c("Month_May", "Month_Jun", "Month_Jul", "Month_Aug", "Month_Sep", "Month_Oct"), Columns[530])
Columns[c(1, 507:536)]
 [1] "filenames"           "X3960"               "X3952"               "project"             "sample_year"         "Sex"                 "Length_cm"           "TMA"                 "structure_weight_g" 
[10] "specimen_id"         "shortName"           "structure_weight_dg" "Length_prop_max"     "Sex_F"               "Sex_M"               "Sex_U"         



for(i in c(2010, 2014:2016, 2019, 2021:2024)[2]) {
    print(i)
    base::load(paste0("C:\\SIDT\\Chilipepper\\CLPR_SWFSC_", i, "_Model_Spectra_Meta_ALL_GOOD_DATA.RData"))
	Table(Model_Spectra_Meta$sample_year)
}


Model_Spectra_Meta_All <- NULL 
for(i in c(2010, 2014:2016, 2019, 2021:2024)) {
    print(i)
    base::load(paste0("C:\\SIDT\\Chilipepper\\CLPR_SWFSC_", i, "_Model_Spectra_Meta_ALL_GOOD_DATA.RData"))
	if(is.null(Model_Spectra_Meta$Sex_U))  Model_Spectra_Meta$Sex_U <- 0
    if(is.null(Model_Spectra_Meta$Month_May))  Model_Spectra_Meta$Month_May <- 0
    if(is.null(Model_Spectra_Meta$Month_Jun))  Model_Spectra_Meta$Month_Jun <- 0
    if(is.null(Model_Spectra_Meta$Month_Jul))  Model_Spectra_Meta$Month_Jul <- 0
    if(is.null(Model_Spectra_Meta$Month_Aug))  Model_Spectra_Meta$Month_Aug <- 0
    if(is.null(Model_Spectra_Meta$Month_Sep))  Model_Spectra_Meta$Month_Sep <- 0
    if(is.null(Model_Spectra_Meta$Month_Oct))  Model_Spectra_Meta$Month_Oct <- 0
    # plotly.Spec(Model_Spectra_Meta, N_Samp = min(c(nrow(Model_Spectra_Meta), 'All')), colorGroup = 'TMA')  
    Model_Spectra_Meta_All <- rbind(Model_Spectra_Meta_All, Model_Spectra_Meta[, Columns])
}

Model_Spectra_Meta <- Model_Spectra_Meta_All

headTail(Model_Spectra_Meta, 3,3,3,40)
Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)

headTail(Model_Spectra_Meta[!is.na(Model_Spectra_Meta$TMA), ], 3,3,3,40)

save(Model_Spectra_Meta, file = "CLPR_SWFSC_2010__2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")


# For the preliminary model with only 2010 and 2014
Model_Spectra_Meta <- Model_Spectra_Meta[Model_Spectra_Meta$sample_year %in% c(2010, 2014), ]
Table(Model_Spectra_Meta$ sample_year, Model_Spectra_Meta$TMA)
save(Model_Spectra_Meta, file = "CLPR_SWFSC_2010_2014_Model_Spectra_Meta_ALL_GOOD_DATA.RData")



# ---- Add CA comm scans - *** need "Columns" vector from above *** ----

load("CLPR_SWFSC_2010__2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta_SWFSC_2010__2024 <- Model_Spectra_Meta

load("CLPR_CACOMM_2019_2020_Model_Spectra_Meta_ALL_GOOD_DATA.RData")

# Model_Spectra_Meta$TMA <- NA
# Model_Spectra_Meta$Length_cm <- NA
# Model_Spectra_Meta$Weight_kg <- NA
# Model_Spectra_Meta$Sex <- NA
Model_Spectra_Meta$Depth_m <- NA
Model_Spectra_Meta$Latitude_dd <- NA
# Model_Spectra_Meta$Month <- NA
Model_Spectra_Meta$Days_into_Year <- NA
# Model_Spectra_Meta$Length_prop_max <- NA
Model_Spectra_Meta$Weight_prop_max <- NA
Model_Spectra_Meta$Depth_prop_max <- NA
Model_Spectra_Meta$Latitude_prop_max <- NA
Model_Spectra_Meta$Days_into_Year_prop_max <- NA


if(is.null(Model_Spectra_Meta$Sex_F))  Model_Spectra_Meta$Sex_F <- 0
if(is.null(Model_Spectra_Meta$Sex_M))  Model_Spectra_Meta$Sex_M <- 0
if(is.null(Model_Spectra_Meta$Sex_U))  Model_Spectra_Meta$Sex_U <- 0

if(is.null(Model_Spectra_Meta$Month_May))  Model_Spectra_Meta$Month_May <- 0
if(is.null(Model_Spectra_Meta$Month_Jun))  Model_Spectra_Meta$Month_Jun <- 0
if(is.null(Model_Spectra_Meta$Month_Jul))  Model_Spectra_Meta$Month_Jul <- 0
if(is.null(Model_Spectra_Meta$Month_Aug))  Model_Spectra_Meta$Month_Aug <- 0
if(is.null(Model_Spectra_Meta$Month_Sep))  Model_Spectra_Meta$Month_Sep <- 0
if(is.null(Model_Spectra_Meta$Month_Oct))  Model_Spectra_Meta$Month_Oct <- 0

Model_Spectra_Meta$sample_year <- paste0(Model_Spectra_Meta$sample_year, "_CA_Comm")
Table(Model_Spectra_Meta$sample_year)

headTail(Model_Spectra_Meta,3,3,3,62)

Model_Spectra_Meta <- rbind(Model_Spectra_Meta_SWFSC_2010__2024, Model_Spectra_Meta[, Columns])  # Columns[c(1, 507:536)]
Table(Model_Spectra_Meta$sample_year)

save(Model_Spectra_Meta, file = "CLPR_SWFSC_2010__2024_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData")




# ---- Add OR comm scans - *** need "Columns" vector from above *** ----

load("C:\\SIDT\\CLPR_Combo_2010__2024\\CLPR_SWFSC_2010__2024_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta_SWFSC_2010__2024 <- Model_Spectra_Meta

load("CLPR_CLPR_ORCOMM_2023_2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")

Model_Spectra_Meta$Depth_m <- NA
Model_Spectra_Meta$Latitude_dd <- NA
Model_Spectra_Meta$Days_into_Year <- NA

Model_Spectra_Meta$Depth_prop_max <- NA
Model_Spectra_Meta$Weight_prop_max <- NA
Model_Spectra_Meta$Latitude_prop_max <- NA
Model_Spectra_Meta$Days_into_Year_prop_max <- NA


if(is.null(Model_Spectra_Meta$Sex_F))  Model_Spectra_Meta$Sex_F <- 0
if(is.null(Model_Spectra_Meta$Sex_M))  Model_Spectra_Meta$Sex_M <- 0
if(is.null(Model_Spectra_Meta$Sex_U))  Model_Spectra_Meta$Sex_U <- 0

if(is.null(Model_Spectra_Meta$Month_May))  Model_Spectra_Meta$Month_May <- 0
if(is.null(Model_Spectra_Meta$Month_Jun))  Model_Spectra_Meta$Month_Jun <- 0
if(is.null(Model_Spectra_Meta$Month_Jul))  Model_Spectra_Meta$Month_Jul <- 0
if(is.null(Model_Spectra_Meta$Month_Aug))  Model_Spectra_Meta$Month_Aug <- 0
if(is.null(Model_Spectra_Meta$Month_Sep))  Model_Spectra_Meta$Month_Sep <- 0
if(is.null(Model_Spectra_Meta$Month_Oct))  Model_Spectra_Meta$Month_Oct <- 0

Model_Spectra_Meta$sample_year <- paste0(Model_Spectra_Meta$sample_year, "_OR_Comm")
Table(Model_Spectra_Meta$sample_year)

headTail(Model_Spectra_Meta,3,3,3,62)

Model_Spectra_Meta <- rbind(Model_Spectra_Meta_SWFSC_2010__2024, Model_Spectra_Meta[, Columns])  # Columns[c(1, 507:536)]
Table(Model_Spectra_Meta$sample_year)

save(Model_Spectra_Meta, file = "CLPR_SWFSC_2010__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData")



























