

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
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/replaceString.R")     
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


for (i in (2010:2016)) {

    Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set = paste0("CLPR_SWFSC_", i), Spectra_Path = paste0(i, "_scans"), htmlPlotFolder = paste0("Figures_", i), Static_Figure = paste0("CLPR_SWFSC_", i, ".png"),
                                        Meta_Path = "C:/SIDT/Chilipepper/Chilipepper_Otolith_Weights_SWFSC_with_Scans.xlsx", excelSheet = i - 2009,
										Extra_Meta_Path = "C:/SIDT/Get Otie Info from Data Warehouse/Chilipepper_Combo_Metadata.RData", Debug = TRUE)
										
	if(i == 2010)  
        Model_Spectra_Meta <- Model_Spectra_Meta[!Model_Spectra_Meta$shortName %in% "CLPR_100165282_Combo_Survey", ]
		
    headTail(Model_Spectra_Meta, 3, 3, 3, 55)
	     
    save(Model_Spectra_Meta, file = paste0("CLPR_SWFSC_", i, "_Model_Spectra_Meta_ALL_GOOD_DATA_TEST.RData"))
}


for (i in c(2018, 2019, 2021:2024)) {

    Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set = paste0("CLPR_NWFSC_", i), Spectra_Path = paste0(i, "_scans"), htmlPlotFolder = paste0("Figures_", i), Static_Figure = paste0("CLPR_SWFSC_", i, ".png"),
                                        Meta_Path = paste0("C:/SIDT/Chilipepper/CLPR_COMBO_", i, "_Scanning_Session_Report_For_NWC.xlsx"),
										Extra_Meta_Path = "C:/SIDT/Get Otie Info from Data Warehouse/Chilipepper_Combo_Metadata.RData", Debug = FALSE)
		
    headTail(Model_Spectra_Meta, 3, 3, 3, 55)
    Table(Model_Spectra_Meta$TMA)
	     
    save(Model_Spectra_Meta, file = paste0("CLPR_SWFSC_", i, "_Model_Spectra_Meta_ALL_GOOD_DATA.RData"))
}


# --- CA Commercial 2019, 2020- The 2018 oties were missing... ---

#  !!!!!! Capitalize the first letter of "length_cm", weight_kg", and "sex" in the Excel file !!!!!!
#  !!!!!! Also "project" needs to be in the first column, and there needs to be "TMA", "specimen_id", "sample_year", and "structure_weight_g" columns. !!!!!!

Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set = "CLPR_CACOMM_2019_2020", Spectra_Path = "2019_2020_CACOMM_Scans", htmlPlotFolder = "Figures_CLPR_CACOMM_2019_2020", 
                                        shortNameSegments = 6, shortNameSuffix = 'CA_Comm', Static_Figure = "CLPR_CACOMM_2019_2020.png",
                                        Meta_Path = "C:/SIDT/Chilipepper/CLPR_CACOMM_2019_2020_Scanning_Session_Report_For_NWC.xlsx", Debug = TRUE)
		
headTail(Model_Spectra_Meta, 3, 3, 3, 55)
	     
save(Model_Spectra_Meta, file = paste0("CLPR_CACOMM_2019_2020_Model_Spectra_Meta_ALL_GOOD_DATA.RData"))


# --- CA Commercial 1985 ---

#  !!!!!! Capitalize the first letter of "length_cm", weight_kg", and "sex" in the Excel file !!!!!!
#  !!!!!! Also "project" needs to be in the first column, and there needs to be "TMA", "specimen_id", "sample_year", and "structure_weight_g" columns. !!!!!!

Cols <- c("project", "specimen_id", "sample_year", "Sex", "Length_cm", "Weight_kg", "TMA", "Month", "structure_weight_g")

C_1985_sheet_1 <- openxlsx::read.xlsx("C:/SIDT/Chilipepper/Chilipepper_1985_Otolith_Weights_SWFSC_OA.xlsx", sheet = 1, detectDates = TRUE)
C_1985_sheet_1$Length_cm <- C_1985_sheet_1$Length_mm/10
C_1985_sheet_1$Weight_kg <- NA
C_1985_sheet_1$Month <- NA

C_1985_sheet_2 <- openxlsx::read.xlsx("C:/SIDT/Chilipepper/Chilipepper_1985_Otolith_Weights_SWFSC_OA.xlsx", sheet = 2, detectDates = TRUE)
C_1985_sheet_2$Length_cm <- C_1985_sheet_2$Length_mm/10
C_1985_sheet_2$specimen_id <- paste(C_1985_sheet_2$specimen_id, replaceString(format(C_1985_sheet_2$clust_no, width = 3), " ", "0"), replaceString(format(C_1985_sheet_2$fish_no, width = 3), " ", "0"), sep= "_")
C_1985_sheet_2$Month <- NA

openxlsx::write.xlsx(rbind(C_1985_sheet_1[, Cols], C_1985_sheet_2[, Cols]), file = "C:/SIDT/Chilipepper/Chilipepper_1985_Otolith_Weights_SWFSC.xlsx")

Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set = "CLPR_CACOMM_1985", Spectra_Path = "1985_CACOMM_Scans", htmlPlotFolder = "Figures_CLPR_CACOMM_1985", 
                                        shortNameSegments = 2, shortNameSuffix = 'CA_Comm', Static_Figure = "CLPR_CACOMM_1985.png", excelSheet = 1,
                                        Meta_Path = "C:/SIDT/Chilipepper/Chilipepper_1985_Otolith_Weights_SWFSC.xlsx", Debug = TRUE)
        
headTail(Model_Spectra_Meta, 3, 3, 3, 55)
Table(Model_Spectra_Meta$TMA)

# match.f(data.frame(FN = substring(fileNames, 13)), data.frame(MD = metadata$specimen_id), "FN", "MD", "MD")

save(Model_Spectra_Meta, file = paste0("CLPR_CACOMM_1985_Model_Spectra_Meta_ALL_GOOD_DATA.RData"))


# --- CA Commercial 1986 ---

#  !!!!!! Capitalize the first letter of "length_cm", weight_kg", and "sex" in the Excel file !!!!!!
#  !!!!!! Also "project" needs to be in the first column, and there needs to be "TMA", "specimen_id", "sample_year", and "structure_weight_g" columns. !!!!!!

Cols <- c("project", "specimen_id", "sample_year", "Sex", "Length_cm", "Weight_kg", "TMA", "Month", "structure_weight_g")

C_1986_sheet_1 <- openxlsx::read.xlsx("C:/SIDT/Chilipepper/Chilipepper_1986_Otolith_Weights_SWFSC_OA.xlsx", sheet = 1, detectDates = TRUE)
C_1986_sheet_1$Length_cm <- C_1986_sheet_1$Length_mm/10
C_1986_sheet_1$Weight_kg <- NA
C_1986_sheet_1$Month <- NA


openxlsx::write.xlsx(C_1986_sheet_1[, Cols], file = "C:/SIDT/Chilipepper/Chilipepper_1986_Otolith_Weights_SWFSC.xlsx")

Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set = "CLPR_CACOMM_1986", Spectra_Path = "1986_CACOMM_Scans", htmlPlotFolder = "Figures_CLPR_CACOMM_1986", 
                                        shortNameSegments = 2, shortNameSuffix = 'CA_Comm', Static_Figure = "CLPR_CACOMM_1986.png", excelSheet = 1,
                                        Meta_Path = "C:/SIDT/Chilipepper/Chilipepper_1986_Otolith_Weights_SWFSC.xlsx", Debug = TRUE)
        
headTail(Model_Spectra_Meta, 3, 3, 3, 55)
Table(Model_Spectra_Meta$TMA)

# match.f(data.frame(FN = substring(fileNames, 13)), data.frame(MD = metadata$specimen_id), "FN", "MD", "MD")

save(Model_Spectra_Meta, file = paste0("CLPR_CACOMM_1986_Model_Spectra_Meta_ALL_GOOD_DATA.RData"))






# --- OR Commercial 2022, 2023, 2024 ---

#  !!!!!! Change age-best to TMA and capitalize the first letter of "length_cm", weight_kg", and "sex" in the Excel file !!!!!!

Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set = "CLPR_ORCOMM_2022__2024", Spectra_Path = "2022__2024_ORCOMM_Scans", htmlPlotFolder = "Figures_CLPR_ORCOMM_2022__2024", 
                                        shortNameSegments = 6, shortNameSuffix = 'OR_Comm', Static_Figure = "CLPR_ORCOMM_2022__2024.png",
                                        Meta_Path = "C:/SIDT/Chilipepper/CLPR_ORCOMM_2022__2024_Scanning_Session_Report_For_NWC.xlsx", Debug = TRUE)
		
headTail(Model_Spectra_Meta, 3, 3, 3, 55)
Table(Model_Spectra_Meta$TMA)
	     
save(Model_Spectra_Meta, file = paste0("CLPR_ORCOMM_2022__2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")) # Was "CLPR_ORCOMM_2023__2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData"

 
       

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
 [1] "filenames"           "X3960"               "X3952"               "project"   "sample_year""Sex"  "Length_cm"   "TMA"     "structure_weight_g" 
[10] "specimen_id"         "shortName"           "structure_weight_dg" "Length_prop_max"     "Sex_F"  "Sex_M"   "Sex_U"         

[1] "filenames"               "X3960"                   "X3952"                   "project"                 "sample_year"             "TMA"                     "specimen_id"             "shortName"              
[9] "structure_weight_g"      "Length_cm"               "Weight_kg"               "Sex"                     "Depth_m"                 "Latitude_dd"             "Month"                   "Days_into_Year"         
[17] "structure_weight_dg"     "Length_prop_max"         "Weight_prop_max"         "Sex_F"                   "Sex_M"                   "Sex_U"                   "Depth_prop_max"          "Latitude_prop_max"      
[25] "Month_May"               "Month_Jun"               "Month_Jul"               "Month_Aug"               "Month_Sep"               "Month_Oct"               "Days_into_Year_prop_max"




for(i in c(2010, 2014:2016, 2019, 2021:2024)[2]) {
    print(i)
    base::load(paste0("C:\\SIDT\\Chilipepper\\CLPR_SWFSC_", i, "_Model_Spectra_Meta_ALL_GOOD_DATA.RData"))
	Table(Model_Spectra_Meta$sample_year)
}


Model_Spectra_Meta_All <- NULL 
for(i in c(2010, 2014:2016, 2018:2019, 2021:2024)[2:10]) {
    print(i)
    base::load(paste0("C:\\SIDT\\Chilipepper\\CLPR_SWFSC_", i, "_Model_Spectra_Meta_ALL_GOOD_DATA.RData"))
	if(is.null(Model_Spectra_Meta$Sex_U))  Model_Spectra_Meta$Sex_U <- 0
    if(is.null(Model_Spectra_Meta$Month_May))  Model_Spectra_Meta$Month_May <- 0
    if(is.null(Model_Spectra_Meta$Month_Jun))  Model_Spectra_Meta$Month_Jun <- 0
    if(is.null(Model_Spectra_Meta$Month_Jul))  Model_Spectra_Meta$Month_Jul <- 0
    if(is.null(Model_Spectra_Meta$Month_Aug))  Model_Spectra_Meta$Month_Aug <- 0
    if(is.null(Model_Spectra_Meta$Month_Sep))  Model_Spectra_Meta$Month_Sep <- 0
    if(is.null(Model_Spectra_Meta$Month_Oct))  Model_Spectra_Meta$Month_Oct <- 0
    if(i == 2018)
      plotly.Spec(Model_Spectra_Meta, N_Samp = min(c(nrow(Model_Spectra_Meta), 50)), colorGroup = 'TMA')  
    Model_Spectra_Meta_All <- rbind(Model_Spectra_Meta_All, Model_Spectra_Meta[, Columns])
}

Model_Spectra_Meta <- Model_Spectra_Meta_All

headTail(Model_Spectra_Meta, 3,3,3,40)
Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)

headTail(Model_Spectra_Meta[!is.na(Model_Spectra_Meta$TMA), ], 3,3,3,40)

save(Model_Spectra_Meta, file = "CLPR_SWFSC_2010_2018_2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")


# For the preliminary model with only 2010 and 2014
Model_Spectra_Meta <- Model_Spectra_Meta[Model_Spectra_Meta$sample_year %in% c(2010, 2014), ]
Table(Model_Spectra_Meta$ sample_year, Model_Spectra_Meta$TMA)
save(Model_Spectra_Meta, file = "CLPR_SWFSC_2010_2014_Model_Spectra_Meta_ALL_GOOD_DATA.RData")



# ---- Add CA comm scans - *** need "Columns" vector from above *** ----

load("CLPR_SWFSC_2010_2018_2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Table(Model_Spectra_Meta$ sample_year, Model_Spectra_Meta$TMA)
Model_Spectra_Meta_SWFSC_2010_2018_2024 <- Model_Spectra_Meta


# -------- Update for sparse TMA results --------
load("C:\\SIDT\\CLPR_Combo_1985__2024\\CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta_SWFSC_1985__2024 <- Model_Spectra_Meta[!Model_Spectra_Meta$sample_year %in% c("2023_OR_Comm", "2024_OR_Comm"), ]
Table(Model_Spectra_Meta_SWFSC_1985__2024$sample_year)
# --------------------------------

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
Table(Model_Spectra_Meta$ sample_year, Model_Spectra_Meta$TMA)

headTail(Model_Spectra_Meta,3,3,3,62)

Model_Spectra_Meta <- rbind(Model_Spectra_Meta_SWFSC_2010_2018_2024, Model_Spectra_Meta[, Columns])  # Columns[c(1, 507:536)]
Table(Model_Spectra_Meta$ sample_year, Model_Spectra_Meta$TMA)


save(Model_Spectra_Meta, file = "CLPR_SWFSC_2010__2024_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData")




# ---- Add OR comm scans - *** need "Columns" vector from above *** ----

# load("C:\\SIDT\\CLPR_Combo_2010__2024\\CLPR_SWFSC_2010__2024_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
load("CLPR_SWFSC_2010__2024_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)
Model_Spectra_Meta_SWFSC_2010__2024 <- Model_Spectra_Meta

# -------- Update for sparse TMA results --------
load("C:\\SIDT\\CLPR_Combo_1985__2024\\CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta_SWFSC_1985__2024 <- Model_Spectra_Meta[!Model_Spectra_Meta$sample_year %in% c("2023_OR_Comm", "2024_OR_Comm"), ]
Table(Model_Spectra_Meta_SWFSC_1985__2024$sample_year)
# --------------------------------

load("CLPR_ORCOMM_2023_2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Table(Model_Spectra_Meta$sample_year)


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
headTail(Model_Spectra_Meta,3,3,3,62)
Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)

                           
Model_Spectra_Meta <- rbind(Model_Spectra_Meta_SWFSC_2010__2024, Model_Spectra_Meta[, Columns])  
Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)

save(Model_Spectra_Meta, file = "CLPR_SWFSC_2010__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData")

# ----------- Update for TMA with 1985+ data ---------------
Model_Spectra_Meta <- rbind( Model_Spectra_Meta_SWFSC_1985__2024, Model_Spectra_Meta[, Columns])  
# Model_Spectra_Meta <- rbind( Model_Spectra_Meta_SWFSC_1985__2024, Model_Spectra_Meta[, names(Model_Spectra_Meta_SWFSC_1985__2024)])  
Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)

save(Model_Spectra_Meta, file = "CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData")

# !!!!!!!!!!!!!!! Find the column names that are missing !!!!!!!!!!!!!
 Columns[!Columns %in% names(Model_Spectra_Meta)]



# ---- Add 1985, 1986 CA comm scans - *** need "Columns" vector from above *** ----

load("CLPR_SWFSC_2010__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)
Model_Spectra_Meta_SWFSC_2010__2024 <- Model_Spectra_Meta


load("C:\\SIDT\\Chilipepper\\CLPR_CACOMM_1985_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta_CLPR_CACOMM_1985 <- Model_Spectra_Meta

load("C:\\SIDT\\Chilipepper\\CLPR_CACOMM_1986_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta <- rbind(Model_Spectra_Meta_CLPR_CACOMM_1985, Model_Spectra_Meta)


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

Model_Spectra_Meta <- Model_Spectra_Meta[!Model_Spectra_Meta$sample_year %in% 'NA_OR_Comm', ]
Table(Model_Spectra_Meta$sample_year)

headTail(Model_Spectra_Meta,3,3,3,62)

Model_Spectra_Meta <- rbind(Model_Spectra_Meta_SWFSC_2010__2024, Model_Spectra_Meta[, Columns])  
Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)


save(Model_Spectra_Meta, file = "CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData")


# ==================================================================================================================

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


# ***** CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData moved to CLPR_Combo_1985__2024 ****
load("C:\\SIDT\\CLPR_Combo_1985__2024\\CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData")


dir.create("Figs_Global", showWarnings = FALSE)


source("C:\\SIDT\\Chilipepper\\plotly.Spec.R")


(ylimGlobal <- c(0, max(Model_Spectra_Meta[, 2:grep("X3952", names(Model_Spectra_Meta))])))
(numColorGlobal <- ifelse(any(is.na(Model_Spectra_Meta$TMA)), length(0:max(Model_Spectra_Meta$TMA, na.rm = TRUE)) + 1, length(0:max(Model_Spectra_Meta$TMA, na.rm = TRUE))))
# (numColorGlobal <- length(0:max(Model_Spectra_Meta$TMA, na.rm = TRUE)))


for( i in sort(unique(Model_Spectra_Meta$sample_year))[4]) {
  
  cat(paste0("\n\n", i, ": "))
  MSM <- Model_Spectra_Meta[Model_Spectra_Meta$sample_year %in% i, ]
  plotly.Spec(MSM, N_Samp = nrow(MSM), colorGroup = 'TMA', numColors = numColorGlobal, ylim = ylimGlobal, main = i, scanUniqueName = 'filenames', Debug = FALSE, 
              paletteFunc = function(n, alpha) hcl.colors(n, "Zissou 1", alpha = alpha, rev = TRUE)) 
  dir.create(paste0("Figs_Global/", i), showWarnings = FALSE)
  saveHtmlFolder(paste0("Figs_Global/", i), view = !interactive())
}


# ========================================= All Year Groups =========================================================================

setwd("C:/SIDT/Chilipepper")

library(JRWToolBox)
library(ggplot2)

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")
# source("C:\\SIDT\\Chilipepper\\plotly.Spec.R")

load("C:\\SIDT\\CLPR_Combo_1985__2024\\CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData")


(ylimGlobal <- c(0, max(Model_Spectra_Meta[, 2:grep("X3952", names(Model_Spectra_Meta))])))

set.seed(c(707, 747)[2])
plotly.Spec(Model_Spectra_Meta, N_Samp = 750, colorGroup = 'sample_year', ylim = ylimGlobal, main = "All Year Groups", scanUniqueName = 'filenames', Debug = TRUE) 

names(Spec)[4] <- "Sample_Year"

#  # https://r-graph-gallery.com/color-palette-finder
#  scale_colour_paletteer_d("vapoRwave::vapoRwave")
#  # scale_fill_paletteer_d("vapoRwave::vapoRwave")
#  
#  # vapoRwave
#  Colors <- colorRampPalette(c("#20DE8BFF", "#CCDE8BFF", "#FFDE8BFF", "#FFA88BFF", "#FF6A8BFF", "#FF6AD5FF", "#C874AAFF", "#C774E7FF", "#AD8CFFFF", "#966BFFFF", "#90CFFFFF"))(16)
#  
#  # GravityFalls
#  # Colors <- colorRampPalette(c("#417BA1FF", "#FF1493FF", "#FFFF2EFF", "#345634FF", "#8B0000FF", "#FF6700FF", "#93C0D5FF", "#8B4513FF", "#9248A7FF", "#1C8859FF", "#474747FF", "#8FBC8FFF", "#D2B48CFF", "#000000FF"))(16)
#  
#  print(ggplotly(ggplot(Spec, aes(x = Waveband, y = Absorbance, z = Scan)) + geom_line(aes(colour = Sample_Year), linewidth = 0.2) + labs(colour = "Sample_Year") + 
#        ylim(ylimGlobal[1], ylimGlobal[2]) + scale_color_manual(values = Colors) + ggtitle("All Year Groups (750 random samples)")))
 

 
# Just stuck with rainbos() - sigh.....
color.palette <- list(function(n) hcl.colors(n, "Zissou 1", rev = FALSE), rainbow, heat.colors, terrain.colors, topo.colors, cm.colors)[[2]]

print(ggplotly(ggplot(Spec, aes(x = Waveband, y = Absorbance, z = Scan)) + geom_line(aes(colour = Sample_Year), linewidth = 0.2) + labs(colour = "Sample_Year") + 
      ylim(ylimGlobal[1], ylimGlobal[2]) + scale_color_manual(values = color.palette(length(unique(Spec$Sample_Year)))) + ggtitle("All Year Groups (750 random samples)")))

saveHtmlFolder(paste0("All Year Groups"), view = !interactive())  





