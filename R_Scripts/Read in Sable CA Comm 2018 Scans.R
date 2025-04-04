

setwd("C:/SIDT/Sable_CA_Comm_2018")
library(JRWToolBox)

  
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

# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/headTail.R") 
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/replaceString.R")     
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Read_OPUS_Spectra.R")

source("C:\\SIDT\\Train_NN_Model\\Read_OPUS_Spectra.R")


# -----------------------------------------------------------------------------

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Import_Species_Metadata_from_NWFSC_Warehouse.R")
# source("C:\\SIDT\\Chilipepper\\Import_Species_Metadata_from_NWFSC_Warehouse.R")

Chilipepper_Combo_Metadata <- Import_Species_Metadata_from_NWFSC_Warehouse("chilipepper")
headTail(Chilipepper_Combo_Metadata)
save(Chilipepper_Combo_Metadata, file = "C:/SIDT/Get Otie Info from Data Warehouse/Chilipepper_Combo_Metadata.RData") # R object name needs to have "Metadata" in it


# --- Sable_CA_Comm_2018 ---

#  !!!!!! Capitalize the first letter of "length_cm", weight_kg", and "sex" in the Excel file !!!!!!
#  !!!!!! Also "project" ** lower case 'p' ** needs to be in the first column, and there needs to be "TMA", "specimen_id", "sample_year", and "structure_weight_g" columns. !!!!!!



Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set = "Sable_CA_Comm_2018", Spectra_Path = "Sable_CA_Comm_2018_Scans", 
                                        htmlPlotFolder = "Figures_Sable_CA_Comm_2018", excelSheet = 3, Debug = TRUE,
                                        shortNameSegments = 6, shortNameSuffix = 'CA_Comm', Static_Figure = "Sable_CA_Comm_2018.png", 
                                        Meta_Path = "C:/SIDT/Sable_CA_Comm_2018/Sable_CA_Comm_2018_Scanning_Session_Report_For_NWC.xlsx")

if(is.null(Spectra_Path)) 
       fileNames.0 <- dir()
else 
       fileNames.0 <- dir(path = Spectra_Path)

 if(length(grep("xlsx", fileNames.0) ) != 0)  { 
       fileNames.0 <- fileNames.0[-grep("xlsx", fileNames.0)] 
       Session_Report_Name <- fileNames.0[grep("xlsx", fileNames.0)] 
 }      
 
Meta_Path <- paste0(Spectra_Path, Session_Report_Name)

excelSheet <- 3
metadata <- openxlsx::read.xlsx(Meta_Path, sheet = excelSheet, detectDates = TRUE) # Load in ancillary data 
names(metadata)[grep('age_best', names(metadata))] <- "TMA"
names(metadata)[grep('sex', names(metadata))] <- "Sex"



for(i in c(2018, 2020:2024)) {
    Model_Spectra_Meta_YR <- Read_OPUS_Spectra(Spectra_Set = paste0("Sable_CA_Comm_", i), 
                                Spectra_Path = paste0("//nwcfile.nmfs.local/FRAM/Assessments/Aging Lab/NIRS Scanning Data/Otoliths/FT_NIRS_Project/PRD_Production/CA_COMM/SABL/", i, "/"),
                                htmlPlotFolder = paste0("Figures_Sable_CA_Comm_", i), Static_Figure = paste0("Sable_CA_Comm_", i, ".png"), Meta_Path = NULL, excelSheet = 3, 
                                shortNameSegments = 6, shortNameSuffix = 'CA_Comm', Debug = TRUE)
    dim(Model_Spectra_Meta_YR)          
    Table(Model_Spectra_Meta_YR$TMA)
    
    assign(paste0("Model_Spectra_Meta_", i), Model_Spectra_Meta_YR)
}    
                 
                 
        
headTail(Model_Spectra_Meta, 3, 3, 3, 55)
Table( Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)


save(Model_Spectra_Meta, file = "C:/SIDT/Sable_CA_Comm_2018/Sable_CA_Comm_2018__2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")



# ----------------------------- PacFIN BDS for Commercial Data ------------------------------------------------

# See C:\SIDT\Sable_CA_Comm_2018\SABL PacFIN MetaData.R

# -----------------------------------------------------------------------------

