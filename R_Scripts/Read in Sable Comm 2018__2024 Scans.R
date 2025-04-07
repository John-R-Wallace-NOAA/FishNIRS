

# ----------------------------- PacFIN BDS for Commercial Data ------------------------------------------------

# Run C:\SIDT\Sable_CA_Comm_2018\SABL PacFIN MetaData.R after importing scans

# -----------------------------------------------------------------------------


setwd("C:/SIDT/Sable_Comm")
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


#  !!!!!! "project" ** lower case 'p' ** needs to be in the first column, and there needs to be "TMA", "specimen_id", "sample_year", and "structure_weight_g" columns. !!!!!!


# =================== CA Comm 2018, 2020 - 2024 ============================================

for(i in c(2018, 2020:2024)) {
    Model_Spectra_Meta_YR <- Read_OPUS_Spectra(Spectra_Set = paste0("Sable_CA_Comm_", i), 
                                Spectra_Path = paste0("//nwcfile.nmfs.local/FRAM/Assessments/Aging Lab/NIRS Scanning Data/Otoliths/FT_NIRS_Project/PRD_Production/CA_COMM/SABL/", i, "/"),
                                htmlPlotFolder = paste0("Figures_Sable_CA_Comm_", i), Static_Figure = paste0("Sable_CA_Comm_", i, ".png"), Meta_Path = NULL, excelSheet = 3, 
                                shortNameSegments = 6, shortNameSuffix = 'CA_Comm', Debug = TRUE)
    dim(Model_Spectra_Meta_YR)          
    Table(Model_Spectra_Meta_YR$TMA)
    
    assign(paste0("Model_Spectra_Meta_", i), Model_Spectra_Meta_YR)
}    
          


# -- Find the column names that are missing --

dim(Model_Spectra_Meta_2018)
dim(Model_Spectra_Meta_2020)
dim(Model_Spectra_Meta_2021)
dim(Model_Spectra_Meta_2022)
dim(Model_Spectra_Meta_2024)

# Use the least common column set
Columns <- names(Model_Spectra_Meta_2023)

Columns[!Columns %in% names(Model_Spectra_Meta_2018)]
Columns[!Columns %in% names(Model_Spectra_Meta_2020)]
Columns[!Columns %in% names(Model_Spectra_Meta_2021)]
Columns[!Columns %in% names(Model_Spectra_Meta_2022)]
Columns[!Columns %in% names(Model_Spectra_Meta_2024)]
                
Model_Spectra_Meta <- rbind(Model_Spectra_Meta_2018[,Columns], Model_Spectra_Meta_2020[, Columns], Model_Spectra_Meta_2021[, Columns], Model_Spectra_Meta_2022[, Columns], Model_Spectra_Meta_2024[, Columns])
        
        
headTail(Model_Spectra_Meta, 3, 3, 3, 55)
Table( Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)


save(Model_Spectra_Meta, file = "C:/SIDT/Sable_Comm/Sable_CA_Comm_2018__2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")



# =================== OR Comm 2018, 2020 - 2024 ============================================

for(i in c("2018", "2020", "2021", "2022", "2023", "2024/NIR0090 Report", "2024/NIR0091 Report", "2024/NIR0092 Report")[2:8]) {  # No otie weight for 2018
    Model_Spectra_Meta_YR <- Read_OPUS_Spectra(Spectra_Set = paste0("Sable_OR_Comm_", i), 
                                Spectra_Path = paste0("//nwcfile.nmfs.local/FRAM/Assessments/Aging Lab/NIRS Scanning Data/Otoliths/FT_NIRS_Project/PRD_Production/OR_COMM/Sable/", i, "/"), # need the last "/"
                                htmlPlotFolder = paste0("Figures_Sable_OR_Comm_", i), Static_Figure = paste0("Sable_OR_Comm_", i, ".png"), Meta_Path = NULL, excelSheet = 3, 
                                shortNameSegments = 6, shortNameSuffix = 'OR_Comm', Debug = TRUE)
    dim(Model_Spectra_Meta_YR)          
    Table(Model_Spectra_Meta_YR$TMA)
    
    if(grepl("2024", i))
       assign(paste0("Model_Spectra_Meta_", paste(get.subs(i, sep = "/")[1], get.subs(get.subs(i, sep = "/")[2], sep = " ")[1], sep = "_")), Model_Spectra_Meta_YR)
    
    if(!grepl("2024", i))
       assign(paste0("Model_Spectra_Meta_", i), Model_Spectra_Meta_YR)
    
}    
                 
 

for(i in "2020") {  
    Model_Spectra_Meta_YR <- Read_OPUS_Spectra(Spectra_Set = paste0("Sable_OR_Comm_", i), 
                                Spectra_Path = "C:/SIDT/Sable_Comm/2020_Scans/",
                                htmlPlotFolder = paste0("Figures_Sable_OR_Comm_", i), Static_Figure = paste0("Sable_OR_Comm_", i, ".png"), Meta_Path = NULL, excelSheet = 3, 
                                shortNameSegments = 6, shortNameSuffix = 'OR_Comm', Debug = TRUE)
    dim(Model_Spectra_Meta_YR)          
    Table(Model_Spectra_Meta_YR$TMA)
  
    assign(paste0("Model_Spectra_Meta_", i), Model_Spectra_Meta_YR)
    
}    
                 
  
 
 

# -- Find the column names that are missing --


# dim(Model_Spectra_Meta_2018)
dim(Model_Spectra_Meta_2020)
dim(Model_Spectra_Meta_2021)
dim(Model_Spectra_Meta_2022)
dim(Model_Spectra_Meta_2023)
dim(Model_Spectra_Meta_2024_NIR0090)
dim(Model_Spectra_Meta_2024_NIR0091)
dim(Model_Spectra_Meta_2024_NIR0092)


Model_Spectra_Meta_2020$Sex_1 <- Model_Spectra_Meta_2020$Sex_2 <- Model_Spectra_Meta_2020$Sex_3 <- Model_Spectra_Meta_2020$Sex_9 <- Model_Spectra_Meta_2020$Sex_U <- NULL
Model_Spectra_Meta_2021$Sex_1 <- Model_Spectra_Meta_2021$Sex_2 <- Model_Spectra_Meta_2021$Sex_3 <- Model_Spectra_Meta_2021$Sex_9 <- Model_Spectra_Meta_2021$Sex_U <- NULL
Model_Spectra_Meta_2022$Sex_1 <- Model_Spectra_Meta_2022$Sex_2 <- Model_Spectra_Meta_2022$Sex_3 <- Model_Spectra_Meta_2022$Sex_9 <- Model_Spectra_Meta_2022$Sex_U <- NULL
Model_Spectra_Meta_2023$Sex_1 <- Model_Spectra_Meta_2023$Sex_2 <- Model_Spectra_Meta_2023$Sex_3 <- Model_Spectra_Meta_2023$Sex_9 <- Model_Spectra_Meta_2023$Sex_U <- NULL
Model_Spectra_Meta_2024_NIR0090$Sex_1 <- Model_Spectra_Meta_2024_NIR0090$Sex_2 <- Model_Spectra_Meta_2024_NIR0090$Sex_3 <- Model_Spectra_Meta_2024_NIR0090$Sex_9 <- Model_Spectra_Meta_2024_NIR0090$Sex_U <- NULL
Model_Spectra_Meta_2024_NIR0091$Sex_1 <- Model_Spectra_Meta_2024_NIR0091$Sex_2 <- Model_Spectra_Meta_2024_NIR0091$Sex_3 <- Model_Spectra_Meta_2024_NIR0091$Sex_9 <- Model_Spectra_Meta_2024_NIR0091$Sex_U <- NULL
Model_Spectra_Meta_2024_NIR0092$Sex_1 <- Model_Spectra_Meta_2024_NIR0092$Sex_2 <- Model_Spectra_Meta_2024_NIR0092$Sex_3 <- Model_Spectra_Meta_2024_NIR0092$Sex_9 <- Model_Spectra_Meta_2024_NIR0092$Sex_U <- NULL


Model_Spectra_Meta_2021$NWFSC_NIR_Scan_Session <- NA
Model_Spectra_Meta_2021$NWFSC_NIR_Filename <- NA


# Use the least common column set
Columns <- names(Model_Spectra_Meta_2023)

# Columns[!Columns %in% names(Model_Spectra_Meta_2018)]
Columns[!Columns %in% names(Model_Spectra_Meta_2020)]
Columns[!Columns %in% names(Model_Spectra_Meta_2021)]
Columns[!Columns %in% names(Model_Spectra_Meta_2022)]
Columns[!Columns %in% names(Model_Spectra_Meta_2024_NIR0090)]
Columns[!Columns %in% names(Model_Spectra_Meta_2024_NIR0091)]        
Columns[!Columns %in% names(Model_Spectra_Meta_2024_NIR0092)]

        
Model_Spectra_Meta <- rbind(Model_Spectra_Meta_2020[,Columns], Model_Spectra_Meta_2021[, Columns], Model_Spectra_Meta_2022[, Columns], Model_Spectra_Meta_2023[, Columns], Model_Spectra_Meta_2024_NIR0090[, Columns],
             Model_Spectra_Meta_2024_NIR0091[, Columns], Model_Spectra_Meta_2024_NIR0092[, Columns])
        
        
headTail(Model_Spectra_Meta, 3, 3, 3, 55)
Table( Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)

save(Model_Spectra_Meta, file = "C:/SIDT/Sable_Comm/Sable_OR_Comm_2020__2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")



# =================== WA Comm 2020-2023 ============================================

for(i in (2020:2023)[2:4]) {  
    Model_Spectra_Meta_YR <- Read_OPUS_Spectra(Spectra_Set = paste0("Sable_WA_Comm_", i), fileNames_Sort_Seqment = NULL,
                                Spectra_Path = paste0("//nwcfile.nmfs.local/FRAM/Assessments/Aging Lab/NIRS Scanning Data/Otoliths/FT_NIRS_Project/PRD_Production/WA_COMM/SABL_Sablefish/", i, "/"),
                                htmlPlotFolder = paste0("Figures_Sable_WA_Comm_", i), Static_Figure = paste0("Sable_WA_Comm_", i, ".png"), Meta_Path = NULL, excelSheet = 3, 
                                shortNameSegments = 6, shortNameSuffix = 'WA_Comm', Debug = TRUE)
    dim(Model_Spectra_Meta_YR)          
    Table(Model_Spectra_Meta_YR$TMA)
    
    assign(paste0("Model_Spectra_Meta_", i), Model_Spectra_Meta_YR)
}    


# Bad waveband splits in 2023

# First scan file: SABL_WACOMM2023_NIR0066A_PRD_1_WA23001-SABL-1_O1.0  Tabulation of differences not all 8's
# -8.20051608310132  -8.2005160830995  -8.2005160830613  -8.2005160830513 
#                23                65                 1  



# -- Find the column names that are missing --


dim(Model_Spectra_Meta_2020)
dim(Model_Spectra_Meta_2021)
dim(Model_Spectra_Meta_2022)
dim(Model_Spectra_Meta_2023)

Table(Model_Spectra_Meta_2021$Sex)
Table(Model_Spectra_Meta_2021$Length_cm)

# Use the least common column set

Model_Spectra_Meta_2021$gear_type <- Model_Spectra_Meta_2021$state_sample_number <- Model_Spectra_Meta_2021$sample_type <- Model_Spectra_Meta_2021$catch_date <- NULL

Columns <- names(Model_Spectra_Meta_2021)

Columns[!Columns %in% names(Model_Spectra_Meta_2020)]
Columns[!Columns %in% names(Model_Spectra_Meta_2022)]
Columns[!Columns %in% names(Model_Spectra_Meta_2023)]
                
                
Model_Spectra_Meta <- rbind(Model_Spectra_Meta_2020[, Columns], Model_Spectra_Meta_2021[, Columns], Model_Spectra_Meta_2022[, Columns], Model_Spectra_Meta_2023[, Columns])
        
        
headTail(Model_Spectra_Meta, 3, 3, 3, 55)
Table( Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)
Table( Model_Spectra_Meta$sample_year, Model_Spectra_Meta$Sex)
Table( Model_Spectra_Meta$sample_year, round(Model_Spectra_Meta$Length_cm))

Model_Spectra_Meta$Sex <- recode.simple(Model_Spectra_Meta$Sex, cbind(c(1, 2, 3, 9, NA), c("M", "F", "U", "U", "U"))) 


save(Model_Spectra_Meta, file = "C:/SIDT/Sable_Comm/Sable_WA_Comm_2020__2023_Model_Spectra_Meta_ALL_GOOD_DATA.RData")







