

# ===============  Import the PacFIN Sablefish metadata =============================

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

# Toolbox functions 
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/PacFIN.Data.Extraction/master/R/PacFIN.BDS.Extraction.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/GitHub_File_Download.R")
GitHub_File_Download("John-R-Wallace-NOAA/PacFIN.Data.Extraction/master/R/PacFIN.BDS.Extraction.R")


PacFIN.Login <- UID <- "wallacej"  
PacFIN.PW <- PWD <- "*********"
DSN <- "PacFIN"
import.sql("Select * from pacfin.bds_sp where rownum < 11", dsn="PacFIN", uid="wallacej", pwd=PacFIN.PW) # Test


PacFIN_SABL_Age <- PacFIN.BDS.Extraction("'SABL'")
save(PacFIN_SABL_Age, file = "C:\\SIDT\\SABL_Comm_2012_2023\\PacFIN_SABL_Age.RData")
base::load("C:\\SIDT\\SABL_Comm_2012_2023\\PacFIN_SABL_Age.RData")
headTail(PacFIN_SABL_Age,2,2,2,70)


#  !!!!!! Capitalize the first letter of "length_cm", weight_kg", and "sex" in the Excel file !!!!!!



# PacFIN_SABL_Age$Length_cm <- PacFIN_SABL_Age$FISH_LENGTH/10
# PacFIN_SABL_Age$Length_cm[PacFIN_SABL_Age$Length_cm < 10 & !is.na(PacFIN_SABL_Age$Length_cm)] <- PacFIN_SABL_Age$Length_cm[PacFIN_SABL_Age$Length_cm < 10 & !is.na(PacFIN_SABL_Age$Length_cm)] * 10

PacFIN_SABL_Age$Length_cm <- PacFIN_SABL_Age$FISH_LENGTH
PacFIN_SABL_Age$Length_cm[PacFIN_SABL_Age$FISH_LENGTH_UNITS == 'MM'] <- PacFIN_SABL_Age$Length_cm[PacFIN_SABL_Age$FISH_LENGTH_UNITS == 'MM']/10
range(PacFIN_SABL_Age$Length_cm, na.rm = TRUE)
plot(PacFIN_SABL_Age$Length_cm)

# PacFIN_SABL_Age$Length_cm[PacFIN_SABL_Age$Length_cm > 100 & !is.na(PacFIN_SABL_Age$Length_cm)] <- PacFIN_SABL_Age$Length_cm[PacFIN_SABL_Age$Length_cm > 100 & !is.na(PacFIN_SABL_Age$Length_cm)] / 10
# range(PacFIN_SABL_Age$Length_cm, na.rm = TRUE)
# plot(PacFIN_SABL_Age$Length_cm)



range(PacFIN_SABL_Age$FISH_WEIGHT, na.rm = TRUE)

Table(PacFIN_SABL_Age$FISH_WEIGHT_UNITS)

          G     P 
45042 10679     1 


PacFIN_SABL_Age$Weight_kg <- PacFIN_SABL_Age$FISH_WEIGHT
PacFIN_SABL_Age$Weight_kg[PacFIN_SABL_Age$FISH_WEIGHT_UNITS == 'G'] <- PacFIN_SABL_Age$Weight_kg[PacFIN_SABL_Age$FISH_WEIGHT_UNITS == 'G']/1000
range(PacFIN_SABL_Age$Weight_kg, na.rm = TRUE)


change(PacFIN_SABL_Age) 
browsePlot('plot(Weight_kg, Length_cm)')


browsePlot('
  par(mfrow = c(2,1))
  plot(AGE_IN_YEARS, Length_cm)
  plot(AGE_IN_YEARS, Weight_kg)
')


PacFIN_SABL_Age$Sex <- PacFIN_SABL_Age$SEX
PacFIN_SABL_Age$Year <- PacFIN_SABL_Age$SAMPLE_YEAR
PacFIN_SABL_Age$Depth <- PacFIN_SABL_Age$DEPTH_AVERAGE_FATHOMS
PacFIN_SABL_Age$Month <- PacFIN_SABL_Age$SAMPLE_MONTH

save(PacFIN_SABL_Age, file = 'PacFIN_SABL_Age.RData')



# ==================================== Match the PacFIN Metadata onto the combined  WA, OR, CA  commercial Model_Spectra_Meta ========================================================================================

setwd("C:/SIDT/Sable_Comm")
library(JRWToolBox)
 

load("C:\\SIDT\\Sable_Comm\\PacFIN_SABL_Age.RData")

 
PacFIN_SABL_Age.NN <- PacFIN_SABL_Age[, c("BDS_ID", "SAMPLE_ID", "SAMPLE_NUMBER", "CLUSTER_SEQUENCE_NUMBER", "AGENCY_CODE", "PACFIN_SPECIES_CODE", "FISH_SEQUENCE_NUMBER", "FINAL_FISH_AGE_IN_YEARS", "Year", "Month", "SAMPLE_DAY", "Length_cm", "Weight_kg", "Sex", "Depth")]

# !!!!! Find the column names that are missing !!!!!
Columns <- c("BDS_ID", "SAMPLE_ID", "SAMPLE_NUMBER", "CLUSTER_SEQUENCE_NUMBER", "AGENCY_CODE", "PACFIN_SPECIES_CODE", "FISH_SEQUENCE_NUMBER", "FINAL_FISH_AGE_IN_YEARS", "Year", "Month", "SAMPLE_DAY", "Length_cm", "Weight_kg", "Sex", "Depth")
Columns[!Columns %in% names(PacFIN_SABL_Age)]




# --- Load Model_Spectra_Meta data ---
load("C:\\SIDT\\Sable_Comm\\Sable_CA_Comm_2018__2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta_CA <- Model_Spectra_Meta

load("C:\\SIDT\\Sable_Comm\\Sable_OR_Comm_2020__2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta_OR <- Model_Spectra_Meta

load("C:\\SIDT\\Sable_Comm\\Sable_WA_Comm_2020__2023_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta_WA <- Model_Spectra_Meta

Table(Model_Spectra_Meta_CA$Sex)
Table(Model_Spectra_Meta_OR$Sex)
Table(Model_Spectra_Meta_WA$Sex)

Model_Spectra_Meta_CA$Sex <- NA

dim(Model_Spectra_Meta_CA)
dim(Model_Spectra_Meta_OR)
dim(Model_Spectra_Meta_WA)


# Use the least common column set
Columns <- names(Model_Spectra_Meta_CA)

Columns[!Columns %in% names(Model_Spectra_Meta_OR)]
Columns[!Columns %in% names(Model_Spectra_Meta_WA)]

              
Model_Spectra_Meta <- rbind(Model_Spectra_Meta_CA[, Columns], Model_Spectra_Meta_OR[, Columns], Model_Spectra_Meta_WA[, Columns])
        
Model_Spectra_Meta$Sex <- recode.simple(Model_Spectra_Meta$Sex, cbind(c(1, 2, 3, 9, NA), c("M", "F", "U", "U", "U"))) 

Table(Model_Spectra_Meta$Sex, Model_Spectra_Meta$sample_year)       

# ---------------------------------------------------------------------------------


Table(!is.na(Model_Spectra_Meta$structure_weight_dg))
 TRUE 
11034

# -- Sex, One hot encoding --
if(!is.null(Model_Spectra_Meta$Sex)) {
    Model_Spectra_Meta$Sex <- as.character(Model_Spectra_Meta$Sex)
    Model_Spectra_Meta$Sex[is.na(Model_Spectra_Meta$Sex)] <- "U"
    Sex_ <- Model_Spectra_Meta$Sex
    Model_Spectra_Meta <- cbind(Model_Spectra_Meta, as.data.frame(model.matrix(formula(~ -1 + Sex_))))  # Three indicator columns added: Sex_F, Sex_M, Sex_U
    if(is.null(Model_Spectra_Meta$Sex_U))  Model_Spectra_Meta$Sex_U <- 0
}   

headTail(Model_Spectra_Meta, 2, 2, 2, 70)

save(Model_Spectra_Meta, file = 'SABL_Comm_2018__2024_Model_Spectra_Meta_ALL_GOOD_DATA_SR_Sex.RData')

 #--------------------------------------------------------------------
 
Model_Spectra_Meta$Sex_SR <- Model_Spectra_Meta$Sex


headTail(Model_Spectra_Meta, 2, 2, 2, 70)
Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)
sum(table(Model_Spectra_Meta$TMA))
100 * sum(table(Model_Spectra_Meta$TMA))/nrow(Model_Spectra_Meta)


Model_Spectra_Meta$age_structure_id[1:5]
[1] "CA1870028-SABL-1-1-O"  "CA1870232-SABL-1-1-O"  "CA1870028-SABL-1-10-O" "CA1870232-SABL-2-1-O"  "CA1870028-SABL-2-1-O" 


tail(Model_Spectra_Meta$age_structure_id)
[1] "WA23001-SABL-98-O"  "WA23078-SABL-18-O"  "WA23001-SABL-99-O"  "WA23078-SABL-19-O"  "WA23001-SABL-100-O" "WA23078-SABL-20-O" 


Model_Spectra_Meta$Agency_ID <- substr(Model_Spectra_Meta$age_structure_id,1,2)
Model_Spectra_Meta$Agency_ID[Model_Spectra_Meta$Agency_ID %in% '10'] <- "WA"
Table(Model_Spectra_Meta$Agency_ID)



# !!!!!!!!!!!!!!!!!!! At some point in 2021, Patrick's 'age_structure_id' switched from a 2 digit year to a 4 digit year !!!!!!!!!!!!!!!

# PacFIN_SABL_Age.NN$SAMPLE_NUMBER[1:5]
# PacFIN_SABL_Age.NN$AGENCY_ID <- recode.simple(PacFIN_SABL_Age.NN$AGENCY_CODE, cbind(c('W', 'O', 'C'), c('WA', 'OR', 'CA')))
# Table(PacFIN_SABL_Age.NN$AGENCY_ID, PacFIN_SABL_Age.NN$AGENCY_CODE)

# --- CA ---

PacFIN_SABL_Age.NN$age_structure_id_CA_1 <- paste0('CA', substr(PacFIN_SABL_Age.NN$SAMPLE_NUMBER, 3, 4), substring(PacFIN_SABL_Age.NN$SAMPLE_NUMBER, 9), "-", PacFIN_SABL_Age.NN$PACFIN_SPECIES_CODE, "-" , PacFIN_SABL_Age.NN$CLUSTER_SEQUENCE_NUMBER, "-", PacFIN_SABL_Age.NN$FISH_SEQUENCE_NUMBER, "-O")
PacFIN_SABL_Age.NN$age_structure_id[1:5]
# [1] "CA67010001-SABL-1-1-O" "CA67010001-SABL-1-2-O" "CA67010001-SABL-1-3-O" "CA67010001-SABL-1-4-O" "CA67010001-SABL-1-5-O"

Model_Spectra_Meta.xx <- Model_Spectra_Meta
Model_Spectra_Meta.xx$Length_cm <- Model_Spectra_Meta.xx$Weight_kg <- Model_Spectra_Meta.xx$Sex <- Model_Spectra_Meta.xx$Depth <- NULL
Model_Spectra_Meta_CA_1 <- match.f(Model_Spectra_Meta.xx, PacFIN_SABL_Age.NN, 'age_structure_id', 'age_structure_id_CA_1', c("Month", "Length_cm", "Weight_kg", "Sex", "Depth"))
Table(!is.na(Model_Spectra_Meta_CA_1$Length_cm), Model_Spectra_Meta_CA_1$sample_year, Model_Spectra_Meta_CA_1$Agency_ID)


PacFIN_SABL_Age.NN$age_structure_id_CA_2 <- paste0('CA', substr(PacFIN_SABL_Age.NN$SAMPLE_NUMBER, 1, 4), substring(PacFIN_SABL_Age.NN$SAMPLE_NUMBER, 9), "-", PacFIN_SABL_Age.NN$PACFIN_SPECIES_CODE, "-" , PacFIN_SABL_Age.NN$CLUSTER_SEQUENCE_NUMBER, "-", PacFIN_SABL_Age.NN$FISH_SEQUENCE_NUMBER, "-O")
tail(PacFIN_SABL_Age.NN$age_structure_id_2)
# [1] "CA2025010010-SABL-1-18-O" "CA2025010010-SABL-1-19-O" "CA2025010010-SABL-1-20-O" "CA2025010010-SABL-1-21-O" "CA2025010010-SABL-1-22-O" "CA2025010010-SABL-1-23-O"

Model_Spectra_Meta.xx <- Model_Spectra_Meta
Model_Spectra_Meta.xx$Length_cm <- Model_Spectra_Meta.xx$Weight_kg <- Model_Spectra_Meta.xx$Sex <- Model_Spectra_Meta.xx$Depth <- NULL
Model_Spectra_Meta_CA_2 <- match.f(Model_Spectra_Meta.xx, PacFIN_SABL_Age.NN, 'age_structure_id', 'age_structure_id_CA_2', c("Month", "Length_cm", "Weight_kg", "Sex", "Depth"))
Table(!is.na(Model_Spectra_Meta_CA_2$Length_cm), Model_Spectra_Meta_CA_2$sample_year, Model_Spectra_Meta_CA_2$Agency_ID)




# --- WA ---

Model_Spectra_Meta$age_structure_id[Model_Spectra_Meta$Agency_ID %in% 'WA'][1:5]
[1] "100397679-SABL-O" "100397685-SABL-O" "100397681-SABL-O" "100397686-SABL-O" "100397697-SABL-O"

tail(Model_Spectra_Meta$age_structure_id[Model_Spectra_Meta$Agency_ID %in% 'WA'], 50)
[1]  "WA23001-SABL-98-O"  "WA23078-SABL-18-O"  "WA23001-SABL-99-O"  "WA23078-SABL-19-O"  "WA23001-SABL-100-O" "WA23078-SABL-20-O" 


PacFIN_SABL_Age.NN$age_structure_id_WA_1 <- paste0(substr(PacFIN_SABL_Age.NN$SAMPLE_NUMBER, 10, 14), substr(PacFIN_SABL_Age.NN$SAMPLE_NUMBER, 2, 5), "-", PacFIN_SABL_Age.NN$PACFIN_SPECIES_CODE, "-O")
# PacFIN_SABL_Age.NN$age_structure_id_WA_1 <- paste0(substr(PacFIN_SABL_Age.NN$SAMPLE_NUMBER, 3, 4), substring(PacFIN_SABL_Age.NN$SAMPLE_NUMBER, 9), "-O")
PacFIN_SABL_Age.NN$age_structure_id_WA_1[PacFIN_SABL_Age.NN$AGENCY_CODE %in% 'W'][1:5]
tail(PacFIN_SABL_Age.NN$age_structure_id_WA_1[PacFIN_SABL_Age.NN$AGENCY_CODE %in% 'W'])


Model_Spectra_Meta.xx <- Model_Spectra_Meta
Model_Spectra_Meta.xx$Length_cm <- Model_Spectra_Meta.xx$Weight_kg <- Model_Spectra_Meta.xx$Sex <- Model_Spectra_Meta.xx$Depth <- NULL
Model_Spectra_Meta_WA_1 <- match.f(Model_Spectra_Meta.xx, PacFIN_SABL_Age.NN, 'age_structure_id', 'age_structure_id_WA_1', c("Month", "Length_cm", "Weight_kg", "Sex", "Depth"))
Table(!is.na(Model_Spectra_Meta_WA_1$Length_cm), Model_Spectra_Meta_WA_1$sample_year, Model_Spectra_Meta_WA_1$Agency_ID)




PacFIN_SABL_Age.NN$age_structure_id_WA_2 <- paste0('WA', substr(PacFIN_SABL_Age.NN$SAMPLE_NUMBER, 8, 12), "-",  PacFIN_SABL_Age.NN$PACFIN_SPECIES_CODE, "-" , PacFIN_SABL_Age.NN$FISH_SEQUENCE_NUMBER, "-O")
PacFIN_SABL_Age.NN$age_structure_id_WA_2[PacFIN_SABL_Age.NN$AGENCY_CODE %in% 'W'][1:5]
# [1] "WA20674-SABL-1-O" "WA20674-SABL-2-O" "WA20674-SABL-3-O" "WA20674-SABL-4-O" "WA20674-SABL-5-O"

tail(PacFIN_SABL_Age.NN$age_structure_id_WA_2[PacFIN_SABL_Age.NN$AGENCY_CODE %in% 'W'])
# [1] "WA20254-SABL-18-O" "WA20254-SABL-19-O" "WA20254-SABL-20-O" "WA20254-SABL-21-O" "WA20254-SABL-22-O" "WA20254-SABL-23-O"


Model_Spectra_Meta.xx <- Model_Spectra_Meta
Model_Spectra_Meta.xx$Length_cm <- Model_Spectra_Meta.xx$Weight_kg <- Model_Spectra_Meta.xx$Sex <- Model_Spectra_Meta.xx$Depth <- NULL
Model_Spectra_Meta_WA_2 <- match.f(Model_Spectra_Meta.xx, PacFIN_SABL_Age.NN, 'age_structure_id', 'age_structure_id_WA_2', c("Month", "Length_cm", "Weight_kg", "Sex", "Depth"))
Table(!is.na(Model_Spectra_Meta_WA_2$Length_cm), Model_Spectra_Meta_WA_2$sample_year, Model_Spectra_Meta_WA_2$Agency_ID)
Table(!is.na(Model_Spectra_Meta_WA_2$TMA), Model_Spectra_Meta_WA_2$sample_year, Model_Spectra_Meta_WA_2$Agency_ID)

Table(!is.na(Model_Spectra_Meta_WA_2$TMA), round(Model_Spectra_Meta_WA_2$Length_cm), Model_Spectra_Meta_WA_2$Agency_ID)



# --- OR ---

Model_Spectra_Meta$age_structure_id[Model_Spectra_Meta$Agency_ID %in% 'OR'][1:5]
[1] "OR2076798-SABL-20-O" "OR2076637-SABL-1-O"  "OR2076637-SABL-2-O"  "OR2076637-SABL-3-O"  "OR2076637-SABL-4-O" 

tail(Model_Spectra_Meta$age_structure_id[Model_Spectra_Meta$Agency_ID %in% 'OR'])
[1] "OR2494375-SABL-19-O" "OR2495171-SABL-5-O"  "OR2494960-SABL-1-O"  "OR2496595-SABL-27-O" "OR2494960-SABL-2-O"  "OR2495171-SABL-9-O" 

Model_Spectra_Meta$age_structure_id[Model_Spectra_Meta$Agency_ID %in% 'OR' & Model_Spectra_Meta$sample_year %in% 2018][1:5]


PacFIN_SABL_Age.NN$age_structure_id_OR_1 <- paste0(substr(PacFIN_SABL_Age.NN$SAMPLE_NUMBER, 1, 9), "-",  PacFIN_SABL_Age.NN$PACFIN_SPECIES_CODE, "-" , PacFIN_SABL_Age.NN$FISH_SEQUENCE_NUMBER, "-O")
PacFIN_SABL_Age.NN$age_structure_id_OR_1[PacFIN_SABL_Age.NN$AGENCY_CODE %in% 'O'][1:5]
tail(PacFIN_SABL_Age.NN$age_structure_id_OR_1[PacFIN_SABL_Age.NN$AGENCY_CODE %in% 'O'])


Model_Spectra_Meta.xx <- Model_Spectra_Meta
Model_Spectra_Meta.xx$Length_cm <- Model_Spectra_Meta.xx$Weight_kg <- Model_Spectra_Meta.xx$Sex <- Model_Spectra_Meta.xx$Depth <- NULL
Model_Spectra_Meta_OR_1 <- match.f(Model_Spectra_Meta.xx, PacFIN_SABL_Age.NN, 'age_structure_id', 'age_structure_id_OR_1', c("Month", "Length_cm", "Weight_kg", "Sex", "Depth"))
Table(!is.na(Model_Spectra_Meta_OR_1$Length_cm), Model_Spectra_Meta_OR_1$sample_year, Model_Spectra_Meta_OR_1$Agency_ID)



# ===================================================

headTail(PacFIN_SABL_Age.NN)


Model_Spectra_Meta.xx <- Model_Spectra_Meta
Model_Spectra_Meta.xx$Length_cm <- Model_Spectra_Meta.xx$Weight_kg <- Model_Spectra_Meta.xx$Sex <- Model_Spectra_Meta.xx$Depth <- NULL

Model_Spectra_Meta_CA_1 <- match.f(Model_Spectra_Meta.xx, PacFIN_SABL_Age.NN, 'age_structure_id', 'age_structure_id_CA_1', c("Month", "Length_cm", "Weight_kg", "Sex", "Depth"))
Model_Spectra_Meta_CA_2 <- match.f(Model_Spectra_Meta.xx, PacFIN_SABL_Age.NN, 'age_structure_id', 'age_structure_id_CA_2', c("Month", "Length_cm", "Weight_kg", "Sex", "Depth"))
Model_Spectra_Meta_OR_1 <- match.f(Model_Spectra_Meta.xx, PacFIN_SABL_Age.NN, 'age_structure_id', 'age_structure_id_OR_1', c("Month", "Length_cm", "Weight_kg", "Sex", "Depth"))
Model_Spectra_Meta_WA_2 <- match.f(Model_Spectra_Meta.xx, PacFIN_SABL_Age.NN, 'age_structure_id', 'age_structure_id_WA_2', c("Month", "Length_cm", "Weight_kg", "Sex", "Depth"))


Model_Spectra_Meta <- rbind(Model_Spectra_Meta_CA_1[!is.na(Model_Spectra_Meta_CA_1$Length_cm), ], Model_Spectra_Meta_CA_2[!is.na(Model_Spectra_Meta_CA_2$Length_cm), ],
                            Model_Spectra_Meta_OR_1[!is.na(Model_Spectra_Meta_OR_1$Length_cm), ], Model_Spectra_Meta_WA_2[!is.na(Model_Spectra_Meta_WA_2$Length_cm), ])

Table(round(Model_Spectra_Meta$Length_cm))

headTail(Model_Spectra_Meta, 2, 2, 2, 70)


Table(Model_Spectra_Meta$sample_year, round(Model_Spectra_Meta$Length_cm))

Table(Model_Spectra_Meta$TMA, Model_Spectra_Meta$sample_year)
sum(table(Model_Spectra_Meta$TMA))
1647
100 * sum(table(Model_Spectra_Meta$TMA))/nrow(Model_Spectra_Meta)
18.6% (= 1647/8858)

Table(Model_Spectra_Meta$Sex, Model_Spectra_Meta$sample_year)
      
    2018 2020 2021 2022 2023 2024
  F  155  782  315 2848  748  965
  M   40  469  172 1093  408  336
  U    2    0   41  290  194    0


Table(Model_Spectra_Meta$Sex_SR, Model_Spectra_Meta$sample_year)

    2018 2020 2021 2022 2023 2024
  F    0  760  277 2693  940  438
  M    0  467  164 1069  298  163
  U  197   24   87  469  112  700

Table(Model_Spectra_Meta$Sex, Model_Spectra_Meta$Sex_SR)

       F    M    U
  F 4686   21 1106
  M  139 2056  323
  U  283   84  160



Table(!is.na(Model_Spectra_Meta$Month))
TRUE 
8858

Table(!is.na(Model_Spectra_Meta$Length_cm))
TRUE 
8858

Table(!is.na(Model_Spectra_Meta$Weight_kg))
FALSE  TRUE 
 2104  6754 

Table(!is.na(Model_Spectra_Meta$Sex))
TRUE 
8859

Table(!is.na(Model_Spectra_Meta$Depth))
FALSE  TRUE 
 8167   691 


Table(!is.na(Model_Spectra_Meta$structure_weight_dg))
TRUE 
8858

# Model_Spectra_Meta <- Model_Spectra_Meta[!is.na(Model_Spectra_Meta$structure_weight_dg), ]
# headTail(Model_Spectra_Meta, 2, 2, 2, 70)




# if(!is.null(Model_Spectra_Meta$Length_cm))        
#    Model_Spectra_Meta$Length_prop_max <- Model_Spectra_Meta$Length_cm/max(Model_Spectra_Meta$Length_cm, na.rm = TRUE)
   
# if(!is.null(Model_Spectra_Meta$Weight_kg))
#    Model_Spectra_Meta$Weight_prop_max <- (Model_Spectra_Meta$Weight_kg - min(Model_Spectra_Meta$Weight_kg, na.rm = TRUE))/(max(Model_Spectra_Meta$Weight_kg, na.rm = TRUE) - min(Model_Spectra_Meta$Weight_kg, na.rm = TRUE))
    
# if(!is.null(Model_Spectra_Meta$Sex))
#    Model_Spectra_Meta$Sex_prop_max <- as.numeric(recode.simple(Model_Spectra_Meta$Sex, data.frame(c('F','M', 'U'), 0:2)))/2  # ** All variables have to be numeric ** 
   
# if(!is.null(Model_Spectra_Meta$Depth_m))
#    Model_Spectra_Meta$Depth_prop_max <- (Model_Spectra_Meta$Depth_m - min(Model_Spectra_Meta$Depth_m, na.rm = TRUE))/(max(Model_Spectra_Meta$Depth_m, na.rm = TRUE) - min(Model_Spectra_Meta$Depth_m, na.rm = TRUE))
   
# if(!is.null(Model_Spectra_Meta$Latitude_dd))
#  Model_Spectra_Meta$Latitude_prop_max <- (Model_Spectra_Meta$Latitude_dd - 30.5)/(49.1 - 30.5)


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
 

headTail(Model_Spectra_Meta, 2, 2, 2, 70)

browsePlot('plot(Model_Spectra_Meta$TMA, Model_Spectra_Meta$structure_weight_dg)')
browsePlot('plot(Model_Spectra_Meta$TMA, Model_Spectra_Meta$Length_cm)')
browsePlot('print(lattice::xyplot(Length_cm ~ TMA, groups = Sex, data = Model_Spectra_Meta))')


save(Model_Spectra_Meta, file = 'SABL_Comm_2018__2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData')



load("C:\\SIDT\\Sable_Comm\\SABL_Comm_2018__2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
headTail(Model_Spectra_Meta,2,2,2,55)

Model_Spectra_Meta_PF <- Model_Spectra_Meta
Model_Spectra_Meta_PF$Sex_PF <- Model_Spectra_Meta_PF$Sex
Table(Model_Spectra_Meta_PF$Sex_PF)

load("C:\\SIDT\\Sable_Comm\\SABL_Comm_2018__2024_Model_Spectra_Meta_ALL_GOOD_DATA_SR_Sex.RData")
headTail(Model_Spectra_Meta,2,2,2,55)

Model_Spectra_Meta <- match.f(Model_Spectra_Meta, Model_Spectra_Meta_PF, 'filenames', 'filenames', 'Sex_PF')
change(Model_Spectra_Meta)
Table(Sex, Sex_PF)
Table(Sex)
Table(Sex_PF)

Model_Spectra_Meta$Sex[Model_Spectra_Meta$Sex %in% 'U'] <- Model_Spectra_Meta$Sex_PF[Model_Spectra_Meta$Sex %in% 'U']
Table(Model_Spectra_Meta$Sex)
   F    M    U <NA> 
7593 2873  160  408 


Model_Spectra_Meta$Sex[is.na(Model_Spectra_Meta$Sex)] <- 'U'
   F    M    U 
7593 2873  568 

Model_Spectra_Meta$Sex_PF <- NULL


Model_Spectra_Meta$Sex_F <- Model_Spectra_Meta$Sex_M <- Model_Spectra_Meta$Sex_U <- NULL
Model_Spectra_Meta$Sex_F.1 <- Model_Spectra_Meta$Sex_M.1 <- Model_Spectra_Meta$Sex_U.1 <- NULL

# -- Sex, One hot encoding --
if(!is.null(Model_Spectra_Meta$Sex)) {
    Model_Spectra_Meta$Sex <- as.character(Model_Spectra_Meta$Sex)
    Model_Spectra_Meta$Sex[is.na(Model_Spectra_Meta$Sex)] <- "U"
    Sex_ <- Model_Spectra_Meta$Sex
    Model_Spectra_Meta <- cbind(Model_Spectra_Meta, as.data.frame(model.matrix(formula(~ -1 + Sex_))))  # Three indicator columns added: Sex_F, Sex_M, Sex_U
    if(is.null(Model_Spectra_Meta$Sex_U))  Model_Spectra_Meta$Sex_U <- 0
}   

headTail(Model_Spectra_Meta,2,2,2,55)
Table(Model_Spectra_Meta$Sex)


save(Model_Spectra_Meta,  file = "C:\\SIDT\\Sable_Comm\\SABL_Comm_2018__2024_Model_Spectra_Meta_ALL_GOOD_DATA_SR_Sex.RData")

# ------------------------------


load("C:\\SIDT\\Sable_Comm\\SABL_Comm_2018__2024_Model_Spectra_Meta_ALL_GOOD_DATA_SR_Sex.RData")

sum(table(Model_Spectra_Meta$TMA))
2600



Table(Model_Spectra_Meta$sample_year, !is.na(Model_Spectra_Meta$TMA))
     FALSE TRUE
  2018    96  101
  2020     0 1773
  2021   450  390
  2022  4535  336
  2023  2052    0
  2024  1301    0


Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)


      
          0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24   25   26   27   28   30   31   32   33   34   37   38   40   41   43   47   48   52   53   54 <NA>
  2018    0    0    4    7   15   34    4    3   14    5    0    2    1    0    1    0    2    1    1    0    0    0    2    0    1    0    0    2    1    0    0    1    0    0    0    0    0    0    0    0    0    0    0    0   96
  2020    0    5   45   67  884   99  121  288   50   33   34   34   33   10    9    5    6    1    5    5    8    5    2    2    1    1    2    2    2    1    1    2    1    1    1    1    0    1    1    0    1    1    1    1    0
  2021    1   27   11   18   17  238   27   18   14    4    3    7    0    0    3    0    0    0    0    0    0    1    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    1    0    0    0    0  450
  2022    0   14  113   60    7   10   61   23    4   10    9    4    3    1    2    3    3    2    0    0    1    1    0    0    0    0    0    0    0    0    0    1    2    0    0    1    1    0    0    0    0    0    0    0 4535
  2023    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0 2052
  2024    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0 1301


load("C:\\SIDT\\Sablefish Combo Multi Year\\Sable_Combo_All_Oties_2017_18_19_21_22\\Sable_Combo_Multi_17_22_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)
      
  
         0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  78  88
  2017   9 211  50 116 189  66  52  46  45  35  25  13  15   9   8  18  17  14  16   8   6   7   7   6   7   4   6   4   4   3   4   1   2   2   2   5   2   0   3   4   2   4   3   0   1   3   2   8   3   3   2   1   0   1   1   1   2   4   2   2   1   1   1   2   2   1   0   2   0   1   0   2   0   0   0
  2018  19  41 312  73 102 173  87  82  58  47  50  18  16   9  12  14  15  14  11  12  13  14   7   5   2   3   2   4   6   8   4   7   5   4   1   5   2   3   5   6   3   0   0   2   4   2   2   1   3   3   1   5   3   1   2   2   4   3   1   1   0   1   4   0   1   0   1   0   0   1   0   0   0   0   0
  2019  30  71  13 180  45  41 116  36  29  24  16  19   6   6  11   5   6   3   9  11  12   3   3   4   1   2   2   1   5   2   3   1   2   1   0   2   1   1   3   3   1   1   0   0   0   0   0   2   0   2   2   1   1   0   1   1   0   1   0   2   0   2   2   0   0   0   1   0   1   0   0   0   0   0   0
  2021 213 450 130  65  60 300  94  76  80  64  66  60  60  41  29  21  23   9  11   6  10  17  17  17   7  12   9  10   9   3   3   2   4   7   3   6   1   3   3   5   3   2   4   6   2   3   0   2   0   1   2   1   3   2   4   1   2   1   2   1   1   2   0   1   1   0   1   1   3   0   1   2   2   0   1
  2022  26 332 368 110  41  29 170  42  36  43  36  33  39  32  22  19  15   6  11   9  11  10   5   6  10   5   6   3   5   0   1   2   2   1   6   2   5   2   3   1   0   2   4   1   2   2   4   0   2   1   0   1   2   2   2   0   4   0   0   1   3   6   1   1   0   2   1   1   0   1   1   0   0   1   0



Table(Model_Spectra_Meta$sample_year, !is.na(Model_Spectra_Meta$TMA))
      
       TRUE
  2017 1099
  2018 1322
  2019  750
  2021 2064
  2022 1553
  
  # All Commercial plus 2017, 2019, 2022

2600 + 1099 + 750 + 1553 + 1032 


# -- Just non-missing TMA for model training  --
load("C:\\SIDT\\Sablefish Combo Multi Year\\Sable_Combo_All_Oties_2017_18_19_21_22\\Sable_Combo_Multi_17_22_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta_Combo <- Model_Spectra_Meta[Model_Spectra_Meta$sample_year %in% c(2017, 2019, 2022), ]
dim(Model_Spectra_Meta_Combo)


load("C:\\SIDT\\Sable_Comm\\SABL_Comm_2018__2024_Model_Spectra_Meta_ALL_GOOD_DATA_SR_Sex.RData")
headTail(Model_Spectra_Meta,2,2,2,55)
Model_Spectra_Meta$sample_year <- paste(Model_Spectra_Meta$sample_year, "Comm", sep = "_")

Model_Spectra_Meta <- Model_Spectra_Meta[!is.na(Model_Spectra_Meta$TMA), ]
dim(Model_Spectra_Meta)



Model_Spectra_Meta$age_structure_side <- Model_Spectra_Meta$NWFSC_NIR_Filename <- Model_Spectra_Meta$unscannable_other <- Model_Spectra_Meta$Sex_F <- Model_Spectra_Meta$Sex_M <- Model_Spectra_Meta$Sex_U <- NULL

Columns <- names(Model_Spectra_Meta)

Columns[!Columns %in% names(Model_Spectra_Meta_Combo)]
                
Model_Spectra_Meta <- rbind(Model_Spectra_Meta[, Columns], Model_Spectra_Meta_Combo[, Columns])
Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)



# -- Sex, One hot encoding --
if(!is.null(Model_Spectra_Meta$Sex)) {
    Model_Spectra_Meta$Sex <- as.character(Model_Spectra_Meta$Sex)
    Model_Spectra_Meta$Sex[is.na(Model_Spectra_Meta$Sex)] <- "U"
    Sex_ <- Model_Spectra_Meta$Sex
    Model_Spectra_Meta <- cbind(Model_Spectra_Meta, as.data.frame(model.matrix(formula(~ -1 + Sex_))))  # Three indicator columns added: Sex_F, Sex_M, Sex_U
    if(is.null(Model_Spectra_Meta$Sex_U))  Model_Spectra_Meta$Sex_U <- 0
}   

headTail(Model_Spectra_Meta, 2,2,2,55)

save(Model_Spectra_Meta, file = "C:\\SIDT\\Sable_Comm\\SABL_Combo_Comm_2017__2022_Model_Spectra_Meta_ALL_GOOD_DATA.RData")




# ========================= All the data for prediction ===============================

load("C:\\SIDT\\Sablefish Combo Multi Year\\Sable_Combo_All_Oties_2017_18_19_21_22\\Sable_Combo_Multi_17_22_Model_Spectra_Meta_ALL_GOOD_DATA.RData") 
Model_Spectra_Meta$sample_year <- paste(Model_Spectra_Meta$sample_year, "Combo_survey", sep = "_")
Model_Spectra_Meta_Combo <- Model_Spectra_Meta
dim(Model_Spectra_Meta_Combo)
headTail(Model_Spectra_Meta_Combo,2,2,2,55)
Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)


base::load("C:\\SIDT\\Sablefish\\Sable_Combo_2015_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta$sample_year <- paste(Model_Spectra_Meta$sample_year, "Combo_survey", sep = "_")
Model_Spectra_Meta_Combo_2015 <- Model_Spectra_Meta

base::load("C:\\SIDT\\Sablefish\\Sable_Combo_2016_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta$sample_year <- paste(Model_Spectra_Meta$sample_year, "Combo_survey", sep = "_")
Model_Spectra_Meta_Combo_2016 <- Model_Spectra_Meta

base::load("C:\\SIDT\\Sablefish\\Sable_Combo_2023_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta$sample_year <- paste(Model_Spectra_Meta$sample_year, "Combo_survey", sep = "_")
Model_Spectra_Meta_Combo_2023 <- Model_Spectra_Meta

base::load("C:\\SIDT\\Sablefish\\Sable_Combo_2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta$sample_year <- paste(Model_Spectra_Meta$sample_year, "Combo_survey", sep = "_")
Model_Spectra_Meta_Combo_2024 <- Model_Spectra_Meta




load("C:\\SIDT\\Sable_Comm\\SABL_Comm_2018__2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta_Comm_Len_Month <- Model_Spectra_Meta
headTail(Model_Spectra_Meta_Comm_Len_Month,2,2,2,55)

load("C:\\SIDT\\Sable_Comm\\SABL_Comm_2018__2024_Model_Spectra_Meta_ALL_GOOD_DATA_SR_Sex.RData")
Model_Spectra_Meta <- match.f(Model_Spectra_Meta, Model_Spectra_Meta_Comm_Len_Month, 'filenames', 'filenames', c('Length_cm', 'Month', 'Agency_ID'))
Model_Spectra_Meta$sample_year <- paste(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$Agency_ID, "Comm", sep = "_")
headTail(Model_Spectra_Meta,2,2,2,55)

Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)
Table(Model_Spectra_Meta$sample_year, round(Model_Spectra_Meta$Length_cm))
Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$Month)



Model_Spectra_Meta$age_structure_side <- Model_Spectra_Meta$NWFSC_NIR_Filename <- Model_Spectra_Meta$unscannable_other <- Model_Spectra_Meta$Sex_F <- Model_Spectra_Meta$Sex_M <- Model_Spectra_Meta$Sex_U <- Model_Spectra_Meta$Agency_ID <- NULL

Columns <- names(Model_Spectra_Meta)
Columns[!Columns %in% names(Model_Spectra_Meta_Combo)]
Columns[!Columns %in% names(Model_Spectra_Meta_Combo_2015)]
Columns[!Columns %in% names(Model_Spectra_Meta_Combo_2016)]
Columns[!Columns %in% names(Model_Spectra_Meta_Combo_2023)]
Columns[!Columns %in% names(Model_Spectra_Meta_Combo_2024)]
                
Model_Spectra_Meta <- rbind(Model_Spectra_Meta[, Columns], Model_Spectra_Meta_Combo[, Columns], Model_Spectra_Meta_Combo_2015[, Columns], Model_Spectra_Meta_Combo_2016[, Columns],
                             Model_Spectra_Meta_Combo_2023[, Columns],  Model_Spectra_Meta_Combo_2024[, Columns])
   

# -- Sex, One hot encoding --
if(!is.null(Model_Spectra_Meta$Sex)) {
    Model_Spectra_Meta$Sex <- as.character(Model_Spectra_Meta$Sex)
    Model_Spectra_Meta$Sex[is.na(Model_Spectra_Meta$Sex)] <- "U"
    Sex_ <- Model_Spectra_Meta$Sex
    Model_Spectra_Meta <- cbind(Model_Spectra_Meta, as.data.frame(model.matrix(formula(~ -1 + Sex_))))  # Three indicator columns added: Sex_F, Sex_M, Sex_U
    if(is.null(Model_Spectra_Meta$Sex_U))  Model_Spectra_Meta$Sex_U <- 0
}   

headTail(Model_Spectra_Meta,2,2,2,55)  # Look for one hot encoding sex and structure_weight_dg for the NN model

Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$TMA)
Table(Model_Spectra_Meta$sample_year, !is.na(Model_Spectra_Meta$structure_weight_g))
Table(Model_Spectra_Meta$sample_year, round(Model_Spectra_Meta$Length_cm))
Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$Month)
Table(Model_Spectra_Meta$sample_year, Model_Spectra_Meta$Sex)


save(Model_Spectra_Meta, file = "C:\\SIDT\\Sable_Comm\\SABL_Combo_Comm_2017__2024_Model_Spectra_Meta_ALL_GOOD_DATA_TMA_NAs.RData")










# =================================================


# FINAL_FISH_AGE_IN_YEARS is 14, but AGE_IN_YEARS by McBride in CA is 98

PacFIN_SABL_Age[PacFIN_SABL_Age$BDS_ID %in% 2469163, ]
       BDS_ID SAMPLE_ID SAMPLE_NUMBER SAMPLE_YEAR SAMPLE_MONTH SAMPLE_DAY SAMPLE_TYPE SAMPLE_METHOD_CODE SAMPLE_AGENCY AGENCY_CODE DATA_TYPE AGENCY_CONDITION_CODE PACFIN_CONDITION_CODE
44149 2469163  63753579 1989220130744        1989            3         31                              R            NA           C         C                     N    
                  
      PACFIN_PORT_CODE PACFIN_PORT_NAME FTID PACFIN_GEAR_CODE PACFIN_GEAR_NAME VESSEL_ID PSMFC_CATCH_AREA_CODE DEPTH_AVERAGE_FATHOMS DEPTH_MAXIMUM_FATHOMS DEPTH_MINIMUM_FATHOMS
44149              ERK           EUREKA                   GFT       GFSH-TRAWL                                                    NA                    NA                    NA

      MARKET_CATEGORY CLUSTER_SEQUENCE_NUMBER CLUSTER_WEIGHT_LBS ADJUSTED_CLUSTER_WEIGHT_LBS FRAME_CLUSTER_WEIGHT_LBS PACFIN_SPECIES_CODE FISH_SEQUENCE_NUMBER OBSERVED_FREQUENCY
44149             190                       1                 80                          NA                       NA                SABL                   16                  1

      FISH_LENGTH_TYPE_CODE FISH_LENGTH FORK_LENGTH FORK_LENGTH_IS_ESTIMATED FISH_WEIGHT SEX_CODE AGENCY_FISH_MATURITY_CODE FISH_MATURITY_CODE WEIGHT_OF_MALES_LBS WEIGHT_OF_FEMALES_LBS
44149                     F         488          NA                       NA          NA        M                         9                  U                  NA                    NA

      NUMBER_OF_MALES NUMBER_OF_FEMALES WEIGHT_OF_LANDING_LBS EXPANDED_SAMPLE_WEIGHT SPECIES_WEIGHT_LBS FINAL_FISH_AGE_CODE FINAL_FISH_AGE_IN_YEARS AGE_SEQUENCE_NUMBER AGE_METHOD_CODE
44149              NA                NA                  1558                     NA                 NA                  NA                      14                   2              BB

      AGE_READABILITY      PERSON_WHO_AGED DATE_AGE_RECORDED AGE_IN_YEARS AGE_STRUCTURE_CODE AGENCY_AGE_STRUCTURE_CODE    PACFIN_LOAD_DATE AGENCY_GRADE_CODE PACFIN_GRADE_CODE
44149              NA McBride                           <NA>           98                                              2025-04-03 04:01:27                 S                 S

      FISH_LENGTH_UNITS FISH_WEIGHT_UNITS FISH_LENGTH_IS_ESTIMATED FISH_WEIGHT_UNITS_NAME all_cluster_sum_kg DAHL_GROUNDFISH_CODE Length_cm Weight_kg Sex Year Depth Month
44149                MM                                      FALSE                                  36.28739                   NA      48.8        NA   M 1989    NA     3




No_NA <- PacFIN_SABL_Age[!is.na(PacFIN_SABL_Age$AGE_IN_YEARS) & !is.na(PacFIN_SABL_Age$FINAL_FISH_AGE_IN_YEARS), ]
renum(No_NA[No_NA$AGE_IN_YEARS > 65| No_NA$FINAL_FISH_AGE_IN_YEARS > 65, 
  c("AGENCY_CODE", "SAMPLE_YEAR", "SAMPLE_MONTH", "SAMPLE_DAY", "BDS_ID", "SAMPLE_ID", "SAMPLE_NUMBER", "CLUSTER_SEQUENCE_NUMBER", "FISH_SEQUENCE_NUMBER", 
    "AGE_SEQUENCE_NUMBER", "PERSON_WHO_AGED", "AGE_IN_YEARS", "FINAL_FISH_AGE_IN_YEARS")])

   AGENCY_CODE SAMPLE_YEAR SAMPLE_MONTH SAMPLE_DAY  BDS_ID SAMPLE_ID  SAMPLE_NUMBER CLUSTER_SEQUENCE_NUMBER FISH_SEQUENCE_NUMBER AGE_SEQUENCE_NUMBER      PERSON_WHO_AGED AGE_IN_YEARS FINAL_FISH_AGE_IN_YEARS 
1            C        1989            9         25 4553212  63738574  1989201330766                       2                    1                   1              Pearson           32                      98 
2            C        1989            9         25 4553213  63738574  1989201330766                       2                    1                   2              McBride           98                      98 

3            C        1989            3         31 2469163  63753579  1989220130744                       1                   16                   2              McBride           98                      14 
4            W        1992            1         22 3090557  63725717 20924301010009                       1                   10                   1              SWFC/JO           68                      68 
5            O        1996            6          4 4641375  63491683       OR960031                       1                    9                   1                                76                      76 
6            O        1997            7          7  515848  63480810       OR970104                       1                    6                   1                                92                      92 
7            O        1997            8         31 4005968  63492217       OR970644                       1                    6                   1                                87                      87 
8            C        2000            4         14 1549261  63784801  2000400230305                       2                   10                   1 PJM - CAP                      67                      67 
9            W        2006            5         22 3911216  63714208 20064301010114                       1                   80                   1              NWFC/PM          102                     102 
10           O        2007           11          9 3511361  63484683       OR072118                       2                   21                   1                                66                      66 
11           O        2013            7         14 2496972  63472358       OR130227                       1                   45                   1                                71                      71 
12           W        2013            4         15 1481477  63725518 20134301010022                       1                   21                   1              NMFS IS           66                      66 
13           O        2014            2         12 3699983  62906135       OR142262                       2                   12                   1       Lance Sullivan           70                      70 
14           O        2017            8         31 3844013  62694790      OR1762861                       1                    2                   1          Nikki Paige           70                      70 
15           O        2017            9         20  429333  62695622      OR1763382                       2                   24                   1           James Hale           75                      75 
16           O        2017           10         27 3469521  62697029      OR1764090                       1                    1                   1     Patrick McDonald           68                      68 
17           O        2018           10          1   46946  62639196      OR1869074                       1                   18                   1     Patrick McDonald           82                      82 
18           O        2018           11         20   45733  62639116      OR1869956                       2                   15                   1     Patrick McDonald           73                      73 
19           O        2018           12         10   47203  62638513      OR1870085                       1                    3                   1     Patrick McDonald           69                      69 





renum(PacFIN_SABL_Age[PacFIN_SABL_Age$SAMPLE_NUMBER == 1989220130744 & PacFIN_SABL_Age$CLUSTER_SEQUENCE_NUMBER == 1 & PacFIN_SABL_Age$FISH_SEQUENCE_NUMBER == 16, 
             c("AGENCY_CODE", "SAMPLE_YEAR", "SAMPLE_MONTH", "SAMPLE_DAY", "BDS_ID", "SAMPLE_ID", "SAMPLE_NUMBER", "CLUSTER_SEQUENCE_NUMBER", 
               "FISH_SEQUENCE_NUMBER", "AGE_SEQUENCE_NUMBER", "PERSON_WHO_AGED", "AGE_IN_YEARS", "FINAL_FISH_AGE_IN_YEARS")])

   AGENCY_CODE SAMPLE_YEAR SAMPLE_MONTH SAMPLE_DAY  BDS_ID SAMPLE_ID SAMPLE_NUMBER CLUSTER_SEQUENCE_NUMBER FISH_SEQUENCE_NUMBER AGE_SEQUENCE_NUMBER      PERSON_WHO_AGED AGE_IN_YEARS FINAL_FISH_AGE_IN_YEARS 
1           C        1989            3         31 2469162  63753579 1989220130744                       1                   16                   1               Pearson           14                      14 
2           C        1989            3         31 2469163  63753579 1989220130744                       1                   16                   2               McBride           98                      14 



PacFIN_SABL_Age$age_structure_id_CA_1 <- paste0('CA', substr(PacFIN_SABL_Age$SAMPLE_NUMBER, 3, 4), substring(PacFIN_SABL_Age$SAMPLE_NUMBER, 9), "-", PacFIN_SABL_Age$PACFIN_SPECIES_CODE, "-" , PacFIN_SABL_Age$CLUSTER_SEQUENCE_NUMBER, "-", PacFIN_SABL_Age$FISH_SEQUENCE_NUMBER, "-O")
PacFIN_SABL_Age$age_structure_id_CA_2 <- paste0('CA', substr(PacFIN_SABL_Age$SAMPLE_NUMBER, 1, 4), substring(PacFIN_SABL_Age$SAMPLE_NUMBER, 9), "-", PacFIN_SABL_Age$PACFIN_SPECIES_CODE, "-" , PacFIN_SABL_Age$CLUSTER_SEQUENCE_NUMBER, "-", PacFIN_SABL_Age$FISH_SEQUENCE_NUMBER, "-O")




renum(PacFIN_SABL_Age[PacFIN_SABL_Age$SAMPLE_NUMBER == 1989220130744 & PacFIN_SABL_Age$CLUSTER_SEQUENCE_NUMBER == 1 & PacFIN_SABL_Age$FISH_SEQUENCE_NUMBER == 16, 
             c("AGENCY_CODE", "SAMPLE_YEAR", "SAMPLE_MONTH", "SAMPLE_DAY", "BDS_ID", "SAMPLE_ID", "SAMPLE_NUMBER", "CLUSTER_SEQUENCE_NUMBER", 
               "FISH_SEQUENCE_NUMBER", "AGE_SEQUENCE_NUMBER", "PERSON_WHO_AGED", "AGE_IN_YEARS", "FINAL_FISH_AGE_IN_YEARS", "age_structure_id_CA_1", "age_structure_id_CA_2")])

























