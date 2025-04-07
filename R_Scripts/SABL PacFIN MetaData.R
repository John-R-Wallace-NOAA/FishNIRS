

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
PacFIN.PW <- PWD <- "Do3rj4$jrw"
DSN <- "PacFIN"
import.sql("Select * from pacfin.bds_sp where rownum < 11", dsn="PacFIN", uid="wallacej", pwd=PacFIN.PW)


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



# ==========================================================================================================================================================================================
# ==========================================================================================================================================================================================
 
setwd("C:/SIDT/Sable_Comm")
library(JRWToolBox)
 


load("C:\\SIDT\\Sable_Comm\\PacFIN_SABL_Age.RData")

 
PacFIN_SABL_Age.NN <- PacFIN_SABL_Age[, c("BDS_ID", "SAMPLE_ID", "SAMPLE_NUMBER", "CLUSTER_SEQUENCE_NUMBER", "AGENCY_CODE", "PACFIN_SPECIES_CODE", "FISH_SEQUENCE_NUMBER", "FINAL_FISH_AGE_IN_YEARS", "Year", "Month", "SAMPLE_DAY", "Length_cm", "Weight_kg", "Sex", "Depth")]

# !!!!! Find the column names that are missing !!!!!
Columns <- c("BDS_ID", "SAMPLE_ID", "SAMPLE_NUMBER", "CLUSTER_SEQUENCE_NUMBER", "AGENCY_CODE", "PACFIN_SPECIES_CODE", "FISH_SEQUENCE_NUMBER", "FINAL_FISH_AGE_IN_YEARS", "Year", "Month", "SAMPLE_DAY", "Length_cm", "Weight_kg", "Sex", "Depth")
Columns[!Columns %in% names(PacFIN_SABL_Age)]




# --- Load Model_Spectra_Meta data ---
load("C:\\SIDT\\Sable_Comm\\Sable_CA_Comm_2018__2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta_CA <-  Model_Spectra_Meta

load("C:\\SIDT\\Sable_Comm\\Sable_OR_Comm_2020__2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta_OR <-  Model_Spectra_Meta

load("C:\\SIDT\\Sable_Comm\\Sable_WA_Comm_2020__2023_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
Model_Spectra_Meta_WA <-  Model_Spectra_Meta

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


















