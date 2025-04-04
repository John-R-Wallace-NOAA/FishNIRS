


#  # From PacFIN codes:
#  
#  BSPR        BLACKSPOTTED ROCKFISH
#  SABL        ROUGHEYE ROCKFISH
#  RBR1        NOM. ROUGHEYE + BLACKSPOT
#  UDW1        SHORTRAKER+ROUGHEYE 
#  


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
# Works on Dell Laptop, but on desktop PC:

   ERROR:
ORA-12545: Connect failed because target host or object does not exist






PacFIN_SABL_Age <- PacFIN.BDS.Extraction("'SABL'")
save(PacFIN_SABL_Age, file = "C:\\SIDT\\SABL_Comm_2012_2023\\PacFIN_SABL_Age.RData")
base::load("C:\\SIDT\\SABL_Comm_2012_2023\\PacFIN_SABL_Age.RData")
headTail(PacFIN_SABL_Age,2,2,2,70)

# PacFIN_BSPR_Age <- PacFIN.BDS.Extraction("'BSPR'")  # NOT BLSP!!!!!!!!!!!!!!!!
# save(PacFIN_BSPR_Age, file = "C:\\SIDT\\BSPR_Comm_NEED_YEARS\\PacFIN_BSPR_Age.RData")  # !!!!!! Need to find years in another table and match in via SAMPLE_ID or BDS_ID
# base::load("C:\\SIDT\\BSPR_Comm_NEED_YEARS\\PacFIN_BSPR_Age.RData")
# headTail(PacFIN_BSPR_Age,2,2,2,70)



# ------------- Landings for Vlada and Jason -----------------

GitHub_File_Download("John-R-Wallace-NOAA/PacFIN.Data.Extraction/master/R/PacFIN.Catch.Extraction.R")


PacFIN.Login <- UID <- "wallacej"
PacFIN.PW <- PWD <- "go2pf4$jw"
DSN <- "PacFIN"
import.sql("Select * from pacfin.bds_sp where rownum < 11", dsn="PacFIN", uid="wallacej", pwd=PacFIN.PW)

PacFIN_BLSP_SABL_Catch <- PacFIN.Catch.Extraction("('BSPR', 'SABL', 'RBR1', 'UDW1')")

PacFIN_BSPR_Catch <- PacFIN.Catch.Extraction("('BSPR')")

BSPR        BLACKSPOTTED ROCKFISH
SABL        ROUGHEYE ROCKFISH
RBR1        NOM. ROUGHEYE + BLACKSPOT
UDW1        SHORTRAKER+ROUGHEYE 

# --------------------------------------------------------





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
plot(Weight_kg, Length_cm)


browsePlot('
  par(mfrow = c(2,1))
  plot(AGE_IN_YEARS, Length_cm)
  plot(AGE_IN_YEARS, Weight_kg)
')





PacFIN_SABL_Age$Sex <- PacFIN_SABL_Age$SEX
PacFIN_SABL_Age$Year <- PacFIN_SABL_Age$SAMPLE_YEAR
PacFIN_SABL_Age$Depth <- PacFIN_SABL_Age$DEPTH_AVERAGE_FATHOMS
PacFIN_SABL_Age$Month <- PacFIN_SABL_Age$SAMPLE_MONTH



# load("W:\\ALL_USR\\JRW\\R.Vanilla\\SABL_Comm_2012-2023_Model_Spectra_Meta_ALL_GOOD_DATA.RData")  # Laptop path
# base::load("C:\\SIDT\\SABL_Comm_2012_2023\\SABL_Comm_2012-2023_Model_Spectra_Meta_Extra_ALL_GOOD_DATA.RData")
base::load("C:/SIDT/SABL_Comm_2012_2023/SABL_Comm_2011_2023_Model_Spectra_Meta_SABL_ALL_GOOD_DATA.RData")  # Now with 2011 data

headTail(Model_Spectra_Meta, 2,2, 2, 70)
 
save(PacFIN_SABL_Age, file = 'PacFIN_SABL_Age.RData')
 
 
 
 
PacFIN_SABL_Age.NN <- PacFIN_SABL_Age[, c("BDS_ID", "SAMPLE_ID", "SAMPLE_NUMBER", "CLUSTER_SEQUENCE_NUMBER", "AGENCY_CODE", "PACFIN_SPECIES_CODE", "FISH_SEQUENCE_NUMBER", "FINAL_FISH_AGE_IN_YEARS", "Year", "Month", "SAMPLE_DAY", "Length_cm", "Weight_kg", "Sex", "Depth")]

# !!!!!!!!!!!!!!! Find the column names that are missing !!!!!!!!!!!!!
Columns <- c("BDS_ID", "SAMPLE_ID", "SAMPLE_NUMBER", "FINAL_FISH_AGE_IN_YEARS", "Year", "Month", "SAMPLE_DAY", "SPID", "Length_cm", "Weight_kg", "Sex", "Depth")
Columns[!Columns %in% names(PacFIN_SABL_Age)]


load("C:/SIDT/Sable_CA_Comm_2018/Sable_CA_Comm_2018_Model_Spectra_Meta_ALL_GOOD_DATA.RData")


Model_Spectra_Meta$age_structure_id[1:5]
[1] "CA1870028-SABL-1-1-O"  "CA1870232-SABL-1-1-O"  "CA1870028-SABL-1-10-O" "CA1870232-SABL-2-1-O"  "CA1870028-SABL-2-1-O" 



PacFIN_SABL_Age.NN$age_structure_id <- paste0("CA", substring(PacFIN_SABL_Age.NN$SAMPLE_NUMBER, 3, 4), substring(PacFIN_SABL_Age.NN$SAMPLE_NUMBER, 9), "-", PacFIN_SABL_Age.NN$PACFIN_SPECIES_CODE, "-" , PacFIN_SABL_Age.NN$CLUSTER_SEQUENCE_NUMBER, "-", PacFIN_SABL_Age.NN$FISH_SEQUENCE_NUMBER, "-O")
PacFIN_SABL_Age.NN$age_structure_id[1:5]
[1] "CA67010001-SABL-1-1-O" "CA67010001-SABL-1-2-O" "CA67010001-SABL-1-3-O" "CA67010001-SABL-1-4-O" "CA67010001-SABL-1-5-O"





Model_Spectra_Meta.xx <- Model_Spectra_Meta
Model_Spectra_Meta.xx$Length_cm <- Model_Spectra_Meta.xx$Weight_kg <- Model_Spectra_Meta.xx$Sex <- Model_Spectra_Meta.xx$Depth <- NULL

Model_Spectra_Meta <- match.f(Model_Spectra_Meta.xx, PacFIN_SABL_Age.NN, 'age_structure_id',  'age_structure_id', c("Length_cm", "Weight_kg", "Sex", "Depth"))

#  dim(Model_Spectra_Meta)
#  Model_Spectra_Meta <- Model_Spectra_Meta[!is.na(Model_Spectra_Meta$TMA), ]
#  dim(Model_Spectra_Meta)

headTail(Model_Spectra_Meta, 2, 2, 2, 70)

                     shortName structure_weight_dg Sex_1 Sex_2 Sex_3 Sex_U Length_cm Weight_kg Sex Depth
1   CA1870028-SABL-1-1_CA_Comm               0.345     1     0     0     0      59.4        NA   M    NA
2   CA1870232-SABL-1-1_CA_Comm               0.434     0     1     0     0      72.4        NA   F    NA
196 CA1870380-SABL-2-7_CA_Comm               0.232     0     1     0     0      55.0        NA   F    NA
197 CA1870380-SABL-2-8_CA_Comm               0.274     0     1     0     0      68.5        NA   F    NA





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











Table(!is.na(Model_Spectra_Meta$Length_cm))

FALSE  TRUE 
  327  1037 
  
  
Table(!is.na(Model_Spectra_Meta$Weight_kg))

FALSE  TRUE 
 1251   113 
 
 
Table(!is.na(Model_Spectra_Meta$Sex))
FALSE  TRUE 
  327  1037 

  
  
Table(!is.na(Model_Spectra_Meta$Depth))

FALSE  TRUE 
  797   567 




  if(!is.null(Model_Spectra_Meta$Length_cm))        
           Model_Spectra_Meta$Length_prop_max <- Model_Spectra_Meta$Length_cm/max(Model_Spectra_Meta$Length_cm, na.rm = TRUE)
           
        if(!is.null(Model_Spectra_Meta$Weight_kg))
           Model_Spectra_Meta$Weight_prop_max <- (Model_Spectra_Meta$Weight_kg - min(Model_Spectra_Meta$Weight_kg, na.rm = TRUE))/(max(Model_Spectra_Meta$Weight_kg, na.rm = TRUE) - min(Model_Spectra_Meta$Weight_kg, na.rm = TRUE))
           
        if(!is.null(Model_Spectra_Meta$Sex))
           Model_Spectra_Meta$Sex_prop_max <- as.numeric(recode.simple(Model_Spectra_Meta$Sex, data.frame(c('F','M', 'U'), 0:2)))/2  # ** All variables have to be numeric ** 
           
        if(!is.null(Model_Spectra_Meta$Depth_m))
           Model_Spectra_Meta$Depth_prop_max <- (Model_Spectra_Meta$Depth_m - min(Model_Spectra_Meta$Depth_m, na.rm = TRUE))/(max(Model_Spectra_Meta$Depth_m, na.rm = TRUE) - min(Model_Spectra_Meta$Depth_m, na.rm = TRUE))
           
        if(!is.null(Model_Spectra_Meta$Latitude_dd))
           Model_Spectra_Meta$Latitude_prop_max <- (Model_Spectra_Meta$Latitude_dd - 30.5)/(49.1 - 30.5)
           
        if(!is.null(Model_Spectra_Meta$Month))   
           Model_Spectra_Meta$Month_Scaled <- Model_Spectra_Meta$Month/12

headTail(Model_Spectra_Meta, 2,2, 2, 70)

Model_Spectra_Meta <- Model_Spectra_Meta[!(is.na(Model_Spectra_Meta$TMA) | is.na(Model_Spectra_Meta$structure_weight_dg) | is.na(Model_Spectra_Meta$Length_prop_max)), ]

headTail(Model_Spectra_Meta, 2,2, 2, 70)

save(Model_Spectra_Meta, file = 'SABL_Comm_2011_2023_Model_Spectra_Meta_Extra_ALL_GOOD_DATA.RData')

















