

setwd("C:/SIDT/")

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

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/headTail.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Table.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Read_OPUS_Spectra.R")






# c("Hake_2019", "Sable_2017_2019", "Sable_Combo_2022", "Sable_Combo_2021", "Sable_Combo_2019")


N_Samp <- 200
Predicted_Ages_Path <- "Predicted_Ages"

for(Year in c(2017, 2018, 2019, 2021, 2022, 2023)[6]) {

    print(Spectra_Set <- paste0('Sable_Combo_', Year))
    Spectra_Path <- paste0('Sablefish ', Year, ' Combo/', 'Sable_Combo_', Year, '_Scans')
    Meta_Path <- paste0('Sablefish ', Year, ' Combo/', Spectra_Set, '_NIRS_Scanning_Session_Report_For_NWFSC.xlsx')

    Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set, Spectra_Path = Spectra_Path, TMA_Ages = TRUE, Max_N_Spectra = N_Samp, verbose = TRUE, 
        Meta_Add = TRUE, Meta_Path = Meta_Path, plot = TRUE, htmlPlotFolder = paste0(Predicted_Ages_Path, '/', Spectra_Set, '_Spectra_Sample_of_', N_Samp))
     
    headTail(Model_Spectra_Meta, 2, 2, 3, 46)
    
    save(Model_Spectra_Meta, file = paste0(Spectra_Set, "_Model_Spectra_Meta_ALL_GOOD_DATA.RData"))
}
 
 
     
load("Sable_Combo_2017_Model_Spectra_Meta_ALL_GOOD_DATA.RData") # 1109 555
load("Sable_Combo_2018_Model_Spectra_Meta_ALL_GOOD_DATA.RData") # 1335 555 
load("Sable_Combo_2019_Model_Spectra_Meta_ALL_GOOD_DATA.RData") #  773 554
load("Sable_Combo_2021_Model_Spectra_Meta_ALL_GOOD_DATA.RData") # 2070 554 
load("Sable_Combo_2022_Model_Spectra_Meta_ALL_GOOD_DATA.RData") # 1556 554


# 2000N from 500N each of 2017, 2018, 2019, 2022
    
Seed_Pick_Scans <- 707     
Sable_Combo_Rdm_500_2017_18_19_22_Model_Spectra_Meta <- NULL
for(Year in c(2017, 2018, 2019, 2022)) {
    
	print(Year)
    load(paste0("C:/SIDT/Sable_Combo_", Year, "_Model_Spectra_Meta_ALL_GOOD_DATA.RData"))
	
	# Remove missing values from Model_Spectra_Meta - some oties may have been removed by Read_OPUS_Spectra() function if there were majors issues (e.g. too much chipping or tissue).
	Model_Spectra_Meta <- Model_Spectra_Meta[!(is.na(Model_Spectra_Meta$TMA) | is.na(Model_Spectra_Meta$structure_weight_dg) | is.na(Model_Spectra_Meta$Length_prop_max) 
                              | is.na(Model_Spectra_Meta$Weight_prop_max) | is.na(Model_Spectra_Meta$Depth_prop_max) | is.na(Model_Spectra_Meta$Latitude_prop_max)), ]
	 
	
	# These 3 variables not in Scanning_Session_Report for 2017 and 2018 - use sex from Data Warehouse anyway. The last 2 are not in 2019+, so number of columns is now 553.
	Model_Spectra_Meta$sex <- Model_Spectra_Meta$age_structure_side <- Model_Spectra_Meta$unscannable_other <- NULL  
	
    set.seed(Seed_Pick_Scans)
    Sable_Combo_Rdm_500_2017_18_19_22_Model_Spectra_Meta <- rbind(Sable_Combo_Rdm_500_2017_18_19_22_Model_Spectra_Meta, 
	     Model_Spectra_Meta[sample(1:nrow(Model_Spectra_Meta), 500), ])   
}
    
headTail(Sable_Combo_Rdm_500_2017_18_19_22_Model_Spectra_Meta, 2, 2, 3, 46)
	
Table(Sable_Combo_Rdm_500_2017_18_19_22_Model_Spectra_Meta$sample_year)

	
Model_Spectra_Meta <- Sable_Combo_Rdm_500_2017_18_19_22_Model_Spectra_Meta
save(Model_Spectra_Meta, file= 'Sable_Combo_Rdm_500_2017_18_19_22_Model_Spectra_Meta.Rdata')
     
	

# Add 200N from 2021	
	 
load("C:\\SIDT\\Train_NN_Model\\Sable_Combo_Rdm_500_2017_18_19_22_Model_Spectra_Meta.Rdata")
Sable_Combo_Rdm_500_2017_18_19_22_Model_Spectra_Meta <- Model_Spectra_Meta
   
   
load(paste0("C:/SIDT/Sable_Combo_", 2021, "_Model_Spectra_Meta_ALL_GOOD_DATA.RData"))
	
# Remove missing values from Model_Spectra_Meta - some oties may have been removed by Read_OPUS_Spectra() function if there were majors issues (e.g. too much chipping or tissue).
Model_Spectra_Meta <- Model_Spectra_Meta[!(is.na(Model_Spectra_Meta$TMA) | is.na(Model_Spectra_Meta$structure_weight_dg) | is.na(Model_Spectra_Meta$Length_prop_max) 
                              | is.na(Model_Spectra_Meta$Weight_prop_max) | is.na(Model_Spectra_Meta$Depth_prop_max) | is.na(Model_Spectra_Meta$Latitude_prop_max)), ]
	 
Model_Spectra_Meta$sex <- Model_Spectra_Meta$age_structure_side <- Model_Spectra_Meta$unscannable_other <- NULL  	 


Seed_Pick_Scans <- 707   	 
set.seed(Seed_Pick_Scans)
Sable_Combo_Rdm_500N_2017_18_19_22_200N_21_Model_Spectra_Meta <- rbind(Sable_Combo_Rdm_500_2017_18_19_22_Model_Spectra_Meta, 
	     Model_Spectra_Meta[sample(1:nrow(Model_Spectra_Meta), 200), ])   	 
	
headTail(Sable_Combo_Rdm_500N_2017_18_19_22_200N_21_Model_Spectra_Meta, 2, 2, 3, 46)

	
Model_Spectra_Meta <- Sable_Combo_Rdm_500N_2017_18_19_22_200N_21_Model_Spectra_Meta 
save(Model_Spectra_Meta , file = "Sable_Combo_Rdm_500N_2017_18_19_22_200N_21_Model_Spectra_Meta.RData")
	 
	 
	 
	 
	 
# All the scans from 2017, 2018, 2019, 2021, 2022
  
Sable_Combo_2017_18_19_21_22_Model_Spectra_Meta <- NULL
for(Year in c(2017, 2018, 2019, 2021, 2022)) {
    
	print(Year)
    load(paste0("C:/SIDT/Sable_Combo_", Year, "_Model_Spectra_Meta_ALL_GOOD_DATA.RData"))
	
	# Remove missing values from Model_Spectra_Meta - some oties may have been removed by Read_OPUS_Spectra() function if there were majors issues (e.g. too much chipping or tissue).
	Model_Spectra_Meta <- Model_Spectra_Meta[!(is.na(Model_Spectra_Meta$TMA) | is.na(Model_Spectra_Meta$structure_weight_dg) | is.na(Model_Spectra_Meta$Length_prop_max) 
                              | is.na(Model_Spectra_Meta$Weight_prop_max) | is.na(Model_Spectra_Meta$Depth_prop_max) | is.na(Model_Spectra_Meta$Latitude_prop_max)), ]
	 
	
	# These 3 variables not in Scanning_Session_Report for 2017 and 2018 - use sex from Data Warehouse anyway. The last 2 are not in 2019+, so number of columns is now 553.
	Model_Spectra_Meta$sex <- Model_Spectra_Meta$age_structure_side <- Model_Spectra_Meta$unscannable_other <- NULL  
	
    set.seed(Seed_Pick_Scans)
    Sable_Combo_2017_18_19_21_22_Model_Spectra_Meta <- rbind(Sable_Combo_2017_18_19_21_22_Model_Spectra_Meta, Model_Spectra_Meta)   
}
    
headTail(Sable_Combo_2017_18_19_21_22_Model_Spectra_Meta, 2, 2, 3, 46)
	
Table(Sable_Combo_2017_18_19_21_22_Model_Spectra_Meta$sample_year)

	
Model_Spectra_Meta <- Sable_Combo_2017_18_19_21_22_Model_Spectra_Meta
save(Model_Spectra_Meta, file= 'Sable_Combo_2017_18_19_21_22_Model_Spectra_Meta.RData')

headTail(Model_Spectra_Meta, 2, 2, 3, 46)  # Dimension: 6788 553 
	
	 

# All the scans from 2017, 2018, 2019, 2021 (2022 saved out for prediction only)

Sable_Combo_2017_18_19_21_Model_Spectra_Meta <- NULL
for(Year in c(2017, 2018, 2019, 2021)) {
    
	print(Year)
    load(paste0("C:/SIDT/Sable_Combo_", Year, "_Model_Spectra_Meta_ALL_GOOD_DATA.RData"))
	
	# Remove missing values from Model_Spectra_Meta - some oties may have been removed by Read_OPUS_Spectra() function if there were majors issues (e.g. too much chipping or tissue).
	Model_Spectra_Meta <- Model_Spectra_Meta[!(is.na(Model_Spectra_Meta$TMA) | is.na(Model_Spectra_Meta$structure_weight_dg) | is.na(Model_Spectra_Meta$Length_prop_max) 
                              | is.na(Model_Spectra_Meta$Weight_prop_max) | is.na(Model_Spectra_Meta$Depth_prop_max) | is.na(Model_Spectra_Meta$Latitude_prop_max)), ]
	 
	
	# These 3 variables not in Scanning_Session_Report for 2017 and 2018 - use sex from Data Warehouse anyway. The last 2 are not in 2019+, so number of columns is now 553.
	Model_Spectra_Meta$sex <- Model_Spectra_Meta$age_structure_side <- Model_Spectra_Meta$unscannable_other <- NULL  
	
    set.seed(Seed_Pick_Scans)
    Sable_Combo_2017_18_19_21_Model_Spectra_Meta <- rbind(Sable_Combo_2017_18_19_21_Model_Spectra_Meta, Model_Spectra_Meta)   
}
    
headTail(Sable_Combo_2017_18_19_21_Model_Spectra_Meta, 2, 2, 3, 46)
	
Table(Sable_Combo_2017_18_19_21_Model_Spectra_Meta$sample_year)


Model_Spectra_Meta <- Sable_Combo_2017_18_19_21_Model_Spectra_Meta
save(Model_Spectra_Meta, file= 'Sable_Combo_2017_18_19_21_Model_Spectra_Meta.RData')

headTail(Model_Spectra_Meta, 2, 2, 3, 46)  # Dimension: 5235 553 
	
	 
	 	 
# Add 200N from 2022 to all the scans from 2017, 2018, 2019, 2021
load("C:\\SIDT\\Sablefish Combo Multi Year\\Sable_Combo_Multi_17_22_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
headTail(Model_Spectra_Meta, 3, 3, 3, 70)


Table(Model_Spectra_Meta$sample_year)

2017 2018 2019 2021 2022 
1099 1322  750 2064 1553 


Sable_Combo_Multi_21_Model_Spectra_Meta_ALL_GOOD_DATA <- Model_Spectra_Meta[Model_Spectra_Meta$sample_year %in% 2021, ]
headTail(Sable_Combo_Multi_21_Model_Spectra_Meta_ALL_GOOD_DATA, 3, 3, 3, 70)
Table(Sable_Combo_Multi_21_Model_Spectra_Meta_ALL_GOOD_DATA$sample_year)


load("C:\\SIDT\\Sablefish Combo Multi Year\\Sable_Combo_2017_18_19_22_Model_Spectra_Meta.RData")
headTail(Model_Spectra_Meta, 3, 3, 3, 70)


Table(Model_Spectra_Meta$sample_year)

2017 2018 2019 2022 
1099 1322  750 1553 


Seed_Pick_Scans <- 707   	 
set.seed(Seed_Pick_Scans)
Sable_Combo_2017_18_19_22_21_200N_Model_Spectra_Meta <- rbind(Model_Spectra_Meta, 
        Sable_Combo_Multi_21_Model_Spectra_Meta_ALL_GOOD_DATA[sample(1:nrow(Sable_Combo_Multi_21_Model_Spectra_Meta_ALL_GOOD_DATA), 200), ])   	 
		 
Table(Sable_Combo_2017_18_19_22_21_200N_Model_Spectra_Meta$sample_year)		 
		 

headTail(Sable_Combo_2017_18_19_22_21_200N_Model_Spectra_Meta, 2, 2, 3, 70)

	
Model_Spectra_Meta <- Sable_Combo_2017_18_19_22_21_200N_Model_Spectra_Meta
save(Model_Spectra_Meta , file = "Sable_Combo_2017_18_19_22_21_200N_Model_Spectra_Meta.RData")
	 


# --------------------------------------------------------


 
source("C:\\SIDT\\Train_NN_Model\\Read_OPUS_Spectra.R")
source("C:\\SIDT\\Predict_NN_Ages\\Predict_NN_Age_Wrapper.R")

 	 
	 
	 
setwd("C:/SIDT")
N_Samp <- 200
Predicted_Ages_Path <- "Predicted_Ages"

for(Year in c(2017, 2018, 2019, 2021, 2022, 2023)[6]) {

    print(Spectra_Set <- paste0('Sable_Combo_', Year))
    Spectra_Path <- paste0('Sable_Combo_', Year, '/', 'Sable_Combo_', Year, '_Scans')
    Meta_Path <- paste0('Sable_Combo_', Year, '/', Spectra_Set, '_NIRS_Scanning_Session_Report_For_NWFSC.xlsx')

    Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set, Spectra_Path = Spectra_Path, TMA_Ages = TRUE, Max_N_Spectra = N_Samp, verbose = TRUE, 
        Meta_Path = Meta_Path, plot = TRUE, htmlPlotFolder = paste0(Predicted_Ages_Path, '/', Spectra_Set, '_Spectra_Sample_of_', N_Samp))
     
    headTail(Model_Spectra_Meta, 2, 2, 3, 46)
    
    save(Model_Spectra_Meta, file = paste0(Spectra_Set, "_Model_Spectra_Meta_ALL_GOOD_DATA.RData"))
}
 

 
	
# Create Sable_Combo_2017_21_22_200N_Model_Spectra_Meta.RData
load("C:\\SIDT\\Sablefish Combo Multi Year\\Sable_Combo_2017_18_19_22_21_200N_Model_Spectra_Meta.RData")
Table(Model_Spectra_Meta$sample_year)
	
Model_Spectra_Meta_2022 <- Model_Spectra_Meta[Model_Spectra_Meta$sample_year %in% 2022, ]
Table(Model_Spectra_Meta_2022$sample_year)


set.seed(707)
Model_Spectra_Meta_2022_200N <- Model_Spectra_Meta_2022[sample(1:nrow(Model_Spectra_Meta_2022), 200), ] 
Table(Model_Spectra_Meta_2022_200N$sample_year)
	 
	 
load("C:\\SIDT\\Sablefish Combo Multi Year\\Sable_Combo_2017_18_19_21_Model_Spectra_Meta.RData")
Table(Model_Spectra_Meta$sample_year)
	 
Model_Spectra_Meta <- rbind(Model_Spectra_Meta, Model_Spectra_Meta_2022_200N)
Table(Model_Spectra_Meta$sample_year)	 

2017 2018 2019 2021 2022 
1099 1322  750 2064  200 
	 
	 
save(Model_Spectra_Meta, file = "Sable_Combo_2017_21_22_200N_Model_Spectra_Meta.RData")
load("C:\\SIDT\\Sablefish Combo Multi Year\\Sable_Combo_2017_21_22_200N_Model_Spectra_Meta.RData")
 
	 
	 
# ================== Read those years of Combo survey (last scanned?) not in any Sablefish NN model, they will be predicted by the Combo_Comm model ==================================


setwd("C:/SIDT/Sablefish")

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

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/headTail.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Table.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Read_OPUS_Spectra.R")


	 "SABL_COMBO2015_NIR0040A_PRD_1_102052421_O1.0"
     

for(Year in c(2015, 2016, 2023, 2024)[4]) {

      Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set = paste0('Sable_Combo_', Year), excelSheet = 3,
                             Spectra_Path = paste0("//nwcfile.nmfs.local/FRAM/Assessments/Aging Lab/NIRS Scanning Data/Otoliths/FT_NIRS_Project/PRD_Production/NWFSC_COMBO/SABL_Sablefish/", Year, "/"), # Need the last slash
                             Meta_Path = list(NULL, paste0('Sablefish ', Year, ' Combo/', Spectra_Set, '_NIRS_Scanning_Session_Report_For_NWFSC.xlsx'))[[1]], shortNameSegments = c(1, 6), shortNameSuffix = 'Combo', TMA_Ages = TRUE, 
                             Extra_Meta_Path = "C:/SIDT/Get Otie Info from Data Warehouse/selectSpAgesFramFeb2025.RData",
                             Max_N_Spectra = 'All', verbose = TRUE, plot = TRUE, htmlPlotFolder = paste0("Figures_Sable_Combo_", Year), Static_Figure = paste0("Sable_Combo_", Year, ".png"), Debug = TRUE)
     
    cat("\n\nModel_Spectra_Meta:\n\n") 
    headTail(Model_Spectra_Meta, 2, 2, 3, 46)
    
    cat("\n\nPresence of otie weight by TMA:\n\n")
    Table(!is.na(Model_Spectra_Meta$structure_weight_g), Model_Spectra_Meta$TMA)
   
    cat("\n\nModel_Spectra_Meta for year", Year, "was saved to working directory\n\n") 
    save(Model_Spectra_Meta, file = paste0('Sable_Combo_', Year, "_Model_Spectra_Meta_ALL_GOOD_DATA.RData"))
}
 
  
	 
 
 
 
	 

	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
     