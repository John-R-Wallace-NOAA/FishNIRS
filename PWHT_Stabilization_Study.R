
# Initial code provided by Jordan Healy <jordan.healy@noaa.gov> 

sourceFunctionURL <- function (URL) {
      " # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() "
      require(xml2)
      File.ASCII <- tempfile()
      on.exit(file.remove(File.ASCII))
          homeDir <- getwd()
      tempDir <- tempfile()
      dir.create(tempDir); setwd(tempDir)
      on.exit(setwd(homeDir), add = TRUE)
      on.exit(system(paste0("rm -r -f ", tempDir)), add = TRUE)
      writeLines(paste0('source("', readLines(textConnection(xml2::download_html(URL))), '")'), File.ASCII)
      source(File.ASCII, local = parent.env(environment()))
}  

    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/lib.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/ColumnMove.R")

   
lib(vegan)
lib(pls)
lib(mdatools)
lib(PLSbiplot1)
lib(prospectr)

lib(FactoMineR)
lib(factoextra)
lib(tidyverse)
lib(ggplot2)
lib(grid)
lib(gridExtra)
lib(ade4)
lib(ExPosition)
lib(spectacles)
lib(signal)
lib(data.table)
lib(reshape2)
lib(dplyr)
lib(bio3d)

# lib('John-R-Wallace-NOAA/FishNIRS')  # Load from GitHub
lib(Fish_NIRS)


##############################################################
### Read in your OPUS files from folder in Windows explorer###
##############################################################
 
UploadDates <- c("1_2019_11", "2_2019_12", "3_2020_01", "4_2020_02", "5_2020_04", "6_2020_05", "7_2020_07", "8_2020_08", "9_2020_09", "10_2020_10")
hakeStabSpcStudy <- readSpectraData(UploadDates, nearestColSubset = TRUE)

save(hakeStabSpcStudy, file = 'hakeStabSpcStudy.RData')


#############################################################
### Analyze Spectral Data                                 ###
#############################################################

base::load('hakeStabSpcStudy.RData') # First 26 columns are metadata
hakeStabSpcStudy <- data.frame(hakeStabSpcStudy)

dim(hakeStabSpcStudy)
hakeStabSpcStudy[1:4, 1:27]


hakeStabSpcStudy$Date_Read <- substr(as.character(hakeStabSpcStudy$Date_Time_rf), 1, 10)

hakeStabSpcStudy <- ColumnMove(hakeStabSpcStudy, 25)
hakeStabSpcStudy[1:4, 1:28]




# Table(hakeStabSpcStudy$Storage, paste(days(hakeStabSpcStudy$Date_Time_rf), months(hakeStabSpcStudy$Date_Time_rf), years(hakeStabSpcStudy$Date_Time_rf)))

Table(hakeStabSpcStudy$Storage, paste(years(hakeStabSpcStudy$Date_Time_rf), months(hakeStabSpcStudy$Date_Time_rf)))


Table(hakeStabSpcStudy$Storage, hakeStabSpcStudy$Date_Read)



Date <- '2019-11-25'
StorageType <- 'Dry'
xmatrix_2019_11_25_Dry <- hakeStabSpcStudy[as.character(hakeStabSpcStudy$Date_Read) %in% Date & hakeStabSpcStudy$Storage %in% StorageType, 27:ncol(hakeStabSpcStudy)]
dim(xmatrix_2019_11_25_Dry)

Age_2019_11_25_Dry <- hakeStabSpcStudy[as.character(hakeStabSpcStudy$Date_Read) %in% Date & hakeStabSpcStudy$Storage %in% StorageType, 'final_age']
len(Age_2019_11_25_Dry)


################################
###Quick view data in Matplot###
################################

dev.new(width = 400, height = 300)
t.x <- as.matrix(t(xmatrix_2019_11_25_Dry))
par(mar = c(2,2,2,2))
# matplot(t.x, type = "l", pch = 1, col = 1:4, main = "All years")
matplot(t.x, type = "l", pch = 1, lty = 1, col = 'green', main = paste0(Date, '; ', StorageType))


###################################################################################################################
### Perform Savitzky-Golay 1st derivative with 17 point window smoothing 2rd order polynomial fit and visualize ###
###################################################################################################################
### NOTE ### If there are multiple years of data, all subsequent transformations should be applied to the whole data set, then re-subset

# sg.17 <- prospectr::savitzkyGolay(xmatrix_2019_11_25_Dry, p = 2, w = 15, m = 1)
sg.17 <- as.data.frame(prospectr::savitzkyGolay(xmatrix_2019_11_25_Dry, p = 2, w = 15, m = 1))


###########################################
###Other data transformations as follows### (NOTE: try mixing and stacking these for your dataset)
###########################################

# Standard Normal Variate (SNV)
snv.17 <- prospectr::standardNormalVariate(xmatrix_2019_11_25_Dry)


# Multipliciative Scatter Correction (MSC)
msc.17 <- pls::msc(as.matrix(xmatrix_2019_11_25_Dry))


# Vector Normalization (VN)

# xmatrix_2019_11_25_Dry <- as.data.frame(xmatrix_2019_11_25_Dry) 
vn.17 <- bio3d::normalize.vector(xmatrix_2019_11_25_Dry) 

dev.new(width = 400, height = 300)
par(mar = c(2,2,2,2))
matplot(as.matrix(t(vn.17)), type = "l", pch = 1,col = 1:4, main = "All years")
