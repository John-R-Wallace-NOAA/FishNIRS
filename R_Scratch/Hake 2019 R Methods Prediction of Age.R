
sourceFunctionURL <- function (URL) {
       " # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() "
       require(httr)
       File.ASCII <- tempfile()
       on.exit(file.remove(File.ASCII))
       getTMP <- httr::GET(URL)
       write(paste(readLines(textConnection(httr::content(getTMP))), collapse = "\n"), File.ASCII)
       source(File.ASCII)}
 
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/lib.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/strSplit.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")

lib(data.table)
lib(mdatools)
lib(dplyr)
lib(hyperSpec) # http://hyperspec.r-forge.r-project.org
lib(prospectr)
lib(e1071)
lib(rpart)
lib(vegan)

# install and load simplerspec
# install.packages("remotes")
# remotes::install_github("philipp-baumann/simplerspec")
# library(simplerspec)
lib("philipp-baumann/simplerspec") # https://github.com/philipp-baumann/simplerspec

# upload all spectral files
PATH <- "W:/ALL_USR/JRW/SIDT/2019 Hake/"
setwd(PATH) # set working directory to folder containing spectral files
getwd()


(listspc <- dir(pattern = '*HAKE*'))[1:20] # creates the list of all the .001 file names in the directory
length(listspc) #  2852

ldf <- list() # creates an empty list
for (k in 1:length(listspc)) { # loops through and uploads each file using read_opus_bin_univ()  from the simplerspec package
       temp_spec <- simplerspec::read_opus_bin_univ(listspc[k], extract = "spc", print_progress = TRUE, atm_comp_minus4offset = FALSE)
       if(any(grepl('Error', temp_spec[[1]])))
           next
       ldf[[as.character(k)]] <- temp_spec
}

(N <- length(ldf)) # 2848

names(ldf[[1]]) # Names of first element
##  [1] "metadata"          "spc"               "spc_nocomp"        "sc_sm"             "sc_rf"             "ig_sm"             "ig_rf"             "wavenumbers"      
##  [9] "wavenumbers_sc_sm" "wavenumbers_sc_rf"


str(ldf[[1]]) # check first element

ldf[[1]]$wavenumbers[1:20] # 8th Element

ldf[[2848]]$wavenumbers[1:20] # 8th Element

len(ldf[[1]]$wavenumbers)
len(ldf[[2848]]$wavenumbers)

save(ldf, file = 'ldf.RData')
 

# Interpolate
# prospectr::resample()  uses spline interpolation method
testdf <- (prospectr::resample(X = ldf[[1]][[2]], wav = ldf[[1]][[8]], new.wav = ldf[[N]][[8]]))

# plot to test
plot(as.vector(ldf[[1]]$wavenumbers), as.vector(ldf[[2]]$spc))
points(as.vector(ldf[[N]]$wavenumbers), as.vector(testdf), col = 'green') 


# Resample()  all the wave lengths
ldf_int <- matrix(data = NA, nrow = length(ldf), ncol = length(ldf[[N]]$wavenumbers)) #make empty matrix for loop


for (j in 1:length(ldf)){ 
  bar(j, length(ldf))
  ldf_int[j,] <- prospectr::resample(X = ldf[[j]][[2]], wav = ldf[[j]]$wavenumbers, new.wav = ldf[[N]]$wavenumbers)
}

colnames(ldf_int) <- ldf[[N]]$wavenumbers
dat_spc <- as.data.frame(ldf_int)
dim(dat_spc)
[1] 2849 1112

dat_spc[1:20, 1:10]

# Add file names back in, could add other variables here as well (age etc.)
# metadat <- sapply(rdat,'[[', 1)
# (filenames <- unlist(metadat[2,]))[1:10]

(filenames <- unlist(sapply(ldf, function(x) x[[1]][2]), use.names = FALSE))[1:10]

(dat <- cbind(filenames, dat_spc))[1:5, c(1:5, 1110:1113)]

save(dat, file = 'dat 6 Oct 2022.RData')

base::load(file = 'dat 6 Oct 2022.RData')



# ADD VESSEL, CRUISE, REGION, LOCATION, AND BIO METADATA
scan_data <- read.csv(paste0(PATH, "hake_scandata_2019.csv"), strip.white = TRUE) #load in ancillary data
(int_data <- left_join(dat, scan_data, by = "filenames"))[1:5, c(1:3, 1112:1120, 1135:(ncol(dat) + ncol(scan_data) - 1))]


# ADD AGE DATA
age_data <- read.csv(paste0(PATH, "hake_agedata_2019.csv"), strip.white = TRUE) #load in age data
(int2_data <- left_join(int_data, age_data, by = "sequence"))[1:5, c(1:3, 1112:1120, 1135:(ncol(int_data) + ncol(age_data) - 1))]


# ADD HAUL METADATA
haul_data <- read.csv(paste0(PATH, "hake_haulmetadata_2019.csv"))
(all_data <- left_join(int2_data, haul_data, by = "Barcode"))[1:5, c(1:3, 1112:1120, 1135:(ncol(int2_data) + ncol(haul_data) - 1))]

hake_all_2019.6.20 <- all_data
hake_all_2019.6.20$Group <- substr(hake_all_2019.6.20$filenames, 26, 26)
hake_all_2019.6.20$GroupNum <- as.numeric(factor(hake_all_2019.6.20$Group))


# ADD Hake_OPUS.RData
load('Hake_OPUS.RData') 
Hake_OPUS[1:5, ]
names(Hake_OPUS)[2] <- "filenames"
Hake_OPUS[1:5, ]

base::load(file = "hake_all_2019.6.20 6 Oct 2022.RData")
preJoinNumCol <- ncol(hake_all_2019.6.20)
(hake_all_2019.6.20 <- left_join(hake_all_2019.6.20, Hake_OPUS[, 2:4], by = "filenames"))[1:5, c(1:3, 1112:1120, 1155:(preJoinNumCol + ncol(Hake_OPUS[, 2:4]) - 1))]

for( i in 1:nrow(Hake_OPUS)) # No match for row 714.  ID = 1239
   cat("\n", i, hake_all_2019.6.20$filenames[grep(Hake_OPUS[i,2],  hake_all_2019.6.20$filenames)])

grep('PACIFIC_HAKE_BMS201906206D_1239_OD1.0',  hake_all_2019.6.20$filenames)
integer(0)


Table(is.na(hake_all_2019.6.20$Age_OPUS))
FALSE  TRUE 
  854  1995 

# With missing row:
 855/(855 + 1995)
[1] 0.3
  
# is.na(hake_all_2019.6.20$Age_OPUS) == TRUE appears to be calibration (70%) and FALSE is test (validation) set (30%)

hake_all_2019.6.20$shortName <- apply(hake_all_2019.6.20[, 'filenames', drop = FALSE], 1, function(x) paste(get.subs(x, sep = "_")[c(2,4)], collapse = "_"))

hake_all_2019.6.20$ID <- as.numeric(strSplit(hake_all_2019.6.20$shortName, "_", elements = 2))
hake_all_2019.6.20 <- sort.f(hake_all_2019.6.20, "ID")

save(hake_all_2019.6.20, file = "hake_all_2019.6.20 6 Oct 2022.RData")




#   ==========================  Analysis  =========================================================

base::load(file = "hake_all_2019.6.20 6 Oct 2022.RData")
# hake_all_2019.6.20[1:5, c(1:3, 1112:1120, 1135:(ncol(int2_data) + ncol(haul_data) - 1))]
hake_all_2019.6.20[1:5, c(1:3, 1112:1120, 1159:ncol(hake_all_2019.6.20))]

dim(hake_all_2019.6.20)
[1] 2849 1158

len(!duplicated(hake_all_2019.6.20$Barcode))
[1] 2849

names(hake_all_2019.6.20[, 2:1114])  # Check end of wavelengths

(WaveLengths <- as.numeric(names(hake_all_2019.6.20[, 2:1113])))
len(WaveLengths)

# Plot by group
Cols <- rainbow(7)
dev.new()
pie(rep(1, length(Cols)), col = Cols)

N <- 400    # Number in random sample
dev.new(height = 8, width = 14)
plot(WaveLengths, hake_all_2019.6.20[1, 2:1113], type = 'l', ylab = 'Reflectance', col = 'black',
   ylim = c(floor(min(hake_all_2019.6.20[, 2:1113])*100)/100, ceiling(max(hake_all_2019.6.20[, 2:1113])*100)/100))
for (i in sample(2:nrow(hake_all_2019.6.20), N)) {
   lines(WaveLengths, hake_all_2019.6.20[i, 2:1113], col = Cols[hake_all_2019.6.20$GroupNum[i]])
   # cat(paste0('Row = ', i, "\n"))
   # ask()
}


names(hake_all_2019.6.20)[-(2:1113)]
 [1] "filenames"         "vessel_code"       "cruise_number"     "date_collected"    "collection_year"   "region"            "latitude"         
 [8] "longitude"         "sequence"          "length"            "weight"            "sex"               "read_age"          "test_age"         
[15] "final_age"         "readability"       "unscannable"       "broken"            "crystallized"      "other_problem"     "percent_affected" 
[22] "instrument_name"   "comments"          "misc_data"         "gear_depth"        "bottom_depth"      "Cruise"            "Barcode"          
[29] "Age"               "Species"           "SampleYear"        "Oto_Weight"        "OtolithSide"       "Processed.by"      "Shipped.Date"     
[36] "CatchDate"         "sex_determination" "fork_length"       "Notes"             "Survey"            "Haul"              "TD_Time"          
[43] "TD_Lat"            "TD_Lon"            "Avg_Bot_Depth_m"   "Avg_Gear_Depth_m"  "Group"             "GroupNum"          "Age_BB"           
[50] "Age_OPUS"         



Table(hake_all_2019.6.20$Age, hake_all_2019.6.20$Group)


# Some of the 'A' group have shifted signature in the reflectance 
plotly.Spec(hake_all_2019.6.20, 200, 'Group')

# Remove all of group A for now...
# hake_all_2019.6.20 <- hake_all_2019.6.20[!hake_all_2019.6.20$Group == 'A', ]
# Table(hake_all_2019.6.20$Group) # Check all the 'A' group was removed

plotly.Spec(hake_all_2019.6.20, 50)

plotly.Spec(hake_all_2019.6.20, 150)

plotly.Spec(hake_all_2019.6.20, 500)

plotly.Spec(hake_all_2019.6.20, 40, 'Sex')

plotly.Spec(hake_all_2019.6.20, 200, 'fork_length')

test <- plotly.Spec(hake_all_2019.6.20, 100)

Glm <- glm(Age ~ poly(Value, 4) + poly(Band, 5), data = test)
Age_Hat <- round(predict(Glm))
Age_Hat[Age_Hat < 1] <- 1
Table(test$Age, round(Age_Hat))

test <- plotly.Spec(hake_all_2019.6.20, 300, plot = FALSE)


Glm <- glm(Age ~ poly(Value, 4) + poly(Band, 5), data = test[test$Band <= 8000, ])
Age_Hat <- round(predict(Glm))
Age_Hat[Age_Hat < 1] <- 1
Table(round(Age_Hat), test[test$Band <= 8000, ]$Age)
classAgreement(Table(round(Age_Hat), test[test$Band <= 8000, ]$Age))
sum(abs(round(Age_Hat) - test[test$Band <= 8000, ]$Age))

Glm <- glm(Age ~ poly(Value, 8):poly(Band, 8), data = test[test$Band <= 8000, ])
Age_Hat <- round(predict(Glm))
Age_Hat[Age_Hat < 1] <- 1
Table(round(Age_Hat), test[test$Band <= 8000, ]$Age)
classAgreement(Table(round(Age_Hat), test[test$Band <= 8000, ]$Age))
sum(abs(round(Age_Hat) - test[test$Band <= 8000, ]$Age))


plot(WaveLengths, hake_all_2019.6.20[1, 2:1113], type = 'l')

# matplot(WaveLengths, t(hake_all_2019.6.20[1:3, 2:1113]), type = 'l')


# Plot scans changing colors by age
Cols <- rainbow(22)
dev.new()
pie(rep(1, length(Cols)), col = Cols)
 

# Only plot a random sample - including group A
N <- 100
dev.new(height = 8, width = 14)
plot(WaveLengths, hake_all_2019.6.20[1, 2:1113], type = 'l', ylab = 'Reflectance', col = Cols[hake_all_2019.6.20$Age[1]],
   ylim = c(floor(min(hake_all_2019.6.20[, 2:1113])*100)/100, ceiling(max(hake_all_2019.6.20[, 2:1113])*100)/100), lwd = 0.5)
for (i in sample(2:nrow(hake_all_2019.6.20), N)) {
   lines(WaveLengths, hake_all_2019.6.20[i, 2:1113], col = Cols[hake_all_2019.6.20$Age[i]], lwd = 0.5)
   # cat(paste0('Row = ', i, "\n"))
   # ask()
} 

  
  
# Only plot  a random sample
N <- 100
dev.new(height = 8, width = 14)
plot(WaveLengths, hake_all_2019.6.20[1, 2:1113], type = 'l', ylab = 'Reflectance', col = Cols[hake_all_2019.6.20$Age[1]],
   ylim = c(floor(min(hake_all_2019.6.20[, 2:1113])*100)/100, ceiling(max(hake_all_2019.6.20[, 2:1113])*100)/100))
for (i in sample(2:nrow(hake_all_2019.6.20), N)) {
   lines(WaveLengths, hake_all_2019.6.20[i, 2:1113] - ifelse(grepl('6A', hake_all_2019.6.20$filenames[i], 1000, 0), col = Cols[hake_all_2019.6.20$Age[i]])
   cat(paste0('Row = ', i, "\n"))
   ask()
}




# Plot all the scans by age
dev.new(height = 8, width = 14)
plot(WaveLengths, hake_all_2019.6.20[1, 2:1113], type = 'l', ylab = 'Reflectance', col = Cols[hake_all_2019.6.20$Age[1]],
   ylim = c(floor(min(hake_all_2019.6.20[, 2:1113])*100)/100, ceiling(max(hake_all_2019.6.20[, 2:1113])*100)/100))
for (i in 2:nrow(hake_all_2019.6.20))
   lines(WaveLengths, hake_all_2019.6.20[i, 2:1113], col = Cols[hake_all_2019.6.20$Age[i]])

# Two outliers are seen in the plot  - need only the 8,000 freq. less than 0.4 reflectance
dim(hake_all_2019.6.20)
hake_all_2019.6.20 <- hake_all_2019.6.20[hake_all_2019.6.20['8000'] < 0.38, ]
dim(hake_all_2019.6.20)


# Plot all the scans by age - no extreme outlies now
dev.new(height = 8, width = 14)
plot(WaveLengths, hake_all_2019.6.20[1, 2:1113], type = 'l', ylab = 'Reflectance', col = Cols[hake_all_2019.6.20$Age[1]],
   ylim = c(floor(min(hake_all_2019.6.20[, 2:1113])*100)/100, ceiling(max(hake_all_2019.6.20[, 2:1113])*100)/100))
for (i in 2:nrow(hake_all_2019.6.20))
   lines(WaveLengths, hake_all_2019.6.20[i, 2:1113], col = Cols[hake_all_2019.6.20$Age[i]])


# Models

set.seed(c(777, 747)[2])
index <- 1:nrow(hake_all_2019.6.20)
testindex <- sample(index, trunc(length(index)/2))
(AgeColNum <- grep('Age', names(hake_all_2019.6.20))[1])
# Columns <- c(2:1113, AgeColNum)  # All Wavelengths 
Columns <- c((562:1112) + 1, AgeColNum)  # Only Wavelengths between 3,600 and 8,000
testset <- hake_all_2019.6.20[testindex, Columns]
trainset <- hake_all_2019.6.20[-testindex, Columns]   


# svm using e1071 package
svm.model <- e1071::svm(Age ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[, -grep('Age', names(testset))])
Table(NIRS_SVM_AGE = round(svm.pred), TMA = testset[, grep('Age', names(testset))])   
e1071::classAgreement(Table(NIRS_SVM_AGE = round(svm.pred), TMA = testset[, grep('Age', names(testset))]))
e1071::classAgreement(Table(NIRS_SVM_AGE = round(svm.pred), TMA = testset[, grep('Age', names(testset))]), match.names = TRUE)
sum(abs(round(svm.pred)- testset[, grep('Age', names(testset))]))


# rpart
rpart.model <- rpart(Age ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[, -grep('Age', names(testset))])
Table(NIRS_RPART_AGE = round(rpart.pred), TMA = testset[, grep('Age', names(testset))])
e1071::classAgreement(Table(NIRS_RPART_AGE = round(rpart.pred), TMA = testset[, grep('Age', names(testset))]))
e1071::classAgreement(Table(NIRS_RPART_AGE = round(rpart.pred), TMA = testset[, grep('Age', names(testset))]), match.names = TRUE) 
sum(abs(round(rpart.pred) - testset[, grep('Age', names(testset))]))   
   
   
   

# Hack in OPUS age for comparison -  a look at limited data from OPUS model by  Brenna, Alica, Beverely, or Irina???????????  
names(hake_all_2019.6.20)[grep('Age', names(hake_all_2019.6.20))]
# [1] "Age"      "Age_BB"   "Age_OPUS"

(AgeColNum_OPUS <- grep('Age', names(hake_all_2019.6.20))[3])
names(hake_all_2019.6.20)[AgeColNum_OPUS]

Columns <- c((562:1112) + 1, AgeColNum, AgeColNum_OPUS)

testset_OPUS <- hake_all_2019.6.20[testindex, Columns]
dim(testset_OPUS)
# testset_OPUS <- testset_OPUS[is.finite(testset_OPUS[, ncol(testset_OPUS)]), ]
# dim(testset_OPUS)

trainset_OPUS <- hake_all_2019.6.20[-testindex, Columns]   
dim(trainset_OPUS)
# trainset_OPUS <- trainset_OPUS[is.finite(trainset_OPUS[, ncol(trainset_OPUS)]), ]
# dim(trainset_OPUS)

   
# OPUS_Age vs TMA_Age
Table(OPUS_Age = round(testset_OPUS[!is.na(testset_OPUS[, ncol(testset_OPUS)]), grep('Age', names(testset_OPUS))[2]]), 
          TMA_Age = round(testset_OPUS[!is.na(testset_OPUS[, ncol(testset_OPUS)]), grep('Age', names(testset_OPUS))[1]]))   
classAgreement(Table(OPUS_Age = round(testset_OPUS[!is.na(testset_OPUS[, ncol(testset_OPUS)]), grep('Age', names(testset_OPUS))[2]]), 
          TMA_Age = round(testset_OPUS[!is.na(testset_OPUS[, ncol(testset_OPUS)]), grep('Age', names(testset_OPUS))[1]])))
classAgreement(Table(OPUS_Age = round(testset_OPUS[!is.na(testset_OPUS[, ncol(testset_OPUS)]), grep('Age', names(testset_OPUS))[2]]), 
          TMA_Age = round(testset_OPUS[!is.na(testset_OPUS[, ncol(testset_OPUS)]), grep('Age', names(testset_OPUS))[1]])), match.names = TRUE)
sum(abs(round(testset_OPUS[!is.na(testset_OPUS[, ncol(testset_OPUS)]), grep('Age', names(testset_OPUS))[2]]) -
          round(testset_OPUS[!is.na(testset_OPUS[, ncol(testset_OPUS)]), grep('Age', names(testset_OPUS))[1]])))   
          
# OPUS_Age vs NIRS_SVM_Age
Table(OPUS_Age = round(testset_OPUS[!is.na(testset_OPUS[, ncol(testset_OPUS)]), grep('Age', names(testset_OPUS))[2]]), 
          NIRS_SVM_Age = round(svm.pred[!is.na(testset_OPUS[, ncol(testset_OPUS)])]))   
classAgreement(Table(OPUS_Age = round(testset_OPUS[!is.na(testset_OPUS[, ncol(testset_OPUS)]), grep('Age', names(testset_OPUS))[2]]), 
          NIRS_SVM_Age = round(svm.pred[!is.na(testset_OPUS[, ncol(testset_OPUS)])])))
classAgreement(Table(OPUS_Age = round(testset_OPUS[!is.na(testset_OPUS[, ncol(testset_OPUS)]), grep('Age', names(testset_OPUS))[2]]), 
          NIRS_SVM_Age = round(svm.pred[!is.na(testset_OPUS[, ncol(testset_OPUS)])])), match.names = TRUE)
sum(abs(round(testset_OPUS[!is.na(testset_OPUS[, ncol(testset_OPUS)]), grep('Age', names(testset_OPUS))[2]]) -
          round(svm.pred[!is.na(testset_OPUS[, ncol(testset_OPUS)])])))   

          
# =============== iPLSR - following Jordan =========================
     
base::load("W:\\ALL_USR\\JRW\\SIDT\\2019 Hake\\hake_all_2019.6.20 6 Oct 2022.RData")
source("W:\\ALL_USR\\JRW\\SIDT\\2019 Hake\\plotly.Spec.R")
      
      
Hake_Spectra_2019 <- hake_all_2019.6.20[, (562:1112) + 1] #spectra matrix
Hake_Age_2019 <- as.numeric(hake_all_2019.6.20$Age) #Vector of Ages       
Hake_Age_2019.fac <- factor(Hake_Age_2019)      


################################
###Quick view data in plotly ###
################################

hake_all_2019.6.20$ID <- as.numeric(subStr(hake_all_2019.6.20$shortName, "_", elements = 2))
hake_all_2019.6.20 <- sort.f(hake_all_2019.6.20, "ID")
hake_all_2019.6.20[1:4, c(1:2, 1160:1164)]
# #                            filenames     12488 GroupNum Age_BB Age_OPUS shortName ID
# # 1 PACIFIC_HAKE_BMS201906206A_1_OD1.0 0.2087611        1      1   0.9209    HAKE_1  1
# # 2 PACIFIC_HAKE_BMS201906206A_2_OD1.0 0.1907212        1      1   0.1541    HAKE_2  2
# # 3 PACIFIC_HAKE_BMS201906206A_3_OD1.0 0.1880061        1     NA       NA    HAKE_3  3


# plotly.Spec(hake_all_2019.6.20, WaveRange = c(0, Inf))

plotly.Spec(hake_all_2019.6.20)

plotly.Spec(hake_all_2019.6.20, 500)

plotly.Spec(hake_all_2019.6.20, NULL, reverse = TRUE)

plotly.Spec(rbind(hake_all_2019.6.20[hake_all_2019.6.20$ID <= 150, ], hake_all_2019.6.20[hake_all_2019.6.20$ID > 200 & hake_all_2019.6.20$ID < 300, ]), NULL)
plotly.Spec(hake_all_2019.6.20[hake_all_2019.6.20$ID <= 150, ], NULL) 
plotly.Spec(hake_all_2019.6.20[hake_all_2019.6.20$ID > 150, ], NULL) 
# plotly.Spec(hake_all_2019.6.20[hake_all_2019.6.20$ID >= 151 & hake_all_2019.6.20$ID < 700, ], NULL) 

cbind(140:150, hake_all_2019.6.20[140:150, 'ID' ]) # 2 Oties missing below 150 ID
plotly.Spec(hake_all_2019.6.20, N_Samp = 150, randomAfterSampNum = 148)
plotly.Spec(hake_all_2019.6.20, N_Samp = 0, randomAfterSampNum = 148)

dev.new()
plot(hake_all_2019.6.20$ID, hake_all_2019.6.20$Age)
dev.new()
plot(hake_all_2019.6.20$ID, hake_all_2019.6.20$Oto_Weight)
dev.new()
plot(hake_all_2019.6.20$ID, hake_all_2019.6.20$fork_length)

plot(hake_all_2019.6.20$latitude, hake_all_2019.6.20$Age)
"Oto_Weight"        "OtolithSide"       "Processed.by"      "Shipped.Date"      "CatchDate"        
[37] "sex_determination" "fork_length"  



plot(jitter(hake_all_2019.6.20$Age), jitter(hake_all_2019.6.20$latitude, 100), ylim = c(31, 48.5))
points(jitter(hake_all_2019.6.20$Age[hake_all_2019.6.20$ID <= 100]), jitter(hake_all_2019.6.20$latitude[hake_all_2019.6.20$ID <= 150], 100), col = 'red', pch = 16)
points(jitter(hake_all_2019.6.20$Age[hake_all_2019.6.20$ID %in% c(15, 113, 122)]), hake_all_2019.6.20$latitude[hake_all_2019.6.20$ID %in% c(15, 113, 122)], col = 'green', pch = 16) 


dev.new()
# imap(latrange = c(26, 49), longrange = c(-128, -113), zoom = FALSE)
imap(latrange = c(34, 36), longrange = c(-122, -120), zoom = FALSE)
TF <- hake_all_2019.6.20$ID > 150
points(jitter(hake_all_2019.6.20$longitude[TF], 200), jitter(hake_all_2019.6.20$latitude[TF], 20))
TF <- hake_all_2019.6.20$ID <= 150
points(jitter(hake_all_2019.6.20$longitude[TF], 1), jitter(hake_all_2019.6.20$latitude[TF], 1), col = 'red')
TF <- hake_all_2019.6.20$ID %in% c(15, 113)
points(hake_all_2019.6.20$longitude[TF], hake_all_2019.6.20$latitude[TF], col = 'green', pch =16) # 2 year olds in green with last peak  < 5,000
TF <- hake_all_2019.6.20$ID %in% 122
points(hake_all_2019.6.20$longitude[TF], hake_all_2019.6.20$latitude[TF], col = 'dodger blue', pch =16) # 1 year olds in Dodger blue with last peak  < 5,000
TF <- hake_all_2019.6.20$ID %in% c(6, 12, 20)
set.seed(c(747, 787)[2])
points(jitter(hake_all_2019.6.20$longitude[TF], 0.05), jitter(hake_all_2019.6.20$latitude[TF], 0.05), col = 'cyan', pch = 16) # 2 year olds in red with lat peak > 5,000


#  Otie weight
dev.new()
TF <- hake_all_2019.6.20$ID > 150
plot(hake_all_2019.6.20$ID[TF], hake_all_2019.6.20$Oto_Weight[TF], xlim = c(-5, 3000), na.rm = TRUE)
TF <- hake_all_2019.6.20$ID <= 150
points(hake_all_2019.6.20$ID[TF], hake_all_2019.6.20$Oto_Weight[TF], col = 'red')
TF <- hake_all_2019.6.20$ID %in% c(15, 113)
points(hake_all_2019.6.20$ID[TF], hake_all_2019.6.20$Oto_Weight[TF], col = 'green', pch =16) # 2 year olds in green
cbind(hake_all_2019.6.20$ID[TF], hake_all_2019.6.20$Oto_Weight[TF])  # Missing otie weight on #113
TF <- hake_all_2019.6.20$ID %in% 122
points(hake_all_2019.6.20$ID[TF], hake_all_2019.6.20$Oto_Weight[TF], col = 'dodger blue', pch =16) #1 year olds in Dodger blue


# Fork length
dev.new()
TF <- hake_all_2019.6.20$ID > 150
plot(hake_all_2019.6.20$ID[TF], hake_all_2019.6.20$fork_length[TF], ylim = c(16.5, 73), xlim = c(-5, 3000), na.rm = TRUE)
TF <- hake_all_2019.6.20$ID <= 150
points(hake_all_2019.6.20$ID[TF], hake_all_2019.6.20$fork_length[TF], col = 'red')
TF <- hake_all_2019.6.20$ID %in% c(15, 113)
points(hake_all_2019.6.20$ID[TF], hake_all_2019.6.20$fork_length[TF], col = 'green', pch =16) # 2 year olds in green
cbind(hake_all_2019.6.20$ID[TF], hake_all_2019.6.20$fork_length[TF])  # Missing otie weight on #113
TF <- hake_all_2019.6.20$ID %in% 122
points(hake_all_2019.6.20$ID[TF], hake_all_2019.6.20$fork_length[TF], col = 'dodger blue', pch =16) #1 year olds in Dodger blue


 
# Find names of all metadata columns 
names(hake_all_2019.6.20[, -(2:(sum(!is.na(as.numeric(names(hake_all_2019.6.20)))) + 1))])

Table(hake_all_2019.6.20$broken) # 98 oties are broken
hake_all_2019.6.20[hake_all_2019.6.20$broken == 1, "ID"]

Table(hake_all_2019.6.20$crystallized)
hake_all_2019.6.20[hake_all_2019.6.20$crystallized == 1, "ID"]



hake_all_2019.6.20[hake_all_2019.6.20$ID %in% c(595, 1083), -(2:(sum(!is.na(as.numeric(names(hake_all_2019.6.20)))) + 1))]

plotly.Spec(hake_all_2019.6.20, nrow(hake_all_2019.6.20))



###################################################################################################################
### Perform Savitzky-Golay 1st derivative with 17 point window smoothing 2rd order polynomial fit and visualize ###
###################################################################################################################
### NOTE ### If there are multiple years of data, all subsequent transformations should be applied to the whole data set, then re-subset
          

Hake_Spectra_2019.sg <- data.frame(prospectr::savitzkyGolay(Hake_Spectra_2019, p = 2, w = 15, m = 1)) 
Hake_Spectra_2019.Age.sg <- data.frame(Age = Hake_Age_2019, prospectr::savitzkyGolay(Hake_Spectra_2019, p = 2, w = 15, m = 1))  

# add Age back to the spectra matrix

# pca <- stats::prcomp(Hake_Spectra_2019.sg, scale = TRUE)
# summary(pca)
# Hake_Spectra_2019.scores <- data.frame(pca$x)
#  
# PCA <- data.frame(Hake_Age_2019.fac, Hake_Spectra_2019.scores) 
# 
# s.class(PCA[,2:3],
#         fac = Hake_Age_2019.fac,  # color by groups
#         col = c("black", "dark red", "red","dark orange", "green", "dark green", "blue", "dark blue", "purple", "brown"),
#         grid = FALSE,
#         label = levels(Hake_Age_2019.fac),
#         cellipse = 2, #2 standard deviations (95% conf interval)
#         clabel = 1.5,
#         cpoint = 1
# )          
# 

# PCA  and CCA, vegan package from :   https://stackoverflow.com/questions/13936051/adding-ellipses-to-a-principal-component-analysis-pca-plot

# --- pca (rda) and cca (same result) ---
prin_comp.pca <- vegan::rda(Hake_Spectra_2019, scale = TRUE)
dev.new()
plot(prin_comp.pca)

prin_comp.pca.sg <- vegan::rda(Hake_Spectra_2019.sg, scale = TRUE)
pca_scores.sg <- vegan::scores(prin_comp.pca.sg)

dev.new()
plot(prin_comp.pca.sg)

dev.new()
plot(prin_comp.pca.sg, xlim=c(-21, 5), ylim=c(-8, 8))

dev.new()
plot(pca_scores.sg$sites[,1], pca_scores.sg$sites[,2], pch=21, bg = rainbow(30), xlim=c(-3, 3), ylim=c(-6, 2))
   
arrows(0,0,pca_scores.sg$species[,1],pca_scores.sg$species[,2],lwd=1,length=0.2)
 

Hake_Spectra_2019 <- hake_all_2019.6.20[, (562:1112) + 1] # Spectra matrix
Hake_Meta_2019 <- hake_all_2019.6.20[, -((1:1112) + 1)] # Meta data  
Hake_Meta_2019$Lat.1.degree <- round(Hake_Meta_2019$latitude) 
names(Hake_Meta_2019)
 [1] "filenames"         "vessel_code"       "cruise_number"     "date_collected"    "collection_year"   "region"           
 [7] "latitude"          "longitude"         "sequence"          "length"            "weight"            "sex"              
[13] "read_age"          "test_age"          "final_age"         "readability"       "unscannable"       "broken"           
[19] "crystallized"      "other_problem"     "percent_affected"  "instrument_name"   "comments"          "misc_data"        
[25] "gear_depth"        "bottom_depth"      "Cruise"            "Barcode"           "Age"               "Species"          
[31] "SampleYear"        "Oto_Weight"        "OtolithSide"       "Processed.by"      "Shipped.Date"      "CatchDate"        
[37] "sex_determination" "fork_length"       "Notes"             "Survey"            "Haul"              "TD_Time"          
[43] "TD_Lat"            "TD_Lon"            "Avg_Bot_Depth_m"   "Avg_Gear_Depth_m"  "Group"             "GroupNum"         
[49] "Age_BB"            "Age_OPUS"          "shortName"         "Lat.1.degree" 


# Hake.2019.cca <- cca(Hake_Spectra_2019 ~ Age, data=Hake_Meta_2019)
# Hake.2019.cca <- cca(Hake_Spectra_2019 ~ Age + fork_length, data=Hake_Meta_2019)
Hake_Meta_2019$Avg_Bot_Depth_m <- as.numeric(Hake_Meta_2019$Avg_Bot_Depth_m)
TF <- !(is.na(Hake_Meta_2019$Age) | is.na(Hake_Meta_2019$fork_length) | is.na(Hake_Meta_2019$Oto_Weight) | is.na(Hake_Meta_2019$sex) |
              is.na(Hake_Meta_2019$Lat.1.degree) | is.na(Hake_Meta_2019$weight) | is.na(Hake_Meta_2019$Avg_Bot_Depth_m))
sum(TF)

Hake.2019.cca <- cca(Hake_Spectra_2019[TF, ] ~ Age + Lat.1.degree + fork_length + Oto_Weight + sex  + weight + Avg_Bot_Depth_m, 
                     data=Hake_Meta_2019[TF, c('Age', 'fork_length', 'Oto_Weight', 'sex', 'Lat.1.degree', 'weight', 'Avg_Bot_Depth_m')])
# Hake.2019.cca
summary(Hake.2019.cca)

dev.new()
plotCCA(Hake.2019.cca, Col = 'green') 

dev.new()
plotCCA(Hake.2019.cca, display = c("sp", "wa", "cn", "lc", "bp", "reg")[c(3)], Col = 'green') 
 
dev.new()
plotCCA(Hake.2019.cca, display = c("sp", "wa", "cn", "lc", "bp", "reg")[c(1:4)], Col = 'dodger blue') 
 

dev.new()
par(mfrow = c(3,2))
plotCCA(Hake.2019.cca, c(1, 2))
plotCCA(Hake.2019.cca, c(1, 3))
plotCCA(Hake.2019.cca, c(1, 4))
plotCCA(Hake.2019.cca, c(2, 3))    
plotCCA(Hake.2019.cca, c(2, 4)) 
plotCCA(Hake.2019.cca, c(3, 4)) 

dev.new()
plot(Hake.2019.cca, display = c("sp", "wa", "cn", "lc", "bp", "reg")[c(3)])
 
 
plotCCA(Hake.2019.cca, Col = 'green') 
 

Hake.2019.cca <- cca(Hake_Spectra_2019[TF, ] ~ Age + sex, 
                     data=Hake_Meta_2019[TF, c('Age', 'fork_length', 'Oto_Weight', 'sex', 'Lat.1.degree', 'weight')])
summary(Hake.2019.cca)
 
Hake.2019.cca <- cca(Hake_Spectra_2019[TF, ] ~ sex + Lat.1.degree + fork_length + Oto_Weight + Age + weight, 
                     data=Hake_Meta_2019[TF, c('Age', 'fork_length', 'Oto_Weight', 'sex', 'Lat.1.degree', 'weight')])
summary(Hake.2019.cca) 

# ------------------------------------------------------------------------------------------------------------------------       

# calculate proportional variance for each PC
PoV <- pca$sdev^2/sum(pca$sdev^2)

#Screen Plot of proportional variances
dev.new()
par(mar =c(5,5,1,1))
plot(PoV[1:20], ylab = "Variance Explained",  xlab = "Principle Components")
lines(PoV[1:20])   
 
 
####################################################
###  iPLS algorithm in mdatools  ### 
####################################################
 
Hake_Spectra_2019.iPLS.F <- mdatools::ipls(Hake_Spectra_2019.sg, Hake_Age_2019, glob.ncomp = 10, center = T, scale = T, cv = 100,
                  int.ncomp = 10, int.num = 10, ncomp.selcrit = "min", method = "forward", silent = F)
save(Hake_Spectra_2019.iPLS.F, file = 'Hake_Spectra_2019.iPLS.F 13 Oct 2022.RData')              

summary(Hake_Spectra_2019.iPLS.F)

dev.new()
plot(Hake_Spectra_2019.iPLS.F)     

dev.new()
plot(Hake_Spectra_2019.iPLS.F, main = NULL)          


# plot predictions before and after selection
dev.new()
par(mfrow = c(2, 1))
mdatools::plotPredictions(Hake_Spectra_2019.iPLS.F$gm)
mdatools::plotPredictions(Hake_Spectra_2019.iPLS.F$om)

dev.new()
mdatools::plotRMSE(Hake_Spectra_2019.iPLS.F)

# RMSE  before and after selection
dev.new()
par(mfrow = c(2, 1))
mdatools::plotRMSE(Hake_Spectra_2019.iPLS.F$gm, ylim = c(-0.2, 3))
mdatools::plotRMSE(Hake_Spectra_2019.iPLS.F$om, ylim = c(-0.2, 3))



# plot the newly selected spectra regions
Hake_Spectra_2019.iPLS.vars <- Hake_Spectra_2019.iPLS.F$var.selected
(p <- length(Hake_Spectra_2019.iPLS.vars))

Hake_Spectra_2019.sg.iPLS <- data.frame(Hake_Spectra_2019.sg[, Hake_Spectra_2019.iPLS.vars])
Hake_Spectra_2019.Age.sg.iPLS <- data.frame(Age = Hake_Age_2019, Hake_Spectra_2019.sg.iPLS)
dim(Hake_Spectra_2019.Age.sg.iPLS)

#Split by age - Age.0.avg, Age.1.avg, ...
for (i in 0:17) { 
    Subset <- subset(Hake_Spectra_2019.Age.sg.iPLS, Hake_Age_2019 == i)
    if(i == 0) cat("\n")
    cat("Dim of", paste0('Age', i), " = ", dim(Subset), "\n")
    assign(paste0('Age', i, '.avg'), apply(Subset, 2, mean))  # Character or numeric works  in subset()
    cat("Length of", paste0('Age', i, '.avg'), " = ", length(eval(parse(text = paste0('Age', i, '.avg')))), "\n\n")
}


### rbind, transpose and plot the averaged spectra matrix
age.avg <- NULL
for ( i in 0:17)
     age.avg <- rbind(age.avg, eval(parse(text = paste0('Age', i, '.avg'))))

dim(age.avg)
age.avg[1:3, 1:5]
age.avg[1:3, (ncol(age.avg) - 3):ncol(age.avg)]
p + 1 # vars plus age 

t.age.avg <- data.frame(t(age.avg[, -1])) # remove the age column so there is only an xmatrix
dim(t.age.avg)
t.age.avg[1:3, 1:5]


#plot the transformed spectra
dev.new(width = 14, height = 8)
par(mar=c(5,5,1,1))
plot(t.age.avg$X10, xlab = "Wavenumber Range", ylab = "Absorbance", cex.lab = 1, type = 'n', 
            ylim = c(min(t.age.avg, na.rm = TRUE) - abs(min(t.age.avg, na.rm = TRUE) * 0.025), max(t.age.avg, na.rm = TRUE) + abs(max(t.age.avg, na.rm = TRUE) * 0.025)))
Cols <- rainbow(22)
for ( i in 0:17)
    lines(t.age.avg[[paste0('X', i)]],  col = Cols[i + 1], lwd = 2)
legend("topleft", legend = paste(0:17, "Years"), col = c('red', 'blue', Cols[1:18]), lty=1, lwd = 2,cex=.8)



#########################################
### Conduct PLSr on iPLSr selected data ###
#########################################


PLSr <- mdatools::pls(Hake_Spectra_2019.sg.iPLS, Hake_Age_2019, ncomp = 10, center = T, scale = F, cv = 100,
            method = "simpls", alpha = 0.05, ncomp.selcrit = "min")
summary(PLSr)
dev.new()
plot(PLSr)



set.seed(c(777, 747)[2])
index <- 1:nrow(Hake_Spectra_2019.sg.iPLS)
testindex <- sample(index, trunc(length(index)/3))
x.testset <- Hake_Spectra_2019.sg.iPLS[testindex, ]
x.trainset <- Hake_Spectra_2019.sg.iPLS[-testindex, ]   
y.test <- Hake_Age_2019[testindex]
y.train <- Hake_Age_2019[-testindex]

# PLSr <- mdatools::pls(x.trainset, y.train, ncomp = 10, center = T, scale = F, cv = 100,
#           method = "simpls", alpha = 0.05, ncomp.selcrit = "min", x.test = x.testset, y.test = y.test)

PLSr <- mdatools::pls(x.trainset, y.train, ncomp = 10, center = T, scale = F, cv = 100,
            method = "simpls", alpha = 0.05, ncomp.selcrit = "min")            
summary(PLSr)
dev.new()
plot(PLSr)


###########################
### Plot the PLSr results ###
###########################

# pull the prediction values of age from the PLSr object
compNum <- length(PLSr$res$cal$slope) # Number of selected components
Predicted_Age <- data.frame(PLSr$cvres$y.pred[ , , 1])[, compNum]

# Reference Ages
Reference_Age <- PLSr$cvres$y.ref  # Equals y.train

# Plot Predicted vs Reference Ages
(Slope <- PLSr$res$cal$slope[length(PLSr$res$cal$slope)]) #  Slope from PLSr

dev.new()
par(mfrow = c(2,1))
# Decimal Predicted age 
# dev.new(height = 8, width = 15)
par(mar = c(5,5,1,1))
plot(Reference_Age, Predicted_Age, ylim = c(0,12), xlim = c(0,12), col = "dark blue")
abline(0, 1, lwd = 2)
abline(0, Slope, col = "dark blue", lwd = 2) 


# Integer Predicted age with reference age jitter
# dev.new(height = 8, width = 15)
par(mar = c(5,5,1,1))
plot(jitter(Reference_Age), round(Predicted_Age), ylim = c(0,12), xlim = c(0,12), col = "dark blue")
abline(0, 1, lwd = 2)
abline(0, Slope, col = "dark blue", lwd = 2) 

summary(lm(Predicted_Age ~ Reference_Age))$r.squared
summary(lm(round(Predicted_Age) ~ Reference_Age))$r.squared

Table(NIRS_PLSr_AGE = round(Predicted_Age), TMA = Reference_Age)
e1071::classAgreement(Table(NIRS_PLSr_AGE = round(Predicted_Age), TMA = Reference_Age)) # $diag  0.1677193
e1071::classAgreement(Table(NIRS_PLSr_AGE = round(Predicted_Age), TMA = Reference_Age), match.names = TRUE) # diag 0.5859649

sum(abs(Reference_Age - round(Predicted_Age))) 

(Results <- data.frame(Reference_Age, Predicted_Age))[1:10, ]

# Store results in an excel.csv file
write.csv(Results, file ="10_smoothing_iPLSR_Res.csv", row.names = FALSE)


#  ----------  Support Vector Machine and RPART --------------------
lib(caret)
lib(gmodels)

setwd("W:/ALL_USR/JRW/SIDT/2019 Hake")
base::load(file = "hake_all_2019.6.20 6 Oct 2022.RData")
index <- 1:nrow(hake_all_2019.6.20)
    
# set.seed(c(777, 747)[2])
# testindex <- sample(index, trunc(length(index)/3))

# Use Hake_OPUS test and train sets
#  is.na(hake_all_2019.6.20$Age_OPUS) == TRUE appears to be calibration (70%) and FALSE is test (validation) set (30%)
testindex <- index[!is.na(hake_all_2019.6.20$Age_OPUS)]
length(testindex)/length(index)


(AgeColNum <- grep('Age', names(hake_all_2019.6.20))[1])
# Columns <- 2:1113 # All Wavelengths 
Columns.set <- (562:1112) + 1  # Only Wavelengths between 3,600 and 8,000
x.testset <- hake_all_2019.6.20[testindex, Columns.set]
x.trainset <- hake_all_2019.6.20[-testindex, Columns.set]   
y.test <- hake_all_2019.6.20[testindex, AgeColNum]
y.train <- hake_all_2019.6.20[-testindex, AgeColNum]   



#  ## In PLS the "test set" creates the model????????????????????????????????????????
#  #
#  ## svm using e1071 package
#  #svm.model <- e1071::svm(y.test ~ ., data = x.testset, cost = 100, gamma = 1)
#  #svm.model
#  #svm.pred <- predict(svm.model, x.trainset)
#  #Table(NIRS_SVM_AGE = round(svm.pred), TMA = y.train)   
#  #e1071::classAgreement(Table(NIRS_SVM_AGE = round(svm.pred), TMA = y.train)) #  $diag   0.3855337
#  #e1071::classAgreement(Table(NIRS_SVM_AGE = round(svm.pred), TMA = y.train) , match.names = TRUE) #  $diag   0.3855337
#  #sum(abs(round(svm.pred)- y.train)) # 1432
#  #
#  #
#  ## rpart
#  #rpart.model <- rpart::rpart(y.test ~ ., data = x.testset)
#  #rpart.model
#  #rpart.pred <- predict(rpart.model, x.trainset)
#  #Table(NIRS_RPART_AGE = round(rpart.pred), TMA = y.train)   
#  #e1071::classAgreement(Table(NIRS_RPART_AGE = round(rpart.pred), TMA = y.train)) # $diag 0.4424157
#  #e1071::classAgreement(Table(NIRS_RPART_AGE = round(rpart.pred), TMA = y.train) , match.names = TRUE) # $diag 0.3735955
#  #sum(abs(round(rpart.pred)- y.train)) # 1434


confusionMatrix(


# svm using e1071 package
svm.model <- e1071::svm(y.train ~ ., data = x.trainset, cost = 100, gamma = 1)
svm.model
svm.pred <- predict(svm.model, x.testset)
Table(NIRS_SVM_AGE = round(svm.pred), TMA = y.test)   
e1071::classAgreement(Table(NIRS_SVM_AGE = round(svm.pred), TMA = y.test)) #  $diag   0.3855337
e1071::classAgreement(Table(NIRS_SVM_AGE = round(svm.pred), TMA = y.test) , match.names = TRUE) #  $diag   0.3855337

caret::confusionMatrix(rbind(Table(NIRS_SVM_AGE = round(svm.pred), TMA = y.test), "14" = rep(0, 16), "15" = rep(0, 16), "17" = rep(0, 16)))

gmodels::CrossTable(round(svm.pred), y.test)
sum(abs(round(svm.pred)- y.test)) # 1432


# Hake_OPUS 
sum(is.na(hake_all_2019.6.20$Age_OPUS[testindex])) # 0
len(hake_all_2019.6.20$Age_OPUS[testindex])
len(y.test)
Table(NIRS_OPUS_AGE = round(hake_all_2019.6.20$Age_OPUS[testindex]), TMA = y.test)   
e1071::classAgreement(Table(NIRS_OPUS_AGE = round(hake_all_2019.6.20$Age_OPUS[testindex]), TMA = y.test)) 
e1071::classAgreement(Table(NIRS_OPUS_AGE = round(hake_all_2019.6.20$Age_OPUS[testindex]), TMA = y.test)  , match.names = TRUE) 

NIRS_OPUS_AGE <- round(hake_all_2019.6.20$Age_OPUS[testindex])
NIRS_OPUS_AGE[NIRS_OPUS_AGE < 1] <- 1
Table(NIRS_OPUS_AGE = NIRS_OPUS_AGE, TMA = y.test)  
caret::confusionMatrix(rbind(Table(NIRS_OPUS_AGE = NIRS_OPUS_AGE, TMA = y.test), "17" = rep(0, 16)))

sum(abs(NIRS_OPUS_AGE - y.test)) 




svm.pred.train <- predict(svm.model)
Table(NIRS_SVM_AGE = round(svm.pred.train), TMA = y.train)   
e1071::classAgreement(Table(NIRS_SVM_AGE = round(svm.pred.train), TMA = y.train)) 
e1071::classAgreement(Table(NIRS_SVM_AGE = round(svm.pred.train), TMA = y.train), match.names = TRUE) 
sum(abs(round(svm.pred.train)- y.train)) 


# rpart
rpart.model <- rpart::rpart(y.train ~ ., data = x.trainset)
rpart.model
rpart.pred <- predict(rpart.model, x.testset)
Table(NIRS_RPART_AGE = round(rpart.pred), TMA = y.test)   
e1071::classAgreement(Table(NIRS_RPART_AGE = round(rpart.pred), TMA = y.test)) # $diag 0.4424157
e1071::classAgreement(Table(NIRS_RPART_AGE = round(rpart.pred), TMA = y.test) , match.names = TRUE) # $diag 0.3735955
sum(abs(round(rpart.pred)- y.test)) # 1434






























# ================================ PLS  using plantspec package  - TOO OLD!!!!!!!!!!!!!=====================================================


# Select the component of interest (i.e., Age)
  TMA_Age <- hake_all_2019.6.20$Age 
  
# Tried this first........  
  
# Optimize the preprocessing for spectra and spectral subsetting to emphasize
# the important spectra features related to predicting N. This can be very time
# consuming.
  N_opt <- optimizePLS(component = TMA_Age, 
                       spectra = hake_all_2019.6.20[, (562:1112) + 1], 
                       training_set = !testindex)
  
# Fit our PLS regression using the optimal setting from our optimization.
  N_cal <- calibrate(component = TMA_Age, 
                     spectra = hake_all_2019.6.20[, (562:1112) + 1], 
                     optimal_params = N_opt, 
                     optimal_model = 1, # In N_opt, use the best model
                     validation = "testset", 
                     training_set = testindex)

# -------------------------------------  
  

# Select a training set for model fitting (i.e., FALSE values for test set)
#  - use "!" because subdivideDataset() returns TRUE for test set selections.
#  - use "type = 'validation'" so both training and test data are representative. 
  training_set_MDKS <- !(subdivideDataset(spectra = hake_all_2019.6.20[, (562:1112) + 1], 
                                          component = TMA_Age, 
                                          method = "MDKS", 
                                          p = 0.5, # 10% for this example  
                                          type = "validation")) 

# Optimize the preprocessing for spectra and spectral subsetting to emphasize
# the important spectra features related to predicting N. This can be very time
# consuming.
  N_opt <- optimizePLS(component = TMA_Age, 
                       spectra = hake_all_2019.6.20[, (562:1112) + 1], 
                       training_set = training_set_MDKS)
  
# Fit our PLS regression using the optimal setting from our optimization.
  N_cal <- calibrate(component = TMA_Age, 
                     spectra = hake_all_2019.6.20[, (562:1112) + 1], 
                     optimal_params = N_opt, 
                     optimal_model = 1, # In N_opt, use the best model
                     validation = "testset", 
                     training_set = training_set_MDKS)










