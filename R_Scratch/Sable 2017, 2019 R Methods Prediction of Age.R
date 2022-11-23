
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
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")

lib(openxlsx)
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
PATH <- "W:/ALL_USR/JRW/SIDT/Sablefish/"
setwd(PATH) # set working directory to folder containing spectral files
getwd()


(listspc.1 <- dir('2017 NWFSC Combo', pattern = '*SABLEFISH*', full.names = TRUE))[1:20] # creates the list of all the .0 file names in the directory
length(listspc.1) #  828

(listspc.2 <- dir('2019 NWFSC Combo', pattern = '*SABLEFISH*', full.names = TRUE))[1:20] # creates the list of all the .0 file names in the directory
length(listspc.2) #  798

listspc <- c(listspc.1, listspc.2)
length(listspc)

ldf <- list() # creates an empty list
for (k in 1:length(listspc)) { # loops through and uploads each file using read_opus_bin_univ()  from the simplerspec package
       temp_spec <- simplerspec::read_opus_bin_univ(listspc[k], extract = "spc", print_progress = TRUE, atm_comp_minus4offset = FALSE)
       if(any(grepl('Error', temp_spec[[1]])))
           next
       ldf[[as.character(k)]] <- temp_spec
}

(N <- length(ldf)) # 1601

names(ldf[[1]]) # Names of first element
##  [1] "metadata"          "spc"               "spc_nocomp"        "sc_sm"             "sc_rf"             "ig_sm"             "ig_rf"             "wavenumbers"      
##  [9] "wavenumbers_sc_sm" "wavenumbers_sc_rf"


str(ldf[[1]]) # check first element

ldf[[1]]$wavenumbers[1:20] # 8th Element

save(ldf, file = 'ldf.RData')
base::load(file = 'ldf.RData') 

# Interpolate
# prospectr::resample()  uses spline interpolation method
sum(ldf[[1]]$wavenumbers - ldf[[N]]$wavenumbers) # 0
testdf <- (prospectr::resample(X = ldf[[1]]$spc), wav = ldf[[1]]$wavenumbers, new.wav = ldf[[N]]$wavenumbers))

# plot to test
plot(as.vector(ldf[[1]]$wavenumbers), as.vector(ldf[[1]]$spc))
points(as.vector(ldf[[2]]$wavenumbers), as.vector(ldf[[2]]$spc), col = 'red')
points(as.vector(ldf[[N]]$wavenumbers), as.vector(ldf[[N]]$spc), col = 'blue')
points(as.vector(ldf[[N]]$wavenumbers), as.vector(testdf), col = 'green') 


ldf_int <- matrix(data = NA, nrow = length(ldf), ncol = length(ldf[[N]]$wavenumbers)) #make empty matrix for loop
dim(ldf_int) # 1601 1154

# Resample()  all the wave lengths if needed
for (j in 1:length(ldf)){ 
  bar(j, length(ldf))
  ldf_int[j,] <- prospectr::resample(X = ldf[[j]]$spc, wav = ldf[[j]]$wavenumbers, new.wav = ldf[[N]]$wavenumbers)
}

# Fill ldf_int with spec if no resampling is needed
for (j in 1:length(ldf)){ 
  bar(j, length(ldf))
  ldf_int[j,] <- unlist(ldf[[j]]$spc)
}


colnames(ldf_int) <- round(ldf[[N]]$wavenumbers)
dat_spc <- as.data.frame(ldf_int)
dim(dat_spc) #  1601 1154

dat_spc[1:10, 1:5]

# Add file names back in, could add other variables here as well (age etc.)
# metadat <- sapply(rdat,'[[', 1)
# (filenames <- unlist(metadat[2,]))[1:10]

(filenames <- unlist(sapply(ldf, function(x) x[[1]][2]), use.names = FALSE))[1:10]
(dat <- cbind(filenames, dat_spc))[1:5, c(1:5, 1150:1155)]

#   #  # Extract Year 
#   #  Year <- function(x) {
#   #     Subs <- get.subs(x, sep = "_")
#   #     substr(Subs[2], 6, 9)
#   #  }   
#   #  dat$Year <- as.numeric(apply(dat[, 'filenames', drop = FALSE], 1, Year))
#   #  Table(dat$Year)
#   #  
#   #  
#   #  # Extract Sequence number
#   #  dat$Sequence <- as.numeric(apply(dat[, 'filenames', drop = FALSE], 1, function(x) get.subs(x, sep = "_")[3]))
#   #  Table(dat$Sequence)[1:10]
#   #  
#   #  dat <- sort.f(dat, c("Year", "Sequence"))
#   #  dat[1:10, 1:4]

save(dat, file = 'dat 18 Oct 2022.RData')

base::load("dat 18 Oct 2022.RData")


# #   # ADD VESSEL, CRUISE, REGION, LOCATION,  BIO, and AGE metadata
# #   meta_data_2017 <- read.xlsx(paste0(PATH, "FT_NIR_NWFSC_Combo_2017_SABL.xlsx")) #load in ancillary data
# #   names(meta_data_2017)[grep('age_structure_weigth_g', names(meta_data_2017))] <- 'age_structure_weight_g'
# #   meta_data_2017$Year <- 2017
# #   Table(meta_data_2017$Year)
# #   
# #   meta_data_2019 <- read.xlsx(paste0(PATH, "FT_NIR_NWFSC_Combo_2019_SABL.xlsx")) 
# #   meta_data_2019$age_structure_weight_g <- NA
# #   meta_data_2019$Year <- 2019
# #   Table(meta_data_2019$Year)
# #   
# #   # [1:5, c(1:3, 1112:1120, 1135:(ncol(dat) + ncol(scan_data) - 1))]
# #   Sable_2017_2019 <- left_join(dat, rbind(meta_data_2017, meta_data_2019), by = c("Year" = "Year", "Sequence" = "specimen"))
# #   #  Sable_2017_2019 <- match.f(dat, rbind(meta_data_2017, meta_data_2019), c("Year", "Sequence"), c("Year","specimen"))


# Using Meredith's meta files with full oty names
meta_data_2017_2019 <- read.xlsx(paste0(PATH, "NWFSC_Combo_2017_2019_SABL_ID_Age_Biodata_NIRname_Data.xlsx")) # Load in ancillary data
Sable_2017_2019 <- left_join(dat, meta_data_2017_2019, by = c("filenames" = "NIR_file_name"))

(Oty_Issues <- Sable_2017_2019[Sable_2017_2019$unscannable == TRUE | Sable_2017_2019$broken == TRUE | Sable_2017_2019$crystallized == TRUE |
                Sable_2017_2019$other_problem == TRUE | !is.na(Sable_2017_2019$comments), 
                c('filenames', 'unscannable', 'broken', 'crystallized', 'other_problem', 'percent_affected', 'comments')])

write.xlsx(Oty_Issues, "Sable 2017 2019 Oty Issues.xlsx")


dim(Sable_2017_2019)
[1] 1601 1181

len(unique(Sable_2017_2019$Barcode))
[1] 1601

sum(is.na(Sable_2017_2019$Barcode))
[1] 0



# #    # ADD Sable_OPUS.RData
# #    load('Sable_OPUS.RData') 
# #    Sable_OPUS[1:5, ]
# #    names(Sable_OPUS)[2] <- "filenames"
# #    Sable_OPUS[1:5, ]
# #    
# #    base::load(file = "Sable_2017_2019 6 Oct 2022.RData")
# #    preJoinNumCol <- ncol(Sable_2017_2019)
# #    (Sable_2017_2019 <- left_join(Sable_2017_2019, Sable_OPUS[, 2:4], by = "filenames"))[1:5, c(1:3, 1112:1120, 1155:(preJoinNumCol + ncol(Sable_OPUS[, 2:4]) - 1))]
# #    
# #    for( i in 1:nrow(Sable_OPUS)) # No match for row 714.  ID = 1239
# #       cat("\n", i, Sable_2017_2019$filenames[grep(Sable_OPUS[i,2],  Sable_2017_2019$filenames)])
# #    
# #    grep('PACIFIC_HAKE_BMS201906206D_1239_OD1.0',  Sable_2017_2019$filenames)
# #    integer(0)
# #    
# #    
# #    Table(is.na(Sable_2017_2019$Age_OPUS))
# #    FALSE  TRUE 
# #      854  1995 
# #    
# #    # With missing row:
# #     855/(855 + 1995)
# #    [1] 0.3
# #      
# #    # is.na(Sable_2017_2019$Age_OPUS) == TRUE appears to be calibration and FALSE is test (validation) set
# #    



# Create 'shortName' with Species, Year, and Number from 'filenames'
Sp_Year_Num_Extract <- function(x) {
   Subs <- get.subs(x, sep = "_")
   Subs[2] <- substr(Subs[2], 6, 9)
   paste(Subs[1:3], collapse = "_")
}
Sable_2017_2019$shortName <- apply(Sable_2017_2019[, 'filenames', drop = FALSE], 1, Sp_Year_Num_Extract)

# #    # Extract Year
# #    Year_Extract <- function(x) {
# #       Subs <- get.subs(x, sep = "_")
# #       substr(Subs[2], 6, 9)
# #    }
# #    Sable_2017_2019$Year <- as.numeric(apply(Sable_2017_2019[, 'filenames', drop = FALSE], 1, Year_Extract))
# #    Table(Sable_2017_2019$Year)

# Extract Year & Group
Year_Group <- function(x) {
   Subs <- get.subs(x, sep = "_")
   Subs[1] <- substr(Subs[2], 6, 9)
   Subs[2] <- substr(Subs[2], 15, 15)
   paste(Subs[1:2], collapse = "_")
}   
Sable_2017_2019$scanGroup <- apply(Sable_2017_2019[, 'filenames', drop = FALSE], 1, Year_Group)
Table(Sable_2017_2019$scanGroup)

# Extract ID ********** New strSplit() which takes vectors ***********
options(digits = 11)
Sable_2017_2019$ID <- JRWToolBox::strSplit(Sable_2017_2019$shortName, sep = "_", elements = 2:3, decimal_factor = 10000)
Sable_2017_2019$ID[1:10]
Sable_2017_2019 <- sort.f(Sable_2017_2019, "ID")

names(Sable_2017_2019)[grep('fish_age', names(Sable_2017_2019))] <- "TMA"

save(Sable_2017_2019, file = "Sable_2017_2019 21 Nov 2022.RData")

base::load(file = "Sable_2017_2019 21 Nov 2022.RData")

options(digits = 11)
Sable_2017_2019[1:2, c(1:2, 1153:1184)]
                          filenames         12490          3610          3603          3595 age_structure_id   Barcode Year
1 SABLEFISH_COMBO201701203A_2_OD1.0 0.42185163498 0.60868144035 0.60703516006 0.60346448421 102118142-SABL-O 102118142 2017
2 SABLEFISH_COMBO201701203A_3_OD1.0 0.40333580971 0.62216740847 0.62038969994 0.61744445562 102118143-SABL-O 102118143 2017
  cruise_number vessel_code haul date_collected     region common_name    scientific_name specimen    latitude     longitude length
1     201703008           8    1   42875.587859 NE Pacific   Sablefish Anoplopoma fimbria        2 45.12444444 -124.29611111    320
2     201703008           8    1   42875.587859 NE Pacific   Sablefish Anoplopoma fimbria        3 45.12444444 -124.29611111    550
  weight sex TMA       session_title unscannable broken crystallized other_problem percent_affected instrument_name comments
1    280   2   1 NIR_COMBO201701203A           0      0            0             0               NA   DEMO_MPA_AFSC     <NA>
2   1540   2   4 NIR_COMBO201701203A           0      0            0             0               NA   DEMO_MPA_AFSC     <NA>
     timestamp        shortName scanGroup        ID
1 44552.522917 SABLEFISH_2017_2    2017_A 2017.0002
2 44552.523611 SABLEFISH_2017_3    2017_A 2017.0003


# ------------------------- Looking at the data ------------------------------------------------------------------------

# 2D  
# Look at the data with plotly.Spec()
plotly.Spec(Sable_2017_2019, 600)
plotly.Spec(Sable_2017_2019, 300, alpha = c(1, rep(0.15, length(unique(Sable_2017_2019$TMA)))))
plotly.Spec(Sable_2017_2019[Sable_2017_2019$TMA <= 1, ], 'all', alpha = c(1, rep(0.15, length(unique(Sable_2017_2019$TMA)))))

plotly.Spec(Sable_2017_2019, 'all')

# scanGroup as facet variable shows the last scan of the last group were elevated  - laser going out on demo MPA???
plotly.Spec(Sable_2017_2019, 300, colorGroup = 'TMA', facetGroup = 'scanGroup')
plotly.Spec(Sable_2017_2019, 'all', colorGroup = 'TMA', facetGroup = 'scanGroup')

# Remove extreme scans
# Most of the last scans starting from 2019.0851 have elevated frequencies - 41 scans in total 
Sable_2017_2019[Sable_2017_2019[, '4004'] > 0.7, "ID"]   

dim(Sable_2017_2019)    
Sable_2017_2019_noEx <- Sable_2017_2019[Sable_2017_2019[, '4004'] < 0.7, ]    
dim(Sable_2017_2019_noEx)  
 
# TMA  with extremes removed
plotly.Spec(Sable_2017_2019_noEx, 300)
plotly.Spec(Sable_2017_2019_noEx[Sable_2017_2019_noEx$TMA <= 1, ], 'all', alpha = c(1, rep(0.15, length(unique(Sable_2017_2019_noEx$TMA))))) # ***** Best to show age zeros

plotly.Spec(Sable_2017_2019_noEx[Sable_2017_2019_noEx$TMA <= 1, ], 'all',  facetGroup = 'scanGroup', alpha = c(1, rep(0.15, length(unique(Sable_2017_2019_noEx$TMA)))))

plotly.Spec(Sable_2017_2019_noEx, 'all')
plotly.Spec(Sable_2017_2019_noEx, 300, colorGroup = 'TMA', facetGroup = 'scanGroup')
plotly.Spec(Sable_2017_2019_noEx, 'all', colorGroup = 'TMA', facetGroup = 'scanGroup')


# 3D
# TMA  with extremes included
d <- plotly.Spec(Sable_2017_2019, 'all', colorGroup = 'TMA', facetGroup = 'scanGroup', plot = FALSE)
d %>% plot_ly(x = ~Band, y = ~Value, z = ~Scan) %>% group_by(Scan) %>% add_lines(color = ~TMA, colors = rainbow(length(unique(d$Scan)))) 
d %>% plot_ly(x = ~Band, y = ~Value, z = ~Scan) %>% group_by(Scan) %>% add_lines(color = ~TMA, colors = rainbow(length(unique(d$TMA)))) 

# TMA ( without group_by(Scan)  you don't get single lines for each scan)
d <- plotly.Spec(Sable_2017_2019[!is.na(Sable_2017_2019$TMA), ], 'all', colorGroup = 'TMA', plot = FALSE)
d %>% plot_ly( x = ~Band, y = ~Value, z = ~Scan) %>% group_by(Scan) %>% add_lines(color = ~TMA, colors = rainbow(length(unique(d$Scan)))) 

# scanGroup as colors variable shows the last scan of the last group were elevated  - laser going out on demo MPA???
plotly.Spec(Sable_2017_2019[!is.na(Sable_2017_2019$scanGroup), ], 'all', colorGroup = 'scanGroup', plot = FALSE)
d %>% plot_ly(d, x = ~Band, y = ~Value, z = ~Scan) %>% group_by(Scan) %>% add_lines(color = ~scanGroup, colors = rainbow(length(unique(d$Scan)))) 

# Subset with 'shortName' as color group - mostly a test of plotly() and the browser
d <- plotly.Spec(Sable_2017_2019[!is.na(Sable_2017_2019$shortName), ], 400, colorGroup = 'shortName', plot = FALSE)
d %>% plot_ly(x = ~Band, y = ~Value, z = ~Scan) %>% group_by(Scan) %>% add_lines(color = ~shortName, colors = rainbow(length(unique(d$Scan)))) 



# 3D with extremes removed
# TMA
d <- plotly.Spec(Sable_2017_2019_noEx[!is.na(Sable_2017_2019_noEx$TMA), ], 'all', colorGroup = 'TMA', plot = FALSE) 
d %>% plot_ly(x = ~Band, y = ~Value, z = ~Scan) %>% group_by(Scan) %>% add_lines(color = ~TMA, colors = rainbow(length(unique(d$Scan)))) 

# Below does NOT work to add a label to the  color axis
d %>% plot_ly(x = ~Band, y = ~Value, z = ~Scan) %>% group_by(Scan) %>% add_lines(color = ~TMA, colors = rainbow(length(unique(d$Scan)))) %>% 
       layout(coloraxis = list(title = 'TMA'))  # Nor this:   layout(scene = list(coloraxis = list(title = 'TMA')))   

# Making TMA a numeric no longer sorts by the discrete color variable       
d$TMA <- as.numeric(d$TMA)
d %>% plot_ly(x = ~Band, y = ~Value, z = ~Scan) %>% group_by(Scan) %>% add_lines(color = ~TMA, colors = rainbow(length(unique(d$Scan)))) 

   
# --- 3D using rgl package - no labels for lines ---
lib(rgl)
d <- na.omit(d)
d <- sort.f(d, "TMA") # Sorting didn't help order below
change(d)
# plot3d(Band, Value, TMA, type = 'p', col = rainbow(length(Scan)/length(unique(Scan))))
# plot3d(Band, Value, as.numeric(factor(Scan)), type = 'p', col = rep(rainbow(length(unique(Scan))), each = 572))
Col <- rainbow(max(as.numeric(TMA)) + 1)
plot3d(Band, Value, as.numeric(factor(Scan)), type = 'p', col = Col[as.numeric(TMA) + 1])
plot3d(Band, Value, as.numeric(factor(Scan)), type = 'l', col = Col[as.numeric(TMA) + 1])
plot3d(Band, Value, TMA, type = 'p', col = Col[as.numeric(TMA) + 1], pch = 0.25)
plot3d(Band, Value, TMA, type = 'l', col = Col[as.numeric(TMA) + 1], pch = 0.25)

# 3D using rgl package with multi-figures
d$Scan <- as.numeric(factor(d$Scan))
d <- sort.f(d, "TMA")
change(d)
TMA_Levels <- unique(d$TMA)
Col <- rainbow(length(TMA_Levels))

mfrow3d(4, 4, sharedMouse = TRUE)
# for (i in 1:length(TMA_Levels)) {
for (i in 1:16) {
  change(d[d$TMA %in% TMA_Levels[i], ], verbose = FALSE)
  plot3d(Band, Value, Scan, type = 'p', col = Col[i], main = paste0("TMA = ", TMA_Levels[i]))
}  
 

mfrow3d(5, 5, sharedMouse = TRUE)
# for (i in 1:length(TMA_Levels)) {
for (i in 1:25) {
  change(d[d$TMA %in% TMA_Levels[i], ], verbose = FALSE)
  plot3d(Band, Value, Scan, type = 'p', col = Col[i], main = paste0("TMA = ", TMA_Levels[i]))
}  
 
#----------------------------------------------------------------------------------------------------------------------------------------------


# Correlation with extremes removed
Sable_Spec_Cor <- renum(data.frame(Freq = as.numeric(names(Sable_2017_2019_noEx[, 2:1155])), Cor = cor(Sable_2017_2019_noEx[, 2:1155], Sable_2017_2019_noEx$TMA, use = "na.or.complete")))
sort.f(Sable_Spec_Cor, 'Cor', rev = T)[1:5,]
  Freq       Cor
1 4081 0.7233784
2 4073 0.7233294
3 4089 0.7229155
4 4066 0.7228655
5 4058 0.7220165

dev.new()
plot(Sable_Spec_Cor$Freq, Sable_Spec_Cor$Cor)

dev.new()
plot(Sable_2017_2019_noEx[, '4081'], Sable_2017_2019_noEx$TMA)

dev.new()
plot(jitter(Sable_2017_2019_noEx[, '4081']), jitter(Sable_2017_2019_noEx$TMA))

cor(Sable_2017_2019_noEx[, '4081'], Sable_2017_2019_noEx$TMA,  use = "na.or.complete")
[1] 0.7233784


# Use correlation plot to define the best freg. areas to use

Sable_Spec_Cor[c(936, 1148), ]
     Freq           Cor
936  5277 0.46227826962
1148 3641 0.46627681593

d <- plotly.Spec(Sable_2017_2019[!is.na(Sable_2017_2019$age_structure_weight_g), ], 'all', colorGroup = 'TMA', facetGroup = 'scanGroup', plot = FALSE)
dim(d)

# WB = Wide Best area from correlation plot
Sable_WB <- d[d$Band >= 3641 & d$Band <= 5277, ]
dim(Sable_WB)

Sable_WB %>% plot_ly(x = ~Band, y = ~Value, z = ~Scan) %>% group_by(Scan) %>% add_lines(color = ~TMA, colors = rainbow(length(unique(d$Scan)))) 









# ------------------------------------------------------------------------------------------------------

LM <- lm(Sable_2017_2019_noEx$TMA ~ Sable_2017_2019_noEx[, '4081'])
summary(LM)
abline(LM$coef, col = 'red')



TF <- !is.na(Sable_2017_2019_noEx$TMA)


summary(GLM1 <- glm(Sable_2017_2019_noEx$TMA[TF] ~ poly(Sable_2017_2019_noEx[TF, '4081'], 2)))
points(Sable_2017_2019_noEx[TF, '4081'], predict(GLM1), col = 'magenta')

summary(GLM2 <- glm(Sable_2017_2019_noEx$TMA[TF] ~ poly(Sable_2017_2019_noEx[TF, '4081'], 2)))
points(Sable_2017_2019_noEx[TF, '4081'], predict(GLM2), col = 'green')

summary(GLM3 <- glm(Sable_2017_2019_noEx$TMA[TF] ~ poly(Sable_2017_2019_noEx[TF, '4081'], 3)))
points(Sable_2017_2019_noEx[TF, '4081'], predict(GLM3), col = 'blue')

summary(GLM4 <- glm(Sable_2017_2019_noEx$TMA[TF] ~ poly(Sable_2017_2019_noEx[TF, '4081'], 4)))
points(Sable_2017_2019_noEx[TF, '4081'], predict(GLM4), col = 'cyan')

summary(GLM5 <- glm(Sable_2017_2019_noEx$TMA[TF] ~ poly(Sable_2017_2019_noEx[TF, '4081'], 5)))
points(Sable_2017_2019_noEx[TF, '4081'], predict(GLM5), col = 'dodgerblue')


# Log
dim(Sable_2017_2019_noEx)
TF <- !is.na(Sable_2017_2019_noEx$TMA)
sum(TF)

Sable_2017_2019_noEx$TMA[Sable_2017_2019_noEx$TMA == 0] <- 0.5

dev.new()
# plot(Sable_2017_2019_noEx[, '4081'], log(Sable_2017_2019_noEx$TMA))
plot(Sable_2017_2019_noEx[, '4081'], Sable_2017_2019_noEx$TMA)

summary(GLM1 <- glm(log(Sable_2017_2019_noEx$TMA[TF]) ~ Sable_2017_2019_noEx[TF, '4081']))
# points(Sable_2017_2019_noEx[TF, '4081'], predict(GLM1), col = 'magenta')
points(Sable_2017_2019_noEx[TF, '4081'], exp(predict(GLM1)), col = 'magenta')

summary(GLM1.ll <- glm(Sable_2017_2019_noEx$TMA[TF] ~ Sable_2017_2019_noEx[TF, '4081']), link = 'log')
# points(Sable_2017_2019_noEx[TF, '4081'], predict(GLM1), col = 'magenta')
points(Sable_2017_2019_noEx[TF, '4081'], predict(GLM1.ll, type = 'response'), col = 'red')





summary(GLM2 <- glm(log(Sable_2017_2019_noEx$TMA[TF]) ~ poly(Sable_2017_2019_noEx[TF, '4081'], 2)))
points(Sable_2017_2019_noEx[TF, '4081'], predict(GLM2), col = 'green')

summary(GLM3 <- glm(log(Sable_2017_2019_noEx$TMA[TF]) ~ poly(Sable_2017_2019_noEx[TF, '4081'], 3)))
points(Sable_2017_2019_noEx[TF, '4081'], predict(GLM3), col = 'blue')

summary(GLM4 <- glm(log(Sable_2017_2019_noEx$TMA[TF]) ~ poly(Sable_2017_2019_noEx[TF, '4081'], 4)))
points(Sable_2017_2019_noEx[TF, '4081'], predict(GLM4), col = 'cyan')

summary(GLM5 <- glm(log(Sable_2017_2019_noEx$TMA[TF]) ~ poly(Sable_2017_2019_noEx[TF, '4081'], 5)))
points(Sable_2017_2019_noEx[TF, '4081'], predict(GLM5), col = 'dodgerblue')














# Sex
plotly.Spec(Sable_2017_2019_noEx, 300, colorGroup = 'sex')
plotly.Spec(Sable_2017_2019_noEx[Sable_2017_2019_noEx$sex %in% 1:2, ], 300, facetGroup = 'sex')


# age_structure_weight_g
plotly.Spec(Sable_2017_2019[!is.na(Sable_2017_2019$age_structure_weight_g), ], 'all', colorGroup = 'age_structure_weight_g', contColorVar = TRUE)

d <- plotly.Spec(Sable_2017_2019[!is.na(Sable_2017_2019$age_structure_weight_g), ], 'all', colorGroup = 'age_structure_weight_g', contColorVar = TRUE, facetGroup = 'scanGroup')
# d <- plotly.Spec(Sable_2017_2019[!is.na(Sable_2017_2019$age_structure_weight_g), ], 'all', colorGroup = 'age_structure_weight_g', facetGroup = 'scanGroup', plot = FALSE)

# d$age_structure_weight_g <- as.character(round(as.numeric(d$age_structure_weight_g) * 100 + 1))

Table(d$age_structure_weight_g)
str(d)
d[1:4,]
               Scan Band     Value age_structure_weight_g scanGroup
1 SABLEFISH_2017_2 8000 0.4512354                 0.0092    2017_A
2 SABLEFISH_2017_2 7992 0.4513096                 0.0092    2017_A
3 SABLEFISH_2017_2 7985 0.4513483                 0.0092    2017_A
4 SABLEFISH_2017_2 7977 0.4514019                 0.0092    2017_A

ggplotly(ggplot(data = d, aes(x = Band, y = Value, z = Scan)) + geom_line(aes(colour = age_structure_weight_g), size = 0.2))
ggplotly(ggplot(data = d, aes(x = Band, y = Value, z = Scan)) + geom_line(aes(colour = age_structure_weight_g), size = 0.2) + facet_wrap(~ scanGroup))
ggplotly(ggplot(data = d, aes(x = Band, y = Value, z = Scan)) + geom_line(aes(colour = age_structure_weight_g), size = 0.2) + facet_grid(rows = vars(scanGroup)))


# 3D
d <- plotly.Spec(Sable_2017_2019[!is.na(Sable_2017_2019$age_structure_weight_g), ], 'all', colorGroup = 'age_structure_weight_g', facetGroup = 'scanGroup', plot = FALSE)
plot_ly(d, x = ~Band, y = ~Value, z = ~Scan) %>% group_by(Scan) %>% add_lines(color = ~age_structure_weight_g, colors = rainbow(length(unique(d$Scan)))) 

  

# Latitude 
Sable_2017_2019_noEx$Lat_Fac <- factor(round(Sable_2017_2019_noEx$latitude))
plotly.Spec(Sable_2017_2019_noEx, 'all', facetGroup = 'Lat_Fac')


# Length
plotly.Spec(Sable_2017_2019_noEx[!is.na(Sable_2017_2019_noEx$length), ], 300, colorGroup = 'length', contColorVar = TRUE)
plotly.Spec(Sable_2017_2019_noEx[!is.na(Sable_2017_2019_noEx$length), ], 300, colorGroup = 'length', contColorVar = TRUE, facetGroup = 'Lat_Fac')

plotly.Spec(Sable_2017_2019_noEx[!is.na(Sable_2017_2019_noEx$length) & !Sable_2017_2019_noEx$TMA %in% 0:1, ], 300, colorGroup = 'length', contColorVar = TRUE, facetGroup = 'Lat_Fac')

# Check some scatter plots
plot(Sable_2017_2019_noEx$TMA, Sable_2017_2019_noEx$length)
plot(Sable_2017_2019_noEx$TMA, Sable_2017_2019_noEx$age_structure_weight_g)


# Too many colors for Lattice and no interactivity 
d <- plotly.Spec(Sable_2017_2019[!is.na(Sable_2017_2019$age_structure_weight_g), ], 100, colorGroup = 'age_structure_weight_g', facetGroup = 'scanGroup', plot = FALSE)
d$age_structure_weight_g <- round(as.numeric(d$age_structure_weight_g) * 100 + 1)

pdf('test.pdf', height = 10, width = 14)
xyplot(Value ~ Band | scanGroup, group = Scan, line.col = rainbow(max(d$age_structure_weight_g))[d$age_structure_weight_g], type = 'l', data = d, auto = TRUE)
dev.off()
browseURL('test.pdf')









 

plotly.Spec(Sable_2017_2019_noEx, 300, colorGroup = 'scanGroup')

plotly.Spec(Sable_2017_2019_noEx, 300, colorGroup = 'scanGroup', facetGroup = 'TMA')
plotly.Spec(Sable_2017_2019_noEx, 300, colorGroup = 'TMA', facetGroup = 'TMA')




plotly.Spec(Sable_2017_2019, 100, colorGroup = 'Year')
plotly.Spec(Sable_2017_2019, 600, colorGroup = 'Year')

plotly.Spec(Sable_2017_2019, 200, colorGroup = 'scanGroup')
plotly.Spec(Sable_2017_2019, 600, colorGroup = 'scanGroup')
    
plotly.Spec(Sable_2017_2019, 200)
plotly.Spec(Sable_2017_2019, 'All')

plotly.Spec(Sable_2017_2019[Sable_2017_2019$Year %in% 2017, ], 300,  ylim = c(0.35, 0.70), main = "2017")
plotly.Spec(Sable_2017_2019[Sable_2017_2019$Year %in% 2019, ], 300, ylim = c(0.35, 0.70), main = "2019")

plotly.Spec(Sable_2017_2019[Sable_2017_2019$Year %in% 2017, ], 300, colorGroup = 'scanGroup', ylim = c(0.35, 0.70))
plotly.Spec(Sable_2017_2019[Sable_2017_2019$Year %in% 2019, ], 300, colorGroup = 'scanGroup', ylim = c(0.35, 0.70))
plotly.Spec(Sable_2017_2019[Sable_2017_2019$Year %in% 2019, ], 'all', colorGroup = 'scanGroup')



# Figure out colors  qqplot
base::load(file = "Sable_2017_2019 7 Nov 2022.RData")
d_all <- plotly.Spec(Sable_2017_2019, 400, colorGroup = 'TMA', facetGroup = 'scanGroup', plot = FALSE)
# d_all$TMA <- factor(d_all$TMA)
print(ggplotly(ggplot(data = d_all, aes(x = Band, y = Value, z = Scan)) + geom_line(aes(colour = TMA), size = 0.2, colors = rainbow(length(unique(d_all$TMA))))))

print(ggplotly(ggplot(data = d_all, aes(x = Band, y = Value, z = Scan)) + geom_line(aes(colour = TMA), size = 0.2) + scale_color_manual(values=rainbow(length(unique(d_all$TMA))))))




p <- ggplot(data = d_all, aes(x = Band, y = Value, z = Scan)) + geom_line(aes(colour = Age), size = 0.2) + facet_wrap(~ scanGroup)
ggplotly(p)




#   ==========================  Analysis  =========================================================

base::load(file = "Sable_2017_2019 7 Nov 2022.RData")
# Sable_2017_2019[1:5, c(1:3, 1112:1120, 1135:(ncol(int2_data) + ncol(haul_data) - 1))]

dim(Sable_2017_2019)
[1] 1601 1157

Sable_2017_2019[1:5, c(1:3, 1112:1120, 1153:ncol(Sable_2017_2019))]

len(!duplicated(Sable_2017_2019$Barcode))
[1] 2849

names(Sable_2017_2019[, 2:1114])  # Check end of wavelengths

(WaveLengths <- as.numeric(names(Sable_2017_2019[, 2:1155])))
len(WaveLengths)

# Plot by group
Cols <- rainbow(7)
dev.new()
pie(rep(1, length(Cols)), col = Cols)

N <- 400    # Number in random sample
dev.new(height = 8, width = 14)
plot(WaveLengths, Sable_2017_2019[1, 2:1113], type = 'l', ylab = 'Reflectance', col = 'black',
   ylim = c(floor(min(Sable_2017_2019[, 2:1113])*100)/100, ceiling(max(Sable_2017_2019[, 2:1113])*100)/100))
for (i in sample(2:nrow(Sable_2017_2019), N)) {
   lines(WaveLengths, Sable_2017_2019[i, 2:1113], col = Cols[Sable_2017_2019$GroupNum[i]])
   # cat(paste0('Row = ', i, "\n"))
   # ask()
}


names(Sable_2017_2019)[-(2:1113)]
 [1] "filenames"         "vessel_code"       "cruise_number"     "date_collected"    "collection_year"   "region"            "latitude"         
 [8] "longitude"         "sequence"          "length"            "weight"            "sex"               "read_age"          "test_age"         
[15] "final_age"         "readability"       "unscannable"       "broken"            "crystallized"      "other_problem"     "percent_affected" 
[22] "instrument_name"   "comments"          "misc_data"         "gear_depth"        "bottom_depth"      "Cruise"            "Barcode"          
[29] "Age"               "Species"           "SampleYear"        "Oto_Weight"        "OtolithSide"       "Processed.by"      "Shipped.Date"     
[36] "CatchDate"         "sex_determination" "fork_length"       "Notes"             "Survey"            "Haul"              "TD_Time"          
[43] "TD_Lat"            "TD_Lon"            "Avg_Bot_Depth_m"   "Avg_Gear_Depth_m"  "Group"             "GroupNum"          "Age_BB"           
[50] "Age_OPUS"         



Table(Sable_2017_2019$Age, Sable_2017_2019$Group)


# Some of the 'A' group have shifted signature in the reflectance 
plotly.Spec(Sable_2017_2019, 200, 'scanGroup')

# Remove all of group A for now...
# Sable_2017_2019 <- Sable_2017_2019[!Sable_2017_2019$Group == 'A', ]
# Table(Sable_2017_2019$Group) # Check all the 'A' group was removed

plotly.Spec(Sable_2017_2019, 50)

plotly.Spec(Sable_2017_2019, 150)

plotly.Spec(Sable_2017_2019, 500)

plotly.Spec(Sable_2017_2019, 40, 'Sex')

plotly.Spec(Sable_2017_2019, 200, 'fork_length')

test <- plotly.Spec(Sable_2017_2019, 100)

Glm <- glm(Age ~ poly(Value, 4) + poly(Band, 5), data = test)
Age_Hat <- round(predict(Glm))
Age_Hat[Age_Hat < 1] <- 1
Table(test$Age, round(Age_Hat))

test <- plotly.Spec(Sable_2017_2019, 300, plot = FALSE)


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


plot(WaveLengths, Sable_2017_2019[1, 2:1113], type = 'l')

# matplot(WaveLengths, t(Sable_2017_2019[1:3, 2:1113]), type = 'l')


# Plot scans changing colors by age
Cols <- rainbow(22)
dev.new()
pie(rep(1, length(Cols)), col = Cols)
 

# Only plot a random sample - including group A
N <- 100
dev.new(height = 8, width = 14)
plot(WaveLengths, Sable_2017_2019[1, 2:1113], type = 'l', ylab = 'Reflectance', col = Cols[Sable_2017_2019$Age[1]],
   ylim = c(floor(min(Sable_2017_2019[, 2:1113])*100)/100, ceiling(max(Sable_2017_2019[, 2:1113])*100)/100), lwd = 0.5)
for (i in sample(2:nrow(Sable_2017_2019), N)) {
   lines(WaveLengths, Sable_2017_2019[i, 2:1113], col = Cols[Sable_2017_2019$Age[i]], lwd = 0.5)
   # cat(paste0('Row = ', i, "\n"))
   # ask()
} 

  
  
# Only plot  a random sample
N <- 100
dev.new(height = 8, width = 14)
plot(WaveLengths, Sable_2017_2019[1, 2:1113], type = 'l', ylab = 'Reflectance', col = Cols[Sable_2017_2019$Age[1]],
   ylim = c(floor(min(Sable_2017_2019[, 2:1113])*100)/100, ceiling(max(Sable_2017_2019[, 2:1113])*100)/100))
for (i in sample(2:nrow(Sable_2017_2019), N)) {
   lines(WaveLengths, Sable_2017_2019[i, 2:1113] - ifelse(grepl('6A', Sable_2017_2019$filenames[i], 1000, 0), col = Cols[Sable_2017_2019$Age[i]])
   cat(paste0('Row = ', i, "\n"))
   ask()
}




# Plot all the scans by age
dev.new(height = 8, width = 14)
plot(WaveLengths, Sable_2017_2019[1, 2:1113], type = 'l', ylab = 'Reflectance', col = Cols[Sable_2017_2019$Age[1]],
   ylim = c(floor(min(Sable_2017_2019[, 2:1113])*100)/100, ceiling(max(Sable_2017_2019[, 2:1113])*100)/100))
for (i in 2:nrow(Sable_2017_2019))
   lines(WaveLengths, Sable_2017_2019[i, 2:1113], col = Cols[Sable_2017_2019$Age[i]])

# Two outliers are seen in the plot  - need only the 8,000 freq. less than 0.4 reflectance
dim(Sable_2017_2019)
Sable_2017_2019 <- Sable_2017_2019[Sable_2017_2019['8000'] < 0.38, ]
dim(Sable_2017_2019)


# Plot all the scans by age - no extreme outlies now
dev.new(height = 8, width = 14)
plot(WaveLengths, Sable_2017_2019[1, 2:1113], type = 'l', ylab = 'Reflectance', col = Cols[Sable_2017_2019$Age[1]],
   ylim = c(floor(min(Sable_2017_2019[, 2:1113])*100)/100, ceiling(max(Sable_2017_2019[, 2:1113])*100)/100))
for (i in 2:nrow(Sable_2017_2019))
   lines(WaveLengths, Sable_2017_2019[i, 2:1113], col = Cols[Sable_2017_2019$Age[i]])


# Models

set.seed(c(777, 747)[2])
index <- 1:nrow(Sable_2017_2019)
testindex <- sample(index, trunc(length(index)/2))
(AgeColNum <- grep('Age', names(Sable_2017_2019))[1])
# Columns <- c(2:1113, AgeColNum)  # All Wavelengths 
Columns <- c((562:1112) + 1, AgeColNum)  # Only Wavelengths between 3,600 and 8,000
testset <- Sable_2017_2019[testindex, Columns]
trainset <- Sable_2017_2019[-testindex, Columns]   


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
names(Sable_2017_2019)[grep('Age', names(Sable_2017_2019))]
# [1] "Age"      "Age_BB"   "Age_OPUS"

(AgeColNum_OPUS <- grep('Age', names(Sable_2017_2019))[3])
names(Sable_2017_2019)[AgeColNum_OPUS]

Columns <- c((562:1112) + 1, AgeColNum, AgeColNum_OPUS)

testset_OPUS <- Sable_2017_2019[testindex, Columns]
dim(testset_OPUS)
# testset_OPUS <- testset_OPUS[is.finite(testset_OPUS[, ncol(testset_OPUS)]), ]
# dim(testset_OPUS)

trainset_OPUS <- Sable_2017_2019[-testindex, Columns]   
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
     
base::load("W:\\ALL_USR\\JRW\\SIDT\\2019 Sable\\Sable_2017_2019 6 Oct 2022.RData")
source("W:\\ALL_USR\\JRW\\SIDT\\2019 Sable\\plotly.Spec.R")
      
      
Sable_Spectra_2019 <- Sable_2017_2019[, (562:1112) + 1] #spectra matrix
Sable_Age_2019 <- as.numeric(Sable_2017_2019$Age) #Vector of Ages       
Sable_Age_2019.fac <- factor(Sable_Age_2019)      


################################
###Quick view data in plotly ###
################################

Sable_2017_2019$ID <- as.numeric(subStr(Sable_2017_2019$shortName, "_", elements = 2))
Sable_2017_2019 <- sort.f(Sable_2017_2019, "ID")
Sable_2017_2019[1:4, c(1:2, 1160:1164)]
# #                            filenames     12488 GroupNum Age_BB Age_OPUS shortName ID
# # 1 PACIFIC_HAKE_BMS201906206A_1_OD1.0 0.2087611        1      1   0.9209    HAKE_1  1
# # 2 PACIFIC_HAKE_BMS201906206A_2_OD1.0 0.1907212        1      1   0.1541    HAKE_2  2
# # 3 PACIFIC_HAKE_BMS201906206A_3_OD1.0 0.1880061        1     NA       NA    HAKE_3  3


# plotly.Spec(Sable_2017_2019, WaveRange = c(0, Inf))

plotly.Spec(Sable_2017_2019)

plotly.Spec(Sable_2017_2019, 500)

plotly.Spec(Sable_2017_2019, NULL, reverse = TRUE)

plotly.Spec(rbind(Sable_2017_2019[Sable_2017_2019$ID <= 150, ], Sable_2017_2019[Sable_2017_2019$ID > 200 & Sable_2017_2019$ID < 300, ]), NULL)
plotly.Spec(Sable_2017_2019[Sable_2017_2019$ID <= 150, ], NULL) 
plotly.Spec(Sable_2017_2019[Sable_2017_2019$ID > 150, ], NULL) 
# plotly.Spec(Sable_2017_2019[Sable_2017_2019$ID >= 151 & Sable_2017_2019$ID < 700, ], NULL) 

cbind(140:150, Sable_2017_2019[140:150, 'ID' ]) # 2 Oties missing below 150 ID
plotly.Spec(Sable_2017_2019, N_Samp = 150, randomAfterSampNum = 148)
plotly.Spec(Sable_2017_2019, N_Samp = 0, randomAfterSampNum = 148)

dev.new()
plot(Sable_2017_2019$ID, Sable_2017_2019$Age)
dev.new()
plot(Sable_2017_2019$ID, Sable_2017_2019$Oto_Weight)
dev.new()
plot(Sable_2017_2019$ID, Sable_2017_2019$fork_length)

plot(Sable_2017_2019$latitude, Sable_2017_2019$Age)
"Oto_Weight"        "OtolithSide"       "Processed.by"      "Shipped.Date"      "CatchDate"        
[37] "sex_determination" "fork_length"  



plot(jitter(Sable_2017_2019$Age), jitter(Sable_2017_2019$latitude, 100), ylim = c(31, 48.5))
points(jitter(Sable_2017_2019$Age[Sable_2017_2019$ID <= 100]), jitter(Sable_2017_2019$latitude[Sable_2017_2019$ID <= 150], 100), col = 'red', pch = 16)
points(jitter(Sable_2017_2019$Age[Sable_2017_2019$ID %in% c(15, 113, 122)]), Sable_2017_2019$latitude[Sable_2017_2019$ID %in% c(15, 113, 122)], col = 'green', pch = 16) 


dev.new()
# imap(latrange = c(26, 49), longrange = c(-128, -113), zoom = FALSE)
imap(latrange = c(34, 36), longrange = c(-122, -120), zoom = FALSE)
TF <- Sable_2017_2019$ID > 150
points(jitter(Sable_2017_2019$longitude[TF], 200), jitter(Sable_2017_2019$latitude[TF], 20))
TF <- Sable_2017_2019$ID <= 150
points(jitter(Sable_2017_2019$longitude[TF], 1), jitter(Sable_2017_2019$latitude[TF], 1), col = 'red')
TF <- Sable_2017_2019$ID %in% c(15, 113)
points(Sable_2017_2019$longitude[TF], Sable_2017_2019$latitude[TF], col = 'green', pch =16) # 2 year olds in green with last peak  < 5,000
TF <- Sable_2017_2019$ID %in% 122
points(Sable_2017_2019$longitude[TF], Sable_2017_2019$latitude[TF], col = 'dodger blue', pch =16) # 1 year olds in Dodger blue with last peak  < 5,000
TF <- Sable_2017_2019$ID %in% c(6, 12, 20)
set.seed(c(747, 787)[2])
points(jitter(Sable_2017_2019$longitude[TF], 0.05), jitter(Sable_2017_2019$latitude[TF], 0.05), col = 'cyan', pch = 16) # 2 year olds in red with lat peak > 5,000


#  Otie weight
dev.new()
TF <- Sable_2017_2019$ID > 150
plot(Sable_2017_2019$ID[TF], Sable_2017_2019$Oto_Weight[TF], xlim = c(-5, 3000), na.rm = TRUE)
TF <- Sable_2017_2019$ID <= 150
points(Sable_2017_2019$ID[TF], Sable_2017_2019$Oto_Weight[TF], col = 'red')
TF <- Sable_2017_2019$ID %in% c(15, 113)
points(Sable_2017_2019$ID[TF], Sable_2017_2019$Oto_Weight[TF], col = 'green', pch =16) # 2 year olds in green
cbind(Sable_2017_2019$ID[TF], Sable_2017_2019$Oto_Weight[TF])  # Missing otie weight on #113
TF <- Sable_2017_2019$ID %in% 122
points(Sable_2017_2019$ID[TF], Sable_2017_2019$Oto_Weight[TF], col = 'dodger blue', pch =16) #1 year olds in Dodger blue


# Fork length
dev.new()
TF <- Sable_2017_2019$ID > 150
plot(Sable_2017_2019$ID[TF], Sable_2017_2019$fork_length[TF], ylim = c(16.5, 73), xlim = c(-5, 3000), na.rm = TRUE)
TF <- Sable_2017_2019$ID <= 150
points(Sable_2017_2019$ID[TF], Sable_2017_2019$fork_length[TF], col = 'red')
TF <- Sable_2017_2019$ID %in% c(15, 113)
points(Sable_2017_2019$ID[TF], Sable_2017_2019$fork_length[TF], col = 'green', pch =16) # 2 year olds in green
cbind(Sable_2017_2019$ID[TF], Sable_2017_2019$fork_length[TF])  # Missing otie weight on #113
TF <- Sable_2017_2019$ID %in% 122
points(Sable_2017_2019$ID[TF], Sable_2017_2019$fork_length[TF], col = 'dodger blue', pch =16) #1 year olds in Dodger blue


 
# Find names of all metadata columns 
names(Sable_2017_2019[, -(2:(sum(!is.na(as.numeric(names(Sable_2017_2019)))) + 1))])

Table(Sable_2017_2019$broken) # 98 oties are broken
Sable_2017_2019[Sable_2017_2019$broken == 1, "ID"]

Table(Sable_2017_2019$crystallized)
Sable_2017_2019[Sable_2017_2019$crystallized == 1, "ID"]



Sable_2017_2019[Sable_2017_2019$ID %in% c(595, 1083), -(2:(sum(!is.na(as.numeric(names(Sable_2017_2019)))) + 1))]

plotly.Spec(Sable_2017_2019, nrow(Sable_2017_2019))



###################################################################################################################
### Perform Savitzky-Golay 1st derivative with 17 point window smoothing 2rd order polynomial fit and visualize ###
###################################################################################################################
### NOTE ### If there are multiple years of data, all subsequent transformations should be applied to the whole data set, then re-subset
          

Sable_Spectra_2019.sg <- data.frame(prospectr::savitzkyGolay(Sable_Spectra_2019, p = 2, w = 15, m = 1)) 
Sable_Spectra_2019.Age.sg <- data.frame(Age = Sable_Age_2019, prospectr::savitzkyGolay(Sable_Spectra_2019, p = 2, w = 15, m = 1))  

# add Age back to the spectra matrix

# pca <- stats::prcomp(Sable_Spectra_2019.sg, scale = TRUE)
# summary(pca)
# Sable_Spectra_2019.scores <- data.frame(pca$x)
#  
# PCA <- data.frame(Sable_Age_2019.fac, Sable_Spectra_2019.scores) 
# 
# s.class(PCA[,2:3],
#         fac = Sable_Age_2019.fac,  # color by groups
#         col = c("black", "dark red", "red","dark orange", "green", "dark green", "blue", "dark blue", "purple", "brown"),
#         grid = FALSE,
#         label = levels(Sable_Age_2019.fac),
#         cellipse = 2, #2 standard deviations (95% conf interval)
#         clabel = 1.5,
#         cpoint = 1
# )          
# 

# PCA  and CCA, vegan package from :   https://stackoverflow.com/questions/13936051/adding-ellipses-to-a-principal-component-analysis-pca-plot

# --- pca (rda) and cca (same result) ---
prin_comp.pca <- vegan::rda(Sable_Spectra_2019, scale = TRUE)
dev.new()
plot(prin_comp.pca)

prin_comp.pca.sg <- vegan::rda(Sable_Spectra_2019.sg, scale = TRUE)
pca_scores.sg <- vegan::scores(prin_comp.pca.sg)

dev.new()
plot(prin_comp.pca.sg)

dev.new()
plot(prin_comp.pca.sg, xlim=c(-21, 5), ylim=c(-8, 8))

dev.new()
plot(pca_scores.sg$sites[,1], pca_scores.sg$sites[,2], pch=21, bg = rainbow(30), xlim=c(-3, 3), ylim=c(-6, 2))
   
arrows(0,0,pca_scores.sg$species[,1],pca_scores.sg$species[,2],lwd=1,length=0.2)
 

Sable_Spectra_2019 <- Sable_2017_2019[, (562:1112) + 1] # Spectra matrix
Sable_Meta_2019 <- Sable_2017_2019[, -((1:1112) + 1)] # Meta data  
Sable_Meta_2019$Lat.1.degree <- round(Sable_Meta_2019$latitude) 
names(Sable_Meta_2019)
 [1] "filenames"         "vessel_code"       "cruise_number"     "date_collected"    "collection_year"   "region"           
 [7] "latitude"          "longitude"         "sequence"          "length"            "weight"            "sex"              
[13] "read_age"          "test_age"          "final_age"         "readability"       "unscannable"       "broken"           
[19] "crystallized"      "other_problem"     "percent_affected"  "instrument_name"   "comments"          "misc_data"        
[25] "gear_depth"        "bottom_depth"      "Cruise"            "Barcode"           "Age"               "Species"          
[31] "SampleYear"        "Oto_Weight"        "OtolithSide"       "Processed.by"      "Shipped.Date"      "CatchDate"        
[37] "sex_determination" "fork_length"       "Notes"             "Survey"            "Haul"              "TD_Time"          
[43] "TD_Lat"            "TD_Lon"            "Avg_Bot_Depth_m"   "Avg_Gear_Depth_m"  "Group"             "GroupNum"         
[49] "Age_BB"            "Age_OPUS"          "shortName"         "Lat.1.degree" 


# Sable.2019.cca <- cca(Sable_Spectra_2019 ~ Age, data=Sable_Meta_2019)
# Sable.2019.cca <- cca(Sable_Spectra_2019 ~ Age + fork_length, data=Sable_Meta_2019)
Sable_Meta_2019$Avg_Bot_Depth_m <- as.numeric(Sable_Meta_2019$Avg_Bot_Depth_m)
TF <- !(is.na(Sable_Meta_2019$Age) | is.na(Sable_Meta_2019$fork_length) | is.na(Sable_Meta_2019$Oto_Weight) | is.na(Sable_Meta_2019$sex) |
              is.na(Sable_Meta_2019$Lat.1.degree) | is.na(Sable_Meta_2019$weight) | is.na(Sable_Meta_2019$Avg_Bot_Depth_m))
sum(TF)

Sable.2019.cca <- cca(Sable_Spectra_2019[TF, ] ~ Age + Lat.1.degree + fork_length + Oto_Weight + sex  + weight + Avg_Bot_Depth_m, 
                     data=Sable_Meta_2019[TF, c('Age', 'fork_length', 'Oto_Weight', 'sex', 'Lat.1.degree', 'weight', 'Avg_Bot_Depth_m')])
# Sable.2019.cca
summary(Sable.2019.cca)

dev.new()
plotCCA(Sable.2019.cca, Col = 'green') 

dev.new()
plotCCA(Sable.2019.cca, display = c("sp", "wa", "cn", "lc", "bp", "reg")[c(3)], Col = 'green') 
 
dev.new()
plotCCA(Sable.2019.cca, display = c("sp", "wa", "cn", "lc", "bp", "reg")[c(1:4)], Col = 'dodger blue') 
 

dev.new()
par(mfrow = c(3,2))
plotCCA(Sable.2019.cca, c(1, 2))
plotCCA(Sable.2019.cca, c(1, 3))
plotCCA(Sable.2019.cca, c(1, 4))
plotCCA(Sable.2019.cca, c(2, 3))    
plotCCA(Sable.2019.cca, c(2, 4)) 
plotCCA(Sable.2019.cca, c(3, 4)) 

dev.new()
plot(Sable.2019.cca, display = c("sp", "wa", "cn", "lc", "bp", "reg")[c(3)])
 
 
plotCCA(Sable.2019.cca, Col = 'green') 
 

Sable.2019.cca <- cca(Sable_Spectra_2019[TF, ] ~ Age + sex, 
                     data=Sable_Meta_2019[TF, c('Age', 'fork_length', 'Oto_Weight', 'sex', 'Lat.1.degree', 'weight')])
summary(Sable.2019.cca)
 
Sable.2019.cca <- cca(Sable_Spectra_2019[TF, ] ~ sex + Lat.1.degree + fork_length + Oto_Weight + Age + weight, 
                     data=Sable_Meta_2019[TF, c('Age', 'fork_length', 'Oto_Weight', 'sex', 'Lat.1.degree', 'weight')])
summary(Sable.2019.cca) 

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
 
Sable_Spectra_2019.iPLS.F <- mdatools::ipls(Sable_Spectra_2019.sg, Sable_Age_2019, glob.ncomp = 10, center = T, scale = T, cv = 100,
                  int.ncomp = 10, int.num = 10, ncomp.selcrit = "min", method = "forward", silent = F)
save(Sable_Spectra_2019.iPLS.F, file = 'Sable_Spectra_2019.iPLS.F 13 Oct 2022.RData')              

summary(Sable_Spectra_2019.iPLS.F)

dev.new()
plot(Sable_Spectra_2019.iPLS.F)     

dev.new()
plot(Sable_Spectra_2019.iPLS.F, main = NULL)          


# plot predictions before and after selection
dev.new()
par(mfrow = c(2, 1))
mdatools::plotPredictions(Sable_Spectra_2019.iPLS.F$gm)
mdatools::plotPredictions(Sable_Spectra_2019.iPLS.F$om)

dev.new()
mdatools::plotRMSE(Sable_Spectra_2019.iPLS.F)

# RMSE  before and after selection
dev.new()
par(mfrow = c(2, 1))
mdatools::plotRMSE(Sable_Spectra_2019.iPLS.F$gm, ylim = c(-0.2, 3))
mdatools::plotRMSE(Sable_Spectra_2019.iPLS.F$om, ylim = c(-0.2, 3))



# plot the newly selected spectra regions
Sable_Spectra_2019.iPLS.vars <- Sable_Spectra_2019.iPLS.F$var.selected
(p <- length(Sable_Spectra_2019.iPLS.vars))

Sable_Spectra_2019.sg.iPLS <- data.frame(Sable_Spectra_2019.sg[, Sable_Spectra_2019.iPLS.vars])
Sable_Spectra_2019.Age.sg.iPLS <- data.frame(Age = Sable_Age_2019, Sable_Spectra_2019.sg.iPLS)
dim(Sable_Spectra_2019.Age.sg.iPLS)

#Split by age - Age.0.avg, Age.1.avg, ...
for (i in 0:17) { 
    Subset <- subset(Sable_Spectra_2019.Age.sg.iPLS, Sable_Age_2019 == i)
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


PLSr <- mdatools::pls(Sable_Spectra_2019.sg.iPLS, Sable_Age_2019, ncomp = 10, center = T, scale = F, cv = 100,
            method = "simpls", alpha = 0.05, ncomp.selcrit = "min")
summary(PLSr)
dev.new()
plot(PLSr)



set.seed(c(777, 747)[2])
index <- 1:nrow(Sable_Spectra_2019.sg.iPLS)
testindex <- sample(index, trunc(length(index)/3))
x.testset <- Sable_Spectra_2019.sg.iPLS[testindex, ]
x.trainset <- Sable_Spectra_2019.sg.iPLS[-testindex, ]   
y.test <- Sable_Age_2019[testindex]
y.train <- Sable_Age_2019[-testindex]

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



#  --------------- Support Vector Machine and RPART -------------------------
      
set.seed(c(777, 747)[2])
index <- 1:nrow(Sable_2017_2019)
testindex <- sample(index, trunc(length(index)/3))
(AgeColNum <- grep('Age', names(Sable_2017_2019))[1])
# Columns <- 2:1113 # All Wavelengths 
Columns.set <- (562:1112) + 1  # Only Wavelengths between 3,600 and 8,000
x.testset <- Sable_2017_2019[testindex, Columns.set]
x.trainset <- Sable_2017_2019[-testindex, Columns.set]   
y.test <- Sable_2017_2019[testindex, AgeColNum]
y.train <- Sable_2017_2019[-testindex, AgeColNum]   

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




# svm using e1071 package
svm.model <- e1071::svm(y.train ~ ., data = x.trainset, cost = 100, gamma = 1)
svm.model
svm.pred <- predict(svm.model, x.testset)
Table(NIRS_SVM_AGE = round(svm.pred), TMA = y.test)   
e1071::classAgreement(Table(NIRS_SVM_AGE = round(svm.pred), TMA = y.test)) #  $diag   0.3855337
e1071::classAgreement(Table(NIRS_SVM_AGE = round(svm.pred), TMA = y.test) , match.names = TRUE) #  $diag   0.3855337
sum(abs(round(svm.pred)- y.test)) # 1432

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



