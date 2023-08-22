
# Install some utility functions,  the plotly.Spec() plotting function, and the simplerspec package - all from GitHub
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

# lib("philipp-baumann/simplerspec") # https://github.com/philipp-baumann/simplerspec
lib(simplerspec)

# Upload all spectral files
PATH <- "W:/ALL_USR/JRW/SIDT/Hake Data 2019/Original"
setwd(PATH) # set working directory to folder containing spectral files
getwd()


(listspc <- dir(pattern = '*HAKE*'))[1:20] # creates the list of all the .001 file names in the directory
length(listspc) #  2340

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

# ==========================================================
lib("pierreroudier/opusreader")

   temp_spec <- opusreader::opus_read(listspc[1222])
   temp_spec_no_atm <- opusreader::opus_read(listspc[1222], type = "spec_no_atm_comp")

   # < simplerspec::read_opus_bin_univ >  does not have the "spec_no_atm_comp" option
   
   # Character vector of spectra types to extract from OPUS binary file. Default is "spec", which will extract the final spectra, e.g. expressed in absorbance 
   # (named AB in Bruker OPUS programs). Possible additional values for the character vector supplied to type are "spec_no_atm_comp"
   # (spectrum of the sample without compensation for atmospheric gases, water vapor and/or carbon dioxide), "sc_sample" (single channel spectrum of the sample measurement),
   # "sc_ref" (single channel spectrum of the reference measurement), "ig_sample" (interferogram of the sample measurement) and "ig_ref" (interferogram of the reference measurement).
   
   gof()
   par(mfrow = c(2,1))
   plot(temp_spec$wavenumbers, temp_spec$spec, type = 'l', col = 'green')
   plot(temp_spec_no_atm$wavenumbers, temp_spec_no_atm$spec_no_atm_comp, type = 'l', col = 'red')
   
# =============================================================

# Interpolate
# prospectr::resample()  uses spline interpolation method
testdf <- (prospectr::resample(X = ldf[[1]][[2]], wav = ldf[[1]][[8]], new.wav = ldf[[N]][[8]]))

# plot to test
plot(as.vector(ldf[[1]]$wavenumbers), as.vector(ldf[[2]]$spc))
points(as.vector(ldf[[N]]$wavenumbers), as.vector(testdf), col = 'green') 


(filenames <- unlist(sapply(ldf, function(x) x[[1]][2]), use.names = FALSE))[1:10]

(dat <- cbind(filenames, dat_spc))[1:5, c(1:5, 1110:1113)]

save(dat, file = 'dat 6 Oct 2022.RData')
base::load(file = 'dat 6 Oct 2022.RData') # If needed



# ADD VESSEL, CRUISE, REGION, LOCATION, AND BIO METADATA
scan_data <- read.csv(paste0(PATH, "hake_scandata_2019.csv"), strip.white = TRUE) #load in ancillary data
(int_data <- left_join(dat, scan_data, by = "filenames"))[1:5, c(1:3, 1112:1120, 1135:(ncol(dat) + ncol(scan_data) - 1))]


# ADD AGE DATA
age_data <- read.csv(paste0(PATH, "hake_agedata_2019.csv"), strip.white = TRUE) #load in age data
(int2_data <- left_join(int_data, age_data, by = "sequence"))[1:5, c(1:3, 1112:1120, 1135:(ncol(int_data) + ncol(age_data) - 1))]



# Up to here compare directly to Morgan's  < hake_all_2019.8.10 ORG, from Morgans csv File.RData >  which is directly from her csv file



# ADD HAUL METADATA
haul_data <- read.csv(paste0(PATH, "hake_haulmetadata_2019.csv"))
(all_data <- left_join(int2_data, haul_data, by = "Barcode"))[1:5, c(1:3, 1112:1120, 1135:(ncol(int2_data) + ncol(haul_data) - 1))]

hake_all_2019.6.20 <- all_data
hake_all_2019.6.20$Group <- substr(hake_all_2019.6.20$filenames, 26, 26)
hake_all_2019.6.20$GroupNum <- as.numeric(factor(hake_all_2019.6.20$Group))


hake_all_2019.6.20$shortName <- apply(hake_all_2019.6.20[, 'filenames', drop = FALSE], 1, function(x) paste(get.subs(x, sep = "_")[c(2,4)], collapse = "_"))

hake_all_2019.6.20$ID <- as.numeric(strSplit(hake_all_2019.6.20$shortName, "_", elements = 2))
hake_all_2019.6.20 <- sort.f(hake_all_2019.6.20, "ID")

save(hake_all_2019.6.20, file = "hake_all_2019.6.20 6 Oct 2022.RData")


#   ==========================  Look at the data and plot it  =========================================================

base::load(file = "hake_all_2019.6.20 6 Oct 2022.RData")  # If needed
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


base::load('hake_all_2019.8.10 ORG, from Morgans csv File.RData')

