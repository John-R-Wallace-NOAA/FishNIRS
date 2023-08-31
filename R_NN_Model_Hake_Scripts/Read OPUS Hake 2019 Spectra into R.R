
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


## ================ Old way to read in spectra =================================

#  ldf <- list() # creates an empty list
#  for (k in 1:length(listspc[1:5])) { # loops through and uploads each file using read_opus_bin_univ()  from the simplerspec package
#         test_spec <- simplerspec::read_opus_bin_univ(listspc[k], extract = "spc", print_progress = TRUE, atm_comp_minus4offset = FALSE)
#         if(any(grepl('Error', test_spec[[1]])))
#             next
#         ldf[[as.character(k)]] <- test_spec
#  }
#  
#  (N <- length(ldf)) # 2848
#  
#  names(ldf[[1]]) # Names of first element
#  ##  [1] "metadata"          "spc"               "spc_nocomp"        "sc_sm"             "sc_rf"             "ig_sm"             "ig_rf"             "wavenumbers"      
#  ##  [9] "wavenumbers_sc_sm" "wavenumbers_sc_rf"
#  
#  
#  str(ldf[[1]]) # check first element
#  
#  ldf[[1]]$wavenumbers[1:20] # 8th Element
#  
#  ldf[[2848]]$wavenumbers[1:20] # 8th Element
#  
#  len(ldf[[1]]$wavenumbers)
#  len(ldf[[2848]]$wavenumbers)
#  
#  save(ldf, file = 'ldf.RData')
#  
#  # Interpolate
#  # prospectr::resample()  uses spline interpolation method
#  testdf <- (prospectr::resample(X = ldf[[1]][[2]], wav = ldf[[1]][[8]], new.wav = ldf[[N]][[8]]))
#  
#  # plot to test
#  plot(as.vector(ldf[[1]]$wavenumbers), as.vector(ldf[[2]]$spc))
#  points(as.vector(ldf[[N]]$wavenumbers), as.vector(testdf), col = 'green') 

# (filenames <- unlist(sapply(ldf, function(x) x[[1]][2]), use.names = FALSE))[1:10]

# (dat <- cbind(filenames, dat_spc))[1:5, c(1:5, 1110:1113)]


# ================== New way to read in spectra with opusreader::opus_read() ========================================

lib("pierreroudier/opusreader")   # https://github.com/pierreroudier/opusreader


# < simplerspec::read_opus_bin_univ >  does not have the "spec_no_atm_comp" option

# Character vector of spectra types to extract from OPUS binary file. Default is "spec", which will extract the final spectra, e.g. expressed in absorbance 
# (named AB in Bruker OPUS programs). Possible additional values for the character vector supplied to type are "spec_no_atm_comp"
# (spectrum of the sample without compensation for atmospheric gases, water vapor and/or carbon dioxide), "sc_sample" (single channel spectrum of the sample measurement),
# "sc_ref" (single channel spectrum of the reference measurement), "ig_sample" (interferogram of the sample measurement) and "ig_ref" (interferogram of the reference measurement).

test_spec <- opusreader::opus_read(listspc[1222])
test_spec_no_comp <- opusreader::opus_read(listspc[1222], type = "spec_no_atm_comp")
# temp_sc_sample <- opusreader::opus_read(listspc[1222], type = "sc_sample")  # No 'sc_sample' spectra found 
# temp_sc_ref <- opusreader::opus_read(listspc[1222], type = "sc_ref") # No 'sc_ref' spectra found
test_spec_ig_ref <- opusreader::opus_read(listspc[1222], type = "ig_ref") 
test_spec_ig_sample <- opusreader::opus_read(listspc[1222], type = "ig_sample")  

data.frame(t(test_spec$metadata))

                                  t.test_spec.metadata.
unique_id                C STD XPP _2021-03-11 14:33:13
sample_id                                    C STD XPP 
date_time_sm                        2021-03-11 14:33:13
date_time_rf                        2021-03-11 13:44:01
sample_name                                  C STD XPP 
instr_name_range                 MPA-High Intensity NIR
resolution_wn                                         4
result_spc                                           AB
beamspl                               1906206D_1253_OT1
laser_wn                                       15799.35
spc_in_file      ig_sample;ig_ref;spec_no_atm_comp;spec
zero_filling                                          2
temp_scanner_sm                                    37.1
temp_scanner_rf                                      37
hum_rel_sm                                           40
hum_rel_rf                                         <NA>
hum_abs_sm                                         <NA>
hum_abs_rf                                         <NA>

   
gof()
par(mfrow = c(2,2))
plot(test_spec$wavenumbers, test_spec$spec, type = 'l', col = 'green', xlab = 'Wave Numbers', ylab = 'Absorbance', main = 'Final Spectra')
plot(test_spec_no_comp$wavenumbers, test_spec_no_comp$spec_no_atm_comp, type = 'l', col = 'red', xlab = 'Wave Numbers', ylab = 'Absorbance', main = 'No compensation for atmos. gas, water vapor, and/or CO2')

y_range <- range(c(test_spec_ig_ref$ig_ref, test_spec_ig_sample$ig_sample)) + c(-0.05, 0.05)
plot(1:len(test_spec_ig_ref$ig_ref), test_spec_ig_ref$ig_ref, type = 'l', col = 'blue', ylim = y_range, xlab = 'Time', ylab = 'Intensity (V)', main = 'Interferogram of the Reference Measurement')
plot(1:len(test_spec_ig_sample$ig_sample), test_spec_ig_sample$ig_sample, type = 'l', col = 'magenta', ylim = y_range, xlab = 'Time', ylab = 'Intensity (V)', main = 'Interferogram of the Sample Measurement')

# plot(test_spec$ig_ref, test_spec$ig_sample, type = 'l', col = 'blue')


#  Wave length differences are not all equal in the first scans, but are by the end.  opusreader::opus_read() with simlify = TRUE  uses  the very first file passed as a reference for linear interpolation. 
#                        So need to put the last scan first temporarily and remove 2 other scans with bad numbers.

test_spec_1 <- opusreader::opus_read(listspc[1])
Table(diff(test_spec_1$wavenumbers))

-7.71452447070442 -7.71452447070351 -7.71452447070305  -7.7145244707026 
              190               259                55               649 

test_spec_2340 <- opusreader::opus_read(listspc[2340])
Table(diff(test_spec_2340$wavenumbers))

  -8 
1111 


ldf <- opusreader::opus_read(listspc[c(2340, 1:914, 916:1207, 1209:2339)], simplify = TRUE) # Last first temporarily and 915 and 1208 have bad numbers

# save(ldf, file = 'ldf using opusread, 30 Aug 2023.RData')


dat <- data.frame(c(listspc[c(2340, 1:914, 916:1207, 1209:2339)]), ldf[[2]])  
names(dat) <- c('filenames', round(ldf[[1]]))
dat <- dat[c(2:2338, 1), ]  # Switch back to order the oties where scanned in.
dat[1:5, c(1:5, 1150:1155)]


Table(diff(as.numeric(names(dat)[-1])))
  -8 
1111 


#  Compare to Morgan's data (which has metadata at the end (see below for adding meta data to 'dat') 
base::load('hake_all_2019.8.10 ORG, from Morgans csv File.RData') # hake_all_2019.8.10
hake_all_2019.8.10[1:5, c(1:5, 1110:1113)]

Table(diff(as.numeric(names(hake_all_2019.8.10)[2:1113])))
  -8 
1111 

sum(as.numeric(names(dat)[-1]) - as.numeric(names(hake_all_2019.8.10)[2:1113]))
[1] 0

save(dat, file = 'dat 30 Aug 2023.RData')
base::load(file = 'dat 30 Aug 2023.RData') # If needed


 
# =============================================================

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

