



# --------------------------------	Check Reference scan using Sablefish 2019 spectra only model -----------------------

# !!!!!!!!!! SREF Otie on day 101 removed from the MREF oties !!!!!!!!

# source("C:\\SIDT\\Predict_NN_Ages\\Read_OPUS_Spectra.R")
source("C:\\SIDT\\Predict_NN_Ages\\Predict_NN_Age.R")

source("C:/SIDT/Predict_NN_Ages/Predict_NN_Age_Wrapper.R")
assign("Spectra_Set", "Sable_Combo_2019", pos = 1)

Predict_NN_Age_Wrapper(Spectra_Set = Spectra_Set, Train_Result_Path = "C:/SIDT/Sable_Combo_2019/Sable_Combo_2019_Predicted_with_2019_Model",  Multi_Year = FALSE, 
      Model_Spectra_Meta_Path = NULL, Extra_Meta_Path = NULL, Use_Session_Report_Meta = TRUE, Meta_Path = paste0('C:/SIDT/Sable_Combo_2019/Sable_Combo_2019_NIRS_Scanning_Session_Report_For_NWFSC.xlsx'))
	  
	  
	  
# To debug Predict_NN_Age_Wrapper()
Spectra_Set = Spectra_Set; Train_Result_Path = "C:/SIDT/Sable_Combo_2019/Sable_Combo_2019_Predicted_with_2019_Model";  Multi_Year = FALSE; Extra_Meta_Path = NULL
Model_Spectra_Meta_Path = NULL; Use_Session_Report_Meta = TRUE; Meta_Path = 'C:/SIDT/Sable_Combo_2019/Sable_Combo_2019_NIRS_Scanning_Session_Report_For_NWFSC.xlsx'
opusReader = 'philippbaumann_opusreader2'; Rdm_Reps_Main = 20; Folds_Num = 10; Max_N_Spectra = list(50, 200, 'All')[[2]]; Seed_Plot = 707; Spectra_Path = "New_Scans"
axes_zoomed_limit = 15; Predicted_Ages_Path = "Predicted_Ages"; Meta_Add = TRUE; TMA_Ages = TRUE; verbose = TRUE; plot = TRUE	  
	
	
# To debug Read_OPUS_Spectra()	  
#  Max_N_Spectra = N_Samp; Extra_Meta_Path = NULL; htmlPlotFolder = paste0(Predicted_Ages_Path, '/', Spectra_Set, '_Spectra_Sample_of_', N_Samp)	  
#  shortNameSegments = c(1, 6); shortNameSuffix = 'COMBO'; yearPosition = 6; fineFreqAdj = 0
#  Meta_Path = 'C:/SIDT/Sable_Combo_2019/Sable_Combo_2019_NIRS_Scanning_Session_Report_For_NWFSC.xlsx'; TMA_Ages = TRUE; excelSheet = 3
#  verbose = c(TRUE, FALSE)[1]; plot = c(TRUE, FALSE)[1]; htmlPlotFolder = "Figures"; spectraInterp = 'stats_splinefun_lowess';  opusReader = 'philippbaumann_opusreader2'	  
	  
	  
	  
# Fix to Predict_NN_Age_Wrapper() has now been done  
	
#     Meta_Path = 'C:/SIDT/Sable_Combo_2019/Sable_Combo_2019_NIRS_Scanning_Session_Report_For_NWFSC.xlsx'	
#     	  
#     metadata <- openxlsx::read.xlsx(Meta_Path, sheet = 3) # Load in ancillary data 
#     
#     
#     metadata[metadata$specimen_id %in% c(102018593, 102018607, 102104383, 102018657), ]
#     
#     
#     metadata[metadata$specimen_id %in% 102129701:102129704, ]
#     
#     
#     
#     metadata_DW[metadata_DW$AgeStr_id %in% c(102018593, 102018607, 102104383, 102018657), ]
#     
#     
#     
#     
#     # specimen_id is the same, but the fileName doesn't match the NWFSC_NIR_Filename  - Fix was to edit Predict_NN_Age_Wrapper() to match only on specimen_id number
#     
#      fileNames
#     [1] "SABL_COMBO2019_NIR0044P_PRD_1_102018593_O1" "SABL_COMBO2019_NIR0044P_PRD_2_102018607_O1" "SABL_COMBO2019_NIR0044P_PRD_3_102104383_O1" "SABL_COMBO2019_NIR0044P_PRD_4_102018657_O1"
#     
#     >  metadata[metadata$specimen_id %in% 102018593, ]
#         project sample_year pacfin_code_id sequence_number age_structure_id specimen_id age_best structure_weight_g age_structure_side NWFSC_NIR_Project NWFSC_NIR_Scan_Session
#     450   COMBO        2019           SABL             463 102018593-SABL-O   102018593       51                 NA               <NA>               PRD               NIR0026C
#                                   NWFSC_NIR_Filename age_structure_side_scan crystallized_scan percent_crystallized_scan broken_scan tip_only_scan anterior_tip_missing posterior_tip_missing
#     450 SABL_COMBO2019_NIR0026C_PRD_463_102018593_O1                       L              <NA>                        NA        <NA>          <NA>                 <NA>                  <NA>
#         percent_missing_scan tissue_present_scan tissue_level_scan oil_clay_contamination_scan stained_scan contamination_other_scan notes_scan unscannable_BB unscannable_Broken_MissingPieces
#     450                 <NA>                  NA                NA                          NA           NA                       NA       <NA>           <NA>                             <NA>
#         unscannable_Crystalized unscannable_sample_mixed unscannable_no_otolith
#     450                    <NA>                     <NA>                   <NA>



# This spectra file renaming was already done for Sablefish 2022 spectra only model

#   # ------ Days Since First Use of the Newport Tango #1 
#   
#   library(JRWToolBox)
#   
#   Ref_Scan_Dir <- dir('C:/SIDT/Sablefish_Reference')
#   for(i in 1:length(Ref_Scan_Dir)) {
#       Ref_Scan_Names <- dir(paste0('C:/SIDT/Sablefish_Reference/', Ref_Scan_Dir[i]))
#       for(j in 1:length(Ref_Scan_Names)) {
#   	   file_Name <- paste0('C:/SIDT/Sablefish_Reference/', Ref_Scan_Dir[i], '/', Ref_Scan_Names[j])
#   	   file_Subs <- get.subs(Ref_Scan_Names[j], sep = '_')
#   	   file_New_Name <- paste0('C:/SIDT/Sablefish_Reference/', file_Subs[1], "_", file_Subs[2], "_", julian(as.Date(Ref_Scan_Dir[i], "%Y_%m_%d"), origin = as.Date('2023_9_29', "%Y_%m_%d")), "_", file_Subs[4], "_", file_Subs[5], "_", file_Subs[6], "_", file_Subs[7])
#   	   print(c(file_Name, file_New_Name))
#          file.rename(file_Name, file_New_Name)
#       }	
#   }
#   
#   # !! Delete empty folders when done !!




load("C:\\SIDT\\Sablefish_Reference\\Predicted_Ages\\NN Predicted Ages, 10 Jul 2024.RData") # New_Ages

New_Ages$Ref_Scans_Days_From_First_Use_Newport_Tango_A <- as.numeric(get.subs(New_Ages$filenames, sep = '_',)[3,])

library(lattice)

Table(New_Ages$TMA)

  2  18  33  38  51 
136 137   1 137 137 

cbind(0:277, c(29:30, 1:31, 1:30, 1:31, 1:31, 1:29, 1:31, 1:30, 1:31, 1:30, 1:2))



# NN_Pred_Median (not rounded)

browsePlot('print(
xyplot(NN_Pred_Median ~ Ref_Scans_Days_From_First_Use_Newport_Tango_A | factor(paste("TMA Estimate =", TMA)), data = New_Ages[!New_Ages$TMA %in% 33, ], 
    xlab = "Ref Scans; Days from First Use on 29 Sept 2023; Newport Tango #1 (Green lines are the first day of the month.)", ylab = "NN Predicted Median Age Estimate from 2022 NN Model (not rounded)",
    panel = function(x, y) { 
	          Y <- mean(y)
	          if(Y > 0 & Y < 10) panel.abline(h = 2, col = "red", lty = 2)
		      if(Y > 15 & Y < 28) panel.abline(h = 18, col = "red", lty = 2)
		      if(Y > 30 & Y < 43) panel.abline(h = 38, col = "red", lty = 2)
		      if(Y > 44 & Y < 100) panel.abline(h = 51, col = "red", lty = 2)
			  panel.abline( v = c(2, 33, 63, 94, 125, 154, 185, 215, 246, 276), col = "dark green", lty = 2)
	          panel.xyplot.loess(x, y)
		  }
	))
', file = "Sablefish Ref Oties, NN_Pred_Median by Days_from_First_Use_on_Tango.png")



# Age rounded

browsePlot('print(
xyplot(Age_Rounded ~ Ref_Scans_Days_From_First_Use_Newport_Tango_A | factor(paste("TMA Estimate =", TMA)), data = New_Ages[!New_Ages$TMA %in% 33, ], 
    xlab = "Ref Scans; Days from First Use on 29 Sept 2023; Newport Tango #1 (Green lines are the first day of the month.)", ylab = "NN Predicted Median Age Estimate from 2022 NN Model, Rounded",
    panel = function(x, y) { 
	          Y <- mean(y)
	          if(Y > 0 & Y < 10) panel.abline(h = 2, col = "red", lty = 2)
		      if(Y > 15 & Y < 28) panel.abline(h = 18, col = "red", lty = 2)
		      if(Y > 30 & Y < 43) panel.abline(h = 38, col = "red", lty = 2)
		      if(Y > 44 & Y < 100) panel.abline(h = 51, col = "red", lty = 2)
			  panel.abline( v = c(2, 33, 63, 94, 125, 154, 185, 215, 246, 276), col = "dark green", lty = 2)
	          panel.xyplot.loess(x, y)
		  }
	))
', file = "Sablefish Ref Oties, NN_Pred_Median_Rounde by Days_from_First_Use_on_Tango.png")


# plotly.Spec(data.frame(filenames = fileNames.0, newScans.RAW, Otie = paste0(LETTERS[floor((rowNums - 0.001)/26) + 1], LETTERS[rowNums %r1% 26], '_', rowNums), shortName = shortName), 
#                     N_Samp = 'All', colorGroup = 'Ref_Scans_Days_From_First_Use_Newport_Tango_A')



Model_Spectra_Meta$Ref_Days <- as.numeric(get.subs(Model_Spectra_Meta$filenames, sep = '_',)[3,])
headTail(Model_Spectra_Meta, 3, 3, 3, 70)


setwd("C:/SIDT/Sablefish_Reference/Figures")

DF <- Model_Spectra_Meta[Model_Spectra_Meta$TMA %in% 2, ]
plotly.Spec(DF, N_Samp = nrow(DF), colorGroup = 'Ref_Days', main = "TMA = 2", ylim = c(0, 0.72), htmlPlotFolder = 'Sablefish_Ref_TMA_2_Spectra')


DF <- Model_Spectra_Meta[Model_Spectra_Meta$TMA %in% 18, ]
plotly.Spec(DF, N_Samp = nrow(DF), colorGroup = 'Ref_Days', main = "TMA = 18", ylim = c(0, 0.72), htmlPlotFolder = 'Sablefish_Ref_TMA_18_Spectra')


DF <- Model_Spectra_Meta[Model_Spectra_Meta$TMA %in% 38, ]
plotly.Spec(DF, N_Samp = nrow(DF), colorGroup = 'Ref_Days', main = "TMA = 38", ylim = c(0, 0.72), htmlPlotFolder = 'Sablefish_Ref_TMA_38_Spectra')


DF <- Model_Spectra_Meta[Model_Spectra_Meta$TMA %in% 51, ]
plotly.Spec(DF, N_Samp = nrow(DF), colorGroup = 'Ref_Days', main = "TMA = 51", ylim = c(0, 0.72), htmlPlotFolder = 'Sablefish_Ref_TMA_51_Spectra')



DF <- Model_Spectra_Meta[Model_Spectra_Meta$TMA %in% c(2, 18, 38, 51), ]
plotly.Spec(DF, N_Samp = nrow(DF), colorGroup = 'TMA', ylim = c(0, 0.72), scanUniqueName = 'filenames', htmlPlotFolder = 'Sablefish_Ref_Spectra_by_TMA')


# ----------- 2 -------------------
freqNum = NULL
WaveRange = c(0, 8000)

spectraMeta <- Model_Spectra_Meta[Model_Spectra_Meta$TMA %in% 2, ]

 options(warn = -1)
   # Remove 'X' prefix from freq names, if present
   oldNames <- names(spectraMeta)
   N <- length(oldNames)
   newNames <- oldNames
   
   for(i in 1:N) {

     if(is.na(as.numeric(substr(oldNames[i], 1, 1))) & !is.na(as.numeric(substring(oldNames[i], 2))))
          newNames[i] <- substring(oldNames[i], 2)
   }
   names(spectraMeta) <- newNames
   if(is.null(freqNum))  {   
      freqNum <- sum(!is.na(as.numeric(names(spectraMeta))))
      if(verbose)
         cat("\nNumber of Frequencies = ", freqNum, "\n")      
   }
 options(warn = 0)   

   WaveLengths <- as.numeric(names(spectraMeta[, 2:(freqNum + 1)]))
   WaveSubset <- as.character(WaveLengths[WaveLengths >= WaveRange[1] & WaveLengths <= WaveRange[2]])
   freqNum.Subset <- length(WaveSubset)
   if(verbose)
         cat("\nNumber of Frequencies subset by the wave range (WaveRange) = ", freqNum.Subset, "\n")  
		 
Cor_DF_2 <- NULL
for(i in WaveSubset) {
   Cor_DF_2 <- rbind(Cor_DF_2, data.frame(Band = i, Cor = cor(spectraMeta[, i], spectraMeta$Ref_Days)))
}


# ----------- 18 -------------------
freqNum = NULL
WaveRange = c(0, 8000)

spectraMeta <- Model_Spectra_Meta[Model_Spectra_Meta$TMA %in% 18, ]

 options(warn = -1)
   # Remove 'X' prefix from freq names, if present
   oldNames <- names(spectraMeta)
   N <- length(oldNames)
   newNames <- oldNames
   
   for(i in 1:N) {

     if(is.na(as.numeric(substr(oldNames[i], 1, 1))) & !is.na(as.numeric(substring(oldNames[i], 2))))
          newNames[i] <- substring(oldNames[i], 2)
   }
   names(spectraMeta) <- newNames
   if(is.null(freqNum))  {   
      freqNum <- sum(!is.na(as.numeric(names(spectraMeta))))
      if(verbose)
         cat("\nNumber of Frequencies = ", freqNum, "\n")      
   }
 options(warn = 0)   

   WaveLengths <- as.numeric(names(spectraMeta[, 2:(freqNum + 1)]))
   WaveSubset <- as.character(WaveLengths[WaveLengths >= WaveRange[1] & WaveLengths <= WaveRange[2]])
   freqNum.Subset <- length(WaveSubset)
   if(verbose)
         cat("\nNumber of Frequencies subset by the wave range (WaveRange) = ", freqNum.Subset, "\n")  
		 

Cor_DF_18 <- NULL
for(i in WaveSubset) {
   Cor_DF_18 <- rbind(Cor_DF_18, data.frame(Band = i, Cor = cor(spectraMeta[, i], spectraMeta$Ref_Days)))
}



# ----------- 38 -------------------
freqNum = NULL
WaveRange = c(0, 8000)

spectraMeta <- Model_Spectra_Meta[Model_Spectra_Meta$TMA %in% 38, ]

 options(warn = -1)
   # Remove 'X' prefix from freq names, if present
   oldNames <- names(spectraMeta)
   N <- length(oldNames)
   newNames <- oldNames
   
   for(i in 1:N) {

     if(is.na(as.numeric(substr(oldNames[i], 1, 1))) & !is.na(as.numeric(substring(oldNames[i], 2))))
          newNames[i] <- substring(oldNames[i], 2)
   }
   names(spectraMeta) <- newNames
   if(is.null(freqNum))  {   
      freqNum <- sum(!is.na(as.numeric(names(spectraMeta))))
      if(verbose)
         cat("\nNumber of Frequencies = ", freqNum, "\n")      
   }
 options(warn = 0)   

   WaveLengths <- as.numeric(names(spectraMeta[, 2:(freqNum + 1)]))
   WaveSubset <- as.character(WaveLengths[WaveLengths >= WaveRange[1] & WaveLengths <= WaveRange[2]])
   freqNum.Subset <- length(WaveSubset)
   if(verbose)
         cat("\nNumber of Frequencies subset by the wave range (WaveRange) = ", freqNum.Subset, "\n")  
		 

Cor_DF_38 <- NULL
for(i in WaveSubset) {
   Cor_DF_38 <- rbind(Cor_DF_38, data.frame(Band = i, Cor = cor(spectraMeta[, i], spectraMeta$Ref_Days)))
}



# ----------- 51 -------------------
freqNum = NULL
WaveRange = c(0, 8000)

spectraMeta <- Model_Spectra_Meta[Model_Spectra_Meta$TMA %in% 51, ]

 options(warn = -1)
   # Remove 'X' prefix from freq names, if present
   oldNames <- names(spectraMeta)
   N <- length(oldNames)
   newNames <- oldNames
   
   for(i in 1:N) {

     if(is.na(as.numeric(substr(oldNames[i], 1, 1))) & !is.na(as.numeric(substring(oldNames[i], 2))))
          newNames[i] <- substring(oldNames[i], 2)
   }
   names(spectraMeta) <- newNames
   if(is.null(freqNum))  {   
      freqNum <- sum(!is.na(as.numeric(names(spectraMeta))))
      if(verbose)
         cat("\nNumber of Frequencies = ", freqNum, "\n")      
   }
 options(warn = 0)   

   WaveLengths <- as.numeric(names(spectraMeta[, 2:(freqNum + 1)]))
   WaveSubset <- as.character(WaveLengths[WaveLengths >= WaveRange[1] & WaveLengths <= WaveRange[2]])
   freqNum.Subset <- length(WaveSubset)
   if(verbose)
         cat("\nNumber of Frequencies subset by the wave range (WaveRange) = ", freqNum.Subset, "\n")  
		 

Cor_DF_51 <- NULL
for(i in WaveSubset) {
   Cor_DF_51 <- rbind(Cor_DF_51, data.frame(Band = i, Cor = cor(spectraMeta[, i], spectraMeta$Ref_Days)))
}

range(c(Cor_DF_2$Cor, Cor_DF_18$Cor, Cor_DF_38$Cor, Cor_DF_51$Cor))


browsePlot("
plot(Cor_DF_2$Band, Cor_DF_2$Cor, ylim = c(0.05, -0.7), col = 'red', xlab = 'Band' , ylab = 'Correlation')
points(Cor_DF_18$Band, Cor_DF_18$Cor, col = 'green')
points(Cor_DF_38$Band, Cor_DF_38$Cor, col = 'cyan')
points(Cor_DF_51$Band, Cor_DF_51$Cor, col = 'purple')

", file = 'Cor of Absorbance and Ref Scan Days vs Band by Ref Otie.png')




# --- Table ----

range(New_Ages$Ref_Scans_Days_From_First_Use_Newport_Tango_A)
[1]   0 277

277/2
[1] 138.5

# 137 is closest to midpoint
sort(unique(New_Ages$Ref_Scans_Days_From_First_Use_Newport_Tango_A))
  [1]   0  11  21  24  25  26  27  28  31  32  33  39  40  41  45  46  47  48  49  52  54  59  60  61  66  70  88  89  90  91  95  96  97  98 101 102 104 105 111 112 115 116 117 118 119 122 123 124 125 126 129 130 131 132 133 136 137 146
 [59] 150 151 152 153 154 157 158 159 160 161 164 165 166 167 168 171 172 173 174 175 178 179 180 181 185 189 192 193 194 195 196 199 201 202 206 207 209 210 213 214 215 216 217 220 221 222 223 224 227 228 229 230 234 236 237 238 242 243
[117] 244 245 248 249 250 251 255 256 257 258 259 262 263 265 266 269 270 271 272 276 277


New_Ages[New_Ages$TMA == 2 & New_Ages$Ref_Scans_Days_From_First_Use_Newport_Tango_A == 0, c("TMA", "Ref_Scans_Days_From_First_Use_Newport_Tango_A", "NN_Pred_Median", "Age_Rounded")]
  TMA Ref_Scans_Days_From_First_Use_Newport_Tango_A NN_Pred_Median Age_Rounded
3   2                                             0         2.3837           2

New_Ages[New_Ages$TMA == 2 & New_Ages$Ref_Scans_Days_From_First_Use_Newport_Tango_A == 137, c("TMA", "Ref_Scans_Days_From_First_Use_Newport_Tango_A", "NN_Pred_Median", "Age_Rounded")]
   TMA Ref_Scans_Days_From_First_Use_Newport_Tango_A NN_Pred_Median Age_Rounded
99   2                                           137         2.9618           3

New_Ages[New_Ages$TMA == 2 & New_Ages$Ref_Scans_Days_From_First_Use_Newport_Tango_A == 277, c("TMA", "Ref_Scans_Days_From_First_Use_Newport_Tango_A", "NN_Pred_Median", "Age_Rounded")]
    TMA Ref_Scans_Days_From_First_Use_Newport_Tango_A NN_Pred_Median Age_Rounded
439   2                                           277         3.8994           4



for( i in c(2, 18, 38, 51)) {
   for( j in c(0, 70, 137, 209, 277))
      print(New_Ages[New_Ages$TMA == i & New_Ages$Ref_Scans_Days_From_First_Use_Newport_Tango_A == j, c("TMA", "Ref_Scans_Days_From_First_Use_Newport_Tango_A", "NN_Pred_Median", "Age_Rounded")])
   cat("\n\n")
}




Reference Scans, from First Use, Newport Tango #1

0	70	137	209





#------------- Look at newScans_meta --------------------


newScans_meta$Ref_Scans_Days_From_First_Use_Newport_Tango_A <- as.numeric(get.subs(newScans_meta$filenames, sep = '_',)[3,])

names(newScans_meta)[c(2, 3, 5, 6)] <- c("Tango Scanner Reference Temp (C)", "Tango Scanner Temp (C)", "Relative Humidity Interferometer", "Scan time (sec)")


newScans_meta_Stacked <- data.frame(Ref_Scans_Days_From_First_Use_Newport_Tango_A = rep(newScans_meta$Ref_Scans_Days_From_First_Use_Newport_Tango_A, 4), stack(newScans_meta[, -c(1, 4, 7)]))

browsePlot('print(xyplot(values ~ Ref_Scans_Days_From_First_Use_Newport_Tango_A | ind, data = newScans_meta_Stacked, 
           panel = function(...) { if(current.row() == 1) { panel.abline(h = 30, lty = 2, col = "green"); panel.abline(h = 35, lty = 2, col = "red") };
		   panel.xyplot(...) }))', file = "C:/SIDT/Sablefish_Reference/Figures/Tange_Metadata_Over_Time.png")
		   
 # only red dashed line
browsePlot('print(xyplot(values ~ Ref_Scans_Days_From_First_Use_Newport_Tango_A | ind, data = newScans_meta_Stacked, 
           panel = function(...) { if(current.row() == 1) { panel.abline(h = 35, lty = 2, col = "red") };
		   panel.xyplot(...) }))', file = "C:/SIDT/Sablefish_Reference/Figures/Tange_Metadata_Over_Time.png")

max(abs(newScans_meta.VIRGIN$TSC - newScans_meta.VIRGIN$TSC_ref), na.rm = TRUE)




