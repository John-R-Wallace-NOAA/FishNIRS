

setwd("C:/SIDT/CLPR_Combo_1985__2024/CLPR_Combo_1985__2024_Str_Wgt_Sex/Predicted_Ages NO BIAS Corr")
# setwd("C:/SIDT/CLPR_Combo_1985__2024/CLPR_Combo_1985__2024_Str_Wgt_Sex/Predicted_Ages") # Bias corrected
# setwd("C:/SIDT/CLPR_Combo_2010__2024/CLPR_Combo_2010__2024_Str_Wgt_Sex/Predicted_Ages NO BIAS Corr")

library(JRWToolBox)
lib(openxlsx)
lib(lattice)
lib(chron)


Don_Tyler_Double_1 <- openxlsx::read.xlsx("C:/SIDT/Chilipepper/Fit Von Bert & Double Reads/All_Double_Reads_CLPR_20250319.xlsx", sheet = 1, startRow = 3, detectDates = TRUE)
Don_Tyler_Double_1$Year <- "2014"
headTail(Don_Tyler_Double_1)

Don_Patrick_Double <- Don_Tyler_Double_1[!is.na(Don_Tyler_Double_1$Patrick.McDonald), c("Don.Pearson", "Patrick.McDonald", "Year")]
headTail(Don_Patrick_Double)


names(Don_Patrick_Double)[1:2] <- c("TMA", "NN_Pred_Median")
Don_Patrick_Double$Reader <- "Patrick_vs_Don"
headTail(Don_Patrick_Double)


Tyler_Patrick_Double <- Don_Tyler_Double_1[, c("Tyler.Johnson", "Patrick.McDonald", "Year")]
headTail(Tyler_Patrick_Double)
names(Tyler_Patrick_Double)[1:2] <- c("TMA", "NN_Pred_Median")
Tyler_Patrick_Double$Reader <- "Patrick_vs_Tyler"
headTail(Tyler_Patrick_Double)



Don_Tyler_Double_2 <- openxlsx::read.xlsx("C:/SIDT/Chilipepper/Fit Von Bert & Double Reads/All_Double_Reads_CLPR_20250319.xlsx", sheet = 2, startRow = 3, detectDates = TRUE)
Don_Tyler_Double_2$Year <- as.character(years(Don_Tyler_Double_2$catch_date))
headTail(Don_Tyler_Double_2)
Don_Tyler_Double <- rbind(Don_Tyler_Double_1[, c("otosag_id", "Don.Pearson", "Tyler.Johnson", "Year")], Don_Tyler_Double_2[, c("otosag_id", "Don.Pearson", "Tyler.Johnson", "Year")])
headTail(Don_Tyler_Double)



names(Don_Tyler_Double)[2:3] <- c("TMA", "NN_Pred_Median", "Year")
Don_Tyler_Double$Reader <- "Tyler_vs_Don"
headTail(Don_Tyler_Double)


Tyler_Patrick_Double_Internal <- openxlsx::read.xlsx("C:/SIDT/Chilipepper/Fit Von Bert & Double Reads/All_Double_Reads_CLPR_20250319.xlsx", sheet = 3, startRow = 3, detectDates = TRUE)
headTail(Tyler_Patrick_Double_Internal)


names(Tyler_Patrick_Double_Internal)[c(2, 6, 7)] <- c("Year", "TMA", "NN_Pred_Median")
Tyler_Patrick_Double_Internal$Reader <- "Patrick_vs_Tyler"
headTail(Tyler_Patrick_Double_Internal)


load("C:\\SIDT\\CLPR_Combo_1985__2024\\CLPR_Combo_1985__2024_Str_Wgt_Sex\\Predicted_Ages NO BIAS Corr\\NN Predicted Ages, 24 Mar 2025.RData")
# load("C:\\SIDT\\CLPR_Combo_1985__2024\\CLPR_Combo_1985__2024_Str_Wgt_Sex\\Predicted_Ages\\NN Predicted Ages, 21 Mar 2025.RData") # Bias corrected
# load("C:\\SIDT\\CLPR_Combo_2010__2024\\CLPR_Combo_2010__2024_Str_Wgt_Sex\\Predicted_Ages NO BIAS Corr\\NN Predicted Ages, 23 Mar 2025.RData")


headTail(New_Ages)

load("C:\\SIDT\\CLPR_Combo_1985__2024\\CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
headTail(Model_Spectra_Meta, 3,3,3,50)

New_Ages <- match.f(New_Ages, Model_Spectra_Meta, 'filenames', 'filenames', 'specimen_id')
Table(!is.na(New_Ages$specimen_id))

New_Ages$specimen_id[New_Ages$Year %in% "2014"][1:10]

# New_Ages <- match.f(New_Ages, Don_Tyler_Double, 'specimen_id', 'otosag_id')
# headTail(New_Ages)
# 
# sum(!is.na(New_Ages$Don.Pearson))
# sum(!is.na(New_Ages$Tyler.Johnson))
# 
# Table(New_Ages$TMA, New_Ages$Tyler.Johnson)

   
New_Ages$Reader <- "Don"
New_Ages$Reader[New_Ages$Year > 2017] <- "Tyler"
Table(New_Ages$Reader, New_Ages$Year)


New_Ages_Double <- rbind(New_Ages[, c("NN_Pred_Median", "TMA", "Year", "Reader")], Don_Tyler_Double[, c("NN_Pred_Median", "TMA", "Year", "Reader")],
                                       Don_Patrick_Double[, c("NN_Pred_Median", "TMA", "Year", "Reader")], Tyler_Patrick_Double[, c("NN_Pred_Median", "TMA", "Year", "Reader")],
                                       Tyler_Patrick_Double_Internal[, c("NN_Pred_Median", "TMA", "Year", "Reader")])
headTail(New_Ages_Double)

Table(New_Ages_Double$Reader)


# ============= Not Bias corrected ===========

# Decimal NN Pred

# First check order with auto key
# browsePlot('print(xyplot(NN_Pred_Median ~ TMA | factor(Year), group = factor(Reader), data = New_Ages_Double,  col = c("dodgerblue", "red", "green", "goldenrod3", "cyan"), pch = c(1, 4, 0, 1, 2), 
#             ylab = paste0("NN Pred Median (decimal; not bias corrected) or TMA Secondary Double Read"), xlab = "TMA Best Age (circles) or TMA Primary Read (x, square, triangle)",
#             panel = function(...) {panel.xyplot.loess(..., span = 1.0); panel.abline(0, 1) }, as.table = TRUE,
#             auto = T))')


browsePlot('print(xyplot(NN_Pred_Median ~ TMA | factor(Year), group = factor(Reader), data = New_Ages_Double,  col = c("dodgerblue", "red", "green", "goldenrod3", "cyan"), pch = c(1, 0, 2, 1, 4), 
            ylab = paste0("NN Pred Median (decimal; not bias corrected) or TMA Secondary Double Read"), xlab = "TMA Best Age (circles) or TMA Primary Read (x, square, triangle)",
            panel = function(...) {panel.xyplot.loess(..., span = 1.0); panel.abline(0, 1) }, as.table = TRUE,
            key = list(space = "right", points = list(col=c("dodgerblue", "goldenrod3", "cyan", "red", "green"), pch = c(1, 1, 4, 0, 2)), 
            text = list(c("Spectra vs Don", "Spectra vs Tyler", "Tyler vs Don", "Patrick vs Don", "Patrick vs Tyler")))))', 
            file = "NN_Pred_vs_TMA_by_Reader_and_Year.png")


# Jitter TMA
browsePlot('print(xyplot(NN_Pred_Median ~ jitter(TMA, 7) | factor(Year), group = factor(Reader), data = New_Ages_Double,  col = c("dodgerblue", "red", "green", "goldenrod3", "cyan"), pch = c(1, 0, 2, 1, 4), 
            ylab = paste0("NN Pred Median (decimal; not bias corrected) or TMA Secondary Double Read"), xlab = "TMA Best Age (circles) or TMA Primary Read (x, square, triangle); All TMA Jittered",
            panel = function(...) {panel.xyplot.loess(..., span = 1.0); panel.abline(0, 1) }, as.table = TRUE,
            key = list(space = "right", points = list(col=c("dodgerblue", "goldenrod3", "cyan", "red", "green"), pch = c(1, 1, 4, 0, 2)), 
            text = list(c("Spectra vs Don", "Spectra vs Tyler", "Tyler vs Don", "Patrick vs Don", "Patrick vs Tyler")))))', 
            file = "NN_Pred_vs_Jittered_TMA_by_Reader_and_Year_.png")




# Rounded NN Pred

# New_Ages_NN_Pred_Median_Rnd <- New_Ages
# New_Ages_NN_Pred_Median_Rnd$NN_Pred_Median <- New_Ages_NN_Pred_Median_Rnd$Pred_Age_Bias_Corr_plus_Delta_rounded
# headTail(New_Ages_NN_Pred_Median_Rnd)
# 
# New_Ages_Double_Rnd <- rbind(New_Ages_NN_Pred_Median_Rnd[, c("NN_Pred_Median", "TMA", "Year", "Reader")], Don_Tyler_Double[, c("NN_Pred_Median", "TMA", "Year", "Reader")],
#                                        Don_Patrick_Double[, c("NN_Pred_Median", "TMA", "Year", "Reader")], Tyler_Patrick_Double[, c("NN_Pred_Median", "TMA", "Year", "Reader")],
#                                        Tyler_Patrick_Double_Internal[, c("NN_Pred_Median", "TMA", "Year", "Reader")])
# 
# 
# browsePlot('print(xyplot(NN_Pred_Median ~ TMA | factor(Year), group = factor(Reader), data = New_Ages_Double_Rnd,  col = c("dodgerblue", "red", "green", "goldenrod3", "cyan"), pch = c(1, 0, 2, 1, 4), 
#             ylab = paste0("NN Pred Median (decimal; not bias corrected) or TMA Secondary Double Read"), xlab = "TMA Best Age (circles) or TMA Primary Read (x, square, triangle)",
#             panel = function(...) {panel.xyplot.loess(..., span = 1.0); panel.abline(0, 1) }, as.table = TRUE,
#             key = list(space = "right", points = list(col=c("dodgerblue", "goldenrod3", "cyan", "red", "green"), pch = c(1, 1, 4, 0, 2)), 
#             text = list(c("Spectra vs Don", "Spectra vs Tyler", "Tyler vs Don", "Patrick vs Don", "Patrick vs Tyler")))))', 
#             file = "NN_Pred_vs_TMA_by_Reader_and_Year.png")







# ============= Bias corrected ===========

browsePlot('print(xyplot(NN_Pred_Median ~ TMA | factor(Year), group = factor(Reader), data = New_Ages_Double,  col = c("dodgerblue", "red", "green", "goldenrod3", "cyan"), pch = c(1, 0, 2, 1, 4), 
            ylab = paste0("NN Pred Median (decimal; bias corrected) or TMA Secondary Double Read"), xlab = "TMA Best Age (circles) or TMA Primary Read (x, square, triangle)",
            panel = function(...) {panel.xyplot.loess(..., span = 1.0); panel.abline(0, 1) }, as.table = TRUE,
            key = list(space = "right", points = list(col=c("dodgerblue", "goldenrod3", "cyan", "red", "green"), pch = c(1, 1, 4, 0, 2)), 
            text = list(c("Spectra vs Don", "Spectra vs Tyler", "Tyler vs Don", "Patrick vs Don", "Patrick vs Tyler")))))', 
            file = "NN_Pred_Bias_Corr_vs_TMA_by_Reader_and_Year.png")


# Jitter TMA
browsePlot('print(xyplot(NN_Pred_Median ~ jitter(TMA, 7) | factor(Year), group = factor(Reader), data = New_Ages_Double,  col = c("dodgerblue", "red", "green", "goldenrod3", "cyan"), pch = c(1, 0, 2, 1, 4), 
            ylab = paste0("NN Pred Median (decimal; not corrected) or TMA Secondary Double Read"), xlab = "TMA Best Age (circles) or TMA Primary Read (x, square, triangle); All TMA Jittered",
            panel = function(...) {panel.xyplot.loess(..., span = 1.0); panel.abline(0, 1) }, as.table = TRUE,
            key = list(space = "right", points = list(col=c("dodgerblue", "goldenrod3", "cyan", "red", "green"), pch = c(1, 1, 4, 0, 2)), 
            text = list(c("Spectra vs Don", "Spectra vs Tyler", "Tyler vs Don", "Patrick vs Don", "Patrick vs Tyler")))))', 
            file = "NN_Pred_Bias_Corr_vs_Jittered_TMA_by_Reader_and_Year_.png")






# ---------------------------------------------------------------------------------------------------------------------------


main = expression(paste("Plot of Real Numbers (", \mathbf{R}, ")")))


# main = "Reader 1 (≤ 2017) in Blue; Reader 2 (> 2017) in Goldenrod",


Don_Tyler_Double_2 <- openxlsx::read.xlsx("C:/SIDT/Chilipepper/Fit Von Bert & Double Reads/All_Double_Reads_CLPR_20250319.xlsx", sheet = 2, startRow = 3, detectDates = TRUE)
Don_Tyler_Double_2$Year <- as.character(years(Don_Tyler_Double_2$catch_date))
headTail(Don_Tyler_Double_2)























