

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

base::load(file = "Sable_2017_2019 7 Nov 2022.RData")

dim(Sable_2017_2019)    
Sable_2017_2019_noEx <- Sable_2017_2019[Sable_2017_2019[, '4004'] < 0.7, ]    
dim(Sable_2017_2019_noEx)  

# -----------------


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


# Use correlation plot to define the best freg. areas to use [using identify()]

Sable_Spec_Cor[c(936, 1148), ]
     Freq           Cor
936  5277 0.46227826962
1148 3641 0.46627681593


# WB = Wide Best area from correlation plot
Bands <- as.numeric(names(Sable_2017_2019_noEx[, 2:1155]))
Bands.WB <- Bands[Bands >= 3641 & Bands <= 5277]
Bands.TF <- Bands %in% Bands.WB
len(Bands.TF) # 1154
sum(Bands.TF) #   213

dim(Sable_2017_2019_noEx)
[1] 1560 1175

Sable_2017_2019_WB <- Sable_2017_2019_noEx[, c(T, Bands.TF, rep(T, len(1156:1175)))]
Sable_2017_2019_WB <- Sable_2017_2019_WB[!is.na(Sable_2017_2019_WB$TMA), ]

plotly.Spec(Sable_2017_2019_WB, 'all')
par(mfrow = c(2,1))
plotly.Spec(Sable_2017_2019_WB[Sable_2017_2019_WB$Year %in% 2017, ], 'all')
plotly.Spec(Sable_2017_2019_WB[Sable_2017_2019_WB$Year %in% 2019, ], 'all')

plotly.Spec(Sable_2017_2019_WB, 'all', facetGroup = 'scanGroup')


d <- plotly.Spec(Sable_2017_2019_WB[!is.na(Sable_2017_2019_noEx$TMA), ], 'all', colorGroup = 'TMA', facetGroup = 'scanGroup', plot = FALSE) 
d %>% plot_ly(x = ~Band, y = ~Value, z = ~Scan) %>% group_by(Scan) %>% add_lines(color = ~TMA, colors = rainbow(length(unique(d$Scan)))) 
d %>% plot_ly(x = ~Band, y = ~Value, z = ~Scan) %>% group_by(TMA) %>% add_lines(color = ~scanGroup, colors = rainbow(length(unique(d$Scan)))) 



         
# =============== iPLSR - following Jordan =========================

base::load(file = "Sable_2017_2019 7 Nov 2022.RData")
      
Sable_Spectra_2017_2019 <- Sable_2017_2019_noEx[!is.na(Sable_2017_2019_noEx$TMA), Bands.TF] # Spectra matrix
dim(Sable_Spectra_2017_2019) #  1358  213

Sable_Age_2017_2019 <- as.numeric(Sable_2017_2019_WB$TMA) # Vector of Ages 
len(Sable_Age_2017_2019) #  1358
Sable_Age_2017_2019.fac <- factor(Sable_Age_2017_2019)      


###################################################################################################################
### Perform Savitzky-Golay 1st derivative with 17 point window smoothing 2rd order polynomial fit and visualize ###
###################################################################################################################
### NOTE ### If there are multiple years of data, all subsequent transformations should be applied to the whole data set, then re-subset
          
Sable_Spectra_2017_2019.sg <- data.frame(prospectr::savitzkyGolay(Sable_Spectra_2017_2019, p = 2, w = 15, m = 1)) 
Sable_Spectra_2017_2019.Age.sg <- data.frame(TMA = Sable_Age_2017_2019, prospectr::savitzkyGolay(Sable_Spectra_2017_2019, p = 2, w = 15, m = 1))  


####################################################
###  iPLS algorithm in mdatools  ### 
####################################################
 
Sable_Spectra_2017_2019.iPLS.F <- mdatools::ipls(Sable_Spectra_2017_2019.sg, Sable_Age_2017_2019, glob.ncomp = 10, center = T, scale = T, cv = 100,
                  int.ncomp = 10, int.num = 10, ncomp.selcrit = "min", method = "forward", silent = F)
# save(Sable_Spectra_2017_2019.iPLS.F, file = 'Sable_Spectra_2017_2019.iPLS.F 11 Nov 2022.RData')              

summary(Sable_Spectra_2017_2019.iPLS.F)

# plot the newly selected spectra regions ??
dev.new()
plot(Sable_Spectra_2017_2019.iPLS.F)     

# dev.new()
# plot(Sable_Spectra_2017_2019.iPLS.F, main = NULL)          

# plot predictions before and after selection
dev.new()
par(mfrow = c(2, 1))
mdatools::plotPredictions(Sable_Spectra_2017_2019.iPLS.F$gm)
mdatools::plotPredictions(Sable_Spectra_2017_2019.iPLS.F$om)

dev.new()
mdatools::plotRMSE(Sable_Spectra_2017_2019.iPLS.F)

# RMSE  before and after selection
dev.new()
par(mfrow = c(2, 1))

# Fine the ylim to apply to both figures
mdatools::plotRMSE(Sable_Spectra_2017_2019.iPLS.F$gm)
mdatools::plotRMSE(Sable_Spectra_2017_2019.iPLS.F$om)

# Use the ylim for both
mdatools::plotRMSE(Sable_Spectra_2017_2019.iPLS.F$gm, ylim = c(4, 11))
mdatools::plotRMSE(Sable_Spectra_2017_2019.iPLS.F$om, ylim = c(4, 11))


# Select out vars
Sable_Spectra_2017_2019.iPLS.vars <- Sable_Spectra_2017_2019.iPLS.F$var.selected
(p <- length(Sable_Spectra_2017_2019.iPLS.vars))

Sable_Spectra_2017_2019.sg.iPLS <- data.frame(Sable_Spectra_2017_2019.sg[, Sable_Spectra_2017_2019.iPLS.vars])
Sable_Spectra_2017_2019.Age.sg.iPLS <- data.frame(Age = Sable_Age_2017_2019, Sable_Spectra_2017_2019.sg.iPLS)
dim(Sable_Spectra_2017_2019.Age.sg.iPLS)

#Split by age - Age.0.avg, Age.1.avg, ...
AGES <- sort(unique(Sable_Age_2017_2019))
for (i in AGES) { 
    Subset <- subset(Sable_Spectra_2017_2019.Age.sg.iPLS, Sable_Age_2017_2019 == i)
    if(i == 0) cat("\n")
    cat("Dim of", paste0('Age', i), " = ", dim(Subset), "\n")
    assign(paste0('Age', i, '.avg'), apply(Subset, 2, mean))  # Character or numeric works  in subset()
    cat("Length of", paste0('Age', i, '.avg'), " = ", length(eval(parse(text = paste0('Age', i, '.avg')))), "\n\n")
}


### rbind, transpose and plot the averaged spectra matrix-
age.avg <- NULL
for (i in AGES)
     age.avg <- rbind(age.avg, eval(parse(text = paste0('Age', i, '.avg'))))

dim(age.avg)
age.avg <- sort.f(age.avg)
age.avg[1:5, 1:10]
age.avg[1:3, (ncol(age.avg) - 3):ncol(age.avg)]
p + 1 # vars plus age 

t.age.avg <- data.frame(t(age.avg[, -1])) # remove the age column so there is only an xmatrix
dim(t.age.avg)
t.age.avg[1:3, 1:5]


# Plot the transformed spectra
dev.new(width = 14, height = 8)
par(mar=c(5,5,1,1))
plot(t.age.avg$X10, xlab = "Wavenumber Range", ylab = "Absorbance", cex.lab = 1, type = 'n', 
            ylim = c(min(t.age.avg, na.rm = TRUE) - abs(min(t.age.avg, na.rm = TRUE) * 0.025), max(t.age.avg, na.rm = TRUE) + abs(max(t.age.avg, na.rm = TRUE) * 0.025)))
Cols <- rainbow(1.2 * length(AGES))
for ( i in AGES)
    lines(t.age.avg[[paste0('X', i)]],  col = Cols[i + 1], lwd = 2)
legend("topleft", legend = paste(AGES, "Years"), col = c('red', 'blue', Cols[length(AGES)]), lty=1, lwd = 2,cex=.8)



#########################################
### Conduct PLSr on iPLSr selected data ###
#########################################

PLSr <- mdatools::pls(Sable_Spectra_2017_2019.sg.iPLS, Sable_Age_2017_2019, ncomp = 10, center = T, scale = F, cv = 100,
            method = "simpls", alpha = 0.05, ncomp.selcrit = "min")
summary(PLSr)
dev.new()
plot(PLSr)


set.seed(c(777, 747)[1])
index <- 1:nrow(Sable_Spectra_2017_2019.sg.iPLS)
testindex <- sample(index, trunc(length(index)/3))
x.testset <- Sable_Spectra_2017_2019.sg.iPLS[testindex, ]
x.trainset <- Sable_Spectra_2017_2019.sg.iPLS[-testindex, ]   
y.test <- Sable_Age_2017_2019[testindex]
y.train <- Sable_Age_2017_2019[-testindex]

PLSr <- mdatools::pls(x.trainset, y.train, ncomp = 10, center = T, scale = F, cv = 100,
           method = "simpls", alpha = 0.05, ncomp.selcrit = "min", x.test = x.testset, y.test = y.test)
summary(PLSr)
dev.new()
plot(PLSr)
           
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
plot(Reference_Age, Predicted_Age, ylim = c(0,12), xlim = c(0,12), col = "dodgerblue")
abline(0, 1, lwd = 2)
abline(0, Slope, col = "dodgerblue", lwd = 2) 


# Integer Predicted age with reference age jitter
# dev.new(height = 8, width = 15)
par(mar = c(5,5,1,1))
plot(jitter(Reference_Age), round(Predicted_Age), ylim = c(0,12), xlim = c(0,12), col = "dodgerblue")
abline(0, 1, lwd = 2)
abline(0, Slope, col = "dodgerblue", lwd = 2) 

summary(lm(Predicted_Age ~ Reference_Age))$r.squared
summary(lm(round(Predicted_Age) ~ Reference_Age))$r.squared

Table(NIRS_PLSr_AGE = round(Predicted_Age), TMA = Reference_Age)
e1071::classAgreement(Table(NIRS_PLSr_AGE = round(Predicted_Age), TMA = Reference_Age)) # $diag 0.03863135
e1071::classAgreement(Table(NIRS_PLSr_AGE = round(Predicted_Age), TMA = Reference_Age), match.names = TRUE) # diag  0.1479029


sum(abs(Reference_Age - round(Predicted_Age))) # 2679

(Results <- data.frame(Reference_Age, Predicted_Age))[1:10, ]

# Store results in an excel.csv file
write.csv(Results, file ="10_smoothing_iPLSR_Res.csv", row.names = FALSE)



