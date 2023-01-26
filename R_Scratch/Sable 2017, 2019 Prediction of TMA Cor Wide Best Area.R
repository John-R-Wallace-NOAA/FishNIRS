
sourceFunctionURL <- function (URL,  type = c("function", "script")[1]) {
       " # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() "
       require(httr)
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

#Toolbox functions
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/openwd.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/lib.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/openwd.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/sort.f.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/predicted_observed_plot.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/residuals_plot.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/R_squared_RMSE_MAE.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/as.num.R")

#FishNIRS funtion
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")


# Set Path
PATH <- "W:/ALL_USR/JRW/SIDT/Sablefish/"
setwd(PATH) # set working directory to folder containing spectral files
getwd()
openwd()
load('.RData')
# base::load('.RData')


# Chat GPT suggestion doesn't work
# remotes::install_url("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/text/lib.txt")


# Source the saved code on GitHub
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R_Scratch/Sable 2017, 2019 Prediction of TMA Cor Wide Best Area.R", type = "script")
# gitAFile("John-R-Wallace-NOAA/FishNIRS/master/R_Scratch/Sable 2017, 2019 Prediction of TMA Cor Wide Best Area.R", type = "script", verbose = TRUE)


#  Packages
lib(openxlsx)
lib(data.table)
lib(mdatools)
lib(dplyr)
lib(hyperSpec) # http://hyperspec.r-forge.r-project.org
lib(prospectr)
lib(e1071)
lib(rpart)
lib(vegan)
lib(ggplot2)
lib(ggjoy)
lib(ggridges)
lib(plotly)

# install and load simplerspec
# install.packages("remotes")
# remotes::install_github("philipp-baumann/simplerspec")

# lib("philipp-baumann/simplerspec") # https://github.com/philipp-baumann/simplerspec
lib(simplerspec)

# ------------------Load up Spectra Data --------------------------------------------------

base::load(file = "Sable_2017_2019 21 Nov 2022.RData")
options(digits = 11)
Sable_2017_2019[1:2, c(1:2, 1153:1184)] # Look at first and last columns
plotly.Spec(Sable_2017_2019, 'all') # Missing TMA (NA) included as grey lines

# Remove extreme data
dim(Sable_2017_2019)    
Sable_2017_2019_noEx <- Sable_2017_2019[Sable_2017_2019[, '4004'] < 0.7, ]    
dim(Sable_2017_2019_noEx)  



# plotly.Spec() on Sable_2017_2019_noEx 
plotly.Spec(Sable_2017_2019_noEx, 'all') # Missing TMA (NA) included as grey lines
plotly.Spec(Sable_2017_2019_noEx[!is.na(Sable_2017_2019_noEx$TMA), ], 'all') #Removed missing TMA 


# -------------- Look at correlations for best freq band and best single band ------------------------------------------------------

# Plot of all lowess smooths from each Freq vs TMA, colored by Freq level
dev.new()
Freq <-  as.numeric(names(Sable_2017_2019_noEx[, 2:1155]))
Cols <- rainbow(1.2 * length(Freq))
smoothing.param <- 2/3
plot.lowess(Sable_2017_2019_noEx$TMA, Sable_2017_2019_noEx[, names(Sable_2017_2019_noEx) %in% Freq[1]], smoothing.param, 
   line.col = col.alpha(Cols[1]), type = 'n', ylim = c(0.38, 0.68))
for(i in 2:length(Freq)) 
   lowess.line(Sable_2017_2019_noEx$TMA, Sable_2017_2019_noEx[, names(Sable_2017_2019_noEx) %in% Freq[i]], smoothing.param, col = col.alpha(Cols[i]))

lowess.line(Sable_2017_2019_noEx$TMA, Sable_2017_2019_noEx[, names(Sable_2017_2019_noEx) %in% 4081], smoothing.param, col = 'black', lwd = 1.5) 


# Correlation, with extremes removed, of each scan freq. versus  TMA ages

# Highest freq => low absorbance 
dev.new()
plot.lowess(Sable_2017_2019_noEx$TMA, Sable_2017_2019_noEx[, names(Sable_2017_2019_noEx) %in% '12490'])

# Low freq => highish absorbance
dev.new()
plot.lowess(Sable_2017_2019_noEx$TMA, Sable_2017_2019_noEx[, names(Sable_2017_2019_noEx) %in% '3595'])

(Sable_Spec_Cor <- renum(data.frame(Freq = as.numeric(names(Sable_2017_2019_noEx[, 2:1155])), Cor = cor(Sable_2017_2019_noEx[, 2:1155], Sable_2017_2019_noEx$TMA, use = "na.or.complete"))))[1:4,]
sort.f(Sable_Spec_Cor, 'Cor', rev = T)[1:5,]
  Freq       Cor
1 4081 0.7233784
2 4073 0.7233294
3 4089 0.7229155
4 4066 0.7228655
5 4058 0.7220165

# Best cor
dev.new()
plot.lowess(Sable_2017_2019_noEx$TMA, Sable_2017_2019_noEx[, names(Sable_2017_2019_noEx) %in% '4081'])


# Best top 5 cor using lowess.  
# Methods: "fmm", "periodic", "natural", "monoH.FC", "hyman" are available in lowess() via the stats::splinefun() function - the methods appear to make no difference here
# The  NA's have to be removed for lowess(), but not for the 'newer' loess() below
dev.new()
plot.lowess(rep(Sable_2017_2019_noEx$TMA, 5), c(as.matrix(Sable_2017_2019_noEx[, names(Sable_2017_2019_noEx) %in% c('4081', '4073', '4089', '4066', '4058')])), method = 'fmm')

tmp <- na.omit(cbind(rep(Sable_2017_2019_noEx$TMA, 5), c(as.matrix(Sable_2017_2019_noEx[, names(Sable_2017_2019_noEx) %in% c('4081', '4073', '4089', '4066', '4058')]))))
lo.Sable <- lowess(tmp[,1], tmp[,2])
x.new <- c(0, 3, 5, 6, 20, 25, 43, 55, 100)
points(x.new, predict.lowess(lo.Sable, x.new, method = 'fmm'), col = 'red', pch =19) 

dev.new()  
plot.lowess.range(rep(Sable_2017_2019_noEx$TMA, 5), c(as.matrix(Sable_2017_2019_noEx[, names(Sable_2017_2019_noEx) %in% c('4081', '4073', '4089', '4066', '4058')])))


# Best top 5 cor using loess
dev.new()
plot.loess(rep(Sable_2017_2019_noEx$TMA, 5), c(as.matrix(Sable_2017_2019_noEx[, names(Sable_2017_2019_noEx) %in% c('4081', '4073', '4089', '4066', '4058')])))

loess.Sable <- loess(c(as.matrix(Sable_2017_2019_noEx[, names(Sable_2017_2019_noEx) %in% c('4081', '4073', '4089', '4066', '4058')])) ~ rep(Sable_2017_2019_noEx$TMA, 5)) 
x.new <- c(0, 3, 5, 6, 20, 25, 43, 55, 100)
points(x.new, predict(loess.Sable, x.new), col = 'red', pch =19) 

dev.new() 
plot.loess.range(rep(Sable_2017_2019_noEx$TMA, 5), c(as.matrix(Sable_2017_2019_noEx[, names(Sable_2017_2019_noEx) %in% c('4081', '4073', '4089', '4066', '4058')])))


# Plot of all lowess smooths from each Freq vs TMA, colored by correlation level
dev.new()
Freq <-  as.numeric(names(Sable_2017_2019_noEx[, 2:1155]))
Cols <- rainbow(1.2 * length(Freq))
smoothing.param <- 2/3
plot.lowess(Sable_2017_2019_noEx$TMA, Sable_2017_2019_noEx[, names(Sable_2017_2019_noEx) %in% Freq[1]], smoothing.param, type = 'n',
     line.col = col.alpha(Cols[1000 * Sable_Spec_Cor$Cor[Sable_Spec_Cor$Freq %in% Freq[1]]]), ylim = c(0.38, 0.68))
for(i in 2:length(Freq)) 
   lowess.line(Sable_2017_2019_noEx$TMA, Sable_2017_2019_noEx[, names(Sable_2017_2019_noEx) %in% Freq[i]], smoothing.param, 
              col = Cols[1000 * abs(Sable_Spec_Cor$Cor[Sable_Spec_Cor$Freq %in% Freq[i]])])
           
lowess.line(Sable_2017_2019_noEx$TMA, Sable_2017_2019_noEx[, names(Sable_2017_2019_noEx) %in% 4081], smoothing.param, col = 'black', lwd = 1.5) 



# Look at loess fit to predict ages using freq. - doesn't work well :-/  But I now can predict values using lowess() and loess()  :-)
#  Top cor only
Freq_Cor_Top <- Sable_2017_2019_noEx[, '4081']
TMA <- Sable_2017_2019_noEx$TMA

loess.Sable <- loess(TMA ~ Freq_Cor_Top, span = 0.75) 
loess_Pred_Ages <- predict(loess.Sable, Freq_Cor_Top_5)
# Table(round(loess_5_Pred_Ages), TMA)

dev.new()
plot(Freq_Cor_Top, jitter(TMA))
points(Freq_Cor_Top, loess_Pred_Ages, col = 'red', pch =19) 


# Top 5 cor
Freq_Cor_Top_5 <- c(as.matrix(Sable_2017_2019_noEx[, names(Sable_2017_2019_noEx) %in% c('4081', '4073', '4089', '4066', '4058')])) 
TMA_5 <- rep(Sable_2017_2019_noEx$TMA, 5)

loess.Sable <- loess(TMA_5 ~ Freq_Cor_Top_5, span = 0.35) 
loess_5_Pred_Ages <- predict(loess.Sable, Freq_Cor_Top_5)
# Table(round(loess_5_Pred_Ages), TMA_5)

dev.new()
plot(Freq_Cor_Top_5, jitter(TMA_5))
points(Freq_Cor_Top_5, loess_5_Pred_Ages, col = 'red', pch =19) 




# ------ Predict age using only the best correlated freq ------

# Best cor
dev.new()
plot.lowess(Sable_2017_2019_noEx$TMA, Sable_2017_2019_noEx[, '4081'])

Sable_2017_2019_Best_Cor <- na.omit(Sable_2017_2019_noEx[, c('4081', 'TMA')])
names(Sable_2017_2019_Best_Cor)[1] <- 'Freq_4081'


#  ---- Straight line and polynomial fits ----
dev.new()
plot.lowess(Sable_2017_2019_Best_Cor$Freq_4081, Sable_2017_2019_Best_Cor$TMA)

summary(G1 <- glm(TMA ~ Freq_4081, data = Sable_2017_2019_Best_Cor ))
points(Sable_2017_2019_Best_Cor$Freq_4081, predict(G1), col = 'violet')

summary(G2 <- glm(TMA ~ poly(Freq_4081, 2), data = Sable_2017_2019_Best_Cor ))
points(Sable_2017_2019_Best_Cor$Freq_4081, predict(G2), col = 'red')

summary(G3 <- glm(TMA ~ poly(Freq_4081, 3), data = Sable_2017_2019_Best_Cor ))
points(Sable_2017_2019_Best_Cor$Freq_4081, predict(G3), col = 'dodgerblue')

summary(G4 <- glm(TMA ~ poly(Freq_4081, 4), data = Sable_2017_2019_Best_Cor ))
points(Sable_2017_2019_Best_Cor$Freq_4081, predict(G4), col = 'magenta')

summary(G5 <- glm(TMA ~ poly(Freq_4081, 5), data = Sable_2017_2019_Best_Cor ))
points(Sable_2017_2019_Best_Cor$Freq_4081, predict(G5), col = 'cyan')

# AIC goes up with 6th degree poly
summary(G6 <- glm(TMA ~ poly(Freq_4081, 6), data = Sable_2017_2019_Best_Cor ))
points(Sable_2017_2019_Best_Cor$Freq_4081, predict(G6), col = 'purple')

# Therefore back to 5th degreee poly
summary(lm(Sable_2017_2019_Best_Cor$TMA ~ predict(G5)))$r.squared
Table(Best_Freq_Cor_AGE = round(predict(G5)), TMA = Sable_2017_2019_Best_Cor$TMA)
e1071::classAgreement(Table(Best_Freq_Cor_AGE = round(predict(G5)), TMA = Sable_2017_2019_Best_Cor$TMA)) # $diag 0.16126656848
e1071::classAgreement(Table(Best_Freq_Cor_AGE = round(predict(G5)), TMA = Sable_2017_2019_Best_Cor$TMA), match.names = TRUE) # diag  0.10162002946

sum(abs(Sable_2017_2019_Best_Cor$TMA - round(predict(G5)))) # 5660



# TableCurveFit 2D best nonlinear fit - Power y = a +bX^c  - Doesn't work as good as the iPLSR method below 
# Sable_2017_2019_Best_Cor$TMA_Power_Eq_Predict  <-  -5.3167806 + 1627183.4 * Sable_2017_2019_Best_Cor$Freq_4081 ^ 20.528681
Sable_2017_2019_Best_Cor$TMA_Power_Eq_Predict  <-  25777445 + 26.24.2351 * Sable_2017_2019_Best_Cor$Freq_4081

dev.new()
plot.lowess(Sable_2017_2019_Best_Cor$Freq_4081, Sable_2017_2019_Best_Cor$TMA)
points(Sable_2017_2019_Best_Cor$Freq_4081, Sable_2017_2019_Best_Cor$TMA_Power_Eq_Predict, col = 'red')

Table(round(Sable_2017_2019_Best_Cor$TMA_Power_Eq_Predict), Sable_2017_2019_Best_Cor$TMA)




# -------- Defining the Wide Best area (WB) via  correlations --------
dev.new()
plot(Sable_Spec_Cor$Freq, Sable_Spec_Cor$Cor)

dev.new()
plot.lowess(Sable_2017_2019_noEx$TMA, Sable_2017_2019_noEx[, '4081'])

dev.new()
plot.lowess(jitter(Sable_2017_2019_noEx$TMA), Sable_2017_2019_noEx[, '4081'])


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

dim(Sable_2017_2019_noEx) #  1560 1175

Sable_2017_2019_WB <- Sable_2017_2019_noEx[, c(T, Bands.TF, rep(T, len(1156:1184)))]
Sable_2017_2019_WB <- Sable_2017_2019_WB[!is.na(Sable_2017_2019_WB$TMA), ]


# ---- 2D plotly.Spec() and 3D plot_ly() plots -------

#  2D plotly.Spec()
plotly.Spec(Sable_2017_2019_WB, 'all')
plotly.Spec(Sable_2017_2019_WB, 'all', colorGroup = 'scanGroup')

plotly.Spec(Sable_2017_2019_WB[Sable_2017_2019_WB$Year %in% 2017, ], 'all')
plotly.Spec(Sable_2017_2019_WB[Sable_2017_2019_WB$Year %in% 2019, ], 'all')

plotly.Spec(Sable_2017_2019_WB, 'all', facetGroup = 'scanGroup')



#  Year with nrows = 2 - one each for years (2017, 2019)
plotly.Spec(Sable_2017_2019_WB, 'all', facetGroup = 'Year')

plotly.Spec(Sable_2017_2019_WB, 'all', facetGroup = 'Year', contColorVar = TRUE)


# #    # Year with nrows = 2  using subplot()
# #    d1 <- plotly.Spec(Sable_2017_2019_WB[Sable_2017_2019_WB$Year %in% 2017, ], 'all', plot = FALSE)
# #    p1 <- d1 %>% plot_ly(x = ~Band, y = ~Value) %>% group_by(Scan) %>% add_lines(color = ~TMA, colors = rainbow(length(unique(d$Scan)))) 
# #    
# #    d2 <- plotly.Spec(Sable_2017_2019_WB[Sable_2017_2019_WB$Year %in% 2019, ], 'all', plot = FALSE)
# #    p2 <- d2 %>% plot_ly(x = ~Band, y = ~Value) %>% group_by(Scan) %>% add_lines(color = ~TMA, colors = rainbow(length(unique(d$Scan)))) 
# #    
# #    subplot(p1, p2, nrows = 2, shareX = TRUE, titleX = FALSE)


# 3D
d <- plotly.Spec(Sable_2017_2019_WB, 'all', colorGroup = 'TMA', facetGroup = 'scanGroup', plot = FALSE) 
d %>% plot_ly(x = ~Band, y = ~Value, z = ~Scan) %>% group_by(Scan) %>% add_lines(color = ~TMA, colors = rainbow(length(unique(d$Scan)))) 

d %>% plot_ly(x = ~Band, y = ~Value, z = ~Scan) %>% group_by(Scan) %>% add_lines(color = ~scanGroup, colors = rainbow(length(unique(d$Scan)))) 

d %>% plot_ly(x = ~Band, y = ~Value, z = ~Scan) %>% group_by(TMA) %>% add_lines(color = ~scanGroup, colors = rainbow(length(unique(d$Scan)))) 
d %>% plot_ly(x = ~Band, y = ~Value, z = ~Scan) %>% group_by(TMA) %>% add_lines(color = ~TMA, colors = rainbow(length(unique(d$Scan)))) 

79/11
        
# =============== iPLSR - following Jordan using selected data =========================

Data_Sel <- c(Sable_2017_2019_noEx = 1, Sable_2017_2019_WB = 2)[c(1, 2)[1]]
Sable_2017_2019_Sel <- list(Sable_2017_2019_noEx, Sable_2017_2019_WB)[[Data_Sel]]
Bands_Sel <- list(Bands, Bands.WB)[[Data_Sel]]

Sable_2017_2019_Sel <- Sable_2017_2019_Sel[!is.na(Sable_2017_2019_Sel$TMA), ]
dim(Sable_2017_2019_Sel)

Sable_Spectra_2017_2019 <- Sable_2017_2019_Sel[, as.character(Bands_Sel)] # Spectra matrix 
dim(Sable_Spectra_2017_2019) #  1358 1154 for Ex;  1358  213 for WB


# Sable_2017_2019_Sel[1:3, c(1:4, 1156:1184)]
plotly.Spec(Sable_2017_2019_Sel, 'all')

Sable_TMA_2017_2019 <- as.numeric(Sable_2017_2019_Sel$TMA) # Vector of Ages  - 1,358 oties
length(Sable_TMA_2017_2019) #  1358
Sable_TMA_2017_2019.fac <- factor(Sable_TMA_2017_2019)      

# Maximum number of components to calculate.
nComp <- c(10, 15)[2]

###################################################################################################################
### Perform Savitzky-Golay 1st derivative with 17 point window smoothing 2rd order polynomial fit and visualize ###
### Intro: http://127.0.0.1:30354/library/prospectr/doc/prospectr.html
###################################################################################################################
### NOTE ### If there are multiple years of data, all subsequent transformations should be applied to the whole data set, then re-subset
     
# Savitzky-Golay smoothing     
Sable_Spectra_2017_2019.sg <- data.frame(prospectr::savitzkyGolay(Sable_Spectra_2017_2019, m = 1, p = 2, w = 15))  # 'Sable_Spectra_2017_2019' is either 'Sable_2017_2019_noEx or 'Sable_2017_2019_WB' selected above
# Sable_Spectra_2017_2019.sg <- data.frame(prospectr::gapDer(Sable_Spectra_2017_2019, m = 1, w = 11, s = 5)) 
dim(Sable_Spectra_2017_2019.sg) #  1,358 1,140 for Ex;   1,358   199 for WB

Sable_Spectra_2017_2019.Age.sg <- data.frame(TMA = Sable_TMA_2017_2019, Sable_Spectra_2017_2019.sg) 

# Add back metadata for plotting
# cbind(Sable_2017_2019_Sel[, 1, drop = FALSE], Sable_Spectra_2017_2019.sg, Sable_2017_2019_Sel[, 1156:1184])[1:3, c(1:4, 1140:1170)]
Sable_Spectra_2017_2019.sg.PLOT <- cbind(Sable_2017_2019_Sel[, 1, drop = FALSE], Sable_Spectra_2017_2019.sg, Sable_2017_2019_Sel[, 1156:1184])

# 2D plot 
plotly.Spec(Sable_Spectra_2017_2019.sg.PLOT, 'all')

# 3D plot
d <- plotly.Spec(Sable_Spectra_2017_2019.sg.PLOT, 'all', colorGroup = 'TMA', facetGroup = 'scanGroup', plot = FALSE) 
d %>% plot_ly(x = ~Band, y = ~Value, z = ~Scan) %>% group_by(Scan) %>% add_lines(color = ~TMA, colors = rainbow(length(unique(d$Scan)))) 



####################################################
###  iPLS algorithm in mdatools  ### 
####################################################
 
Sable_Spectra_2017_2019.iPLS.F <- mdatools::ipls(Sable_Spectra_2017_2019.sg, Sable_TMA_2017_2019, glob.ncomp = nComp, center = TRUE, scale = TRUE, cv = 100,
                  int.ncomp = nComp, int.num = nComp, ncomp.selcrit = "min", method = "forward", silent = FALSE)
# save(Sable_Spectra_2017_2019.iPLS.F, file = 'Sable_Spectra_2017_2019.iPLS.F 11 Nov 2022.RData')              

summary(Sable_Spectra_2017_2019.iPLS.F)

# plot the newly selected spectra regions ??
dev.new()
plot(Sable_Spectra_2017_2019.iPLS.F)     
Sable_Spectra_2017_2019.iPLS.F$int.selected
sort(Sable_Spectra_2017_2019.iPLS.F$var.selected)

# dev.new()  - With a main title
# plot(Sable_Spectra_2017_2019.iPLS.F, main = NULL)          

# plot predictions before and after selection
dev.new()
par(mfrow = c(2, 1))
mdatools::plotPredictions(Sable_Spectra_2017_2019.iPLS.F$gm) # gm = global PLS model with all variables included
mdatools::plotPredictions(Sable_Spectra_2017_2019.iPLS.F$om) # om = optimized PLS model with selected variables

dev.new()
mdatools::plotRMSE(Sable_Spectra_2017_2019.iPLS.F)


# RMSE  before and after selection

# Find the ylim to apply to both figures  and over all areas and WB
dev.new()
par(mfrow = c(2, 1))
mdatools::plotRMSE(Sable_Spectra_2017_2019.iPLS.F$gm)
mdatools::plotRMSE(Sable_Spectra_2017_2019.iPLS.F$om)

# Use the ylim for both
dev.new()
par(mfrow = c(2, 1))
mdatools::plotRMSE(Sable_Spectra_2017_2019.iPLS.F$gm, ylim = c(3.4, 11))
mdatools::plotRMSE(Sable_Spectra_2017_2019.iPLS.F$om, ylim = c(3.4, 11))



# Select out vars
(p <- length(Sable_Spectra_2017_2019.iPLS.F$var.selected)) # 380 freq selected out of a total of 1140

Sable_Spectra_2017_2019.sg.iPLS <- data.frame(Sable_Spectra_2017_2019.sg[, sort(Sable_Spectra_2017_2019.iPLS.F$var.selected)])
Sable_Spectra_2017_2019.Age.sg.iPLS <- data.frame(Age = Sable_TMA_2017_2019, Sable_Spectra_2017_2019.sg.iPLS)
dim(Sable_Spectra_2017_2019.Age.sg.iPLS)

# 2D plot
Sable_Spectra_2017_2019.sg.iPLS.PLOT <- cbind(Sable_2017_2019_Sel[, 1, drop = FALSE], Sable_Spectra_2017_2019.sg.iPLS, Sable_2017_2019_Sel[, 1156:1184])
plotly.Spec(Sable_Spectra_2017_2019.sg.iPLS.PLOT, 'all')



# Plot the transformed spectra by age using only variables selected using iPLS
(Sable_Spectra_2017_2019.Age.sg.iPLS.Long <- reshape2::melt(Sable_Spectra_2017_2019.Age.sg.iPLS, id = 'Age', variable.name = 'Freq', value.name = 'Absorbance'))[1:4, ]
Sable_Spectra_2017_2019.Age.sg.iPLS.Long$Freq <- as.numeric(substring(Sable_Spectra_2017_2019.Age.sg.iPLS.Long$Freq, 2))
Sable_Spectra_2017_2019.Age.sg.iPLS.Long <- sort.f(Sable_Spectra_2017_2019.Age.sg.iPLS.Long, 'Freq')

dev.new(width = 18, height = 10)
xyplot(Absorbance ~ Freq, group = factor(Age), data = Sable_Spectra_2017_2019.Age.sg.iPLS.Long, type = 'l')  
xyplot(Absorbance ~ Freq | factor(Age), data = Sable_Spectra_2017_2019.Age.sg.iPLS.Long, type = 'l')  
xyplot(Absorbance ~ Freq, group = factor(Age), data = Sable_Spectra_2017_2019.Age.sg.iPLS.Long[Sable_Spectra_2017_2019.Age.sg.iPLS.Long$Age %in% 10, ])  
xyplot(Absorbance ~ Freq | factor(Age), data = Sable_Spectra_2017_2019.Age.sg.iPLS.Long[Sable_Spectra_2017_2019.Age.sg.iPLS.Long$Age %in% c(1, 5, 30, 40), ], type = 'l')  
xyplot(Absorbance ~ Freq | factor(Age), data = Sable_Spectra_2017_2019.Age.sg.iPLS.Long[Sable_Spectra_2017_2019.Age.sg.iPLS.Long$Age %in% c(1, 5, 30, 40), ]) 

# plotly::ggplotly
Sable_Spectra_2017_2019.Age.sg.iPLS.Agg <- aggregate(list(Absorbance = Sable_Spectra_2017_2019.Age.sg.iPLS.Long$Absorbance), 
     list(Freq = Sable_Spectra_2017_2019.Age.sg.iPLS.Long$Freq, Age = Sable_Spectra_2017_2019.Age.sg.iPLS.Long$Age), mean, na.rm = TRUE)
 
Sable_Spectra_2017_2019.Age.sg.iPLS.Agg$Age <- ordered(Sable_Spectra_2017_2019.Age.sg.iPLS.Agg$Age, sort(unique(Sable_Spectra_2017_2019.Age.sg.iPLS.Agg$Age)))
plotly::ggplotly(ggplot2::ggplot(data = Sable_Spectra_2017_2019.Age.sg.iPLS.Agg, aes(x = Freq, y = Absorbance, z = Age)) + geom_line(aes(colour = Age), size = 0.2) + 
                    scale_color_manual(values=rainbow(length(unique(Sable_Spectra_2017_2019.Age.sg.iPLS.Agg$Age)), alpha = 1)))
                 

# --------------- Try ipls() with smoothed spectra data and metadata  - NO METADATA WAS SELECTED ------------------------

# Remove NA's with predictors and response together - then resplit
Sable_Spectra_2017_2019.sg.META <- na.omit(cbind(Sable_Spectra_2017_2019.sg, Sable_2017_2019_Sel[, c("latitude", "longitude", "length", "weight", "sex")], TMA = Sable_TMA_2017_2019))
Sable_Spectra_2017_2019.sg.META[1:3, c(1:2, 1140:1146)]

TMA.META <- Sable_Spectra_2017_2019.sg.META[,1146]
Sable_Spectra_2017_2019.sg.META <- Sable_Spectra_2017_2019.sg.META[, -1146]
   
Sable_Spectra_2017_2019.iPLS.META.F <- mdatools::ipls(Sable_Spectra_2017_2019.sg.META, TMA.META, glob.ncomp = nComp, center = TRUE, scale = TRUE, cv = 100,
                  int.ncomp = nComp, int.num = nComp, ncomp.selcrit = "min", method = "forward", silent = FALSE)

summary(Sable_Spectra_2017_2019.iPLS.META.F)

# Plot the newly selected spectra regions 
dev.new()
plot(Sable_Spectra_2017_2019.iPLS.META.F)     

Sable_Spectra_2017_2019.iPLS.META.F$int.selected
sort(Sable_Spectra_2017_2019.iPLS.META.F$var.selected)

Sable_Spectra_2017_2019.sg.META[, Sable_Spectra_2017_2019.iPLS.F$var.selected][1:3, c(1:3, 373:380)]
names(Sable_Spectra_2017_2019.sg.META[, Sable_Spectra_2017_2019.iPLS.F$var.selected])
                    
               
                    

#########################################
### Conduct PLSr on iPLSr selected data ###
### https://mdatools.com/docs/index.html
#########################################

# All the data 
PLSr <- mdatools::pls(Sable_Spectra_2017_2019.sg.iPLS, Sable_TMA_2017_2019, ncomp = nComp, center = TRUE, scale = FALSE, cv = 100,
            method = "simpls", alpha = 0.05, gamma = 0.01, ncomp.selcrit = "min")
summary(PLSr)
dev.new()
plot(PLSr)

#  Split the data into training set (2/3) and test set (1/3)
set.seed(c(777, 747)[1])
index <- 1:nrow(Sable_Spectra_2017_2019.sg.iPLS)
testindex <- sample(index, trunc(length(index)/3))
x.testset <- Sable_Spectra_2017_2019.sg.iPLS[testindex, ]
x.trainset <- Sable_Spectra_2017_2019.sg.iPLS[-testindex, ]   
y.test <- Sable_TMA_2017_2019[testindex]
y.train <- Sable_TMA_2017_2019[-testindex]

# Test set included in mdatools::pls()
PLSr_testset <- mdatools::pls(x.trainset, y.train, ncomp = nComp, center = T, scale = F, cv = 100,
           method = "simpls", alpha = 0.05, ncomp.selcrit = "min", x.test = x.testset, y.test = y.test)
summary(PLSr_testset)
dev.new()
plot(PLSr_testset)

# No test set    
set.seed(c(777, 747)[1])       
PLSr_No_testset <- mdatools::pls(x.trainset, y.train, ncomp = nComp, center = T, scale = F, cv = 100,
            method = "simpls", alpha = 0.05, ncomp.selcrit = "min")            
summary(PLSr_No_testset)
dev.new()
plot(PLSr_No_testset)


# -------------- Try mdatools::pls() with metadata - RESULT IS THE SAME AS WITHOUT THE METADATA --------------------------

Sable_Spectra_2017_2019.sg.META.iPLS <- cbind(Sable_Spectra_2017_2019.sg.iPLS, Sable_2017_2019_Sel[, c("latitude", "longitude", "length", "weight", "sex")])

PLSr.META <- mdatools::pls(pca.mvreplace(Sable_Spectra_2017_2019.sg.META.iPLS), Sable_TMA_2017_2019, ncomp = nComp, center = TRUE, scale = FALSE, cv = 100,
            method = "simpls", alpha = 0.05, gamma = 0.01, ncomp.selcrit = "min")
summary(PLSr)
dev.new()
plot(PLSr)

# pull the prediction values of age from the PLSr object
(compNum.META <- length(PLSr.META$res$cal$slope)) # Number of selected components
Predicted_Age.META <- data.frame(PLSr.META$cvres$y.pred[ , , 1])[, compNum]
plot(Predicted_Age, Predicted_Age.META) #  Same result


   
###########################
### Plot the PLSr results ###
###########################

PLSr <- PLSr_testset   # ************** Just the test set ****************

# pull the prediction values of age from the PLSr object
(compNum <- length(PLSr$res$cal$slope)) # Number of selected components
Predicted_Age <- data.frame(PLSr$cvres$y.pred[ , , 1])[, compNum]

# Reference Ages
Reference_Age <- PLSr$cvres$y.ref  # Equals y.train
sum(Reference_Age - y.train) # 0

# Plot Predicted vs Reference Ages
(Slope <- PLSr$res$cal$slope[length(PLSr$res$cal$slope)]) #  Slope from PLSr

dev.new()
par(mfrow = c(2,1))
# Decimal Predicted age 
# dev.new(height = 8, width = 15)
par(mar = c(5,5,1,1))
plot(Reference_Age, Predicted_Age, ylim = c(0, 75), xlim = c(0, 75), col = "dodgerblue")
abline(0, 1, lwd = 2)
abline(0, Slope, col = "dodgerblue", lwd = 2) 

# Integer Predicted age with reference age jittered
# dev.new(height = 8, width = 15)
par(mar = c(5,5,1,1))
plot(jitter(Reference_Age), round(Predicted_Age), ylim = c(0, 75), xlim = c(0 ,75), col = "dodgerblue")
abline(0, 1, lwd = 2)
abline(0, Slope, col = "dodgerblue", lwd = 2) 


# Plot predicted vs observed values and residuals
PLSr_pred_obs <- predicted_observed_plot(observed_val = Reference_Age, predicted_val = Predicted_Age,  model_predicted_line = c(0, Slope), model_name = "PLSr")
PLSr_residuals <- residuals_plot(observed_val = Reference_Age, predicted_val = Predicted_Age, model_name = "PLSr")

PLSr_pred_obs_rndPred <- predicted_observed_plot(observed_val = Reference_Age, predicted_val = round(Predicted_Age), model_predicted_line = c(0, Slope), model_name = "PLSr; Predicted Rounded")
PLSr_residuals_rndPred <- residuals_plot(observed_val = Reference_Age, predicted_val = round(Predicted_Age), model_name = "PLSr; Predicted Rounded")

dev.new()
g <- gridExtra::grid.arrange(PLSr_pred_obs, PLSr_residuals, PLSr_pred_obs_rndPred, PLSr_residuals_rndPred)


# R squared , RMSE, MAE
R_squared_RMSE_MAE(Predicted_Age, Reference_Age)
R_squared_RMSE_MAE(round(Predicted_Age), Reference_Age)



# Table
Table(NIRS_PLSr_AGE = round(Predicted_Age), TMA = Reference_Age)
e1071::classAgreement(Table(NIRS_PLSr_AGE = round(Predicted_Age), TMA = Reference_Age)) # $diag 0.0386 for Ex; 0.03311 for WB

e1071::classAgreement(Table(NIRS_PLSr_AGE = round(Predicted_Age), TMA = Reference_Age), match.names = TRUE) # diag  0.14790 for Ex' 0.12914



(TabRefPredAge <- aggregate(list(Count = rep(1, length(Predicted_Age))), list(NIRS_PLSr_AGE = round(ifelse(Predicted_Age < 0, 0, Predicted_Age)), TMA = as.vector(Reference_Age)), sum))[1:4,]
agg.table(TabRefPredAge)


# Lattice levelplot() of TMA vs NIRS_PLSr_AGE
dev.new(width = 16, height = 12)
lattice::levelplot(Count ~ TMA + NIRS_PLSr_AGE , data = TabRefPredAge, col.regions = rev(rainbow(max(TabRefPredAge$Count) * 1.3)[1:max(TabRefPredAge$Count)]), 
                   ylim = c(-1, 72), xlim = c(-1, 72), panel = function(...) { panel.levelplot(...); panel.abline(0,1) },  )

#  Zoomed in
dev.new(width = 16, height = 12)
lattice::levelplot(Count ~ TMA + NIRS_PLSr_AGE , data = TabRefPredAge[TabRefPredAge$TMA <= 20,], col.regions = rev(rainbow(max(TabRefPredAge$Count) * 1.3)[1:max(TabRefPredAge$Count)]), 
                   ylim = c(-1, 33), xlim = c(-1, 33), panel = function(...) { panel.levelplot(...); panel.abline(0,1) } )
                   

# Lattice wireframe()  of TMA vs NIRS_PLSr_AGE
dev.new(width = 16, height = 12)
lattice::wireframe(Count ~ TMA + NIRS_PLSr_AGE , data = TabRefPredAge, col.regions = rev(rainbow(max(TabRefPredAge$Count) * 1.3)[1:max(TabRefPredAge$Count)]), 
                   ylim = c(-1, 72), xlim = c(-1, 72), panel = function(...) { panel.wireframe(...); panel.abline(0,1) },  )
                             

# Lattice cloud() of TMA vs NIRS_PLSr_AGE
dev.new(width = 16, height = 12)
lattice::cloud(Count ~ TMA + NIRS_PLSr_AGE , data = TabRefPredAge, col.regions = rev(rainbow(max(TabRefPredAge$Count) * 1.3)[1:max(TabRefPredAge$Count)]), 
                   ylim = c(-1, 72), xlim = c(-1, 72), panel = function(...) { panel.cloud(...); panel.abline(0,1) },  )


                 year = rep(2001:2010, each=100))

                 
# Use ggplot2 and ggjoy packages  for ridge plotting

#    https://github.com/wilkelab/ggridges
#    https://stackoverflow.com/questions/45299043/how-to-reproduce-this-moving-distribution-plot-with-r


# https://stackoverflow.com/questions/65924548/add-points-to-geom-density-ridges-for-groups-with-small-number-of-observations

Ages <- c('All', 'le 30', 'le 15')[1]
# d <- data.frame(NIRS_PLSr_AGE = round(ifelse(Predicted_Age < 0, 0, Predicted_Age)), TMA = as.vector(Reference_Age))
d <- data.frame(NIRS_PLSr_AGE = ifelse(Predicted_Age < 0, 0, Predicted_Age), TMA = as.vector(Reference_Age))
# d <- data.frame(NIRS_PLSr_AGE = Predicted_Age, TMA = as.vector(Reference_Age))

if(Ages == 'All')
  dev.new(width = 20, height = 10) # After plotting, resize the window larger

if(Ages == 'le 30') {
  d <- d[d$TMA <= 30, ]
  dev.new(width = 20, height = 12)
}

if(Ages == 'le 15') {
  d <- d[d$TMA <= 15, ]
  dev.new(width = 20, height = 12)
}

d$TMA <- factor(d$TMA)

#  # d_Mean_NIRS_PLSr_AGE <- aggregate(list(NIRS_PLSr_AGE = d$NIRS_PLSr_AGE), list(TMA = d$TMA), mean, na.rm = TRUE)
#  # names(d_Mean_NIRS_PLSr_AGE) <- names(d_Mean_NIRS_PLSr_AGE)[2:1]
#  # d_Mean_NIRS_PLSr_AGE$TMA <- factor(round(d_Mean_NIRS_PLSr_AGE$TMA))
#  # d_Mean_NIRS_PLSr_AGE$NIRS_PLSr_AGE <- as.num(d_Mean_NIRS_PLSr_AGE$NIRS_PLSr_AGE)
#  # ggplot(d, aes(x = NIRS_PLSr_AGE, y = TMA)) + scale_x_continuous(breaks = as.num(d$TMA)) + geom_density_ridges(scale = 2, alpha = .5, rel_min_height = 0.01, col = 'red', fill = 'cyan') + 
#  #                 theme_joy() + geom_abline(intercept = 3, slope = 1, col = 'green') + geom_point(data = d_Mean_NIRS_PLSr_AGE, col = 'red', pch = 19) 
  
ggplot(d, aes(x = NIRS_PLSr_AGE, y = TMA, fill = TMA)) + scale_x_continuous(breaks = as.num(d$TMA)) + 
          ggridges::geom_density_ridges(scale = 2, alpha = .5, jittered_points = TRUE, point_alpha = 1, point_shape = 21, rel_min_height = 0.01) + 
               ggjoy::theme_joy() + geom_abline(intercept = 3, slope = 1, col = 'green', lwd = 1) + guides(fill = FALSE, color = FALSE)
  
dev.new(width = 20, height = 10)
plot(as.num(d$TMA), d$NIRS_PLSr_AGE, xlab = 'TMA', ylab = 'NIRS_PLSr_AGE')
abline(h = 0:70, v = 0:68, col = col.alpha('grey', 0.25))
abline(h = seq(0, 70, by = 5), v = seq(0, 65, by = 5), col = col.alpha('grey', 0.75))
abline(0, 1,, col = 'red')

dev.new(width = 20, height = 10)  
plot(factor(d$TMA), d$NIRS_PLSr_AGE, xlab = 'TMA', ylab = 'NIRS_PLSr_AGE')  
abline(h = 0:70, v = 0:68, col = col.alpha('grey', 0.25))
abline(h = seq(0, 70, by = 5), v = seq(0, 65, by = 5), col = col.alpha('grey', 0.75))  # ****** FIX THIS **********************************
abline(0, 1,, col = 'red')

sum(abs(round(d$TMA) - d$NIRS_PLSr_AGE))
sum(abs(round(d$TMA)[d$NIRS_PLSr_AGE <= 5] - d$NIRS_PLSr_AGE[d$NIRS_PLSr_AGE <= 5]))

absDiff_Table <- Table(absDiff = abs(round(d$NIRS_PLSr_AGE) - d$TMA), TMA = d$TMA)
absDiff_6 <- data.frame(rowNamesToCol(absDiff_Table[,7, drop = FALSE], 'Diff'))
sum(absDiff_6$Diff * absDiff_6$X6)

e1071::classAgreement(Table(Best_Freq_Cor_AGE = round(d$NIRS_PLSr_AGE), TMA = d$TMA)) 
e1071::classAgreement(Table(Best_Freq_Cor_AGE = round(d$NIRS_PLSr_AGE), TMA = d$TMA), match.names = TRUE)

  
# hist()
ds <- d[as.num(d$TMA) <= 20, ]         
ds$TMA <- as.num(ds$TMA)        
   
dev.new(width = 20, height = 12)
par(mfrow = c(3, 7))
for ( i in sort(unique(ds$TMA))) {
     hist(ds$NIRS_PLSr_AGE[ds$TMA == i], xlab = paste0('TMA = ', i), main = "", xlim = c(0, 35), ylim = c(0, 108), ylab = "")
     abline(v = i, col = 'red')
}     
  
dev.new(width = 20, height = 12)
par(mfrow = c(3, 7))
for ( i in sort(unique(ds$TMA))) {
     hist(ds$NIRS_PLSr_AGE[ds$TMA == i], xlab = paste0('TMA = ', i), main = "", xlim = c(0, 35), ylab = "")
     abline(v = i, col = 'red')
}   

ds <- d[as.num(d$TMA) >= 21 & as.num(d$TMA) <= 44, ]         
ds$TMA <- as.num(ds$TMA)          
  
dev.new(width = 20, height = 12)
par(mfrow = c(3, 7))
for ( i in sort(unique(ds$TMA))) {
     hist(ds$NIRS_PLSr_AGE[ds$TMA == i], xlab = paste0('TMA = ', i), main = "", xlim = c(0, 60), ylab = "")
     abline(v = i, col = 'red')
}
     
ds <- d[as.num(d$TMA) >= 45 & as.num(d$TMA) <= 71, ]         
ds$TMA <- as.num(ds$TMA)          
  
dev.new(width = 20, height = 12)
par(mfrow = c(3, 7))
for ( i in sort(unique(ds$TMA))) {
     hist(ds$NIRS_PLSr_AGE[ds$TMA == i], xlab = paste0('TMA = ', i), main = "", xlim = c(0, 70), ylab = "")
     abline(v = i, col = 'red')
}
          
      
 

# heatmap()                 
# dev.new()
# stats::heatmap(as.matrix(agg.table(TabRefPredAge, Print = FALSE, NA.to.zeros = TRUE)))
# dev.new()
# heatmap(xtabs(~ round(ifelse(Predicted_Age < 0, 0, Predicted_Age)) + Reference_Age))

# Similiar plot using table() and melt()
# TabRefPredAge_2 <- reshape2::melt(Table(NIRS_PLSr_AGE = round(ifelse(Predicted_Age < 0, 0, Predicted_Age)), TMA = Reference_Age))
# dev.new()
# lattice::levelplot(value ~ TMA + NIRS_PLSr_AGE , data = TabRefPredAge_2, col.regions = rev(rainbow(max(TabRefPredAge_2$value) * 1.3)[1:max(TabRefPredAge_2$value)]), 
#                    ylim = c(-1, 72), xlim = c(-1, 72), panel = function(...) { panel.levelplot(...); panel.abline(0,1) },  )
# 
# 
dev.new()
heatmap(Table(NIRS_PLSr_AGE = round(ifelse(Predicted_Age < 0, 0, Predicted_Age))[Reference_Age <= 25], TMA = Reference_Age[Reference_Age <= 25]))




sum(abs(Reference_Age - round(Predicted_Age))) # 2679 for Ex; 2730 for WB

(Results <- data.frame(Reference_Age, Predicted_Age))[1:10, ]

# Store results in an excel.csv file
write.csv(Results, file ="10_smoothing_iPLSR_Res.csv", row.names = FALSE)

                 Count 

 NIRS_PLSr_AGE                   TMA 

    0  1  2  3  4  5  6  7  9  8 10 14 11 13 12 15 18 20 17 22 16 19 24 23 21 27 26 28 29 30 36 39 33 32 34 25 50 35 41 46 38 42 47 57 49 44 40 58 52 56 60 61 48 59 51 45 62 67 55 65 71 66
0   8 45  7 32  5  1                                                                                                                                                                        
1   1 35  5 17  6                                                                                                                                                                           
2   6 27  5 28  9  4  3  1  2                                                                                                                                                               
3   3 15  5 27 14  2  2  1     1  1                                                                                                                                                         
4      7  1 22 17  4 12  2     2  1  1                                                                                                                                                      
5   1  4  2 16 20  7 14  1  3  5  1     1                                                                                                                                                   
6      1     6 12  5  8  5  3  3  2     1                                                                                                                                                   
7      1     5  8  6 12  4  3  6  3        1                                                                                                                                                
8      1        6  6 11  3  2  4  3     3     2  1  1                                                                                                                                       
9            1  5  6 10  6  1  4  2     2  2  1        1                                                                                                                                    
10              4  3  3  4  7  5        2  1  1  1                                                                                                                                          
11              2  3  5  4  1  1  1  1  1     1  1                                                                                                                                          
12           1     1  3  1  4  2  1     4           1  1  1  1                                                                                                                              
13                 1  3     1  2  4  2  1     1     2           1                                                                                                                           
14                             1  1     1  1  3  1     2           3  1                                                                                                                     
15                          2     2  1        1  1              1  2                                                                                                                        
16     1                    1  1  1  2        1  3           1  1     1  1                                                                                                                  
17                                1        1        1           1  2        1  1                                                                                                            
18                       2           1  1  1        1     1     1        1  3                                                                                                               
19                    1        1        1  1        2  2     1     1                                                                                                                        
20                                                  1  1              1     1     1                                                                                                         
21                                                     2                 1        1                                                                                                         
22                          1                                            1           1  1  1                                                                                                
23                                                     1  1     1  2                    1                                                                                                   
24                                         1                 1        1  2  2  1                                                                                                            
25                                                  1        2  1                    1                                                                                                      
26                                                                                2           1                                                                                             
27                                                                             1                 1                                                                                          
28                                                                                         1        1                                                                                       
29                                                                                         1           1                                                                                    
30                                                                                1  1           1     1  1                                                                                 
31                       1                                                                       1     1                                                                                    
32                                                     1                                            1        1  1                                                                           
33                                                                                         1                       1  1  1                                                                  
34                                                           1                    1                                                                                                         
35                                                                                               1                          1  1                                                            
36                                                                                               1                          1     1  1                                                      
37                                                                                                                 2                    1                                                   
38                                                                                                                                         1                                                
39                                                                                                                                   1                                                      
40                                                                    1                                                                       1  1                                          
41                                                                                               1  1                                                                                       
42                                                                                         1                                      1                                                         
43                                                                                                                          1        1                                                      
44                                                                                                                                                  1                                       
45                                                                                                                                                     1  1  1                              
46                                                                                                                 1                                            1                           
47                                                                                                                                1                                                         
48                                                                                                              1                                                                           
49                                                                                                                                                                 1                        
50                                                                                                                                                     1              1                     
51                                                                                                                                1                                                         
52                                                                                                                                                                       1  1               
54                                                                                                                                                                             1            
56                                                                                                                                   1                       1                              
59                                                                                                                                                                                1         
60                                                                                                                                                                                   1  1   
61                                                                                                                                   1                                                     1
64                                                                                                                                                           1                              
66                                                                                                                                                                 1                        
67             

# ======================== pls::plsr  ===========================================================================

# Try the pls::plsr() model
# https://www.tidymodels.org/learn/models/pls/

lib(tidymodels)
lib(pls)
lib(modeldata)

Sable_Spectra_2017_2019.Age.Last.sg.iPLS <- data.frame(Sable_Spectra_2017_2019.sg.iPLS, TMA = Sable_TMA_2017_2019)

norm_rec <- 
  recipe(TMA ~ ., data = Sable_Spectra_2017_2019.Age.Last.sg.iPLS) %>%
  step_normalize(everything()) 

set.seed(57343)
folds <- vfold_cv(Sable_Spectra_2017_2019.Age.Last.sg.iPLS, v = 3, repeats = 4)

folds <- 
  folds %>%
  mutate(recipes = purrr::map(splits, prepper, recipe = norm_rec))  


get_var_explained <- function(recipe, ...) {
  
    # Extract the predictors and outcomes into their own matrices
    y_mat <- bake(recipe, new_data = NULL, composition = "matrix", all_outcomes())
    x_mat <- bake(recipe, new_data = NULL, composition = "matrix", all_predictors())
    
    # The pls package prefers the data in a data frame where the outcome
    # and predictors are in _matrices_. To make sure this is formatted
    # properly, use the `I()` function to inhibit `data.frame()` from making
    # all the individual columns. `pls_format` should have two columns.
    pls_format <- data.frame(
      endpoints = I(y_mat),
      measurements = I(x_mat)
    )
    # Fit the model
    mod <- pls::plsr(endpoints ~ measurements, data = pls_format)
    dev.new()
    par(mfrow = c(2, 2))
    validationplot(mod, val.type = "RMSEP")
    validationplot(mod, val.type =  "MSEP")
    validationplot(mod, val.type =    "R2")
    
    # Get the proportion of the predictor variance that is explained
    # by the model for different number of components. 
    xve <- explvar(mod)/100 
   
    # To do the same for the outcome, it is more complex. This code 
    # was extracted from pls:::summary.mvr. 
      mod_R2 <- pls::R2(mod, estimate = "train", intercept = FALSE)  # All for now - not train yet....
      plot(mod_R2)
      drop(mod_R2$val) %>% 
      matrix(nrow = 1, dimnames = list("TMA_Train")) %>%  # Only one dependent variable 
      
      # drop(pls::R2(mod, estimate = "adjCV", intercept = FALSE)$val) %>% 
      # matrix(nrow = 1, dimnames = list("TMA_adjCV")) %>%  # Only one dependent variable 
      
      # transpose so that components are in rows
      t() %>% 
      as_tibble() %>%
      # Add the predictor proportions
      mutate(predictors = cumsum(xve) %>% as.vector(),
             components = seq_along(xve)) %>%
      # Put into a tidy format that is tall
      pivot_longer(
        cols = c(-components),
        names_to = "source",
        values_to = "proportion"
      )
}


# We compute this data frame for each resample and save the results in the different columns.

(folds <- 
  folds %>%
  mutate(var = map(recipes, get_var_explained),
         var = unname(var)))
 
 
# To extract and aggregate these data, simple row binding can be used to stack the data vertically. Most of the action happens in the first 15 components 
#      so let’s filter the data and compute the average proportion.
variance_data <- 
  bind_rows(folds[["var"]]) %>%
  filter(components <= 20) %>%
  group_by(components, source) %>%
  summarize(proportion = mean(proportion))

variance_data
tail(variance_data)  
  
  
  
# The plot below shows that, if the protein measurement is important, you might require 10 or so components to achieve a good representation of that outcome.
#      Note that the predictor variance is captured extremely well using a single component. This is due to the high degree of correlation in those data.
dev.new()
ggplot(variance_data, aes(x = components, y = proportion, col = source)) + 
  geom_line(alpha = 0.5, linewidth = 1.2) + 
  geom_point() 
  

# --------------- Just do the model -----------------

Sable_Spectra_2017_2019.Age.Last.sg.iPLS <- data.frame(Sable_Spectra_2017_2019.sg.iPLS, TMA = Sable_TMA_2017_2019) #  Copy from above for this section

norm_rec <- 
  recipe(TMA ~ ., data = Sable_Spectra_2017_2019.Age.Last.sg.iPLS) %>%
  step_normalize(everything()) 
  
# Extract the predictors and outcomes into their own matrices
y_mat <- bake(prep(norm_rec), new_data = NULL, composition = "matrix", all_outcomes())    
x_mat <- bake(prep(norm_rec), new_data = NULL, composition = "matrix", all_predictors())

# The pls package prefers the data in a data frame where the outcome
# and predictors are in _matrices_. To make sure this is formatted
# properly, use the `I()` function to inhibit `data.frame()` from making
# all the individual columns. `pls_format` should have two columns.
pls_format <- data.frame(
  endpoints = I(y_mat),
  measurements = I(x_mat)
)

# Fit the model

# mod <- pls::plsr(endpoints ~ measurements, data = pls_format)    
mod <- pls::plsr(TMA ~ ., data = Sable_Spectra_2017_2019.Age.Last.sg.iPLS)

    
summary(mod)

dev.new()
plot(mod) # Same as predplot(mod)

dev.new()
scoreplot(mod)

dev.new()
loadingplot(mod)

dev.new()
corrplot(mod)     
    
    
dev.new()
par(mfrow = c(2, 2))
validationplot(mod, val.type = "RMSEP")
validationplot(mod, val.type =  "MSEP")
validationplot(mod, val.type =    "R2")

# Get the proportion of the predictor variance that is explained
# by the model for different number of components. 
(xve <- explvar(mod)/100 )[1:10]
rev(sort(xve))[1:20]

# To do the same for the outcome, it is more complex. This code 
# was extracted from pls:::summary.mvr. 
mod_R2 <- pls::R2(mod, estimate = "train", intercept = FALSE)  # All for now - not train yet....
plot(mod_R2)

NIRS_plrs_AGE <- apply(drop(predict(mod)), 1, mean)
length(NIRS_plrs_AGE)
    
    

d.plrs <- data.frame(NIRS_plrs_AGE = NIRS_plrs_AGE, TMA = Sable_Spectra_2017_2019.Age.Last.sg.iPLS$TMA)

if(Ages == 'All')
  dev.new(width = 20, height = 10) # After plotting, resize the window larger

if(Ages == 'le 30') {
  d.plrs <- d.plrs[d.plrs$TMA <= 30, ]
  dev.new(width = 20, height = 12)
}

if(Ages == 'le 15') {
  d.plrs <- d.plrs[d.plrs$TMA <= 15, ]
  dev.new(width = 20, height = 12)
}

d.plrs$TMA <- factor(d.plrs$TMA)
 
ggplot(d.plrs, aes(x = NIRS_plrs_AGE, y = TMA, fill = TMA)) + scale_x_continuous(breaks = as.num(d.plrs$TMA)) + 
          ggridges::geom_density_ridges(scale = 2, alpha = .5, jittered_points = TRUE, point_alpha = 1, point_shape = 21, rel_min_height = 0.01) + 
               ggjoy::theme_joy() + geom_abline(intercept = 3, slope = 1, col = 'green', lwd = 1) + guides(fill = FALSE, color = FALSE)

    
    
dev.new(width = 20, height = 10)   
plot(Sable_Spectra_2017_2019.Age.Last.sg.iPLS$TMA, NIRS_plrs_AGE, xlab = 'TMA', ylab = 'NIRS_plrs_AGE')

dev.new(width = 20, height = 10)   
plot(factor(Sable_Spectra_2017_2019.Age.Last.sg.iPLS$TMA), NIRS_plrs_AGE, xlab = 'TMA', ylab = 'NIRS_plrs_AGE')

sum(abs(round(NIRS_plrs_AGE) - Sable_Spectra_2017_2019.Age.Last.sg.iPLS$TMA))
sum(abs(round(NIRS_plrs_AGE)[Sable_Spectra_2017_2019.Age.Last.sg.iPLS$TMA <= 5] - Sable_Spectra_2017_2019.Age.Last.sg.iPLS$TMA[Sable_Spectra_2017_2019.Age.Last.sg.iPLS$TMA <= 5]))

absDiff_Table <- Table(absDiff = abs(round(NIRS_plrs_AGE) - Sable_Spectra_2017_2019.Age.Last.sg.iPLS$TMA), TMA = Sable_Spectra_2017_2019.Age.Last.sg.iPLS$TMA)
absDiff_6 <- data.frame(rowNamesToCol(absDiff_Table[,7, drop = FALSE], 'Diff'))
sum(absDiff_6$Diff * absDiff_6$X6)


Table(NIRS_plrs_AGE = ifelse(round(d.plrs$NIRS_plrs_AGE) < 0, 0, round(d.plrs$NIRS_plrs_AGE)), TMA = d.plrs$TMA)  

e1071::classAgreement(Table(NIRS_plrs_AGE = ifelse(round(d.plrs$NIRS_plrs_AGE) < 0, 0, round(d.plrs$NIRS_plrs_AGE)), TMA = d.plrs$TMA)  ) 
e1071::classAgreement(Table(NIRS_plrs_AGE = ifelse(round(d.plrs$NIRS_plrs_AGE) < 0, 0, round(d.plrs$NIRS_plrs_AGE)), TMA = d.plrs$TMA)  , match.names = TRUE)



                TMA
NIRS_plrs_AGE     1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 65 66 67 71
              11 73 10 35  4  1                                                                                                                                                                                      0
           1   7 41 11 34 10     1                                                                                                                                                                                   0
           2   7 46  8 43 18  3  5        1                                                                                                                                                                          0
           3   5 26  4 45 29  7  6     1  2                                                                                                                                                                          0
           4   2 15  2 39 19  5 12  4  3  2  1  1                                                                                                                                                                    0
           5   1  8  3 21 26 11 18  4  5     1  3                                                                                                                                                                    0
           6      1    14 23  7 27  6  7  3  1                                                                                                                                                                       0
           7      1     4 13 10 21  8  7  4  3        1                                                                                                                                                              0
           8         1  1  7  7 17  6  9  6  5  1  1     1                                                                                                                                                           0
           9            1  3 10 12 10  6 10  8  2     2                                                                                                                                                              0
           10     1        1  2  5  6  6  3  3  7  2  1                                                                                                                                                              0
           11                 1  3  4  4  3  1     4  3     2                                                                                                                                                        0
           12                    6  1  6  4  5  6  3  2  2  2              2                                                                                                                                         0
           13              1     2  1  1  3  3  1  2  1  2  3  1  1  1                                                                                                                                               0
           14                    1     2  1  1  1  1     1  2  2  3  4  2                                                                                                                                            0
           15                       1        1  1  2     1        1  2  1        1     1                                                                                                                             0
           16                          1  1  1     1     2  3  2     1  4           1                                                                                                                                0
           17                                            1        1     1        1                                                                                                                                   0
           18                                   1     1  2  1  1  2  4     1  1                                                                                                                                      0
           19                                   1     1  1     2  1  2  1  3  3  1     1        1                                                                                                                    0
           20                             1     1              2  1     1  3  2        1        1                                                                                                                    0
           21                                                  1     1  2  1     1  1  1     1  1                                                                                                                    0
           22                                         2        1        1  1  1     2                                                                                                                                0
           23                                                        1        2     2  1     1                                                                                                                       0
           24                                                           1  1        1     1        1  1  1                                                                                                           0
           25                       1                                            2     1     1  1  1  1     1                                                                                                        0
           26                                                  1     1           1  1        1     1                                                                                                                 0
           27                                                        1                                   1        1                                                                                                  0
           28                                                                                      2                                                                                                                 0
           29                                                                                1  1  1           2     1     1                                                                                         0
           30                                                              1     1                                1                                                                                                  0
           31                                                     1        1              2  1        1  1                 1              1                                                                          0
           32                                                              1                                   1              1     2                                                                                0
           33                                                                                      1                                2                                                                                0
           34                                                                                            1              2        1  1                 1                                                              0
           35                                                                                                                       1     1  1                                                                       0
           36                                                                                                                             1                                                                          0
           37                                                                                            1        1              1                                                                                   0
           38                                                                          1                                         1                                1                                                  0
           39                                                                                                           3                          1     1     1                                                     0
           40                                                                                                                       1  1        1              1              1                                      0
           43                                                                                                           1                                1                             1  1           1              0
           44                                                                                                                                                                          1                             0
           45                                                                                                                                                        2                 1                             0
           46                                                                                                                    1                       1  1     1     1                                            0
           48                                                                                                                                                                                      1                 0
           49                                                                                                                                      1                 1                       1                       0
           50                                                                                                                                            1                          1        1                       1
           51                                                                                                                                                                       1                                0
           53                                                                                                                                                                    1        1           1              0
           54                                                                                                                                                     1                             1                    0
           55                                                                                                                                                                                            1           0
           57                                                                                                                                                                          1                             0
           58                                                                                                                                                  1                                                  1  0
           59                                                                                                                                                                          1           1                 0
           60                                                                                                                                            1                                                     1     0
           61                                                                                                                                                                    1                                   0
           62                                                                                                                                                                                1     1        1        0
           65                                                                                                                                                              1                                         0
           70                                                                                                                                                                                                        1
>     
> 
 

# ======================== SPLSDA - does TMA Ages 1-5 better.... ===========================================================================
  
# SPLS/SPLSDA
# https://www.quantargo.com/help/r/latest/packages/spls/2.2-3/splsda

lib(MASS)
lib(nnet)
lib(pls)
lib(spls)

Sable_Spectra_2017_2019.sg.iPLS.mat <- as.matrix(Sable_Spectra_2017_2019.sg.iPLS)
colnames(Sable_Spectra_2017_2019.sg.iPLS.mat) <- NULL  
Sable_Spectra_2017_2019.sg.iPLS.mat[1:4, 1:5]
  
cv <- cv.spls(as.matrix(Sable_Spectra_2017_2019.sg.iPLS.mat), Sable_TMA_2017_2019, eta = seq(0.1,0.9,0.1), K = c(5:10) )  
  
  K=3, eta=0.8
 
(mod_splsda <- splsda(as.matrix(Sable_Spectra_2017_2019.sg.iPLS.mat), Sable_TMA_2017_2019, K = 3, eta = 0.8, scale.x = FALSE, classifier = 'logistic')) 
(mod_splsda <- splsda(as.matrix(Sable_Spectra_2017_2019.sg.iPLS.mat), Sable_TMA_2017_2019, eta = cv$eta.opt, K = cv$K.opt, scale.x = FALSE, classifier = 'logistic'))
(coef.mod_splsda <- coef(mod_splsda))
coef.mod_splsda[coef.mod_splsda !=0, ]

plot.spls(mod_splsda, yvar=1)
coefplot.spls(mod_splsda, nwin=c(2,2), xvar=c(1:4) )

splsda_AGE <- as.num(predict(mod_splsda))

Ages <- c('All', 'le 30', 'le 15')[1]
# d <- data.frame(NIRS_PLSr_AGE = round(ifelse(Predicted_Age < 0, 0, Predicted_Age)), TMA = as.vector(Reference_Age))
d <- data.frame(NIRS_PLSr_AGE = ifelse(Predicted_Age < 0, 0, Predicted_Age), TMA = as.vector(Reference_Age))
# d <- data.frame(NIRS_PLSr_AGE = Predicted_Age, TMA = as.vector(Reference_Age))

d.splsda <- data.frame(splsda_AGE = splsda_AGE, TMA = Sable_TMA_2017_2019)

if(Ages == 'All')
  dev.new(width = 20, height = 10) # After plotting, resize the window larger

if(Ages == 'le 30') {
  d.splsda <- d.splsda[d.splsda$TMA <= 30, ]
  dev.new(width = 20, height = 12)
}

if(Ages == 'le 15') {
  d.splsda <- d.splsda[d.splsda$TMA <= 15, ]
  dev.new(width = 20, height = 12)
}

d.splsda$TMA <- factor(d.splsda$TMA)
 
ggplot(d.splsda, aes(x = jitter(splsda_AGE), y = TMA, fill = TMA)) + scale_x_continuous(breaks = as.num(d.splsda$TMA)) + 
          ggridges::geom_density_ridges(scale = 2, alpha = .5, jittered_points = TRUE, point_alpha = 1, point_shape = 21, rel_min_height = 0.01) + 
               ggjoy::theme_joy() + geom_abline(intercept = 3, slope = 1, col = 'green', lwd = 1) + guides(fill = "none", color = "none")

               
dev.new(width = 20, height = 10)              
plot(jitter(as.num(d.splsda$TMA)), d.splsda$splsda_AGE, xlab = 'TMA', ylab = 'splsda_AGE')  
abline(h = 0:70, v = 0:68, col = col.alpha('grey', 0.25))
abline(h = seq(0, 70, by = 5), v = seq(0, 65, by = 5), col = col.alpha('grey', 0.75))
abline(0, 1,, col = 'red')

dev.new(width = 20, height = 10)             
plot(factor(d.splsda$TMA), d.splsda$splsda_AGE, xlab = 'TMA', ylab = 'splsda_AGE')
abline(h = 0:70, v = 0:68, col = col.alpha('grey', 0.25))
abline(h = seq(0, 70, by = 5), v = c(1, 6, 11, 16, 21, 26, 31, 36, 41, 45, 50, 55, 60, 65), col = col.alpha('grey', 0.75))
abline(0, 1,, col = 'red')

sum(abs(round(d.splsda$splsda_AGE) - as.num(d.splsda$TMA)))
sum(abs(round(d.splsda$splsda_AGE)[as.num(d.splsda$TMA) <= 5] - as.num(d.splsda$TMA)[as.num(d.splsda$TMA) <= 5]))

absDiff_Table <- Table(absDiff = abs(round(d.splsda$splsda_AGE) - as.num(d.splsda$TMA)), TMA = as.num(d.splsda$TMA))
absDiff_6 <- data.frame(rowNamesToCol(absDiff_Table[,7, drop = FALSE], 'Diff'))
sum(absDiff_6$Diff * absDiff_6$X6)


Table(splsda_AGE = d.splsda$splsda_AGE, TMA = d.splsda$TMA)

e1071::classAgreement(Table(splsda_AGE = d.splsda$splsda_AGE, TMA = d.splsda$TMA)  ) 
e1071::classAgreement(Table(splsda_AGE = d.splsda$splsda_AGE, TMA = d.splsda$TMA), match.names = TRUE)


            TMA
splsda_AGE       1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  65  66  67  71
            16   4                   3   3       2   1       1   1       1               2   1           1                                                                                                                                                                           0
        1   17 200  14   4   3                                                                                                                                                                                                                                                       0
        2        2   1   1   1                                                                                                                                                                                                                                                       0
        3        5  18 190  53  13  29   4   3   2   1   2                                                                                                                                                                                                                           0
        4        1   4  28  60  14  25  11  18   7  12   1   2   2   1                                                                                                                                                                                                               0
        5                    1   3   1       1   1                                                                                                                                                                                                                                   0
        6            2  11  23  24  62  18  20  17   6  11   2   5   1   1               2                                                                                                                                                                                           0
        7                2       1   1   3   4   1   1   2                   1                   1                                                                                                                                                                                   0
        8                    5   1   5   6   3   4   2   2   3           2   1       1               1       1                                                                                                                                                                       0
        9                1   1   4   5   1   3   3   5   3   1       2   1       1   1                                                                                                                                                                                               0
        10                   5   1               3   3       2       2       1   2   2   1   1                                                                                                                                                                                       0
        11                   1       1   1   3           2                                                   1                                                                                                                                                                       0
        13                                       1   1       1   2       1   1   1   1   1           1                                   1                                                                                                                                           0
        14                           1       1           1   2   1   5       1           2   3           1                                                                                                                                                                           0
        15                           1   3                           1   3   1   1   4   1       1                       1                                                                                                                                                           0
        16                               1                           1       2       2           1   1   1                       1                                                                                                                                                   0
        17                       1               1                               3           3                                                                                                                                                                                       0
        18                       1           1                           2   1   1   2               1   2   1       1   1       1   1                                                                                                                                               0
        19                           1   1       1       1       1       1           1   4       1               1   1                                                   1                                                                                                           0
        20                                           1   1       1           1       1       3   2       1   1               1                   1           1           2                                                                                                           0
        21                                   1                           1   1   1       1   2   1                                   1                       1                                                                                                                       0
        22                                                                   2                       1       1                                                           1                                                                                                           0
        23                                                   1                               1   1       2           1       1                                                   1                                                                                                   0
        24                   1                                                       1                                                                                                                                                                                               0
        26                                                                           1                                   1                               1                                                                                                                           0
        27                       1               1   1                                           1   1                   2           1                                                                                                                                               0
        28                                                                           1                       1               4       1                           1                                           1                                                                       0
        29                                                   1                                                                       1                                                                                                                                               0
        30                                                                                           1               1                                                                   1                       1                                                                   0
        32                                                                                                                                   2                                                                                                                                       0
        33                                                                                   1                       1                           1                                                                                                                                   0
        34                                                                                                                                           1                                                                                                                               0
        35                                                                                                                                       1       4           1   1           1       1                                                                       1               0
        38                                                                                                                                               1           2                                                                                                               0
        39                                                                                                       2   1           1           1                       1   1                       1       1   1                                                                       0
        40                                                                                                                                                                   1                                                                                                       0
        41                                                                                                                                                                       2                                                                                                   0
        42                                                       1                                                                                                                                                                                                                   0
        44                                                                                                   1                                                                                                                                                                       0
        45                                                                                                                                                                                           1                                                                               0
        47                                                                                                                                                                                   1       4               1                                                   1           1
        49                                                                       1                                                                                       1                                   1                                                                       0
        50                           1                                                               1                       1                                                                                   1                                                                   0
        51                                                                                                                                                                                                           2                               1                               0
        52                                                                                                                                                                                                               1                                                           0
        53                                                                                                                                                                                                                   1                                                       0
        54                                                                                                                                                                                                                       1                                                   0
        55                                                                                                                                                                                                                           1                                               0
        56                                                                                                                                                                                                                               2   1                                       0
        57                                                                                                                                                                                                                           1       4                                       0
        58                                                                                                                                                                                                                                       2                                   0
        59                                                                                                                                                                                                       1                                   2                               0
        60                                                                                                                                                                                                                                               1                           0
        61                                                                                                                                                                                                                                                   3                       0
        62                                                                                                                                                                                                                                                       2                   0
        66                                                                                                                                                                                                                                                                   1       0
        67                                                                                                                                                                                                                                                                       1   0
        71                                                                                                                                                                                                                                                                           1



# sPLS2

# pls package
# https://www.tidymodels.org/learn/models/pls/


# plsRglm
# https://cran.r-project.org/web/packages/plsRglm/index.html











#================= Old code ==================        
         #Split by age - Age.0.avg, Age.1.avg, ...
         AGES <- sort(unique(Sable_TMA_2017_2019))
         for (i in AGES) { 
             Subset <- subset(Sable_Spectra_2017_2019.Age.sg.iPLS, Sable_TMA_2017_2019 == i)
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
         age.avg[1:5, c(1:4, (ncol(age.avg) - 3):ncol(age.avg))]
         p + 1 # vars plus age 
         
         t.age.avg <- data.frame(t(age.avg[, -1])) # remove the age column so there is only an xmatrix and transpose
         dim(t.age.avg) # 342  67 for Ex; 79  67 for WB
         t.age.avg[1:3, 1:5]
         
         
         
         # Plot the transformed spectra
         dev.new(width = 14, height = 8)
         par(mar=c(5,5,1,1))
         plot(t.age.avg$X10, xlab = "Wavenumber Range", ylab = "Absorbance", cex.lab = 1, type = 'n', xlim = c(-23, nrow(t.age.avg)), 
                     ylim = c(min(t.age.avg, na.rm = TRUE) - abs(min(t.age.avg, na.rm = TRUE) * 0.025), max(t.age.avg, na.rm = TRUE) + 
                     abs(max(t.age.avg, na.rm = TRUE) * 0.025)))
         Cols <- rainbow(1.2 * length(AGES))
         for ( i in AGES)
             lines(t.age.avg[[paste0('X', i)]],  col = Cols[i + 1], lwd = 2)
         legend("topleft", legend = paste(AGES, "Years"), col = Cols[AGES + 1], lty=1, lwd = 2,cex=0.495)


                                                                                                   1                                                         







