

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

           
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/lib.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/openwd.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")

# Source the saved code on GitHub
# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R_Scratch/Sable 2017 2019 Prediction of TMA Cor Wide Best Area.R", type = "script")
# gitAFile("John-R-Wallace-NOAA/FishNIRS/master/R_Scratch/Sable 2017 2019 Prediction of TMA Cor Wide Best Area.R", type = "script", verbose = TRUE)


# Set Path
PATH <- "W:/ALL_USR/JRW/SIDT/Sablefish/"
setwd(PATH) # set working directory to folder containing spectral files
getwd()
openwd()



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

# lib("philipp-baumann/simplerspec") # https://github.com/philipp-baumann/simplerspec
lib(simplerspec)

# ------------------Load up Spectra Data --------------------------------------------------

base::load(file = "Sable_2017_2019 21 Nov 2022.RData")
options(digits = 11)
Sable_2017_2019[1:2, c(1:2, 1153:1184)]

dim(Sable_2017_2019)    
Sable_2017_2019_noEx <- Sable_2017_2019[Sable_2017_2019[, '4004'] < 0.7, ]    
dim(Sable_2017_2019_noEx)  

# --------------------------------------------------------------------


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

dim(Sable_2017_2019_noEx) #  1560 1175

Sable_2017_2019_WB <- Sable_2017_2019_noEx[, c(T, Bands.TF, rep(T, len(1156:1184)))]
Sable_2017_2019_WB <- Sable_2017_2019_WB[!is.na(Sable_2017_2019_WB$TMA), ]

plotly.Spec(Sable_2017_2019_WB, 'all')

plotly.Spec(Sable_2017_2019_WB[Sable_2017_2019_WB$Year %in% 2017, ], 'all')
plotly.Spec(Sable_2017_2019_WB[Sable_2017_2019_WB$Year %in% 2019, ], 'all')


plotly.Spec(Sable_2017_2019_WB, 'all', facetGroup = 'scanGroup')



#  Year with nrows = 2 
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
d <- plotly.Spec(Sable_2017_2019_WB[!is.na(Sable_2017_2019_noEx$TMA), ], 'all', colorGroup = 'TMA', facetGroup = 'scanGroup', plot = FALSE) 
d %>% plot_ly(x = ~Band, y = ~Value, z = ~Scan) %>% group_by(Scan) %>% add_lines(color = ~TMA, colors = rainbow(length(unique(d$Scan)))) 

d %>% plot_ly(x = ~Band, y = ~Value, z = ~Scan) %>% group_by(TMA) %>% add_lines(color = ~scanGroup, colors = rainbow(length(unique(d$Scan)))) 


        
# =============== iPLSR - following Jordan =========================

Sable_Spectra_2017_2019 <- Sable_2017_2019_noEx[!is.na(Sable_2017_2019_noEx$TMA), as.character(Bands)] # Spectra matrix 
dim(Sable_Spectra_2017_2019) #  1358 1154

# Sable_Spectra_2017_2019 <- Sable_2017_2019_noEx[!is.na(Sable_2017_2019_noEx$TMA), Bands.TF] # Spectra matrix - Wide Best area selected via 'Bands.TMA'
# dim(Sable_Spectra_2017_2019) #  1358  213

Sable_Age_2017_2019 <- as.numeric(Sable_2017_2019_noEx[!is.na(Sable_2017_2019_noEx$TMA), ]$TMA) # Vector of Ages 
length(Sable_Age_2017_2019) #  1358
Sable_Age_2017_2019.fac <- factor(Sable_Age_2017_2019)      


###################################################################################################################
### Perform Savitzky-Golay 1st derivative with 17 point window smoothing 2rd order polynomial fit and visualize ###
###################################################################################################################
### NOTE ### If there are multiple years of data, all subsequent transformations should be applied to the whole data set, then re-subset
          
Sable_Spectra_2017_2019.sg <- data.frame(prospectr::savitzkyGolay(Sable_Spectra_2017_2019, p = 2, w = 15, m = 1)) 
dim(Sable_Spectra_2017_2019.sg) #  1358 1140

Sable_Spectra_2017_2019.Age.sg <- data.frame(TMA = Sable_Age_2017_2019, Sable_Spectra_2017_2019.sg) 

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
dim(t.age.avg) # 342  67
t.age.avg[1:3, 1:5]


# Plot the transformed spectra
dev.new(width = 14, height = 8)
par(mar=c(5,5,1,1))
plot(t.age.avg$X10, xlab = "Wavenumber Range", ylab = "Absorbance", cex.lab = 1, type = 'n', xlim = c(-23, nrow(t.age.avg)), 
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

                 
# Use ggplot2 and ggjoy packages  

#    https://github.com/wilkelab/ggridges
#    https://stackoverflow.com/questions/45299043/how-to-reproduce-this-moving-distribution-plot-with-r

d <- data.frame(NIRS_PLSr_AGE = round(ifelse(Predicted_Age < 0, 0, Predicted_Age)), TMA = as.vector(Reference_Age))
d <- d[d$TMA <= 30,]
d$TMA <- factor(d$TMA)

dev.new(width = 20, height = 12)
ggplot(d, aes(x = NIRS_PLSr_AGE, y = TMA)) + scale_x_continuous(breaks = as.num(d$TMA)) + geom_density_ridges(scale = 2, alpha = .5, rel_min_height = 0.01, col = 'red', fill = 'cyan') + 
               theme_joy() + geom_abline(intercept = 3, slope = 1, col = 'green') 
     
      

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

dev.new()
heatmap(Table(NIRS_PLSr_AGE = round(ifelse(Predicted_Age < 0, 0, Predicted_Age))[Reference_Age <= 25], TMA = Reference_Age[Reference_Age <= 25]))




sum(abs(Reference_Age - round(Predicted_Age))) # 2679

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
67                                                                                                                                1                                                         







