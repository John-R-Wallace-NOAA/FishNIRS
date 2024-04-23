
# --- Download functions from GitHub ---
sourceFunctionURL <- function (URL,  type = c("function", "script")[1]) {
          '   # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git()   '
		  '   # Example to save a function to the working directory:   '
          '   # sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/rgit/master/R/gitAFile.R")   '
          '   # gitAFile("John-R-Wallace-NOAA/JRWToolBox/master/R/browsePlot.R", File = "browsePlot.R")   '
		  
          if (!any(installed.packages()[, 1] %in% "httr"))  install.packages("httr") 
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

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly.Spec.R")

library(JRWToolBox)


load("C:\\SIDT\\Predict_NN_Ages\\Sable_Combo_2022_Model_Spectra_Meta_ALL_GOOD_DATA_1556N.RData")

dim(Model_Spectra_Meta)
Model_Spectra_Meta <- Model_Spectra_Meta[!is.na(Model_Spectra_Meta$TMA) & !is.na(Model_Spectra_Meta$Sex), ]
dim(Model_Spectra_Meta)


for (Age in 0:15)
    plotly.Spec(Model_Spectra_Meta[Model_Spectra_Meta$TMA == Age,], N_Samp = 'All',  colorGroup = 'Sex', main = paste0("Age = ", Age), htmlPlotFolder = paste0("Sablefish 2022 Combo Age = ", Age))


AgeLow <- 16; AgeHigh <- 19
plotly.Spec(Model_Spectra_Meta[Model_Spectra_Meta$TMA >= AgeLow & Model_Spectra_Meta$TMA < AgeHigh,], N_Samp = 'All',  colorGroup = 'Sex', main = paste0("Age >= ", AgeLow, " and < ", AgeHigh), 
               htmlPlotFolder = paste0("Sablefish 2022 Combo Age ge ", AgeLow, "; lt ", AgeHigh))


AgeLow <- 20; AgeHigh <- 29
plotly.Spec(Model_Spectra_Meta[Model_Spectra_Meta$TMA >= AgeLow & Model_Spectra_Meta$TMA < AgeHigh,], N_Samp = 'All',  colorGroup = 'Sex', main = paste0("Age >= ", AgeLow, " and < ", AgeHigh), 
               htmlPlotFolder = paste0("Sablefish 2022 Combo Age ge ", AgeLow, "; lt ", AgeHigh))


AgeLow <- 30; AgeHigh <- 39
plotly.Spec(Model_Spectra_Meta[Model_Spectra_Meta$TMA >= AgeLow & Model_Spectra_Meta$TMA < AgeHigh,], N_Samp = 'All',  colorGroup = 'Sex', main = paste0("Age >= ", AgeLow, " and < ", AgeHigh), 
               htmlPlotFolder = paste0("Sablefish 2022 Combo Age ge ", AgeLow, "; lt ", AgeHigh))


Age <- 40
plotly.Spec(Model_Spectra_Meta[Model_Spectra_Meta$TMA >= Age,], N_Samp = 'All',  colorGroup = 'Sex', main = paste0("Age Greater Than or Equal to ", Age), htmlPlotFolder = paste0("Sablefish 2022 Combo Age ge ", Age))


 