
setwd("C:/SIDT/Sable_Test")


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
###


sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/Scripts/plotly.Spec.R")


usr.warn <- options(warn = -1)
on.exit(options(usr.warn))   
  
download.file("https://raw.githubusercontent.com/John-R-Wallace-NOAA/GSHHG_High_Rez_for_R/master/world.f.land.A.RData", "world.f.land.A.RData")

    

"/nwcfile.nmfs.local/FRAM/Assessments/Aging Lab/NIRS Scanning Analysis/Sable_Test"





































