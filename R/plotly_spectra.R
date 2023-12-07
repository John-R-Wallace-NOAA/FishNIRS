 

# Sable_Combo_2022_Scans <- Read_OPUS_Spectra(Spectra_Path = 'Model_Scans')
# plotly_spectra(Sable_Combo_2022_Scans)
# plotly_spectra(Sable_Combo_2022_Scans, N_Samp = 300, htmlPlotFolder = 'Figures/Sablefish_2022_Spectra_Sample_of_300')


 plotly_spectra <- function(Spectra, N_Samp = min(c(nrow(Spectra), 50)), htmlPlotFolder = NULL, shortName = " ") { 
 
    rowNums <- 1:nrow(Spectra)
    if(length(rowNums <= 26^2))
       plotly.Spec(data.frame(filenames = rowNums, Spectra, Otie = paste0(LETTERS[floor((rowNums - 0.001)/26) + 1], LETTERS[rowNums %r1% 26], '_', rowNums), shortName = shortName), N_Samp = N_Samp, colorGroup = 'Otie')
    else
       plotly.Spec(data.frame(filenames = rowNums, Spectra, Otie = factor(rowNums), shortName = shortName), N_Samp = N_Samp, colorGroup = 'Otie') 
    
    if(!is.null(htmlPlotFolder)) {
       sourceFunctionURL <- function (URL,  type = c("function", "script")[1]) {
                 " # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() "
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
          
       sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/saveHtmlFolder.R") 
     
       saveHtmlFolder(htmlPlotFolder, view = !interactive())
    }
}   
       
