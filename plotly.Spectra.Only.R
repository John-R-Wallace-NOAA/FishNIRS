plotly.Spectra.Only <- function(Spectra, N_Samp = min(c(nrow(Spectra), 50)), htmlPlotFolder = NULL, shortName = " ") { 
 
    rowNums <- 1:nrow(Spectra)
    if(length(rowNums <= 26^2))
       plotly.Spec(data.frame(filenames = rowNums, Spectra, htmlPlotFolder = htmlPlotFolder, Otie = paste0(LETTERS[floor((rowNums - 0.001)/26) + 1], LETTERS[rowNums %r1% 26], '_', rowNums), 
             shortName = shortName), N_Samp = N_Samp, colorGroup = 'Otie')
    else
       plotly.Spec(data.frame(filenames = rowNums, Spectra, htmlPlotFolder = htmlPlotFolder, Otie = factor(rowNums), shortName = shortName), N_Samp = N_Samp, colorGroup = 'Otie') 
}    
