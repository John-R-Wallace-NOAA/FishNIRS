

# viewSpectra(c("Model_Scans/SABL_COMBO2022_NIR0022A_PRD_4_102157424_O1.0", "Model_Scans/SABL_COMBO2022_NIR0022H_PRD_1330_102167380_O1.0", "Model_Scans/SABL_COMBO2022_NIR0022H_PRD_1205_102150775_O1.0"))
# viewSpectra(folder = "Model_scans", maxNum = 10)

 viewSpectra <- function(fileNames = dir(path = folder), folder = NULL, maxNum = length(fileNames), opusReader = c('pierreroudier_opusreader', 'philippbaumann_opusreader2')[2]) {
 
    
    # --- Download functions from GitHub ---
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
    
    sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/plotly_spectra.R")
    
     
    if(opusReader == 'pierreroudier_opusreader') {
       if (!any(installed.packages()[, 1] %in% "opusreader")) 
          remotes::install_github("pierreroudier/opusreader")   # https://github.com/pierreroudier/opusreader
    }   
    
    if(opusReader == 'philippbaumann_opusreader2') {  
       if (!any(installed.packages()[, 1] %in% "opusreader2")) 
         remotes::install_github("spectral-cockpit/opusreader2")   #  https://github.com/spectral-cockpit/opusreader2
    }
    
    cat(paste0("\nNumber of spectral files to be read in: ", length(fileNames[1:maxNum]), "\n\n"))
        
    newScans <- list()
    for (i in fileNames[1:maxNum])  {
    
       print(i)
       if(!is.null(folder))
       i <- paste0(folder, "/", i)
       
       if(opusReader == 'pierreroudier_opusreader')
          try(newScans[[i]] <- opusreader::opus_read(i, simplify = TRUE, wns_digits = 0)[[2]] )
          
       if(opusReader == 'philippbaumann_opusreader2')
          try(newScans[[i]] <- opusreader2::read_opus_single(i)[[3]]$data) 
    }
    
    newScans_int <- matrix(data = NA, nrow = length(newScans), ncol = length(colnames(newScans[[1]])))
    for (j in 1:length(newScans))
         newScans_int[j,] <- newScans[[j]]
    
    
    plotly_spectra(data.frame(newScans_int), N_Samp = maxNum)
}


  