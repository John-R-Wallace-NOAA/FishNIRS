

# viewSpectra(c("Model_Scans/SABL_COMBO2022_NIR0022A_PRD_4_102157424_O1.0", "Model_Scans/SABL_COMBO2022_NIR0022H_PRD_1330_102167380_O1.0", "Model_Scans/SABL_COMBO2022_NIR0022H_PRD_1205_102150775_O1.0"))


 viewSpectra <- function(fileNames, opusReader = c('pierreroudier_opusreader', 'philippbaumann_opusreader2')[2]) {
 
    if(opusReader == 'pierreroudier_opusreader') {
       if (!any(installed.packages()[, 1] %in% "opusreader")) 
          remotes::install_github("pierreroudier/opusreader")   # https://github.com/pierreroudier/opusreader
    }   
    
    if(opusReader == 'philippbaumann_opusreader2') {  
       if (!any(installed.packages()[, 1] %in% "opusreader2")) 
         remotes::install_github("spectral-cockpit/opusreader2")   #  https://github.com/spectral-cockpit/opusreader2
    }
    
    newScans <- list()
    for (i in fileNames)  {
       print(i)
       if(opusReader == 'pierreroudier_opusreader')
          try(newScans[[i]] <- opusreader::opus_read(i, simplify = TRUE, wns_digits = 0)[[2]] )
          
       if(opusReader == 'philippbaumann_opusreader2')
          try(newScans[[i]] <- opusreader2::read_opus_single(i)[[3]]$data) 
    }
    
    newScans_int <- matrix(data = NA, nrow = length(newScans), ncol = length(colnames(newScans[[1]])))
    for (j in 1:length(newScans))
         newScans_int[j,] <- newScans[[j]]
    
    
    plotly_spectra(data.frame(newScans_int))
}

