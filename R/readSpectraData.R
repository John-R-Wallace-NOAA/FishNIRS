

readSpectraData <- function(UploadDates, WaveFreqs1331 = WaveFreqs1331, WaveFreqs921 = WaveFreqs921, nearestColSubset = TRUE) {

   '  # Initial code provided by Jordan Healy <jordan.healy@noaa.gov>  '

   sourceFunctionURL <- function (URL) {
      " # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() "
      require(xml2)
      File.ASCII <- tempfile()
      on.exit(file.remove(File.ASCII))
          homeDir <- getwd()
      tempDir <- tempfile()
      dir.create(tempDir); setwd(tempDir)
      on.exit(setwd(homeDir), add = TRUE)
      on.exit(system(paste0("rm -r -f ", tempDir)), add = TRUE)
      writeLines(paste0('source("', readLines(textConnection(xml2::download_html(URL))), '")'), File.ASCII)
      source(File.ASCII, local = parent.env(environment()))
   }  
    
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/readXlsx.R")
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/get.subs.R")
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/match.f.R")  
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/ColumnMove.R")
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/catf.R")
   
   # Nearest Neighbor; x is the shorter column. The y elements nearest the x elements are returned. 
   nearbor <- function(x, y) {
   
     Out <- NULL
     for (i in 1:length(x)) 
         Out <- c(Out, y[order(abs(y - x[i]))[1]])
     Out
   }
   
   require(hexView)
   require(simplerspec)
   require(readxl)
   
   homeDir <- paste0(getwd(), '/')
   on.exit(setwd(homeDir))
   
   # Load standard wave freq.
   # load("WaveFreqs1331.RData", envir = .GlobalEnv)
   # load("WaveFreqs921.RData", envir = .GlobalEnv)
   # load("WaveFreqs921.B.RData")
   
   WaveFreqs1331.subset.for.921 <- as.numeric(WaveFreqs1331) %in% nearbor(as.numeric(WaveFreqs921), as.numeric(WaveFreqs1331))
   # WaveFreqs921.B.subset.for.921 <- as.numeric(WaveFreqs921.B) %in% nearbor(as.numeric(WaveFreqs921), as.numeric(WaveFreqs921.B)) # Both 921 - just use the WaveFreqs921 column labels below
   
   hakeStabSpcStudy <- NULL 
   
   for(h in 1:length(UploadDates)) {
   
      if( h %% 3 == 1) {
         dev.new(width = 400, height = 300)  #   homeDir, homeDir, "../OPUS Spectra
         par(mfrow = c(3, 2))
      }    
      
     
      for( i in UploadDates[h]) {
           for(j in c("Dry", "Etoh")) {
           
                setwd(paste0(homeDir, "../OPUS Spectra/", i, "/", j)) # Set working directory to folder containing spectral files
                
                ldf <- list() # creates an empty list
                (listspc <- dir(pattern = '*HAKE*')) # creates the list of all the .001 file names in the directory
                
                # Loops through and uploads each file, depends on simplerspec package
                for (k in 1:length(listspc)) { 
                      ldf[[k]] <- simplerspec::read_opus_bin_univ(listspc[k], extract = "spc", print_progress = TRUE, atm_comp_minus4offset = FALSE)
                }
                # print(str(ldf[[1]])) # check first element
                cat("\n\n"); print(names(ldf[[1]]))
                cat("\n\n"); print(str(ldf[[1]]))
                
                # The '..' usage is a data.table construct (data.table inherits from data.frame). Try it without the '..' for more info.
                # Using '..WaveFreqs1331' and '..WaveFreqs921' is subsetting to just those repective vectors of freq., if that complete subset is there.
                
                Spc_df <- NULL
                assign('WaveFreqs1331', WaveFreqs1331)
                assign('WaveFreqs1331', WaveFreqs1331, envir = parent.frame())
              
                for (k in 1:length(ldf)) {
                   # cat("\n", k, "\n")
                   if(ncol(ldf[[k]][['spc']]) >= 1331) {
                      if(ncol(ldf[[k]][['spc']][, ..WaveFreqs1331]) == 1331) { 
                         if(nearestColSubset) {
                         
                             WaveFreqs1331.subset <- cbind(Sample_ID = ldf[[k]]$metadata$sample_id, Date_Time_rf = ldf[[k]]$metadata$date_time_rf, 
                                             ldf[[k]][['spc']][, ..WaveFreqs1331])[, c(TRUE, TRUE, ..WaveFreqs1331.subset.for.921)] 
                             names(WaveFreqs1331.subset)[-(1:2)] <- WaveFreqs921       # Need the WaveFreqs921 labels for matching with true 921's       
                             Spc_df <- rbind(Spc_df, WaveFreqs1331.subset)                
                             WaveFreqsUsed <- WaveFreqs921
                             N <- 921
                          
                         } else {
                             Spc_df <- rbind(Spc_df, cbind(Sample_ID = ldf[[k]]$metadata$sample_id, Date_Time_rf = ldf[[k]]$metadata$date_time_rf, ldf[[k]][['spc']][, ..WaveFreqs1331])) 
                             WaveFreqsUsed <- WaveFreqs1331
                             N <- 1331
                          }
                      } else 
                          catf('\nThe number of wave freqs is greater than 1,331, but the standard wave freqs are not a subset of that greater number\n')
                          
                   } else {
                      # Code could handle Wavefreqs for which WaveFreqs921 or WaveFreqs921.B are a subset (as seen above for 1,331). This was not seen here for 921.
                      if(ncol(ldf[[k]][['spc']]) >= 921) {
                        if(ncol(ldf[[k]][['spc']]) == 921)
                           names(ldf[[k]][['spc']]) <- WaveFreqs921  # WaveFreqs921.B freqs are all close to WaveFreqs921 so just use WaveFreqs921
                        try( if(ncol(ldf[[k]][['spc']][, ..WaveFreqs921]) == 921) {
                            Spc_df <- rbind(Spc_df, cbind(Sample_ID = ldf[[k]]$metadata$sample_id, Date_Time_rf = ldf[[k]]$metadata$date_time_rf, ldf[[k]][['spc']][, ..WaveFreqs921])) 
                            WaveFreqsUsed <- WaveFreqs921
                            N <- 921
                        } )   
                      } else 
                         catf('\nNumber', k, 'Sample', ldf[[k]]$metadata$sample_id, 'has length', length(ldf[[k]][['spc']]), 'which is less than 1,331, but not greater or equal to 921.\n')
                   }       
                } 
                
                if(is.null(Spc_df)) {
                   
                   # Read in raggged data
                   Spc_df.ragged <- NULL
                   for (l in 1:length(ldf)) 
                       Spc_df.ragged <- rbind(Spc_df.ragged, renum(data.frame(Sample_ID = ldf[[l]]$metadata$sample_id, 
                                 Spectra = as.numeric(names(ldf[[l]][['spc']])), Absorbance = unlist(ldf[[l]][['spc']]))))
                   
                   # Look at the ragged data
                   plot(0, type = 'n', xlim = range(Spc_df.ragged$Spectra) + c(-10, 10), ylim = range(Spc_df.ragged$Absorbance) + c(-0.1, 0.1), 
                        main = paste0(i, '; ', j), xlab = "Wavelength", ylab = "Absorbance")
                   for( m in unique(Spc_df.ragged$Sample_ID) ) {
                      sampData <- Spc_df.ragged[Spc_df.ragged$Sample_ID %in% m, ]
                      lines(sampData$Spectra, sampData$Absorbance, col = ifelse(j == 'Dry', 'green', 'red'))
                   }
                   
                } else {
                
                   Spc_df$Sample_ID <- substr(Spc_df$Sample_ID, 1, 45)  # Remove '.OA1'  or 'OA2' suffix
                   
                   # Need to have the standard wave freqs loaded above to check Spc_df on the fly
                   # WaveFreqs1331 <- names(Spc_df)[-(1:2)] #  1,331 Wave freq.
                   # save(WaveFreqs1331, file = 'WaveFreqs1331.RData')
                   
                   # Look at spectra
                   
                   # This shows extra data after the dry oties 1,331 readings
                   # dev.new()
                   # matplot(1:ncol(Spc_df), t(Spc_df), type = 'l', lty = 1, col = 'red', )  # 
                   
                   # Remove extra data after the dry oties 1,331 readings
                   # Spc_df <- Spc_df[, 1:1331]
                   
                   # dev.new(width = 400, height = 300)
                   # matplot(1:ncol(Spc_df), t(Spc_df), col = ifelse(j == 'Dry', 'green', 'red'), type = 'l', lty = 1, main = paste0(i, '; ', j)) 
                   matplot(as.numeric(WaveFreqsUsed), t(Spc_df[, ..WaveFreqsUsed]), col = ifelse(j == 'Dry', 'green', 'red'), type = 'l', lty = 1,
                           main = paste0(i, '; ', j), xlab = "Wavelength", ylab = "Absorbance")
                   
                   # Look at the data
                   cat("\n\n")
                   print(Spc_df[1:3, 1:10])
                   
                   # Read in metadata
                   # hakeMetaData <- xlsxToR(homeDir, "../OPUS Spectra/NIR export exportQueryData 3_2_2020.xlsx")
                   # hakeMetaData <- readxl::read_excel(homeDir, "../OPUS Spectra/NIR export exportQueryData 3_2_2020.xlsx")
                   
                   # Regardless of 'OA1' or 'OA2' - change all to 'OA1'
                   # Spc_df$Sample_ID <- paste0(substr(Spc_df$Sample_ID , 1, 46), 'OA1')
                   
                   #  if(i == "2019_11_01") {
                   #        hakeMetaData <- read.csv(homeDir, "../OPUS Spectra/NIR export exportQueryData 3_2_2020_OA1.csv")[1:87, ]  # 87 good rows
                   #  } else
                   #        hakeMetaData <- read.csv(homeDir, "../OPUS Spectra/NIR export exportQueryData 3_2_2020_OA2.csv")[1:86, ]  # 86 good dry oties & 89 good Etoh
                   
                   hakeMetaData <- data.frame(readxl::read_excel(paste0(homeDir, "../OPUS Spectra/", i, "/", j, "/", dir(pattern = '*QueryData*'))))
                   # hakeMetaData <- JRWToolBox::readXlsx(paste0(homeDir, "../OPUS Spectra/", i, "/", j, "/", dir(pattern = '*QueryData*'))) 
                                  
                   hakeMetaData <- hakeMetaData[!is.na(hakeMetaData$cruise_number), ]
                   if(!'readability' %in% names(hakeMetaData)) {
                         hakeMetaData$readability <- NA 
                         hakeMetaData <- ColumnMove(hakeMetaData, 14)
                   }      
                                              
                         
                   hakeMetaData$date_collected <- as.POSIXct(as.character(hakeMetaData$date_collected), format = "%Y-%m-%d")
                   hakeMetaData$Sample_ID <- substr(hakeMetaData$file_name, 1, 45) # Remove '.OA1'  or 'OA2' suffix
                   hakeMetaData$file_name <- NULL
                   
                   # Match metadata to spectral data
                   Spc_Meta_df <- Spc_df
                   Spc_Meta_df$Storage <- j
                   Spc_Meta_df <- JRWToolBox::match.f(Spc_Meta_df, hakeMetaData, 'Sample_ID', 'Sample_ID', names(hakeMetaData)[-23]) 
                   catf('\nNumber of matches of metadata to Spectra table that occured:',  sum(!is.na(Spc_Meta_df$collection_year)), "\n\n")
                   
                   if(any(is.na(Spc_Meta_df$collection_year)))
                       stop("Good matching to metadata did not occur!")
                    
                   Cols <- c(1:2, N + 3:25, 3:(N + 2))                    
                   Spc_Meta_df <- Spc_Meta_df[, ..Cols]  # Move metadata to the front
                                      
                   metaDataNames <- c(names(Spc_df)[1:2], 'Storage', names(hakeMetaData)[-23]) # 25 metadata columns
                   
                   Cols <- c(metaDataNames, WaveFreqsUsed[1:2]) # '..' data.table construct needs an object name
                   print(Spc_Meta_df[1:4, ..Cols])
                   
                   hakeStabSpcStudy <- rbind(hakeStabSpcStudy, Spc_Meta_df)
                }
           }
      }     
   }
   hakeStabSpcStudy   
}   
