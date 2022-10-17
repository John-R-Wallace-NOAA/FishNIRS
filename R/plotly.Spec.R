plotly.Spec <- function(spectraMeta, N_Samp = 50, randomAfterSampNum = NULL, colorGroup = 'Age', WaveRange = c(0, 8000), scanUniqueName = 'shortName', freqNum = NULL, xlab = "Wavenumber", 
                           ylab = "Absorbance", plot = TRUE, reverse.plot.order = FALSE, verbose = FALSE) {
    
   require(plotly)
   renum <- function(x, no.num = F) {
      "https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/renum.R"
      if (nrow(x) == 0) 
          return(x)
      if (no.num) 
          dimnames(x)[[1]] <- rep("", nrow(x))
      else dimnames(x)[[1]] <- 1:nrow(x)
      x
    }

   a <- list(
     ticks = "outside",
     showline = TRUE,
     showticklabels = TRUE,
     showgrid = FALSE,
     mirror = TRUE
   )  
   
   xax <- a
   yax <- a
   xax$title <- xlab
   yax$title <- ylab
   
   if(reverse.plot.order)
      spectraMeta <- spectraMeta[nrow(spectraMeta):1, ]
  
   if(is.null(freqNum))  {   
      optOLD <- options(warn = -1)
      on.exit(options(optOLD))      
      freqNum <- sum(!is.na(as.numeric(names(spectraMeta))))
      if(verbose)
         cat("\nNumber of Frequencies = ", freqNum, "\n")      
   }
   
   WaveLengths <- as.numeric(names(spectraMeta[, 2:(freqNum + 1)]))
   WaveSubset <- as.character(WaveLengths[WaveLengths >= WaveRange[1] & WaveLengths <= WaveRange[2]])
   freqNum.Subset <- length(WaveSubset)
   
   spectraMeta <- spectraMeta[, c(scanUniqueName, WaveSubset, colorGroup)] 
   
   if(is.null(N_Samp)) {
       N_Samp <- nrow(spectraMeta)
       sampRows <- 1:N_Samp
       Spectra <- spectraMeta[, -c(1, ncol(spectraMeta))]
   } else {
      if(is.null(randomAfterSampNum)) {
         sampRows <- sample(1:nrow(spectraMeta), N_Samp)
      } else {
         sampRows <- c(1:randomAfterSampNum, sample((randomAfterSampNum + 1):nrow(spectraMeta), N_Samp))
         N_Samp <- length(1:randomAfterSampNum) + N_Samp
      }   
      Spectra <- spectraMeta[sampRows, -c(1, ncol(spectraMeta))]      
   }
   
   Spec <- renum(data.frame(as.matrix(data.frame(Scan = rep(spectraMeta[sampRows, scanUniqueName], each = freqNum.Subset),  # Double data.frame() is needed
                 Band = rep(as.numeric(names(Spectra)), N_Samp), Value = c(as.matrix(t(Spectra))), 
                 Color = rep(spectraMeta[sampRows, grep(colorGroup, names(spectraMeta))[1]], each = freqNum.Subset))))) 
   Spec$Band <- as.numeric(Spec$Band)
   Spec$Value <- as.numeric(Spec$Value)
      
   if(verbose) {
      print(str(Spec))
      print(Spec[1:4,])                
   }   
   if(plot) {
      print(plot_ly(Spec, 
                   x = ~Band, y = ~Value, split = ~Scan, color = ~Color, 
                   colors = rainbow(length(unique(Spec$Color))), 
                   hoverinfo = 'text',
                   text = ~paste('Scan: ', Scan,
                                 '</br></br> x-axis: ', Band,
                                 '</br> y-axis: ', Value)) %>%
          layout(xaxis = xax, yaxis = yax) %>%
          add_lines(line = list(width = 0.75)))
  }     
       
  if(sum(is.na(as.numeric(Spec$Color))) < 0.20 * length(Spec$Color))
       Spec$Color <- as.numeric(Spec$Color)    
       
  names(Spec)[4] <- colorGroup 

  invisible(Spec)       
}  
