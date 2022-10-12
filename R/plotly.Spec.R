plotly.Spec <- function(spectraMeta, N_Samp = 50, colorGroup = 'Age', scanUniqueName = 'shortName', freqNum = NULL, xlab = "Wavenumber", 
                           ylab = "Absorbance", plot = TRUE, verbose = FALSE) {
    
   require(plotly)
   
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
   
   if(is.null(freqNum))  {   
      optOLD <- options(warn = -1)
      on.exit(options(optOLD))      
      freqNum <- sum(!is.na(as.numeric(names(spectraMeta))))
      if(verbose)
         cat("\nfreqNum = ", freqNum, "\n")      
   }
   
   sampRows <- sample(1:nrow(spectraMeta), N_Samp)
   Spectra <- spectraMeta[sampRows, 2:(freqNum + 1)]
   Spec <- renum(as.matrix(data.frame(Scan = rep(spectraMeta[sampRows, scanUniqueName], each = freqNum), 
                  Band = rep(as.numeric(names(Spectra)), N_Samp), Value = c(as.matrix(t(Spectra))), 
                 Color = rep(spectraMeta[sampRows, grep(colorGroup, names(spectraMeta))[1]], each = freqNum))))               
                    
   Spec <- data.frame(Spec)
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
