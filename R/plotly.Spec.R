plotly.Spec <- function(spectraMeta, N_Samp = 50, colorGroup = 'Age', scanUniqueName = 'filenames', freqNum = NULL, xlab = "Wavenumber", 
                           ylab = "Absorbance", verbose = FALSE) {
    
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
      freqNum <- sum(!is.na(as.numeric(names(hake_all_2019.6.20))))      
   }
   
   sampRows <- sample(1:nrow(spectraMeta), N_Samp)
   Spectra <- spectraMeta[sampRows, 2:(freqNum + 1)]
   Spec <- renum(as.matrix(data.frame(Scan = rep(spectraMeta[sampRows, scanUniqueName], each = freqNum), 
                  Band = rep(as.numeric(names(Spectra)), N_Samp), Value = c(as.matrix(t(Spectra))), 
                 Color = rep(spectraMeta[sampRows, grep(colorGroup, names(spectraMeta))[1]], each = freqNum))))               
                    
   Spec <- data.frame(Spec)
   Spec$Scan <- as.character(Spec$Scan)
   if(verbose) 
      Spec[1:4,]                 
      
   plot_ly(Spec, 
                x = ~Band, y = ~Value, split = ~Scan, color = ~Color, 
                colors = rainbow(length(unique(Spec$Color))), 
                hoverinfo = 'text',
                text = ~paste('Scan: ', Scan,
                              '</br></br> x-axis: ', Band,
                              '</br> y-axis: ', Value)) %>%
       layout(xaxis = xax, yaxis = yax) %>%
       add_lines(line = list(width = 0.75))
}  
