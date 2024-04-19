plotly.Spec <- function(spectraMeta, N_Samp = min(c(nrow(spectraMeta), 50)), htmlPlotFolder = NULL, randomAfterSampNum = NULL, colorGroup = 'TMA', contColorVar = FALSE, facetGroup = NULL, WaveRange = c(0, 8000), 
                  scanUniqueName = 'shortName', freqNum = NULL, xlab = "Wavenumber", ylab = "Absorbance", plot = TRUE, alpha = 1, 
                  bgcolor = "#e5ecf6", main = NULL, xlim = NULL, ylim = NULL, verbose = FALSE, ...) {
   
   if (!any(installed.packages()[, 1] %in% "ggplot2")) 
     install.packages("ggplot2")
     
   if (!any(installed.packages()[, 1] %in% "plotly")) 
     install.packages("plotly")  
   
   require(ggplot2)   
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
  
   xax <- list(title = xlab, range = xlim)
   yax <- list(title = ylab, range = ylim)
   
   oldOpts <- options(warn = -1)
   
   # Remove 'X' prefix from freq names, if present
   oldNames <- names(spectraMeta)
   N <- length(oldNames)
   newNames <- oldNames
   
   for(i in 1:N) {

     if(is.na(as.numeric(substr(oldNames[i], 1, 1))) & !is.na(as.numeric(substring(oldNames[i], 2))))
          newNames[i] <- substring(oldNames[i], 2)
   }
   names(spectraMeta) <- newNames
  
  
   if(is.null(freqNum))  {   
      freqNum <- sum(!is.na(as.numeric(names(spectraMeta))))
      if(verbose)
         cat("\nNumber of Frequencies = ", freqNum, "\n")      
   }
   
   options(oldOpts)
   
   WaveLengths <- as.numeric(names(spectraMeta[, 2:(freqNum + 1)]))
   WaveSubset <- as.character(WaveLengths[WaveLengths >= WaveRange[1] & WaveLengths <= WaveRange[2]])
   freqNum.Subset <- length(WaveSubset)
   if(verbose)
         cat("\nNumber of Frequencies subset by the wave range (WaveRange) = ", freqNum.Subset, "\n")   
   
   if(is.null(facetGroup)) {
   
      spectraMeta <- spectraMeta[, c(scanUniqueName, WaveSubset, colorGroup)] 
 
      if(casefold(N_Samp) == "all"  ) {
          N_Samp <- nrow(spectraMeta)
          sampRows <- 1:N_Samp
      } else {
         if(is.null(randomAfterSampNum)) {
            sampRows <- sample(1:nrow(spectraMeta), N_Samp)
         } else {
            sampRows <- c(1:randomAfterSampNum, sample((randomAfterSampNum + 1):nrow(spectraMeta), N_Samp))
            N_Samp <- length(1:randomAfterSampNum) + N_Samp
         }   
      }
      Spectra <- spectraMeta[sampRows, -c(1, ncol(spectraMeta))]  
      
      Spec <- renum(data.frame(as.matrix(data.frame(Scan = rep(spectraMeta[sampRows, scanUniqueName], each = freqNum.Subset),  # Double use of data.frame() is needed
                    Band = rep(as.numeric(names(Spectra)), N_Samp), Value = c(as.matrix(t(Spectra))), 
                    Color = rep(spectraMeta[sampRows, grep(colorGroup, names(spectraMeta))[1]], each = freqNum.Subset)
                    )))) 
      Spec$Band <- as.numeric(Spec$Band)
      Spec$Value <- as.numeric(Spec$Value)
      
      if(contColorVar)
          Spec$Color <- as.numeric(Spec$Color)    
         
      if(verbose) {
         print(str(Spec))
         print(Spec[1:4,])                
      }   
      #   if(plot) {
      #     print(plot_ly(Spec, 
      #                  x = ~Band, y = ~Value, split = ~Scan, color = ~Color, 
      #                  colors = rainbow(length(unique(Spec$Color))), 
      #                  hoverinfo = 'text',
      #                  text = ~paste('Scan: ', Scan,
      #                                '</br></br> x-axis: ', Band,
      #                                '</br> y-axis: ', Value), ...) %>%
      #         layout(title = main, plot_bgcolor = bgcolor, xaxis = xax, yaxis = yax) %>%
      #         add_lines(line = list(width = 0.75)))
      #      
      #   }     
      if(plot)  {  
         if(contColorVar)
             print(ggplotly(ggplot(data = Spec, aes(x = Band, y = Value, z = Scan)) + geom_line(aes(colour = Color), linewidth = 0.2) + labs(colour = colorGroup) + ggtitle(main)))
         else    
             print(ggplotly(ggplot(data = Spec, aes(x = Band, y = Value, z = Scan)) + geom_line(aes(colour = Color), linewidth = 0.2) + labs(colour = colorGroup) + 
                    scale_color_manual(values=rainbow(length(unique(Spec$Color)), alpha = alpha)) + ggtitle(main)))
                    
            # print(ggplotly(ggplot(data = Spec, aes(x = Band, y = Value, z = Scan)) + geom_line(aes(colour = Color), linewidth = 0.2) + 
            #           scale_color_manual(values=rainbow(length(unique(Spec$Color)), alpha = c(1, rep(alpha, length(unique(Spec$Color)) - 1))))))           
      }
      
      #  if(sum(is.na(as.numeric(Spec$Color))) < 0.20 * length(Spec$Color))
      #    Spec$Color <- as.numeric(Spec$Color)    
          
      names(Spec)[4] <- colorGroup
      
   } else {
   
      spectraMeta <- spectraMeta[, c(scanUniqueName, WaveSubset, colorGroup, facetGroup)] 
      
      if(casefold(N_Samp) == "all"  ) {
          N_Samp <- nrow(spectraMeta)
          sampRows <- 1:N_Samp
      } else {
         if(is.null(randomAfterSampNum)) {
            sampRows <- sample(1:nrow(spectraMeta), N_Samp)
         } else {
            sampRows <- c(1:randomAfterSampNum, sample((randomAfterSampNum + 1):nrow(spectraMeta), N_Samp))
            N_Samp <- length(1:randomAfterSampNum) + N_Samp
         }    
      }
      Spectra <- spectraMeta[sampRows, -c(1, ncol(spectraMeta) - 1, ncol(spectraMeta))] 
      
      Spec <- renum(data.frame(as.matrix(data.frame(Scan = rep(spectraMeta[sampRows, scanUniqueName], each = freqNum.Subset),  # Double use of  data.frame() is needed
                    Band = rep(as.numeric(names(Spectra)), N_Samp), Value = c(as.matrix(t(Spectra))), 
                    Color = rep(spectraMeta[sampRows, grep(colorGroup, names(spectraMeta))[1]], each = freqNum.Subset),
                    Facet = rep(spectraMeta[sampRows, grep(facetGroup, names(spectraMeta))[1]], each = freqNum.Subset)
                    )))) 
      Spec$Band <- as.numeric(Spec$Band)
      Spec$Value <- as.numeric(Spec$Value)
      
      if(contColorVar)
         Spec$Color <- as.numeric(Spec$Color)    
         
      if(verbose) {
         print(str(Spec))
         print(Spec[1:4,])                
      }   
     
      if(plot) {
          if(contColorVar)
             print(ggplotly(ggplot(data = Spec, aes(x = Band, y = Value, z = Scan)) + geom_line(aes(colour = Color), linewidth = 0.2) + facet_grid(Facet ~ .) + labs(colour = colorGroup) + ggtitle(main))) 
          else 
            print(ggplotly(ggplot(data = Spec, aes(x = Band, y = Value, z = Scan)) + geom_line(aes(colour = Color), linewidth = 0.2) + facet_grid(Facet ~ .) + labs(colour = colorGroup) + 
                       scale_color_manual(values=rainbow(length(unique(Spec$Color)), alpha = alpha)) + ggtitle(main)))            
      }                  
               
      # if(sum(is.na(as.numeric(Spec$Color))) < 0.20 * length(Spec$Color))
      #   Spec$Color <- as.numeric(Spec$Color)    
          
      names(Spec)[4] <- colorGroup 
      names(Spec)[5] <- facetGroup  
   }
   
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
    
  invisible(Spec)       
}  

