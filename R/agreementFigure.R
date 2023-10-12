
agreementFigure <- function(Observed, Predicted, Delta = 0, Iter = 0, main = "", xlab = deparse(substitute(Observed)), 
                      ylab = paste0(deparse(substitute(Predicted)), " (rounded after adding Delta)"), full = FALSE, axes_zoomed_limits = 0:15, 
                      cex = ifelse(full, 0.75, 1.25), col_equal = 'red', col_off_1_or_2 = 'gold', col_off_3_or_4 = 'green', col_off_5_or_greater = 'navyblue', ...) {
  
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
   
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/renum.R")
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/match.f.R") 
   
   noNA <- na.omit(data.frame(Observed, Predicted))
   Obs <- noNA$Observed
   Predicted.rd <- round(noNA$Predicted + Delta) 
    
   Agreement_Agg <- aggregate(list(N = rep(1, length(Obs))), list(Observed = Obs, Predicted = Predicted.rd), length)
   maxAge <- max(c(Obs, Predicted.rd))
   Agreement_Table <- expand.grid( Predicted = 0:maxAge, Observed = 0:maxAge)
   Agreement_Table <- renum(match.f(Agreement_Table, Agreement_Agg, c('Observed', 'Predicted'), c('Observed', 'Predicted'), 'N'))
   Agreement_Table$N[is.na(Agreement_Table$N)] <- 0
   Agreement_Table$N_char <- as.character(Agreement_Table$N)
   Agreement_Table$N_char[Agreement_Table$N_char == "0"] <- " "
   
   if(Iter == 0)
        main <- ifelse(main == "", paste0("Delta = ", Delta), paste0(main, "; Delta = ", Delta))
   else     
        main <- ifelse(main == "", paste0("Iter = ", Iter), paste0(main, "; Iter = ", Iter))
       
   if(full) {
      X <- 0:max(Agreement_Table$Observed)
      Y <- 0:max(Agreement_Table$Predicted)
   } else {
      X <- Y <- axes_zoomed_limits   
   }   
   
   print(Correlation_R_squared_RMSE_MAE_SAD(Obs, Predicted.rd))
   
   plot(X, Y, main = main,
      xlab = paste0(xlab,': RMSE = ', signif(sqrt(mean((Predicted.rd - Obs)^2, na.rm = TRUE)), 6), '; SAD = ', 
                    signif(sum(abs(Predicted.rd - Obs)), 6), " (Prediction rounded after adding Delta for Stats)"), ylab = ylab, type = 'n', ...)
   
   text(Agreement_Table$Observed, Agreement_Table$Predicted, Agreement_Table$N_char, cex = cex, 
             col = ifelse(Agreement_Table$Observed == Agreement_Table$Predicted, 'red', 
                   ifelse(Agreement_Table$Observed == Agreement_Table$Predicted + 1 | Agreement_Table$Observed == Agreement_Table$Predicted - 1 |
                          Agreement_Table$Observed == Agreement_Table$Predicted + 2 | Agreement_Table$Observed == Agreement_Table$Predicted - 2, 'gold', 
                      ifelse(Agreement_Table$Observed == Agreement_Table$Predicted + 3 | Agreement_Table$Observed == Agreement_Table$Predicted - 3 |
                             Agreement_Table$Observed == Agreement_Table$Predicted + 4 | Agreement_Table$Observed == Agreement_Table$Predicted - 4, 'green', 'navyblue'))))    
     
}
