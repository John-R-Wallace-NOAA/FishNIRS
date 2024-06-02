
agreementFigure <- function(Observed, Predicted, Rdm_Reps = NULL, Folds = NULL, Delta = NULL, Iter = 0, main = "", xlab = deparse(substitute(Observed)), browserPlot = FALSE, 
                      ylab = paste0(deparse(substitute(Predicted)), ifelse(is.null(Delta), "", " (rounded after adding Delta)")), full = TRUE, axes_zoomed_limit = 15, 
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
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/col.alpha.R") 
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Cor_R_squared_RMSE_MAE_SAD_APE.R")
   
   noNA <- na.omit(data.frame(Observed, Predicted))
   Obs <- noNA$Observed
   Predicted.rd <- round(noNA$Predicted + ifelse(is.null(Delta), 0, Delta))
    
   Agreement_Agg <- aggregate(list(N = rep(1, length(Obs))), list(Observed = Obs, Predicted = Predicted.rd), length)
   maxAge <- max(c(Obs, Predicted.rd))
   Agreement_Table <- expand.grid( Predicted = 0:maxAge, Observed = 0:maxAge)
   Agreement_Table <- renum(match.f(Agreement_Table, Agreement_Agg, c('Observed', 'Predicted'), c('Observed', 'Predicted'), 'N'))
   Agreement_Table$N[is.na(Agreement_Table$N)] <- 0
   Agreement_Table$N_char <- as.character(Agreement_Table$N)
   Agreement_Table$N_char[Agreement_Table$N_char == "0"] <- " "
   
   if(!is.null(Rdm_Reps))
        main <- ifelse(main == "", paste0("Random Reps = ", Rdm_Reps), paste0(main, "; Random Reps = ", Rdm_Reps))
   
   if(!is.null(Folds)) {
   
        if(Folds == 1) 
		   main <- ifelse(main == "", paste0("Fold = ", Folds), paste0(main, "; Fold = ", Folds))
		else
           main <- ifelse(main == "", paste0("Folds = ", Folds), paste0(main, "; Folds = ", Folds))
   }
   
   if(!is.null(Delta))
        main <- ifelse(main == "", paste0("Delta = ", Delta), paste0(main, "; Delta = ", Delta))
			
   if(Iter != 0)    
        main <- ifelse(main == "", paste0("Iter = ", Iter), paste0(main, "; Iter = ", Iter))
        
       
   if(full) {
      X <- 0:max(Agreement_Table$Observed)
      Y <- 0:max(Agreement_Table$Predicted)
   } else {
      X <- Y <- 0:axes_zoomed_limit  
   }   

   cat("\n\n")
   Stats <- Cor_R_squared_RMSE_MAE_SAD_APE(Obs, Predicted.rd)  
   Stats <- signif(Stats, digits = 4) # Less digits for the figure
   
   if(!is.null(Delta))
       cat(paste0("(Prediction has been rounded to the nearest integer after adding a Delta of ", Delta, ")\n\n"))
   	   
   plot(X, Y, main = main,
      xlab = paste0(xlab, ': R^2 = ', format(Stats$R_squared, nsmall = 4), '; RMSE = ', format(Stats$RMSE, nsmall = 4), '; SAD = ', 
                    Stats$SAD, '; APE = ', Stats$APE, '; N_Pred = ', Stats$N, ifelse(is.null(Delta), "", " (Prediction rounded after adding Delta for Stats)")), ylab = ylab, type = 'n', ...)
                    
   abline(0, 1, col = col.alpha('grey', ifelse(full, 0.50, 0.35)))
   
   text(Agreement_Table$Observed, Agreement_Table$Predicted, Agreement_Table$N_char, cex = cex, 
             col = ifelse(Agreement_Table$Observed == Agreement_Table$Predicted, 'red', 
                   ifelse(Agreement_Table$Observed == Agreement_Table$Predicted + 1 | Agreement_Table$Observed == Agreement_Table$Predicted - 1 |
                          Agreement_Table$Observed == Agreement_Table$Predicted + 2 | Agreement_Table$Observed == Agreement_Table$Predicted - 2, 'gold', 
                      ifelse(Agreement_Table$Observed == Agreement_Table$Predicted + 3 | Agreement_Table$Observed == Agreement_Table$Predicted - 3 |
                             Agreement_Table$Observed == Agreement_Table$Predicted + 4 | Agreement_Table$Observed == Agreement_Table$Predicted - 4, 'green', 'navyblue'))))   
}


