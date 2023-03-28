

agreementFigure <- function(Observed, Predicted, Delta = 0, Iter = 0, main = "", xlab = deparse(substitute(Observed)), 
                      ylab = paste0(deparse(substitute(Predicted)), " (rounded after adding Delta)"), full = FALSE, axes_zoomed_limits = 0:15, 
                      cex = ifelse(full, 0.75, 1.25), col_equal = 'red', col_off_1_or_2 = 'gold', col_off_3_or_4 = 'green', col_off_5_or_greater = 'navyblue', ...) {

   Predicted.rd <- round(Predicted + Delta) 
    
   Agreement_Agg <- aggregate(list(N = rep(1, length(Observed))), list(Observed = Observed, Predicted = Predicted.rd), length)
   maxAge <- max(c(Observed, Predicted.rd))
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
   
   print(Correlation_R_squared_RMSE_MAE_SAD(Observed, Predicted.rd))
   
   plot(X, Y, main = main,
      xlab = paste0(xlab,': RMSE = ', signif(sqrt(mean((Predicted.rd - Observed)^2, na.rm = TRUE)), 6), '; SAD = ', 
                    signif(sum(abs(Predicted.rd - Observed)), 6), " (Prediction rounded after adding Delta for Stats)"), ylab = ylab, type = 'n', ...)
   
   text(Agreement_Table$Observed, Agreement_Table$Predicted, Agreement_Table$N_char, cex = cex, 
             col = ifelse(Agreement_Table$Observed == Agreement_Table$Predicted, 'red', 
                   ifelse(Agreement_Table$Observed == Agreement_Table$Predicted + 1 | Agreement_Table$Observed == Agreement_Table$Predicted - 1 |
                          Agreement_Table$Observed == Agreement_Table$Predicted + 2 | Agreement_Table$Observed == Agreement_Table$Predicted - 2, 'gold', 
                      ifelse(Agreement_Table$Observed == Agreement_Table$Predicted + 3 | Agreement_Table$Observed == Agreement_Table$Predicted - 3 |
                             Agreement_Table$Observed == Agreement_Table$Predicted + 4 | Agreement_Table$Observed == Agreement_Table$Predicted - 4, 'green', 'navyblue'))))    
     
}
