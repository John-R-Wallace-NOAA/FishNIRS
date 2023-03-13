
agreementFigure <- function(Observed, Predicted, Delta, main = paste0("Iter = ", Iter), xlab = deparse(substitute(Observed)), ylab = deparse(substitute(Predicted)), full = FALSE,
                      axes_zoomed_limits = 0:15, cex = ifelse(full, 0.75, 1.25), col_equal = 'red', col_off_1_or_2 = 'gold', col_off_3_or_4 = 'green', col_off_5_or_greater = 'navyblue') {

   Predicted.rd <- round(Predicted + Delta) 
    
   Agreement_Agg <- aggregate(list(N = rep(1, length(Observed))), list(Observed = Observed, Predicted = Predicted.rd), length)
   maxAge <- max(c(Observed, Predicted.rd))
   Agreement_Table <- expand.grid( Predicted = 0:maxAge, Observed = 0:maxAge)
   Agreement_Table <- renum(match.f(Agreement_Table, Agreement_Agg, c('Observed', 'Predicted'), c('Observed', 'Predicted'), 'N'))
   Agreement_Table$N[is.na(Agreement_Table$N)] <- 0
   Agreement_Table$N_char <- as.character(Agreement_Table$N)
   Agreement_Table$N_char[Agreement_Table$N_char == "0"] <- " "
   
   if(full)  {
   
      plot(0:max(Agreement_Table$Observed), 0:max(Agreement_Table$Predicted), main = ifelse(is.null(main), paste0('Delta = ', Delta), paste0(main, ': Delta = ', Delta)),
         xlab = paste0(xlab,': RMSE = ', signif(sqrt(mean((Predicted.rd - Observed)^2, na.rm = TRUE)), 6), '; SAD = ', 
                       signif(sum(abs(Predicted.rd - Observed)), 6), " (Prediction rounded)"), ylab = ylab, type = 'n')
      text(Agreement_Table$Observed, Agreement_Table$Predicted, Agreement_Table$N_char, cex = cex, 
                col = ifelse(Agreement_Table$Observed == Agreement_Table$Predicted, 'red', 
                   ifelse(Agreement_Table$Observed == Agreement_Table$Predicted + 1 | Agreement_Table$Observed == Agreement_Table$Predicted - 1 |
                          Agreement_Table$Observed == Agreement_Table$Predicted + 2 | Agreement_Table$Observed == Agreement_Table$Predicted - 2, 'gold', 
                      ifelse(Agreement_Table$Observed == Agreement_Table$Predicted + 3 | Agreement_Table$Observed == Agreement_Table$Predicted - 3 |
                             Agreement_Table$Observed == Agreement_Table$Predicted + 4 | Agreement_Table$Observed == Agreement_Table$Predicted - 4, 'green', 'navyblue'))))    
   } else { 
       
     plot(axes_zoomed_limits, axes_zoomed_limits, main = ifelse(is.null(main), paste0('Delta = ', Delta), paste0(main, ': Delta = ', Delta)),
        xlab = paste0(xlab,': RMSE = ', signif(sqrt(mean((Predicted.rd - Observed)^2, na.rm = TRUE)), 6), '; SAD = ', 
                       signif(sum(abs(Predicted.rd - Observed)), 6), " (Prediction rounded)"), ylab = ylab, type = 'n')
     text(Agreement_Table$Observed, Agreement_Table$Predicted, Agreement_Table$N_char, cex = cex,
            col = ifelse(Agreement_Table$Observed == Agreement_Table$Predicted, col_equal, 
                    ifelse(Agreement_Table$Observed == Agreement_Table$Predicted + 1 | Agreement_Table$Observed == Agreement_Table$Predicted - 1 |
                           Agreement_Table$Observed == Agreement_Table$Predicted + 2 | Agreement_Table$Observed == Agreement_Table$Predicted - 2, col_off_1_or_2, 
                       ifelse(Agreement_Table$Observed == Agreement_Table$Predicted + 3 | Agreement_Table$Observed == Agreement_Table$Predicted - 3 |
                              Agreement_Table$Observed == Agreement_Table$Predicted + 4 | Agreement_Table$Observed == Agreement_Table$Predicted - 4, col_off_3_or_4, col_off_5_or_greater))))
   }
}

