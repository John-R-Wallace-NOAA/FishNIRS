
agreementFigure <- function(y.test, y.test.pred, Delta, full = FALSE, col_equal = 'red', col_off_1_or_2 = 'gold', col_off_3_or_4 = 'green', col_off_5_or_greater = 'navyblue') {

   y.test.pred.rd <- round(y.test.pred + Delta) 
    
   Agreement_Agg <- aggregate(list(N = rep(1, length(y.test))), list(y.test = y.test, y.test.pred = y.test.pred.rd), length)
   maxAge <- max(c(y.test, y.test.pred.rd))
   Agreement_Table <- expand.grid( y.test.pred = 0:maxAge, y.test = 0:maxAge)
   Agreement_Table <- renum(match.f(Agreement_Table, Agreement_Agg, c('y.test', 'y.test.pred'), c('y.test', 'y.test.pred'), 'N'))
   Agreement_Table$N[is.na(Agreement_Table$N)] <- 0
   Agreement_Table$N_char <- as.character(Agreement_Table$N)
   Agreement_Table$N_char[Agreement_Table$N_char == "0"] <- " "
   
   if(full)  {
   
      plot(0:max(Agreement_Table$y.test), 0:max(Agreement_Table$y.test.pred), 
         xlab = paste0('y.test: Sum of Absolute Differences = ', signif(sum(abs(y.test.pred.rd - y.test)), 6)), ylab = 'y.test.pred', type = 'n')
      text(Agreement_Table$y.test, Agreement_Table$y.test.pred, Agreement_Table$N_char, cex = 0.75, 
                col = ifelse(Agreement_Table$y.test == Agreement_Table$y.test.pred, 'red', 
                   ifelse(Agreement_Table$y.test == Agreement_Table$y.test.pred + 1 | Agreement_Table$y.test == Agreement_Table$y.test.pred - 1 |
                          Agreement_Table$y.test == Agreement_Table$y.test.pred + 2 | Agreement_Table$y.test == Agreement_Table$y.test.pred - 2, 'gold', 
                      ifelse(Agreement_Table$y.test == Agreement_Table$y.test.pred + 3 | Agreement_Table$y.test == Agreement_Table$y.test.pred - 3 |
                             Agreement_Table$y.test == Agreement_Table$y.test.pred + 4 | Agreement_Table$y.test == Agreement_Table$y.test.pred - 4, 'green', 'navyblue'))))    
   } else { 
       
     plot(0:15, 0:15, xlab = paste0('y.test: Sum of Absolute Differences = ', signif(sum(abs(y.test.pred.rd - y.test)), 6)), ylab = 'y.test.pred', type = 'n', 
           main = paste0('Iter = ', Iter, '; Delta = ', Delta))
     text(Agreement_Table$y.test, Agreement_Table$y.test.pred, Agreement_Table$N_char, cex = 1.25,
            col = ifelse(Agreement_Table$y.test == Agreement_Table$y.test.pred, col_equal, 
                    ifelse(Agreement_Table$y.test == Agreement_Table$y.test.pred + 1 | Agreement_Table$y.test == Agreement_Table$y.test.pred - 1 |
                           Agreement_Table$y.test == Agreement_Table$y.test.pred + 2 | Agreement_Table$y.test == Agreement_Table$y.test.pred - 2, col_off_1_or_2, 
                       ifelse(Agreement_Table$y.test == Agreement_Table$y.test.pred + 3 | Agreement_Table$y.test == Agreement_Table$y.test.pred - 3 |
                              Agreement_Table$y.test == Agreement_Table$y.test.pred + 4 | Agreement_Table$y.test == Agreement_Table$y.test.pred - 4, col_off_3_or_4, col_off_5_or_greater))))
   }
}

