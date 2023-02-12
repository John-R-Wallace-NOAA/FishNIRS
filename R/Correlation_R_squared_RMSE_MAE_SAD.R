
Correlation_R_squared_RMSE_MAE_SAD <- function(Obs, Pred, na.rm = TRUE) {

    signif(c(
      Correlation = cor(Pred, Obs),
        R_squared = cor(Pred, Obs)^2, 
             RMSE = sqrt(mean((Pred - Obs)^2, na.rm = TRUE)), 
              MAE = mean(abs(Pred - Obs), na.rm = TRUE),
              SAD = sum(abs(Pred - Obs))
    ), 6)
}       
