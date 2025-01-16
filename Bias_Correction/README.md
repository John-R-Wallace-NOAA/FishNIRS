Bias Correction for Older NN Predicted Ages

Since the NN prediction bias in older ages is consistently in one direction it can be adjusted for.

The bias correction needs to be done not only when a TMA (Traditional Method of Aging) age is given, but also when there is no corresponding TMA for a given age structure. Hence the goal of a bias corrected prediction, from scans and/or metadata without the need for traditional aging, can be achieved. 

A functional form (model) is needed that predicts the difference between TMA and the NN predicted age given a new value of the NN predicted age. Below a LOWESS (locally weighted scatterplot smoothing) non-parametric model is used with R's splinefun() function for prediction, but if an estimate of the additional error added by using a bias correction is wanted, a GAM with a smoother or a parametric functional form may work.

<br>
First some functions are needed, see below for what they will be used for:

    # Avoid source_url() in the bloatware devtools package. The guts of this code is from sourceFunctionURL() in my rgit package.

    File.ASCII <- tempfile()
    
    for(Func in c("predict.lowess.R", "lowess.line.R", "browsePlot.R")) {
       write(readLines(textConnection(httr::content(httr::GET( 
         paste0("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/", Func))))), File.ASCII)
       source(File.ASCII)
    }
    
     write(readLines(textConnection(httr::content(httr::GET( 
         "https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Cor_R_squared_RMSE_MAE_SAD_APE.R")))), File.ASCII)
     source(File.ASCII)
    
    nul <- file.remove(File.ASCII); rm(File.ASCII, nul)
    "  "
    
<br>
Download a dataset with some missing TMA:

     download.file("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/main/Bias_Correction/TMA_Pred.RData",
                   "TMA_Pred.RData")
     load("C:\\ALL_USR\\JRW\\R_Scratch\\TMA_Pred.RData")
     " "
    
<br>
Plot the data with a 1-1 line and calculate stats.  My toolbox function browsePlot() was downloaded above and will used for plotting the figures directly into a browser and saved into a file.

    browsePlot('plot(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED, xlim = c(0, 18.5), ylim = c(0, 18.5))
                abline(0, 1, col = "grey")', file = 'NN_Pred_vs_TMA.png')

    # The default is na.rm = TRUE for Cor_R_squared_RMSE_MAE_SAD_APE()
    Cor_R_squared_RMSE_MAE_SAD_APE(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED, digits = 6)
     " "
     
<br> 

The stats for the [biased NN_Pred plotted against TMA](https://github.com/John-R-Wallace-NOAA/FishNIRS/tree/main/Bias_Correction/NN_Pred_vs_TMA.png) are:

      Correlation R_squared  RMSE   MAE   SAD   APE  N
           0.9577    0.9172 2.055 1.497 52.38 8.766 35

    
<br>   
Using predict.lowess() from my toolbox [which uses stats::splinefun()], the difference between TMA and NN_Pred is fitted against NN_Pred using lowess(). The difference upon being added to NN_Pred is plotted with lowess smoothed lines using lowess.line().
		 

     (Bias_Adjustment <- predict.lowess(lowess(TMA_Pred$NN_Pred[!is.na(TMA_Pred$TMA)], TMA_Pred$TMA[!is.na(TMA_Pred$TMA)] - 
                                                TMA_Pred$NN_Pred[!is.na(TMA_Pred$TMA)], f = 2/3), newdata = TMA_Pred$NN_Pred))[1:10]

    # Note the need to get the quoting correct inside the plotting code when using browsePlot ( '  " "  ')
     browsePlot('
       plot(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED, xlim = c(0, 16), ylim = c(0, 16)); abline(0, 1, col = "grey")
       lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED)
       points(TMA_Pred$TMA + 0.15, TMA_Pred$NN_Pred_BIASED + Bias_Adjustment, col = "green")
       lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED + Bias_Adjustment, col = "green"smoothing.param = 2/3) 
       lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED + Bias_Adjustment, col = "green", smoothing.param = 0.8, lty = 2)
       lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED + Bias_Adjustment, col = "green", smoothing.param = 1, lty = 3)
     ', file = 'NN_Pred_Bias_Adj_vs_TMA_.png')
    
     
     Cor_R_squared_RMSE_MAE_SAD_APE(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED + Bias_Adjustment, digits = 6)
     "  "

The stats for the [lowess biased adjusted NN_Pred plotted against TMA](https://github.com/John-R-Wallace-NOAA/FishNIRS/tree/main/Bias_Correction/NN_Pred_Bias_Adj_vs_TMA_.png) are:
    
     Correlation R_squared  RMSE    MAE   SAD   APE  N
           0.968     0.937 1.143 0.9053 31.69 5.983 35


     
     
  asdfsdf   

    # TMA_Pred_SAVE <- TMA_Pred
    # TMA_Pred_SAVE -> TMA_Pred

    Bias_Adj_Factor_Ages <- c(8, 9:15)
    Ages_Diff <- Bias_Adj_Factor_Ages[-1] - apply(matrix(Bias_Adj_Factor_Ages[-1], ncol = 1), 1, function(x) mean(TMA_Pred$NN_Pred_BIASED[TMA_Pred$TMA == x],na.rm = T))
    Bias_Increase_Factor <- mean(Ages_Diff/predict.lowess(lowess(TMA_Pred$NN_Pred_BIASED[!is.na(TMA_Pred$TMA)], TMA_Pred$TMA[!is.na(TMA_Pred$TMA)] - 
                            TMA_Pred$NN_Pred_BIASED[!is.na(TMA_Pred$TMA)], f = 2/3), newdata = Bias_Adj_Factor_Ages[-1]))
                                      
    TMA_Pred$Bias_Adjustment <- (1 + Bias_Increase_Factor/2) * predict.lowess(lowess(TMA_Pred$NN_Pred_BIASED[!is.na(TMA_Pred$TMA)], TMA_Pred$TMA[!is.na(TMA_Pred$TMA)] - 
                   TMA_Pred$NN_Pred_BIASED[!is.na(TMA_Pred$TMA)], f = 2/3), newdata = TMA_Pred$NN_Pred_BIASED)
    TMA_Pred$NN_Pred <- TMA_Pred$NN_Pred_BIASED + TMA_Pred$Bias_Adjustment 
    
    TMA_Pred$Bias_Adj <- TRUE			
    TMA_Pred$Bias_Adj[TMA_Pred$NN_Pred_BIASED < Bias_Adj_Factor_Ages[1]] <- FALSE
    TMA_Pred$NN_Pred[!TMA_Pred$Bias_Adj] <- TMA_Pred$NN_Pred_BIASED[!TMA_Pred$Bias_Adj]
                                            
    # assign('TMA_Pred', TMA_Pred, pos = 1)  # These assignments to ".GlobalEnv" would be needed if browsePlot() was inside a function
    # assign('Bias_Adj_Factor_Ages', Bias_Adj_Factor_Ages, pos = 1)

    browsePlot('
       plot(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED, xlim = c(0, 16), ylim = c(0, 16),
            xlab = "TMA; Bias corrected points staggered to the right", ylab = "NN Predicted Median", 
            main = paste0("Lowess Bias Corr using ", Bias_Adj_Factor_Ages[2], ":", Bias_Adj_Factor_Ages[length(Bias_Adj_Factor_Ages)], 
                          " NN_Pred, Starting at ", Bias_Adj_Factor_Ages[1], "; No Bias Correction is Black, Bias Corrected is Green"))
       points(TMA_Pred$TMA + 0.25, TMA_Pred$NN_Pred, col = "green")
       abline(0, 1, col = "grey"
    )', file = 'dfasGFDGdf.png')
   
    
    browsePlot('
       plot(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED, xlim = c(0, 16), ylim = c(0, 16),
            xlab = "TMA; Bias corrected points staggered to the right (Lowess line is not moved over.)", ylab = "NN Predicted Median", 
            main = paste0("Lowess Bias Corr using ", Bias_Adj_Factor_Ages[2], ":", Bias_Adj_Factor_Ages[length(Bias_Adj_Factor_Ages)], 
                          " NN_Pred, Starting at ", Bias_Adj_Factor_Ages[1], "; No Bias Correction is Black, Bias Corrected is Green"))
       lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED, smoothing.param = 2/3)
       points(TMA_Pred$TMA + 0.25, TMA_Pred$NN_Pred, col = "green")
       lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred, col = "green", smoothing.param = 2/3)
       lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred, col = "green", smoothing.param = 0.8, lty = 2)
       lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred, col = "green", smoothing.param = 1, lty = 3)
        
       abline(0, 1, col = "grey"
    )', file = 'dfasdf.png')

    Cor_R_squared_RMSE_MAE_SAD_APE(TMA_Pred$TMA, TMA_Pred$NN_Pred, digits = 6)
    "  "
    
    
<br>   
kl;'kl;'										

     







   
    (Bias_Adjustment <- predict(gam(lo(TMA_Pred$TMA - TMA_Pred$NN_Pred, span = 4) ~ TMA_Pred$NN_Pred), newdata = TMA_Pred[, 'NN_Pred', drop = F],  type = "response"))

     dev.new(width = 14, height = 9)
     plot(TMA_Pred); abline(0, 1)
     lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred)
     points(TMA_Pred$TMA, TMA_Pred$NN_Pred + Bias_Adjustment, col = 'red')
     lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred + Bias_Adjustment, col = 'red')
     "  "  





















    
