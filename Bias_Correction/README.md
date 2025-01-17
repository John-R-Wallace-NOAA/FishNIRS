<h3> Bias Correction for Older NN Predicted Ages </h3>

<br>

Since the NN prediction bias in older ages is consistently in one direction it can be adjusted for.

The bias correction needs to be done not only when a TMA (Traditional Method of Aging) age is given, but also when there is no corresponding TMA for a given age structure. Hence the goal of a bias corrected prediction, from scans and/or metadata without the need for traditional aging, can be achieved. 

A functional form (model) is needed that predicts the difference between TMA and the NN predicted age given a new value of the NN predicted age. Below a LOWESS (locally weighted scatterplot smoothing) non-parametric model is used with R's splinefun() function for prediction. mgcv R package's gam() with s() smoother is also looked at for standard errors of bhe bias adjustment.

<br>
First some functions are needed:
  
    File.ASCII <- tempfile()
    for(Func in c("predict.lowess.R", "lowess.line.R", "browsePlot.R", "headTail.R")) {
       download.file(paste0("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/", Func), File.ASCII)
       source(File.ASCII)
    }

    for(Func in c("agreementFigure.R", "Cor_R_squared_RMSE_MAE_SAD_APE.R")) {
        download.file(paste0("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/", Func), File.ASCII)
        source(File.ASCII)
    }
    nul <- file.remove(File.ASCII); rm(File.ASCII, Func, nul)
    "  "
    
<br>
Download an example dataset with some missing TMA in the last 10 rows:

     download.file("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/main/Bias_Correction/TMA_Pred.RData",
                   "TMA_Pred.RData")
     load("TMA_Pred.RData")
     headTail(TMA_Pred, 3, 12)
     " "
    
<br>

Plot the data to create this [agreement figure](https://github.com/John-R-Wallace-NOAA/FishNIRS/tree/main/Bias_Correction/NN_Pred_vs_TMA_Agreement_Fig.png). 
My toolbox function browsePlot() was downloaded above and will be used for viewing the figures directly in browser and (optionally) saved into a file.

    browsePlot('agreementFigure(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED, xlim = c(0, 18.5), ylim = c(0, 18.5), 
               main = "NN Predicted Ages with Bias at Older Ages")', file = 'NN_Pred_vs_TMA_Agreement_Fig.png')
     " "
     
<br>   
The difference between TMA and NN_Pred is fitted against the biased NN_Pred using lowess(). A prediction of the bias adjustment given a new value of NN_PRed is done with predict.lowess() from my toolbox [which uses stats::splinefun()]. The resulting bias adjustment is added to NN_Pred and plotted along with lowess smoothed lines using lowess.line() with 3 different values of the smoothing parameter (all in green). The original biased data is plotted in black and only one smoothed line for the biased data is shown. 
		 

     (Bias_Adjustment <- predict.lowess(lowess(TMA_Pred$NN_Pred_BIASED[!is.na(TMA_Pred$TMA)], 
          TMA_Pred$TMA[!is.na(TMA_Pred$TMA)] - TMA_Pred$NN_Pred_BIASED[!is.na(TMA_Pred$TMA)], f = 2/3), 
          newdata = TMA_Pred$NN_Pred_BIASED))[1:10]

    # Note the need to use double quotes inside the plotting code when using browsePlot()
     browsePlot('
       plot(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED, xlim = c(0, 16), ylim = c(0, 16)); abline(0, 1, col = "grey")
       lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED)
       points(TMA_Pred$TMA + 0.15, TMA_Pred$NN_Pred_BIASED + Bias_Adjustment, col = "green")
       lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED + Bias_Adjustment, col = "green", smoothing.param = 2/3) 
       lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED + Bias_Adjustment, col = "green", smoothing.param = 0.8, lty = 2)
       lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED + Bias_Adjustment, col = "green", smoothing.param = 1, lty = 3)
     ', file = 'NN_Pred_Bias_Adj_Lowess_vs_TMA.png')
    
     
     Cor_R_squared_RMSE_MAE_SAD_APE(TMA_Pred$TMA, round(TMA_Pred$NN_Pred_BIASED + Bias_Adjustment), digits = 4)
     "  "
     
<br>

The stats for the [lowess biased adjusted NN_Pred plotted against TMA](https://github.com/John-R-Wallace-NOAA/FishNIRS/tree/main/Bias_Correction/NN_Pred_Bias_Adj_Lowess_vs_TMA.png) where the NN_pred is rounded to the newest integer (no Delta added) are:
    
      Correlation R_squared   RMSE    MAE  SAD   APE    N
           0.9649     0.931 0.7732 0.4725 2589 5.699 5479

<br>

The lowess based adjustment above does not move the older ages sufficiently (perhaps it will for other data), so a bias adjustment factor was implemented. First, older ages where bias still existed, and where there was sufficient data, is defined (ages 9-15 in the example below). For each of these TMA ages, the difference between each age and the average of the biased NN predicted ages which have that TMA value was calculated (Ages_Diff below). Next the those differences were divided by the lowess predicted ages at each TMA value, and the average taken (Bias_Increase_Factor below). Note that the "Bias_Increase_Factor" cannot depend on TMA, since predictions need to made when TMA is unknown, hence the need for a single scalar value. One plus the Bias_Increase_Factor divided by two is then multiplied by the lowess predictions looked at above and added to biased NN predictions to create less biased estimates (NN_Pred below). (Note that the code below repeats the lowess predictions and does not depend on the code looking only at the lowess predictions above.) 
[A bias adjustmnent figure](https://github.com/John-R-Wallace-NOAA/FishNIRS/tree/main/Bias_Correction/NN_Pred_Bias_Adj_Lowess_Factor_vs_TMA.png) and an 
[agreement figure](https://github.com/John-R-Wallace-NOAA/FishNIRS/tree/main/Bias_Correction/NN_Pred_Bias_Corrected_vs_TMA_Agreement_Fig.png) are created. Smoothers are fickle and caution is needed when using them, looking past the smoothers and looking only at the data should also be done.  One the smoothers being right on the 1-1 line for older ages, as seen in this bias adjustment figure, is atypical. 

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
            xlab = "TMA; Bias corrected points staggered to the right (Lowess line is not moved over.)", ylab = "NN Predicted Median", 
            main = paste0("Lowess Bias Corr using ", Bias_Adj_Factor_Ages[2], ":", Bias_Adj_Factor_Ages[length(Bias_Adj_Factor_Ages)], 
                          " NN_Pred, Starting at ", Bias_Adj_Factor_Ages[1], "; No Bias Correction is Black, Bias Corrected is Green"))
       abline(0, 1, col = "grey")
       lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED, smoothing.param = 2/3)
       points(TMA_Pred$TMA + 0.25, TMA_Pred$NN_Pred, col = "green")
       lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred, col = "green", lwd = 1.2, smoothing.param = 0.1)
       lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred, col = "green", lwd = 1.2, smoothing.param = 1/3, lty = 2)
       lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred, col = "green", lwd = 1.2, smoothing.param = 2/3, lty = 3)
    ', file = 'NN_Pred_Bias_Adj_Lowess_Factor_vs_TMA.png')

    Cor_R_squared_RMSE_MAE_SAD_APE(TMA_Pred$TMA, round(TMA_Pred$NN_Pred), digits = 4)

    headTail(TMA_Pred, 3, 12)
    
    browsePlot('agreementFigure(TMA_Pred$TMA, TMA_Pred$NN_Pred, xlim = c(0, 18.5), ylim = c(0, 18.5), 
               main = "NN Predicted Ages with Corrected Bias at Older Ages")', file = 'NN_Pred_Bias_Corrected_vs_TMA_Agreement_Fig.png')
    "  "


    
<br>   

If a standard error for the bias adjustment is wanted, then the mgcv R package could be 
[tried](https://github.com/John-R-Wallace-NOAA/FishNIRS/tree/main/Bias_Correction/mgcv_gam_bias_adjustmet.png) :

    # mgcv R package's gam() with mgcv's s() smoother  (Trevor Hastie's 'gam' package also has a s() smoother.)  
    
    if(!any(installed.packages()[, 1] %in% "mgcv"))  install.packages('mgcv')  
    library(mgcv)
    
    Bias_Adjustment <- predict(mgcv::gam(TMA - NN_Pred_BIASED ~ s(NN_Pred_BIASED), data = TMA_Pred, 
            newdata = TMA_Pred[, 'NN_Pred_BIASED', drop = F],  type = "response"), se.fit = TRUE)

    browsePlot('
      plot(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED); abline(0, 1, col = "grey")
      lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED)
      points(TMA_Pred$TMA + 0.2, TMA_Pred$NN_Pred_BIASED + Bias_Adjustment$fit, col = "red")
      lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED + Bias_Adjustment$fit, col = "red")
    ', file = 'mgcv_gam_bias_adjustmet.png')
     
     headTail(data.frame(fit = Bias_Adjustment$fit, se.fit = Bias_Adjustment$se.fit), 3, 12)
     
    #          fit     se.fit
    #  1  0.2228624 0.02133048
    #  2  0.1223436 0.03055174
    #  3  0.1698469 0.02567175
    #  4  0.1789458 0.02482816
    #  5  0.3300349 0.01944323
    #  6  0.3573969 0.02153072
    #  7  0.1926023 0.02363083
    #  8  0.3055291 0.02017020
    #  9  0.1708344 0.02557855
    #  10 0.1837648 0.02439579
     "  "  





















    
