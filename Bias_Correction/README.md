Bias Correction for Older NN Predicted Ages

Since the NN prediction bias in older ages is consistently in one direction it can be adjusted for.

The bias correction needs to be done not only when a TMA (Traditional Method of Aging) age is given, but also when there is no corresponding TMA for a given age structure. Hence the goal of a bias corrected prediction, from scans and/or metadata without the need for traditional aging, can be achieved. 

A functional form (model) is needed that predicts the difference between TMA and the NN predicted age given a new value of the NN predicted age. Below a LOWESS (locally weighted scatterplot smoothing) non-parametric model is used with R's splinefun() function for prediction, but if an estimate of the additional error added by using a bias correction is wanted, a GAM with a smoother or a parametric functional form may work.

First some functions are needed, see below for what they will be used for:

     predict.lowess <- 
     function(loFit, newdata = loFit$x, method = c("fmm", "periodic", "natural", "monoH.FC", "hyman"), ties = mean) {  
          "  "
          "  # dev.new()   "
          "  # JRWToolBox::plot.lowess(cars$speed, cars$dist)   "
      
          "  # lo.car <- lowess(cars$speed, cars$dist)   "
          "  # If the lowess() x variable is descending, then it has to be called explictly,   " 
          "        i.e. predict.lowess(lo.car, cars$speed)  "
          "  # points(cars$speed, predict.lowess(lo.car), col = 'dodgerblue', pch = 19)   "
      
          "  # x.new <- c(5.3, 6.8, 20.5, 25.2)   "
          "  # points(x.new, predict.lowess(lo.car, x.new), col = 'red', pch = 19)   "
          "  "
       (stats::splinefun(loFit, method = method, ties = ties))(newdata)
    }

    lowess.line <- 
    function(x, y, smoothing.param = 2/3, method = c("fmm", "periodic", "natural", "monoH.FC", "hyman"), ties = mean, ...) {
    
           tmp <- na.omit(cbind(x, y))
           lo <- stats::lowess(tmp[, 1], tmp[, 2], f = smoothing.param)
           predictLo <- predict.lowess(lo, method = c("fmm", "periodic", "natural", "monoH.FC", "hyman"), ties = mean)
           j <- order(lo$x)
           lines(lo$x[j], predictLo[j], ...)
    }

    # Avoid source_url() in the bloatware devtools package.  This code is from sourceFunctionURL() in my rgit package.
    write(readLines(textConnection(httr::content(httr::GET( 
      "https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/browsePlot.R")))), 'browsePlot.R')
    source('browsePlot.R')



Create a simple example dataset with some missing TMA:


    (TMA_Pred <- structure(list(TMA = c(1, 1, 2, 2, 2, 2, 3, 4, 3, 4, 5, 5, 6, 
                6, 7, 7, 8, 8, 9, 9, 10, 10, 10, 11, 12, 12, 12, 13, 13, 13, 
                14, 14, 14, 15, 16, NA, NA, NA, NA), NN_Pred = c(0.590654205607477, 1.43177570093458, 
                1.37570093457944, 2.04859813084112, 2.41308411214953, 2.02056074766355, 
                3.05794392523365, 2.97383177570094, 4.09532710280374, 4.74018691588785, 
                4.20747663551402, 5.94579439252337, 5.74953271028037, 6.87102803738318, 
                7.43177570093458, 6.64672897196262, 8.24485981308411, 7.3196261682243, 
                8.4411214953271, 7.85233644859813, 9.42242990654206, 8.2, 7.96448598130841, 
                9.5, 8.1, 9, 9.31028037383178, 11.581308411215, 11.3, 9.4785046728972, 
                12.1420560747664, 10.9364485981308, 9.92710280373832, 10.2, 10.7, 9.2, 9.9, 12.3, 16.1
                )), row.names = c(NA, -39L), class = "data.frame"))
    " "

Plot the data with a 1-1 line to the bias.  My toolbox function browsePlot() was downloaded above and will used for plotting the figures directly into a browser and saved into a file. Those saved files can also be found in this repo.

    browsePlot('plot(TMA_Pred); abline(0, 1)', file = 'NN_Pred_vs_TMA.png')	
    " "
    
Using predict.lowess() from my toolbox [which uses stats::splinefun()], the difference between TMA and NN_Pred is fitted against NN_Pred using lowess(). The difference upon being added to NN_Pred is plotted with lowess smoothed lines using lowess.line().
		 

    (Bias_Adjustment <- predict.lowess(lowess(TMA_Pred$NN_Pred[!is.na(TMA_Pred$TMA)], TMA_Pred$TMA[!is.na(TMA_Pred$TMA)] - 
                                                TMA_Pred$NN_Pred[!is.na(TMA_Pred$TMA)]), newdata = TMA_Pred$NN_Pred))

     browsePlot('
       plot(TMA_Pred); abline(0, 1)
       lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred)
       points(TMA_Pred$TMA, TMA_Pred$NN_Pred + Bias_Adjustment, col = "green")
       lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred + Bias_Adjustment, col = "green")  # Note the need to get the quoting correct when using browsePlot: ' "" '
     ')


     
     Bias_Adj_Factor_Ages <- c(8, 9:15)
     Ages_Diff <- Bias_Adj_Factor_Ages[-1] - apply(matrix(Bias_Adj_Factor_Ages[-1], ncol = 1), 1, function(x) mean(TMA_Pred$NN_Pred[TMA_Pred$TMA == x],na.rm = T))
     Bias_Increase_Factor <- mean(Ages_Diff/predict.lowess(lowess(TMA_Pred$NN_Pred[!is.na(TMA_Pred$TMA)], TMA_Pred$TMA[!is.na(TMA_Pred$TMA)] - 
                             TMA_Pred$NN_Pred[!is.na(TMA_Pred$TMA)], f = 2/3), newdata = Bias_Adj_Factor_Ages[-1]))
											 
     TMA_Pred$NN_Pred_BIASED <- TMA_Pred$NN_Pred 									 
     TMA_Pred$Bias_Adjustment <- Bias_Increase_Factor * predict.lowess(lowess(TMA_Pred$NN_Pred[!is.na(TMA_Pred$TMA)], TMA_Pred$TMA[!is.na(TMA_Pred$TMA)] - 
			        TMA_Pred$NN_Pred[!is.na(TMA_Pred$TMA)], f = 2/3), newdata = TMA_Pred$NN_Pred)
     TMA_Pred$NN_Pred <- TMA_Pred$NN_Pred_BIASED + TMA_Pred$Bias_Adjustment
											 
     # assign('TMA_Pred', TMA_Pred, pos = 1)
     # assign('Bias_Adj_Factor_Ages_Vec', Bias_Adj_Factor_Ages, pos = 1)
    
     browsePlot('
        plot(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED,
    	     xlab = "TMA; Bias corrected points staggered to the right (Lowess line is not moved over.)", ylab = "NN Predicted Median", 
             main = paste0("Lowess Bias Corr using ", Bias_Adj_Factor_Ages_Vec[2], ":", Bias_Adj_Factor_Ages_Vec[length(Bias_Adj_Factor_Ages_Vec)], 
                           " NN_Pred, Starting at ", Bias_Adj_Factor_Ages_Vec[1], "; No Bias Correction is Black, Bias Corrected is Green"))
    	lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred_BIASED, smoothing.param = 2/3)
        points(TMA_Pred$TMA + 0.25, TMA_Pred$NN_Pred, col = "green")
    	lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred, col = "green", smoothing.param = 2/3)
         
        abline(0, 1, col = "grey"
     )', file = 'dfasdf.png')
										

     







   
    (Bias_Adjustment <- predict(gam(lo(TMA_Pred$TMA - TMA_Pred$NN_Pred, span = 4) ~ TMA_Pred$NN_Pred), newdata = TMA_Pred[, 'NN_Pred', drop = F],  type = "response"))

     dev.new(width = 14, height = 9)
     plot(TMA_Pred); abline(0, 1)
     lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred)
     points(TMA_Pred$TMA, TMA_Pred$NN_Pred + Bias_Adjustment, col = 'red')
     lowess.line(TMA_Pred$TMA, TMA_Pred$NN_Pred + Bias_Adjustment, col = 'red')
     "  "  





















    
