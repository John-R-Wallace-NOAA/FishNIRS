Bias Correction for Older NN Predicted Ages

Since the NN prediction bias in older ages is consistently in one direction it can be adjusted for.

The bias correction needs to be done not only when a TMA (Traditional Method of Aging) age is given, but also when there is no corresponding TMA for a given age structure. Hence the goal of a bias corrected prediction, from scans and/or metadata without the need for traditional aging, can be achieved. 

A functional form (model) is needed that predicts the difference between TMA and the NN predicted age given a new value of the NN predicted age. Below a LOWESS (locally weighted scatterplot smoothing) non-parametric model is used with R's splinefun() function for prediction, but if an estimate of the additional error added by using a bias correction is wanted, a GAM with a smoother or a parametric functional form may work.


Create a simple example dataset:


    TMA_Pred <- structure(list(TMA = c(1, 1, 2, 2, 2, 2, 3, 4, 3, 4, 5, 5, 6, 
                6, 7, 7, 8, 8, 9, 9, 10, 10, 10, 11, 12, 12, 12, 13, 13, 13, 
                14, 14, 14, 15, 16), NN_Pred = c(0.590654205607477, 1.43177570093458, 
                1.37570093457944, 2.04859813084112, 2.41308411214953, 2.02056074766355, 
                3.05794392523365, 2.97383177570094, 4.09532710280374, 4.74018691588785, 
                4.20747663551402, 5.94579439252337, 5.74953271028037, 6.87102803738318, 
                7.43177570093458, 6.64672897196262, 8.24485981308411, 7.3196261682243, 
                8.4411214953271, 7.85233644859813, 9.42242990654206, 8.2, 7.96448598130841, 
                9.5, 8.1, 9, 9.31028037383178, 11.581308411215, 11.3, 9.4785046728972, 
                12.1420560747664, 10.9364485981308, 9.92710280373832, 10.2, 10.7
                )), row.names = c(NA, -35L), class = "data.frame")
    


Plot the data with a 1-1 line to see where the bias starts and see where the unbiased data is not 

    plot(TMA_Pred); abline(0, 1)

    

