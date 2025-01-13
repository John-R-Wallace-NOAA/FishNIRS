Bias Correction for Older NN Predicted Ages

Since the NN prediction bias in older ages is consistently in one direction it can be adjusted for.

The bias correction needs to be done not only when a TMA (Tradional Method of Aging) age is given but also when there is no corresponding TMA for a given age structure. Hence the goal of a new bias corrected prediction, from scans and/or metadata without the need for tradional aging, can be acheived. 

A functional form (model) is needed that predicts the difference between TMA and the NN predicted age given a new value of the NN predicted age. Below a LOWESS (locally weighted scatterplot smoothing) non-parametric model is used with R's splinefun() function for prediction, but if an esitmate of additional error added with this bias correction is wanted a GAM with a smoother or a parametic functional form may work.
