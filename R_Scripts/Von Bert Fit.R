
setwd("C:/SIDT/Chilipepper/Fit Von Bert & Double Reads")


load("C:\\SIDT\\CLPR_Combo_1985__2024\\CLPR_SWFSC_1985__2024_CA_OR_Comm_Model_Spectra_Meta_ALL_GOOD_DATA.RData")
# load("C:\\SIDT\\CLPR_Combo_2010_2014\\CLPR_SWFSC_2010_2014_2015_2016_Model_Spectra_Meta_ALL_GOOD_DATA.RData")


change(Model_Spectra_Meta)
browsePlot('
  par(mfrow = c(2,1))
  plot(jitter(TMA[Sex == "F"]), Length_cm[Sex == "F"], main = "Female")
  lines(sort(TMA[Sex == "F"]), 48.5876683  * (1 - exp(-0.1945 * sort(TMA[Sex == "F"]) - 0.6659983)))
  
  plot(jitter(TMA[Sex == "M"]), Length_cm[Sex == "M"], col = "green", pch = 4, main = "Male")
  lines(sort(TMA[Sex == "M"]), 33.3796543  * (1 - exp(-0.3561786 * sort(TMA[Sex == "M"]) - 0.3796227)), col = "green")
')



change(Model_Spectra_Meta)
browsePlot('
  par(mfrow = c(2,1))
  plot(jitter(TMA[Sex == "F"]), Length_cm[Sex == "F"], main = "Female")
  lines(sort(TMA[Sex == "F"]), 48.5876683  * (1 - exp(-0.1945 * (sort(TMA[Sex == "F"]) - 0.6659983))))
  
  plot(jitter(TMA[Sex == "M"]), Length_cm[Sex == "M"], col = "green", pch = 4, main = "Male")
  lines(sort(TMA[Sex == "M"]), 33.3796543  * (1 - exp(-0.3561786 * (sort(TMA[Sex == "M"]) - 0.3796227))), col = "green")
')


browsePlot('
  par(mfrow = c(3,1))
  plot(jitter(TMA[Sex == "F"]), Length_cm[Sex == "F"], main = "Female")
  lines(sort(TMA[Sex == "F"]), 40.7  * (1 - exp(-0.14 * sort(TMA[Sex == "F"]) - 0.6)))
  
  plot(jitter(TMA[Sex == "M"]), Length_cm[Sex == "M"], col = "green", pch = 4, main = "Male")
  lines(sort(TMA[Sex == "M"]), 40.7  * (1 - exp(-0.14 * sort(TMA[Sex == "M"]) - 0.6)), col = "green")
  
  plot(jitter(TMA), Length_cm, col = "green", pch = 4, main = "All")
  lines(sort(TMA), 40.7  * (1 - exp(-0.14 * sort(TMA) - 0.6)), col = "green")
')



browsePlot('
  par(mfrow = c(3,1))
  plot(jitter(TMA[Sex == "F"]), Length_cm[Sex == "F"], main = "Female")
  lines(sort(TMA[Sex == "F"]), 40.7  * (1 - exp(-0.14 * (sort(TMA[Sex == "F"]) + 0.6))))
  
  plot(jitter(TMA[Sex == "M"]), Length_cm[Sex == "M"], col = "green", pch = 4, main = "Male")
  lines(sort(TMA[Sex == "M"]), 40.7  * (1 - exp(-0.14 * (sort(TMA[Sex == "M"]) + 0.6))), col = "green")
  
  plot(jitter(TMA), Length_cm, col = "green", pch = 4, main = "All")
  lines(sort(TMA), 40.7  * (1 - exp(-0.14 * (sort(TMA) + 0.6))), col = "green")
')



browsePlot('
  par(mfrow = c(3,1))
  plot(jitter(TMA[Sex == "F"]), Length_cm[Sex == "F"], main = "Female")
  lines(sort(TMA[Sex == "F"]), 40.7  * (1 - exp(-0.14 * sort(TMA[Sex == "F"]) - 0.6)))
  
  plot(jitter(TMA[Sex == "M"]), Length_cm[Sex == "M"], col = "green", pch = 4, main = "Male")
  lines(sort(TMA[Sex == "M"]), 40.7  * (1 - exp(-0.14 * sort(TMA[Sex == "M"]) - 0.6)), col = "green")
  
  plot(jitter(TMA), Length_cm, col = "green", pch = 4, main = "All")
  lines(sort(TMA), 40.7  * (1 - exp(-0.14 * sort(TMA) - 0.6)), col = "green")
')








browsePlot('
  par(mfrow = c(2,1))
  plot(jitter(TMA[Sex == "F"]), Length_cm[Sex == "F"], main = "Female")
  lines(sort(TMA[Sex == "F"]), 48.5876683  * (1 - exp(-0.1945 * (sort(TMA[Sex == "F"]) + 0.6659983))))
  
  plot(jitter(TMA[Sex == "M"]), Length_cm[Sex == "M"], col = "green", pch = 4, main = "Male")
  lines(sort(TMA[Sex == "M"]), 33.3796543  * (1 - exp(-0.3561786 * (sort(TMA[Sex == "M"]) + 0.3796227))), col = "green")
')


browsePlot('
  par(mfrow = c(2,1))
  plot(jitter(TMA[Sex == "F"]), Length_cm[Sex == "F"], main = "Female")
  lines(sort(TMA[Sex == "F"]), 40.7  * (1 - exp(-0.14 * (sort(TMA[Sex == "F"]) + 0.6))))
  
  plot(jitter(TMA[Sex == "M"]), Length_cm[Sex == "M"], col = "green", pch = 4, main = "Male")
  lines(sort(TMA[Sex == "M"]), 40.7  * (1 - exp(-0.14 * (sort(TMA[Sex == "M"]) + 0.6))), col = "green")
')





Lt = L∞ * (1 - e^(-k*(t-t0)))


Female:
Linf: 48.5876683
k: 0.1945
t0: -0.6659983

Male:
Linf: 33.3796543
k: 0.3561786
t0: -0.3796227


Chili_F <- data.frame(age = TMA[Sex == "F"], Len = Length_cm[Sex == "F"])

vbTypical <- Len~Linf*(1-exp(-K*(age-t0)))
fitTypical <- nls(vbTypical, data=Chili_F, start = list(Linf = 48.5876683, K = 0.1945, t0 = -0.6659983))

Nonlinear regression model
  model: Len ~ Linf * (1 - exp(-K * (age - t0)))
   data: Chili_F
   Linf       K      t0 
48.4386  0.2064 -1.0882 
 residual sum-of-squares: 10984

Number of iterations to convergence: 3 
Achieved convergence tolerance: 5.666e-07

summary(fitTypical)$coeff
       Estimate  Std. Error   t value      Pr(>|t|)
Linf 48.4385574 0.157713360 307.13034  0.000000e+00
K     0.2063962 0.002768034  74.56418  0.000000e+00
t0   -1.0881519 0.029115079 -37.37417 1.309336e-238



Chili_M <- data.frame(age = TMA[Sex == "M"], Len = Length_cm[Sex == "M"])

vbTypical <- Len~Linf*(1-exp(-K*(age-t0)))
fitTypical <- nls(vbTypical, data = Chili_M, start = list(Linf = 48.5876683, K = 0.1945, t0 = -0.6659983))
summary(fitTypical)$coeff

      Estimate  Std. Error   t value      Pr(>|t|)
Linf 35.6959143 0.113754982 313.79649  0.000000e+00
K     0.3156431 0.005532286  57.05472  0.000000e+00
t0   -1.0573124 0.033896044 -31.19280 1.612406e-172



browsePlot('
  par(mfrow = c(2,1))
  plot(jitter(TMA[Sex == "F"]), Length_cm[Sex == "F"], main = "Female")
  lines(sort(TMA[Sex == "F"]), 48.5876683  * (1 - exp(-0.1945 * sort(TMA[Sex == "F"]) - 0.6659983)))
  lines(sort(TMA[Sex == "F"]), 48.4385574  * (1 - exp(-0.2063962 * sort(TMA[Sex == "F"]) - 1.0881519)), lty = 2)
  lines(sort(TMA[Sex == "F"]), 48.4385574  * (1 - exp(-0.2063962 * sort(TMA[Sex == "F"]) - 0.28)), col = "dodgerblue", lty = 3, lwd = 1.2)
  
  plot(jitter(TMA[Sex == "M"]), Length_cm[Sex == "M"], col = "green", pch = 4, main = "Male")
  lines(sort(TMA[Sex == "M"]), 33.3796543  * (1 - exp(-0.3561786 * sort(TMA[Sex == "M"]) - 0.3796227)), col = "green")
  lines(sort(TMA[Sex == "M"]), 35.6959143  * (1 - exp(-0.3156431 * sort(TMA[Sex == "M"]) - 1.0573124)), col = "green", lty = 2)
  lines(sort(TMA[Sex == "M"]), 35.6959143  * (1 - exp(-0.3156431 * sort(TMA[Sex == "M"]) - 0.3796227)), col = "red", lty = 3, lwd = 1.2)
', file = "Chilipepper Age Length von Bert.png")








load("C:\\SIDT\\CLPR_Combo_2010_2014\\Str_Weight_as.logical(Sex_M)odel\\Predicted_Ages\\NN Predicted Ages, 21 Feb 2025.RData")

change(New_Ages)
browsePlot('
  par(mfrow = c(2,1))
  plot(jitter(Pred_Age_Bias_Corr_plus_Delta_rounded[as.logical(Sex_F)]), Length_cm[as.logical(Sex_F)], main = "Female")
  lines(sort(Pred_Age_Bias_Corr_plus_Delta_rounded[as.logical(Sex_F)]), 48.5876683  * (1 - exp(-0.1945 * sort(Pred_Age_Bias_Corr_plus_Delta_rounded[as.logical(Sex_F)]) - 0.6659983)))
  
  plot(jitter(Pred_Age_Bias_Corr_plus_Delta_rounded[as.logical(Sex_M)]), Length_cm[as.logical(Sex_M)], col = "green", pch = 4, main = "Male")
  lines(sort(Pred_Age_Bias_Corr_plus_Delta_rounded[as.logical(Sex_M)]), 33.3796543  * (1 - exp(-0.3561786 * sort(Pred_Age_Bias_Corr_plus_Delta_rounded[as.logical(Sex_M)]) - 0.3796227)), col = "green")
')
   


# -------------- Females only -------------------------


change(Model_Spectra_Meta)

browsePlot('
  par(mfrow = c(4,4))
  for(i in sort(unique(sample_year))) {
     plot(jitter(TMA[Sex == "F"]), Length_cm[Sex == "F"], main = i)
     lines(sort(TMA[Sex == "F"]), 48.5876683  * (1 - exp(-0.1945 * sort(TMA[Sex == "F"])) - 0.6659983))
  }
  
', file = "Chilipepper Age Length von Bert, Females Only by Year Group.png")




browsePlot('
  par(mfrow = c(4,4))
  for(i in sort(unique(sample_year))) {
     plot(jitter(TMA[Sex == "F"]), Length_cm[Sex == "F"], main = i)
     lines(sort(TMA[Sex == "F"]), 48.5876683  * (1 - (exp(-0.1945 * sort(TMA[Sex == "F"]) - 0.6659983))))
  }
  
', file = "Chilipepper Age Length von Bert, Females Only by Year Group.png")



# ------------------------------------------



Chili_F <- data.frame(age = TMA[Sex == "F"], Len = Length_cm[Sex == "F"])

vbTypical <- Len~Linf*(1-exp(-K*age+t0))
fitTypical <- nls(vbTypical, data=Chili_F, start = list(Linf = 48.5876683, K = 0.1945, t0 = -0.6659983))
summary(fitTypical)$coeff

       Estimate  Std. Error   t value Pr(>|t|)
Linf 48.7030996 0.156198260 311.80309        0
K     0.2037147 0.002702931  75.36808        0
t0   -0.2244905 0.003609395  62.19617        0





Chili_M <- data.frame(age = TMA[Sex == "M"], Len = Length_cm[Sex == "M"])

vbTypical <- Len~Linf*(1-exp(-K*age+t0))
fitTypical <- nls(vbTypical, data = Chili_M, start = list(Linf = 48.5876683, K = 0.1945, t0 = -0.6659983))
summary(fitTypical)$coeff

      Estimate  Std. Error   t value Pr(>|t|)
Linf 35.6674871 0.112558762 316.87881        0
K     0.3165704 0.005529543  57.25074        0
t0   -0.3335626 0.005954828  56.01549        0





change(Model_Spectra_Meta)
browsePlot('
  par(mfrow = c(2,1))
  plot(jitter(TMA[Sex == "F"]), Length_cm[Sex == "F"], main = "Female")
  lines(sort(TMA[Sex == "F"]), 48.7031 * (1 - exp(-0.2037 * sort(TMA[Sex == "F"]) - 0.2245)))
  
  plot(jitter(TMA[Sex == "M"]), Length_cm[Sex == "M"], col = "green", pch = 4, main = "Male")
  lines(sort(TMA[Sex == "M"]), 35.667  * (1 - exp(-0.3166 * sort(TMA[Sex == "M"]) - 0.33356)), col = "green")
', file = "Chilipepper Age Length von Bert using nls.png")


















 
