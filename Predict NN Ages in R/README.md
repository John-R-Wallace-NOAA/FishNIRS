<h3 >Predicting Neural Net Ages in R under Windows </h3>

<h4> What's Missing </h4>
  
- A TensorFlow Anaconda environment necessary for the R package 'tensorflow' to work. 
- A NN model (saved as a '.RData' binary file) is missing from the folder 'FCNN Model' since it is too large to be loaded into GitHub.
  
<h4> Making it Work </h4>

- Copy this repo's files and folder structure to a Windows 10 or 11 machine with R ver 3.0 or greater.
- Put the Anaconda environment into an appropriate folder on the Window's machine (i.e. "C:/Users/John.Wallace/AppData/Local/miniconda3/envs/tf") and change the 'Conda_TF_Eniv' path in 'Predict_NN_Age_Script.R' (around line 15) to reflect where it is put.
- Put the R NN model (as a '.Rdata' file) into the 'FCNN Model' directory and change the name in 'NN_Model' relative path (around line 90) to reflect the model's name.
- Put Bruker spectra files obtained from OPUS into the 'New_Scans' folder. (e.g. PACIFIC_HAKE_BMS201906206C_1191_OD1.0)
- Double click 'Predict New Ages.bat' to run the batch file in Windows. The batch file uses 'Rscript.exe' to run the code in 'Predict_NN_Age_Script.R'. Likewise you can source() 'Predict_NN_Age_Script.R' into R or copy paste the code into R's Command Window.
- After running 'Predict_NN_Age_Script.R', the 'New_Scans' folder will contain four new items:
  - 'NN Predicted Ages <date & time>.csv' will contain the NN predicted ages with corresponging lower (0.025) and upper (0.975) quantiles based on the random replicates (10-20, say) of the 10 full 'k-fold' models . The quantiles are a reflection of the NN models precision based on the random replicates of the full 10-fold models, not the accuracy to a Traditional Method of Aging (TMA) age.
  - The plotly (R package) HTML folder for the spectra figure ('Spectra Figure for New Ages') seen while running the script.
  - A similar HTML folder for a figure of the new ages with quantile precision bars where the order is derived from the OPUS file names.
  - A HTML folder for a figure of the predicted ages, with quantile precision bars, where ages have been sorted.