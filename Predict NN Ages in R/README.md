<h3 >Predicting Neural Net Ages in R under Windows </h3>

<h4> What's Missing (Contact the author for either or both, John.Wallace@noaa.gov)</h4>
  
- A TensorFlow Anaconda environment necessary for the R package 'tensorflow' to work. See the folder 'Setting_up_TensorFlow_Conda_Environment' in this repo. The MiniConda environment is ~4.5 GB in total and ~1.28 Gb compressed with 7-Zip.
- An example NN model (saved as a '.RData' binary file) is missing from the folder 'FCNN Model' since it is too large to be loaded into GitHub.
  
<h4> Making it Work </h4>

- A Github Personal Access Token (PAT) will be needed for extended use. This is added to the R session environment with
  
        Sys.setenv(GITHUB_PAT = '**********')

  Search the Web for how to get a GitHub PAT, if help is needed.

- Copy this repo's files and folder structure to a Windows 10 or 11 machine running R ver 3.0 or greater.
  - Either always start R from where the files and folder structure was created or change the working directory on line 7 in 'Predict_NN_Age_Script.R' (and uncomment).
  - Running the batch file (see below) in the normal way within the folder structure, forces one to be in the correct place.
- Put the Anaconda environment (uncompressed) into an appropriate folder on the Window's machine (i.e. "C:/m3/envs/tf") and change the 'Conda_TF_Eniv' path in 'Predict_NN_Age_Script.R' (around line 15) to reflect where it is located.  ('m3' is short for miniconda3.)
- Put an R Keras NN model (as a '.Rdata' file) into the 'FCNN Model' directory and change the name in 'NN_Model' relative path (around line 94) to reflect the model's name.
- After scanning new material comparable to what the NN model was trained on (e.g. otloliths, or gonad material, from the same fish species), put the Bruker spectra files from OPUS into the 'New_Scans' folder. (e.g. PACIFIC_HAKE_BMS201906206C_1191_OD1.0). For testing, there are 'old' spectra files used to train the NN model currently in 'New_scans'.  (Do not put anything else in the 'New_Scans' folder except spectra files.)
- Double click 'Predict New Ages.bat' to run the batch file in Windows. The batch file uses 'Rscript.exe' to run the code in 'Predict_NN_Age_Script.R'. Likewise you can source() 'Predict_NN_Age_Script.R' into R or copy and paste the code into R's Command Window. 'Predict_NN_Age_Script.R' is a wrapper for 'Predict_NN_Age.R' which is in the 'R' folder of this repo.
- After running 'Predict_NN_Age_Script.R', the 'Predicted_Ages' folder will contain four new items:
  - 'NN Predicted Ages <date & time>.csv' will contain the NN predicted ages with corresponging lower (0.025) and upper (0.975) quantiles based on the random replicates (10-20, say) of 10 full 'k-fold' models. The quantiles are a reflection of the NN models precision based on the random replicates of the full 10-fold models, not the accuracy to a Traditional Method of Aging (TMA) age.
  - The plotly (R package) HTML folder for the spectra figure ('Spectra Figure for New Ages') seen while running the script.
  - A similar HTML folder for a figure of the new ages with quantile precision bars where the order on the x-axis is derived from the OPUS file names.
  - A HTML folder for a figure of the predicted ages, with quantile precision bars, where ages have been sorted on the x-axis.
- Note that if the file and folders are overwritten in the 'Predicted_Ages' folder, the files within the folders will have an updated time of modification, but the folders will retain the time they were first created.

- If you get this error: < Error in `[.data.frame`(data.frame(prospectr::savitzkyGolay(newScans.RAW, : undefined columns selected >, or you know that the spectra scan(s) you are currently working with do not have the same frequencies as the NN model expects, add the file 'FCNN\\*****_AAA_Correct_Scan_Freq' matching the current NN model to your scans in the 'New_Scans' folder. An interpolation will be done on those scans that do not match those used in the NN model. For example, if the NN model is for Pacific hake then use 'FCNN\PACIFIC_HAKE_AAA_Correct_Scan_Freq' to match the model being used.
  - The opus_read() function from the 'opusreader' R package (https://github.com/pierreroudier/opusreader) does the interpolation. opusreader() uses the wavebands of the very first file read in as a reference for linear interpolation. (Hence the 'AAA' in the correct scan frequency file.)
  - For testing, the folder 'Fix Scan Freq' contains an early 2019 Pacific Hake spectra file which has different wavebands then the majority of the scans and the NN model. Copying that spectra file to 'New_Scans' will show the error mentioned above for an interactive session or crash a batch file run. Adding the 'FCNN\\PACIFIC_HAKE_AAA_Correct_Scan_Freq' file to the 'New_Scans' folder will fix the error.
