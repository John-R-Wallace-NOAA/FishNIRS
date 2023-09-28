<h3 >Predicting Neural Net Ages in R under Windows

<h4> What's missing
  
The Anaconda environment necessary for the R package 'tensorflow' to work is missing from the folder 'FCNN Model' since it is too large to be loaded into GitHub. Contact the author for a copy (John.Wallce@noaa.gov).

<h4> Making it work

- Copy this repo's files and folder structure to a Windows 10 or 11 machine with R ver 3.0 or greater.
- Put the Anaconda environment obtained from the author into the 'FCNN Model' directory.
- Put Bruker spectra files obtained from OPUS into the "New_Scans" folder. (e.g. PACIFIC_HAKE_BMS201906206C_1191_OD1.0)
- Double click 'Predict New Ages.bat' to run the batch file in Windows. The batch file uses 'Rscript.exe' to run 'Predict_NN_Age_Script.R".
- Likewise you can source() 'Predict_NN_Age_Script.R' into R or copy paste the code into R's Command Window.
- 


<br/> 
