

dir.create("C:/SIDT/Sable_Test", showWarnings = FALSE, recursive = TRUE)
setwd("C:/SIDT/Sable_Test")


# --- Importing scans into R  ---

# --- Download functions from GitHub ---
sourceFunctionURL <- function (URL,  type = c("function", "script")[1]) {
          '   # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git()   '
          '   # Example to save a function to the working directory:   '
          '   # sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/rgit/master/R/gitAFile.R")   '
          '   # gitAFile("John-R-Wallace-NOAA/JRWToolBox/master/R/browsePlot.R", File = "browsePlot.R")   '
          
          if (!any(installed.packages()[, 1] %in% "httr"))  install.packages("httr") 
          File.ASCII <- tempfile()
          if(type == "function")
            on.exit(file.remove(File.ASCII))
          getTMP <- httr::GET(gsub(' ', '%20', URL))
          
          if(type == "function") {
            write(paste(readLines(textConnection(httr::content(getTMP))), collapse = "\n"), File.ASCII)
            source(File.ASCII)
          } 
          if(type == "script") {
            fileName <- strsplit(URL, "/")[[1]]
            fileName <- rev(fileName)[1]
            fileName <- replaceString(replaceString(fileName, "%2C", ","), "%20", " ")
            write(paste(readLines(textConnection(httr::content(getTMP))), collapse = "\n"), fileName)
          }  
} 
###

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/headTail.R")
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R/Read_OPUS_Spectra.R")

# download.file("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/Sable_Test/Sable_2024_Scans.zip", "Sable_2024_Scans.zip")
# unzip("Sable_2024_Scans.zip")


# download.file("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R_Scripts/iPLS%2C%20NN%20Model%20Batch%20Self%20Call%20Loop.R", "iPLS, NN Model Batch Self Call Loop.R") # !!! File is double spaced !!!

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/FishNIRS/master/R_Scripts/iPLS%2C%20NN%20Model%20Batch%20Self%20Call%20Loop.R", type = "script")  # 'iPLS, NN Model Batch Self Call Loop.R'
dir.create("C:/SIDT/Train_NN_Model", showWarnings = FALSE, recursive = TRUE)
file.copy("C:/SIDT/Sable_Test/iPLS, NN Model Batch Self Call Loop.R", "C:/SIDT/Train_NN_Model", overwrite = TRUE)



Model_Spectra_Meta <- Read_OPUS_Spectra(Spectra_Set = "Sable_Test", fileNames_Sort_Seqment = NULL,
      # Spectra_Path = "2024_Scans/", # Need the last "/" for now
      Spectra_Path = "//nwcfile.nmfs.local/FRAM/Assessments/Aging Lab/NIRS Scanning Data/Otoliths/FT_NIRS_Project/PRD_Production/NWFSC_COMBO/SABL_Sablefish/2024/",
      htmlPlotFolder = "Figures_Sable_Test_2024", Static_Figure = "Sable_Test_2024.png", Meta_Path = NULL, 
      Extra_Meta_Path = "C:/SIDT/Get Otie Info from Data Warehouse/selectSpAgesFramFeb2025.RData", excelSheet = 3, 
      shortNameSegments = c(1, 5), shortNameSuffix = 'Sable_Test', Debug = TRUE)
      
      
headTail(Model_Spectra_Meta, 2, 2, 2, 57)   
       
Table(Model_Spectra_Meta$TMA)

save(Model_Spectra_Meta, file = "Sable_Test_2024_Model_Spectra_Meta_ALL_GOOD_DATA.RData")


# --- Train the NN model and predict the ages ---

# Remove flags for a new run
if(file.exists("C:/SIDT/Train_NN_Model/Run_NN_Model_Flag"))
   file.remove("C:/SIDT/Train_NN_Model/Run_NN_Model_Flag")

if(file.exists("C:/SIDT/Train_NN_Model/Rdm_reps_Iter_Flag.RData"))
   file.remove("C:/SIDT/Train_NN_Model/Rdm_reps_Iter_Flag.RData")
   
if(file.exists("C:/SIDT/Train_NN_Model/.Rprofile"))
   file.remove("C:/SIDT/Train_NN_Model/.Rprofile")   

# The presence of a file named "NN_Verbose_Flag.txt" in will give more NN fitting graphics. A file such as "NN_Verbose_Flag XX.txt" can kept in the directory and renamed to "NN_Verbose_Flag.txt" was wanted.

# Sable_Test_Model_Spectra.sg.iPLS.RData and Figures/Raw Metadata Sex_U vs TMA.png also need to moved out of "C:/SIDT/Train_NN_Model" for a new run.  Save them for going back if desired.


# Make sure the Anaconda TensorFlow environment is downloaded from:

#    \\nwcfile.nmfs.local\FRAM\Assessments\Aging Lab\NIRS Scanning Analysis\Anaconda TensorFlow Environ/tf.7z  # 7zip compression

# and uncompressed into: "C:/m3/envs/" with a resulting 'tf' path of: "C:/m3/envs/tf"  
# Search for "Conda_TF_Eniv" in 'iPLS, NN Model Batch Self Call Loop.R' to change the path as needed.

# A properly setup Nvidia graphics card may be needed (ask IT for help), but a Dell laptop with Intel UHD Graphics 630 integrated into the i7 CPU does work with the GPU mode disabled. 
# Disabling the GPU is faster for these one-dimensioal spectral models anyway. Too much IO otherwise it seems. 
# However, under Windows 11 (vs 10) setting 'disable_gpu' to FALSE may be the only workable solution.
# Search for "tensorflow::set_random_seed" in 'iPLS, NN Model Batch Self Call Loop.R' to set 'disable_gpu'.
# See: https://github.com/John-R-Wallace-NOAA/FishNIRS/tree/main/Setting_up_TensorFlow_Conda_Environment 



# Load 'C:/SIDT/Train_NN_Model/iPLS, NN Model Batch Self Call Loop.R' into an editor which reduces sections between curly brackets like Notepad++ and run the major sections.  

# The 'NpptoR' app will push code from Notepad++ to R. 

# The default in 'iPLS, NN Model Batch Self Call Loop.R' is set to use the "Sable_Test" Spectra_Set.

# See: https://github.com/John-R-Wallace-NOAA/Calling_Rgui_from_Rgui for calling Rgui from Rgui in general.
   







