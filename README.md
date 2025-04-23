
An extended abstract for the talk given on this work at the 2023 Fourth Research Workshop on the Rapid Estimation of Fish Age Using Fourier Transform Near Infrared Spectroscopy is here:

Proceedings of the Fourth Research Workshop on the Rapid Estimation of Fish Age Using Fourier Transform Near Infrared Spectroscopy. 2024, Editors: Matta, M. E. (editors); Helser, T. E. AFSC processed report; 2024-01. DOI: [https://doi.org/10.25923/g30w-8268.](https://doi.org/10.25923/g30w-8268) Follow this link for the presentation [slide deck.](https://docs.google.com/presentation/d/e/2PACX-1vSQFW9qVyJOABbeKCsdxHjaz9_HRYZpabRwhBh6GgCZFqnMEM5NzKENF7PbNIMnKw/pub?start=true&loop=false&delayms=60000&slide=id.p1)  

The Groundfish Subcommittee, a part of the Scientific and Statistical Committee (SSC) of the Pacific Fishery Management Council, reveiwed these methods in early [Oct 2024.](https://docs.google.com/presentation/d/1fo570JmkbccNnl9Rt7epeqSMe4-L6T-2Ou7eXMs_kzY/edit?usp=sharing]) The slide deck also includes a results section that was not presented due to lack of time. 

Information on [Pacific hake](https://docs.google.com/presentation/d/19_nQMLzEmGpiChAYMeOcr2lyXGsVh9NCok1FG56sWvI/edit?usp=sharing) and the [Rougheye/Blackspotted rockfish Complex](https://docs.google.com/presentation/d/15nZqnT1AZPilrg_OI9jS5ATgJYBUhRZBohU57OlT5xA/edit?usp=sharing) (with additional slides by Jim Hastie) was also presented.

This [slide deck](https://docs.google.com/presentation/d/1CWaZ3Szq2zO5Rr007N2YUYPOd94pIYjLkIlvYmANNVc/edit?usp=sharing) is a look at the Sablefish reference scan data.

---

<h3> Neural Net Models using the keras R Package with a Custom TensorFlow Conda Environment in Windows</h3>

<h4> Neural Net Model Code in R </h4>
The Hake scripts under 'R_NN_Model_Hake_Scripts' are cleaned up.  All other species to be looked at, including Sablefish, will start will this code with the species changed. A function taking a species name would be possible down the road.

<h4> Setting Up the TensorFlow Conda Environment under Windows </h4>
See the code in 'Setting_up_TensorFlow_Conda_Environment_under_Windows' to setup the custom TensorFlow Conda environment.  Note that as of March 2023, the keras R package's ability to create its own TensorFlow Conda environment is currently broken due to incompatible versions of supporting software in the environment setup.
<br/> 
<br/>
A TensorFlow Conda environment setup under Windows 10 also works under Windows 11 and can be copied and shared (zip first since there are a lot of small files).  However, the Windows 10 Conda environment does not work in R on a client being served by Windows Server 2019, but I do include a partial solution in 'Install TensorFlow on Win Server 2019 Conda Env.md' for R and another that works fully under Python with GPU support. A conda Keras install would be needed inside the environmnent to run Keras under Python, see:  https://www.activestate.com/resources/quick-reads/what-is-a-keras-model/   
<br/> 
<br/>        

---

<h3>Steps for reading in spectra, running the NN model, and predicting ages from the NN model </h3>

Preliminary information:

  - This code is optimized for Notepad++ ability to minimize any code seqment surrounded by curly brackets, this is often 'if' and 'for'
    statements, but any code seqment (often quite large) can be surrounded by extra curly brackets and be minimized by the user. 

1. Reading in spectra using Read_OPUS_Spectra.R()
  - Example using the script: FishNIRS/R_Scripts/Read in Sable Comm 2018__2024 Scans.R

Code snippet:

     for(i in (2020:2023)) {  
         Model_Spectra_Meta_YR <- Read_OPUS_Spectra(Spectra_Set = paste0("Sable_WA_Comm_", i), fileNames_Sort_Seqment = NULL,
                                     Spectra_Path = paste0("//nwcfile.nmfs.local/FRAM/Assessments/Aging Lab/NIRS Scanning Data/Otoliths/FT_NIRS_Project/PRD_Production/WA_COMM/SABL_Sablefish/", i, "/"),
                                     htmlPlotFolder = paste0("Figures_Sable_WA_Comm_", i), Static_Figure = paste0("Sable_WA_Comm_", i, ".png"), Meta_Path = NULL, Extra_Meta_Path = NULL, excelSheet = 3, 
                                     shortNameSegments = 6, shortNameSuffix = 'WA_Comm', Debug = TRUE)
         dim(Model_Spectra_Meta_YR)          
         Table(Model_Spectra_Meta_YR$TMA)
         
         assign(paste0("Model_Spectra_Meta_", i), Model_Spectra_Meta_YR)
     }      


The first spectra set name is:

     Sable_WA_Comm_2020

and the 2020 spectra and session report information is in:

     //nwcfile.nmfs.local/FRAM/Assessments/Aging Lab/NIRS Scanning Data/Otoliths/FT_NIRS_Project/PRD_Production/WA_COMM/SABL_Sablefish/2020

The Excel session report's main data sheet that lists metadata for each otolith in on sheet 3.

For the above example there was no extra metadata available except what was in the session report. When there is metadata availble in the Data Warehouse, then the function: R/Import_Species_Metadata_from_NWFSC_Warehouse.R() can be run to extract that data and the argument would look something like:

     Extra_Meta_Path = "C:/SIDT/Get Otie Info from Data Warehouse/selectSpAgesFramFeb2025.RData"  
     Extra_Meta_Path = "C:/SIDT/Get Otie Info from Data Warehouse/Chilipepper_Combo_Metadata.RData"


If you have metadata in a CSV or Excel file, convert it to a 'RData' file and use that file: 
     
    PWHT_Acoustic2019_Extra_Metadata <- openxlsx::read.xlsx("C:/SIDT/PWHT_Acoustic2019/SH201906_Hake_Ages.xlsx")
    save(PWHT_Acoustic2019_Extra_Metadata, file = 'PWHT_Acoustic2019_Extra_Metadata.Rdata')  # 'Rdata' here 

    Extra_Meta_Path = "C:/PWHT_Acoustic2019/PWHT_Acoustic2019_Extra_Metadata.RData"
     

2. Setup and run the NN model batch script which calls itself to reset R's memory leak issue: R_Scripts/iPLS, NN Model Batch Self Call Loop.R
  - sdf
  - 


     
   


---

<h3> Notes for the readSpectraData() Function </h3>

The directory structure used by readSpectraData() is for the R working directory to be directly below the current directory:

     ../R 
     
with the spectra data to be beside it in:

    ../OPUS Spectra
    
 and the downloads for the data to be in this format:
 
     ../OPUS Spectra/1_2019_11
     ../OPUS Spectra/2_2019_12
     ../OPUS Spectra/3_2020_01
     ...
     
     ../OPUS Spectra/10_2020_10 
    
