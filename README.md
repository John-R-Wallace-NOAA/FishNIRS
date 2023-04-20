<h3> Neural Net Models using the keras R Package with a Custom TensorFlow Conda Environment in Windows</h3>

<h4> Setting Up the TensorFlow Conda Environment under Windows </h4>
See the code in 'Setting_up_TensorFlow_Conda_Environment_under_Windows' to setup the custom TensorFlow Conda environment.  Note that as of March 2023, the keras R package's ability to create its own TensorFlow Conda environment is currently broken due to incompatible versions of supporting software in the environment setup. A TensorFlow Conda environment setup under Windows 10 also works under Windows 11 and can be copied and shared (zip first since there are a lot of small files).  However, the Windows 10 Conda environment does not work in R on a client being served by Windows Server 2019, but I do include a partial solution in 'Install TensorFlow on Win Server 2019 Conda Env.md' for R and another that works fully under Python (a Conda Keras install would be needed < install -y -c anaconda keras >).

<h4> Neural Net Model Code in R </h4>
The Hake scripts in 'R_NN_Model_Hake_Scripts' are cleaned up.  All other species to be looked at, including Sablefish, will start will this code with the species changed. A function taking a species name would be possible down the road.


<h4> Notes for the readSpectraData() Function </h4>

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
    
