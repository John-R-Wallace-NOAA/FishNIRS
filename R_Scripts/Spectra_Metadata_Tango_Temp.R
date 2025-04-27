
library(opusreader2)

test <- opusreader2::read_opus_single("W:/ALL_USR/JRW/SIDT/2019 Hake/PACIFIC_HAKE_BMS201906206C SCANS/PACIFIC_HAKE_BMS201906206A_10_OD1.0")

test[["instrument_ref"]]$parameters$TSC  # Scanner Temperature
test[["instrument_ref"]]$parameters$TSC$parameter_value               # C
test[["instrument_ref"]]$parameters$TSC$parameter_value * 9/5 + 32    # F


test[["instrument"]]$parameters$TSC  # Scanner Temperature
test[["instrument"]]$parameters$TSC$parameter_value
test[["instrument"]]$parameters$TSC$parameter_value * 9/5 + 32


test[["instrument_ref"]]$parameters$TSM  # NULL
test[["instrument_ref"]]$parameters$TSM$parameter_value  


test[["instrument_ref"]]$parameters$HUM  # Relative Humidity Interferometer
test[["instrument_ref"]]$parameters$HUM$parameter_value  


test[["instrument_ref"]]$parameters$DUR  # Scan time (sec)
test[["instrument_ref"]]$parameters$DUR$parameter_value  
 
  
  
..$ FC1:List of 4
  .. ..$ parameter_name     : chr "FC1"
  .. ..$ parameter_name_long: chr "Product Group"
  .. ..$ parameter_value    : chr "Otolith"
  .. ..$ parameter_type     : chr "str"
  
..$ FC2:List of 4
  .. ..$ parameter_name     : chr "FC2"
  .. ..$ parameter_name_long: chr "Product"
  .. ..$ parameter_value    : chr "PACIFIC_HAKE"
  .. ..$ parameter_type     : chr "str"
  
..$ I04:List of 4
  .. ..$ parameter_name     : chr "I04"
  .. ..$ parameter_name_long: chr "User1"
  .. ..$ parameter_value    : chr "Morgan"
  .. ..$ parameter_type     : chr "str"
  

test_data_only <- opusreader2::read_opus_single("W:/ALL_USR/JRW/SIDT/2019 Hake/PACIFIC_HAKE_BMS201906206C SCANS/PACIFIC_HAKE_BMS201906206A_10_OD1.0", data_only = TRUE)$data


# help for simplerspec::read_opus_bin_univ:

   # atm_comp_minus4offset 
     # Logical whether spectra after atmospheric compensation are read with an offset of -4 bites from Bruker OPUS files. Default is FALSE.


# https://www.bruker.com/en/products-and-solutions/test-and-measurement/3d-optical-profilers/npflex.html

# Whenever tight control of environmental factors is not possible, the NPFLEX-1000 still maintains excellent reproducibility thanks to its integrated vibration isolation 
# and self-calibration. A built-in reference signal laser interferometer continuously calibrates the temperature to account for environmental influences and to always ensure 
# high accuracy and reproducible measurements, ideal for applications that require absolute long-term stability.


# https://www.bruker.com/en/products-and-solutions/infrared-and-raman/ft-nir-spectrometers/tango-ft-nir-spectrometer.html?document+type_tags_1=Application%2520Note

# Contributing to the high precision of FT-NIR spectrometers is the exact alignment with the background. TANGO goes one step further: The background measurements are performed
# automatically without user intervention. This alignment can even take place if a sample is located in the sampling position. The perfect prerequisite for optimal and secure 
# measurements at any time – without (human) errors.


#  https://file.yzimgs.com/427772/2013092310065343.pdf

# The stated operating temperature for the Tango is 35° C. 



