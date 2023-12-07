


# For 'RcppTOML' R package to install the 'CXX17' labels are needed in the ~/.R/Makevars file.

# For 'reticulate' R package to install in Linux - need "just plain old CXX=/path/to/modern/g++" in ~/.R/Makevars file
# https://community.rstudio.com/t/python-cpp36-error-call-of-overloaded-vector-brace-enclosed-initializer-list-is-ambiguous-return-charactervector-id-str/165733/3


# Therefore, here is what is needed in the  ~/.R/Makevars file:

CXX14 = /opt/rh/devtoolset-7/root/usr/bin/g++
CXX14FLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -fPIC
CXX14FLAGS+=-flto -Wno-unused-local-typedefs
LDFLAGS=-L/opt/rh/devtoolset-7/root/usr/lib/gcc/x86_64-redhat-linux/7
CXX17 = /opt/rh/devtoolset-11/root/usr/bin/g++
CXX17FLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -fPIC
CXX17FLAGS+=-flto -Wno-unused-local-typedefs
LDFLAGS=-L/opt/rh/devtoolset-11/root/usr/lib/gcc/x86_64-redhat-linux/11
CXX = /opt/rh/devtoolset-11/root/usr/bin/g++


# Need to put miniconda3 library in the path
#  https://stackoverflow.com/questions/49875588/importerror-lib64-libstdc-so-6-version-cxxabi-1-3-9-not-found

# In BASH shell:
$ export LD_LIBRARY_PATH=/home/jwallace/miniconda3/lib:$LD_LIBRARY_PATH

$ /opt/R/64-bit/R-4.1.2_MKL/bin/R

options(width = 160)

# install.packages('RcppArmadillo')


# Conda_TF_Eniv <- "/home/jwallace/miniconda3/envs/tf-py38/bin" # This gives: The TensorFlow library was compiled to use AVX instructions, but these aren't available on your machine.  Aborted (core dumped)
Conda_TF_Eniv <- "/more_home/h_jwallace/Python/tf_cpu_only/bin" # Change this path as needed
Sys.setenv(RETICULATE_PYTHON = Conda_TF_Eniv) 
Sys.getenv("RETICULATE_PYTHON") 

   
if (!any(installed.packages()[, 1] %in% "tensorflow")) 
     install.packages("tensorflow")
     
if (!any(installed.packages()[, 1] %in% "keras")) 
     install.packages("keras") 
   
library(tensorflow)
library(keras)      

a <- tf$Variable(5.56)

{
  a <- tf$Variable(5.56)
  cat("\n\nTensorFlow Math Check\n\na = "); print(a)
  b <- tf$Variable(2.7)
  cat("\nb = "); print(b)
  cat("\na + b = "); print(a + b)
  cat("\n\n")
}

k_clear_session() 


# Got error for latest version of RcppArmadillo

# https://stackoverflow.com/questions/71696731/fail-to-install-package-rcpparmadillo-on-ubuntu-20-04-r-version-4-1-3
sudo add-apt-repository --enable-source --yes "ppa:c2d4u.team/c2d4u4.0+" 
sudo apt install r-cran-rcpparmadillo


# lib('RcppCore/RcppArmadillo') # Didn't help to intall from FitHub

# Grabbed the 0.10.5.0.0 version from the R ver 4.0 pacakges - seems to be working....




