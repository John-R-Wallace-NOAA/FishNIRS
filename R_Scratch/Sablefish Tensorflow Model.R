
# https://tensorflow.rstudio.com
# https://www.tensorflow.org/install/pip
# https://www.r-bloggers.com/2018/07/an-introduction-to-tensorflow/

# TensorFlow GitHub
https://github.com/tensorflow/tensorflow

# https://docs.python.org/3.7/using/windows.html#windows-nuget
nuget.exe install python-full-x64 -Version 3.9.2 -ExcludeVersion -OutputDirectory .

# Install python Windows
https://www.codingforentrepreneurs.com/guides/install-python-on-windows/




# In PowerShell
python -m ensurepip --upgrade
# Caution: TensorFlow 2.10 was the last TensorFlow release that supported GPU on native-Windows. Starting with TensorFlow 2.11, you will need to install TensorFlow in WSL2, 
#     or install tensorflow-cpu and, optionally, try the TensorFlow-DirectML-Plugin
pip3.exe install tensorflow==2.10.1  
pip install -i https://pypi.org/tensorflow.2.10.1
pip3.exe install https://files.pythonhosted.org/packages/10/45/773ef490f1d0df9c843163f2990408453ff54be4481ec3fec729a8ea7e6c/tensorflow-2.11.0-cp310-cp310-win_amd64.whl


# In R
install.packages("tensorflow", "reticulate")
lib(tensorflow)
lib(reticulate)


# lib(keras)
# install_keras(envname = "r-reticulate")

# path_to_python <- "C:/python-full-x64/tools/python.exe"
path_to_python <- "W:/Win_apps/Python/Python310/python.exe"
virtualenv_create("r-reticulate", python = path_to_python)

tensorflow::install_tensorflow(envname = "r-reticulate")


# Test TensorFlow
sess <- tf$Session()

sudo find . -name mnt -prune -o -name 'libcudart*' -print

hello <- tf$constant("Hello, TensorFlow!")
sess$run(hello)
a <- tf$constant(10)
b <- tf$constant(32)
sess$run(a + b)
sess$close()



# NVIDIA CUDA Installation Guide for Linux
https://docs.nvidia.com/cuda/cuda-installation-guide-linux/index.html#post-installation-actions



# ============================= Under WSL2 ==============================================================

# What flavor and version of Linux
$ uname -m && cat /etc/*release # Ubuntu 20.04.5 LTS

# Version of the kernel the system is running
$ uname -r  # 4.4.0-19041-Microsoft

# How to use Linux 'find' command and not search the mounted (mnt) drives 
# https://stackoverflow.com/questions/70967651/could-not-load-dynamic-library-libcudart-so-11-0 
$ sudo find . -name mnt -prune -o -name 'python3.8*' -print

# Find what versions you have
# https://www.tensorflow.org/install/source#gpu




#TF
sudo find . -name mnt -prune -o -name 'tensorflow*' -print # 2.11.0

#GCC
gcc --version  # gcc (Ubuntu 9.4.0-1ubuntu1~20.04.1) 9.4.0
sudo find . -name mnt -prune -o -name 'gcc' -print # 9-base

# NVIDIA CUDA Installation Guide for Linux
https://docs.nvidia.com/cuda/cuda-installation-guide-linux/index.html#wsl


# Install CUDA ********* Do not install the CUDA drivers in WSL **********************
$ sudo apt update
$ sudo apt-get upgrade
$ sudo apt install nvidia-cuda-toolkit
$ sudo find . -name mnt -prune -o -name 'cuda*' -print

$ sudo apt install nvidia-cuDNN-toolkit

# Test for CUDA
$ nvcc --version  # release 10.1, V10.1.243

# Try to remove CUDA
https://forums.developer.nvidia.com/t/cuda-remove-10-1-and-install-10-0-ubuntu-18-04/73804




# ================================= Install into windows-wsl2 using a Minicoda environment ============================================

# import TensorFlow tar file into wsl and set as the default
wsl --import Ubuntu_18.04_TMB_TensorFlow . C:\Users\John.Wallace\AppData\Local\Packages\Ubuntu_18.04_TMB_TensorFlow.tar --version 2
wsl -s Ubuntu_18.04_TMB_TensorFlow

# List distro names
wsl --list --verbose

# Terminate a distro
wsl --terminate <distroname> 
wsl --terminate Ubuntu_18.04_TMB_TensorFlow

#  If wsl starts as root, use the following (a single space ' ' is the password)
login wallacej  

# To stop booting into root see here:
https://superuser.com/questions/1566022/how-to-set-default-user-for-manually-installed-wsl-distro/1627461#1627461

# All this and directly under Windows doesn't work due to version issues
     # https://www.tensorflow.org/install/pip#windows-wsl2
     
     # curl https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -o Miniconda3-latest-Linux-x86_64.sh
     # bash Miniconda3-latest-Linux-x86_64.sh

     
# This works!!
# TensorFlow 2.10.0 not compatible with TensorRT 8.4.3
# https://github.com/tensorflow/tensorflow/issues/57679#issuecomment-1249197802

conda create --name tf-py38 python=3.8
conda install -c conda-forge cudatoolkit=11.2 cudnn=8.1.0


conda activate tf-py38

# Use below to end conda session
conda deactivate

# No GPU.........for now

pip install --upgrade setuptools pip
pip install nvidia-pyindex

pip install nvidia-tensorrt==7.2.3.4

# verify
python3 -c "import tensorrt; print(tensorrt.__version__); assert tensorrt.Builder(tensorrt.Logger())"


# Configure the system paths once again as before to contain tensorrt path:
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$CONDA_PREFIX/lib/python3.8/site-packages/tensorrt/:/home/wallacej/miniconda3/envs/tf-py38/lib/python3.8/site-packages/tensorrt

# or with recommended automation:  ****** FIX ********
# echo 'export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$CONDA_PREFIX/lib/python3.8/site-packages/tensorrt/' > $CONDA_PREFIX/etc/conda/activate.d/env_vars.sh

pip install tensorflow

# Only work with gpu?
# python3 -c "import tensorflow as tf; print(tf.config.list_physical_devices('GPU'))"
# python3 -c "import tensorflow as tf; print(tf.reduce_sum(tf.random.normal([1000, 1000])))"


sudo find . -iname mnt -prune -o -name 'libnvinfer.so*' -print # missing

# Look here for missing files
cd /usr/lib/x86_64-linux-gnu
ls libnvinfer.so*  # libnvinfer.so.7


# # # Missing Libraries When Trying to Use GPU with Tensorflow
# # https://askubuntu.com/questions/1229464/missing-libraries-when-trying-to-use-gpu-with-tensorflow
# #   
# # wget https://developer.download.nvidia.com/compute/cuda/repos/ubuntu1810/x86_64/cuda-repo-ubuntu1810_10.1.168-1_amd64.deb
# # sudo dpkg -i cuda-repo-ubuntu1810_10.1.168-1_amd64.deb
# # wget https://developer.download.nvidia.com/compute/cuda/repos/ubuntu1810/x86_64/7fa2af80.pub
# # sudo apt-key adv --fetch-keys http://developer.download.nvidia.com/compute/machine-learning/repos/ubuntu1804/x86_64/7fa2af80.pub
# # sudo apt update
# # wget http://developer.download.nvidia.com/compute/machine-learning/repos/ubuntu1804/x86_64/nvidia-machine-learning-repo-ubuntu1804_1.0.0-1_amd64.deb
# # sudo dpkg -i nvidia-machine-learning-repo-ubuntu1804_1.0.0-1_amd64.deb
# # sudo apt update
# # 
# # sudo apt-get install --no-install-recommends \
# #     cuda-10-1 \
# #     libcudnn7=7.6.4.38-1+cuda10.1  \
# #     libcudnn7-dev=7.6.4.38-1+cuda10.1
# # 
# #     # Install TensorRT. Requires that libcudnn7 is installed above.
# # sudo apt-get install -y --no-install-recommends  
# #     libnvinfer6=6.0.1-1+cuda10.1 \
# #     libnvinfer-dev=6.0.1-1+cuda10.1 \
# #     libnvinfer-plugin6=6.0.1-1+cuda10.1
    
# # # reinstall NVIDIA graphics card drives:
# # sudo add-apt-repository ppa:graphics-drivers
# # sudo apt update
# # sudo apt -u dist-upgrade



# Path to python for R
sudo find . -name mnt -prune -o -name 'python3.8*' -print #  ./miniconda3/envs/tf-py38/bin/python3.9


# Run below when there is no Gpu or you don't want to use one
# https://sodocumentation.net/tensorflow/topic/10621/tensorflow-gpu-setup#run-tensorflow-on-cpu-only---using-the--cuda-visible-devices--environment-variable-
python3 -c "import os; os.environ['CUDA_VISIBLE_DEVICE']='-1'; import tensorflow as tf"

# Run below to use gpu
python3 -c "import os; os.environ['CUDA_VISIBLE_DEVICE']='-1'; import tensorflow as tf"

# In R (R Packages are here: /R/x86_64-pc-linux-gnu-library/4.2)
R
options(width = 160)
library(JRWToolBox)
lib(tensorflow)
lib(reticulate)
lib(keras)
lib(tidyverse)
lib(recipes)
lib(rsample)
lib(GGally)
lib(skimr)

path_to_python <- "./home/wallacej/miniconda3/envs/tf-py38/bin/python3.8"
virtualenv_create("r-reticulate", python = path_to_python)

tensorflow::install_tensorflow(envname = "r-reticulate")

# Test TensorFlow
hello <- tf$constant('Hello, TensorFlow!')
zeros <- tf$Variable(tf$zeros(shape(1L)))

tf$print(hello)
tf$print(zeros)

# Tensorflow regression example
# https://tensorflow.rstudio.com/tutorials/keras/regression

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data"
col_names <- c("mpg","cylinders","displacement","horsepower","weight","acceleration","model_year", "origin","car_name")

raw_dataset <- read.table(
  url,
  header = T,
  col.names = col_names,
  na.strings = "?"
)

dataset <- raw_dataset %>% select(-car_name)
tail(dataset)

lapply(dataset, function(x) sum(is.na(x))) %>% str()

dataset <- na.omit(dataset)

library(recipes)
dataset <- recipe(mpg ~ ., dataset) %>%
  step_num2factor(origin, levels = c("USA", "Europe", "Japan")) %>%
  step_dummy(origin, one_hot = TRUE) %>%
  prep() %>%
  bake(new_data = NULL)

tibble::glimpse(dataset)

split <- rsample::initial_split(dataset, 0.8)
train_dataset <- training(split)
test_dataset <- testing(split)

train_dataset %>%
  select(mpg, cylinders, displacement, weight) %>%
  GGally::ggpairs()

skimr::skim(train_dataset)

train_features <- train_dataset %>% select(-mpg)
test_features <- test_dataset %>% select(-mpg)

train_labels <- train_dataset %>% select(mpg)
test_labels <- test_dataset %>% select(mpg)

# (my_skim <- skimr::skim_with(numeric = skimr::sfl(mean, sd))) # Same output table - extra??

# Another data summary
train_dataset %>%
  select(where(~is.numeric(.x))) %>%
  pivot_longer(
    cols = everything(), names_to = "variable", values_to = "values") %>%
  group_by(variable) %>%
  summarise(mean = mean(values), sd = sd(values))
  

normalizer <- keras::layer_normalization(axis = -1L) 
normalizer$mean  # Test - no mean yet

normalizer %>% keras::adapt(as.matrix(train_features))  # Fits the state of the preprocessing layer to the data being passed 
normalizer$mean

# keras::adapt(keras::layer_normalization(axis = -1L), as.matrix(train_features))   #  How is this not an error??  What object is changeed??

first <- as.matrix(train_features[1,])
# cat('First example:', first)
# cat('Normalized:', as.matrix(normalizer(first)))

# Better
data.frame('First example:' = c(first), Normalized = c(as.matrix(normalizer(first))))



# Linear regression with one variable
horsepower <- matrix(train_features$horsepower)
horsepower_normalizer <- layer_normalization(input_shape = shape(1), axis = NULL)
horsepower_normalizer %>% adapt(horsepower)

horsepower_model <- keras_model_sequential() %>%
  horsepower_normalizer() %>%
  layer_dense(units = 1)

summary(horsepower_model)

predict(horsepower_model, horsepower[1:10,])

# ?compile.keras.engine.training.Model
horsepower_model %>% keras::compile(
  optimizer = keras::optimizer_adam(learning_rate = 0.1),
  loss = 'mean_absolute_error'
)

# ?fit.keras.engine.training.Model
history <- horsepower_model %>% fit(
  as.matrix(train_features$horsepower),
  as.matrix(train_labels),
  epochs = 100,
  # Suppress logging.
  verbose = 0,
  # Calculate validation results on 20% of the training data.
  validation_split = 0.2
)

plot(history)

# ?evaluate.keras.engine.training.Model
test_results <- list()
(test_results[["horsepower_model"]] <- horsepower_model %>% evaluate(
  as.matrix(test_features$horsepower),
  as.matrix(test_labels),
  verbose = 0
))

x <- seq(0, 250, length.out = 251)
y <- predict(horsepower_model, x)

ggplot(train_dataset) +
  geom_point(aes(x = horsepower, y = mpg, color = "data")) +
  geom_line(data = data.frame(x, y), aes(x = x, y = y, color = "prediction"))
  
  
plot(train_dataset$horsepower, train_dataset$mpg, col = 'red')
lines(x, y, col= 'green')


# Use of Artificial Neural Networks and NIR Spectroscopy for Non-Destructive Grape Texture Prediction
https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8834220/

# Getting Started with Keras
https://cran.r-project.org/web/packages/keras/vignettes/index.html
































 
# Could not load dynamic library ‘libnvinfer.so.7’
https://forums.developer.nvidia.com/t/could-not-load-dynamic-library-libnvinfer-so-7/231606






# Install NVIDIA CUDA on Ubuntu
https://docs.vmware.com/en/VMware-vSphere-Bitfusion/3.0/Example-Guide/GUID-ABB4A0B1-F26E-422E-85C5-BA9F2454363A.html
 
 
# Installing cuDNN and CUDA Toolkit on Ubuntu 20.04 for Machine Learning Tasks ********** NOT WSL2 ***************
https://medium.com/geekculture/installing-cudnn-and-cuda-toolkit-on-ubuntu-20-04-for-machine-learning-tasks-f41985fcf9b2

# Install Docker on Windows (WSL) without Docker Desktop 
https://dev.to/bowmanjd/install-docker-on-windows-wsl-without-docker-desktop-34m9

# Tested  TensorFlow build configurations
https://www.tensorflow.org/install/source#gpu


# Making a flawless ML env. with Tensorflow 2 and CUDA 10.1 on Ubuntu 20.04 with dual boot 2021
https://towardsdatascience.com/making-a-flawless-ml-env-with-tensorflow-2-and-cuda-10-1-on-ubuntu-20-04-with-dual-boot-2021-3731c92692fb
























