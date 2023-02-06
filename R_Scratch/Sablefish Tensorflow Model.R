
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

cd /mnt/w/ALL_USR/JRW/SIDT/Sablefish

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



# ================================== CNN with Keras ===================================================================================

# Getting Started with Keras
https://cran.r-project.org/web/packages/keras/vignettes/index.html

https://tensorflow.rstudio.com/tutorials/quickstart/beginner

setwd('/mnt/w/ALL_USR/JRW/SIDT/Sablefish')
base::load('RData')

#  Split the data into training set (2/3) and test set (1/3)
set.seed(c(777, 747)[1])
index <- 1:nrow(Sable_Spectra_2017_2019.sg.iPLS)
testindex <- sample(index, trunc(length(index)/3))
x.test <- Sable_Spectra_2017_2019.sg.iPLS[testindex, ]
x.train <- Sable_Spectra_2017_2019.sg.iPLS[-testindex, ]   
y.test <- Sable_TMA_2017_2019[testindex]
y.train <- Sable_TMA_2017_2019[-testindex]


dim(x.train)  # [1] 906 380

# ???????? keras.backend.clear_session()

model <- keras_model_sequential(input_shape = 380) %>%
 layer_flatten() %>%
 layer_dense(32, activation = "relu") %>%
 layer_dropout(0.2) %>%
 layer_dense(72)

model %>% compile(
  optimizer = "adam",
  loss = loss_fn,
  metrics = "accuracy"
)  

model %>% fit(as.matrix(x.train), y.train, epochs = 100, batch_size=32, view_metrics = TRUE)

model %>% evaluate(as.matrix(x.test),  y.test, verbose = 2)
probability_model <- keras_model_sequential() %>%
model() %>%  layer_activation_softmax()


  model() %>%
  layer_activation_softmax() %>%
  layer_lambda(tf$argmax)

probability_model(x_test[1:5, , ])

# y.train.pred <- keras:::predict.keras.engine.training.Model(model, as.matrix(x.train))

y.train.pred <- apply(keras:::predict.keras.engine.training.Model(model, as.matrix(x.train)), 1, mean)

tf$nn$softmax(y.train.pred)

dev.new()
hist(y.train.pred)

dev.new()
plot(y.train, y.train.pred)

dev.new()
plot(y.test, y.test.pred)


options(width = 300)
Table(round(y.test.pred), y.test)


model.1 <- model


# ---------------------------  NN Regression model -------------------------------------------------------
 # Vignette or here:
https://cran.r-project.org/web/packages/keras/vignettes/index.html

#  Split the data into training set (2/3) and test set (1/3)

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
lib(e1071)


load('Sable_Spectra_2017_2019.sg.iPLS.RData')
load('Sable_TMA_2017_2019.RData')

set.seed(c(777, 747)[1])
index <- 1:nrow(Sable_Spectra_2017_2019.sg.iPLS)
testindex <- sample(index, trunc(length(index)/3))
x.test <- Sable_Spectra_2017_2019.sg.iPLS[testindex, ]
x.train <- Sable_Spectra_2017_2019.sg.iPLS[-testindex, ]   
y.test <- y.test.raw <- Sable_TMA_2017_2019[testindex]
y.train <- y.train.raw <- Sable_TMA_2017_2019[-testindex]


# Change the range of x matries to [0, 1]
x.train <- as.matrix(x.train)
x.train <- x.train - min(x.train)
x.train <- x.train/max(x.train)
range(x.train)

x.test <- as.matrix(x.test)
x.test <- x.test - min(x.test)
x.test <- x.test/max(x.test)
range(x.test)

# Make y categorical
# # y.train <- to_categorical(y.train, 72)
# # y.test <- to_categorical(y.test, 72)

# Setup TensorFlow in R
path_to_python <- "./home/wallacej/miniconda3/envs/tf-py38/bin/python3.8"
virtualenv_create("r-reticulate", python = path_to_python)
tensorflow::install_tensorflow(envname = "r-reticulate")

# Grape texture
https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8834220/

dim(x.train)
[1] 906 380

model <- keras_model_sequential() 
model %>% 
   layer_dense(units = 381, activation = 'relu', kernel_initializer = initializer_he_normal(), input_shape = c(380)) %>% 
#    layer_dropout(rate = 0.4) %>% 
   layer_dense(units = 190, activation = 'relu', kernel_initializer = initializer_he_normal()) %>%
#    layer_dropout(rate = 0.3) %>%
   layer_dense(units = 1, activation = 'relu')  %>%
 #   layer_dense(units = 1, activation = 'softmax')
  
 summary(model)  
 
 
 # Loss function that ignores missing target values in keras for R
 # https://stackoverflow.com/questions/68290705/loss-function-that-ignores-missing-target-values-in-keras-for-r
 

 mse_na_loss <- function(y_true = 0, y_pred = 0){k_pow(y_true-y_pred, 2) * k_cast(k_not_equal(y_true, -1), 'float32') }
 cce_na_loss <- function(y.true = 0, y_pred = 0 ){ - y.true*log(y_pred) * k_cast(k_not_equal(y.true, -1), 'float32') }
 
model %>% compile(
 #   loss = 'categorical_crossentropy',
   loss = loss_mean_squared_error(),
   optimizer = "adam",
   metrics = metric_mean_absolute_error()
 )

history <- fit(model, x.train, y.train, epochs = 1000, batch_size = 32, validation_split = 0.2, view_metrics = TRUE)
summary(history)

dev.new()
plot(history)

model <- keras_model_sequential(input_shape = 380) %>%
 layer_flatten() %>%
 layer_dense(16, activation = "relu") %>%
 layer_dropout(0.2) %>%
 layer_dense(72)

model %>% compile(
  optimizer = "adam",
  loss = loss_sparse_categorical_crossentropy(from_logits = FALSE),
  metrics = "accuracy"
)  
summary(model)


# # file.create('Run_NN_Model.txt', showWarnings = TRUE)
# # Loop <- 1
# # Epochs = 100
# # while(file.exists('Run_NN_Model.txt')) {
# #    history <- fit(model, as.matrix(x.train), y.train, epochs = Epochs, batch_size=32, view_metrics = TRUE, initial_epoch = (Loop - 1 ) * Epochs + 1)
# #    Loop <- Loop + 1
# # }

# Stop this run by putting R in the background (<ctl - z>), remove the flag in the Linux shell (rm Run_NN_Model_Flag) , and put R back in the foreground with 'fg' and the latest run of epochs will finish smoothly.
file.create('Run_NN_Model_Flag', showWarnings = TRUE)
Loop <- 1
while(file.exists('Run_NN_Model_Flag')) {
   history <- fit(model, as.matrix(x.train), y.train, epochs = 1000, batch_size=32, validation_split = 0.2, view_metrics = TRUE)
   cat("\n\nLoop =", Loop, "\n")
   evaluate(model, as.matrix(x.test),  y.test, verbose = 2)
   cat("\n")
   Loop <- Loop + 1   
}
summary(history)
dev.new()
plot(history)




     loss  accuracy
2.3647032 0.3938053

     loss  accuracy
2.3128023 0.3960177

   loss  accuracy
2.2922294 0.4026549

    loss  accuracy
2.2893882 0.4048673

    loss  accuracy
2.3105097 0.4225664

# 500 epochs
     loss  accuracy
2.2932701 0.3960177

    loss  accuracy
2.2980106 0.4115044

# Windows doesn't kill Firefox started by WSL
shell("echo Taskkill /IM Firefox.exe /F > run.bat")
shell("start run.bat")
shell("del run.bat")

start cmd Taskkill /IM Firefox.exe /F

# In Ubuntu find firefox process
ps aux | grep firefox
 
# Claims to kill firefox - but does not under WSL
sudo killall -v firefox



history$params$epochs
   

probability_model <- keras_model_sequential() %>%
model() %>%  layer_activation_softmax()

dev.new()
plot(history)

evaluate(model, x.test, y.test, verbose = 2)

y.test.pred <- predict(model, x.test)
# y.test.pred <- apply(predict(model, x.test), 1, mean)
dev.new(width = 20, height = 8)
plot(y.test.raw, y.test.pred)
abline(0, 1, col = 'green', lty = 2)

dev.new(width = 20, height = 8)
plot(y.test.pred, y.test.raw)
abline(0, 1, col = 'green', lty = 2)

# y.test.pred <- y.test.pred - min(y.test.pred)
# y.test.pred <- 71 * y.test.pred/max(y.test.pred)
# dev.new(width = 20, height = 8)
# plot(y.test.raw, y.test.pred)

cor(y.test, y.test.pred) # cor(y.test, y.test.pred)     # ]0.9292393
sum(abs(y.test - round(y.test.pred))) # 1,088

options(width = 300)
Table(round(y.test.pred), y.test)

e1071::classAgreement(Table(round(y.test.pred), y.test)) # $diag[1]  0.3053097
e1071::classAgreement(Table(round(y.test.pred), y.test), match.names = TRUE) #  $diag [1] 0.300885


# Save the NN models
save_model_weights_tf(model, './checkpoints/Model_Save_Reg_1')

load_model_weights_tf(model, './checkpoints/Model_Save_Reg_1')
evaluate(model, as.matrix(x.test),  y.test, verbose = 2)


Model_Save_Reg_1 <- serialize_model(model, include_optimizer = TRUE)
save(Model_Save_Reg_1, file = 'Model_Save_Reg_1.RData')

load('Model_Save_Reg_1.RData')
unserialize_model(Model_Save_Reg_1, custom_objects = NULL, compile = TRUE)

  y.test
      0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 27 28 29 31 35 36 37 38 39 41 45 49 50 51 53 54 55 58 59 62 63 71
  0  14  8                                                                                                                                          
  1     56        1                                                                                                                                 
  2     11  4  4        3                                                                                                                           
  3         6 25  4     4                                                                                                                           
  4         3 39 14  2 10  4  3  4  1  2  1  1                                                                                                      
  5         1 12 13  8 14  4  4  2  4  2        1                                                                                                   
  6            1  6  2  8  2  3  3     2     1                                                                                                      
  7            1  3  1  4  2  3  1  1  1  1        1                                                                                                
  8               4  1     2  2  3  2     2                                   1                                                                     
  9                  1  1  2  2                             2  1              1     1                                                               
  10                    1     2              2  1     1                                                                                             
  11                          1                    3     1  1     1                                                                                 
  12              1                                   1  1     1                                                                                    
  13                    1  1              1        1                                                                                                
  14                    1                       1     2  3  1                                1                                                      
  15                    1                                                                                                                           
  16                    1                    1  1        1     1        1  1                                                                        
  17                                2                                                                                                               
  18                                                  1     1        1                                                                              
  19                                   1        1                                   1                                                               
  21                                                           1  1  1     1                                                                        
  22                                                     1  1                    1                                                                  
  23                                                     1                                                                                          
  24                                                        1                                                                                       
  26                                                                                   1                                                            
  27                                                                                                           1                                    
  28                                                                                            1     1                                             
  29                                                              1                                         1                                       
  30                                                  1     1                                                                                       
  31                                                                                   1                                                            
  32                                                                                   1                                                            
  33                                                                                      1                                                         
  35                                                                                                                 1                              
  37                                                                                               1                                                
  38                                                                                   1        1              1                                    
  39                                                                             1                       1                                          
  40                                                              1                                                              1                  
  44                                                                                                              1                          1      
  53                                                                                                                                   1        1  
  54                                                                                                                       1                       
  55                                                                                                                       1                       
  58                                                                                                                                               1
  61                                                                                                                                1               
  62                                                                                                                                      1         
  63                                                                                                                 1                              
  66                                                                                                                    1                           
  75                                                                                                                          1                     


 
 
 
 
 
 
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


