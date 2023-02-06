
# FYI, R Packages are here: /R/x86_64-pc-linux-gnu-library/4.2)

# In Ubuntu running on WSL

cd /mnt/w/ALL_USR/JRW/SIDT/Sablefish/Keras_CNN_Models
conda activate tf-py38

# Use below to end conda session
conda deactivate

R # Start R
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


# Setup TensorFlow in R
path_to_python <- "./home/wallacej/miniconda3/envs/tf-py38/bin/python3.8"
virtualenv_create("r-reticulate", python = path_to_python)
tensorflow::install_tensorflow(envname = "r-reticulate")

# Test TensorFlow
hello <- tf$constant('Hello, TensorFlow!')
zeros <- tf$Variable(tf$zeros(shape(1L)))

tf$print(hello)
tf$print(zeros)

# Getting Started with Keras
    https://cran.r-project.org/web/packages/keras/vignettes/index.html

#Tutorial
    https://tensorflow.rstudio.com/tutorials/quickstart/beginner

# setwd('/mnt/w/ALL_USR/JRW/SIDT/Sablefish')
# base::load('RData')

# Load data
base::load('Sable_Spectra_2017_2019.sg.iPLS.RData')
base::load('Sable_TMA_2017_2019.RData')

#  Split the data into training set (2/3) and test set (1/3)
set.seed(c(777, 747)[2])
index <- 1:nrow(Sable_Spectra_2017_2019.sg.iPLS)
testindex <- sample(index, trunc(length(index)/3))
x.test <- Sable_Spectra_2017_2019.sg.iPLS[testindex, ]
x.train <- Sable_Spectra_2017_2019.sg.iPLS[-testindex, ]   
y.test <- Sable_TMA_2017_2019[testindex]
y.train <- Sable_TMA_2017_2019[-testindex]

dim(x.train) #  906 380

# Setup the keraas CNN model
model <- keras_model_sequential() 
model %>% 
   layer_dense(units = ncol(x.train) + 1, activation = 'relu', kernel_initializer = initializer_he_normal(), input_shape = ncol(x.train),
           # kernel_regularizer = regularizer_l1(0.01)
           ) %>%
   layer_dense(units = floor(ncol(x.train)/2), activation = 'relu', kernel_initializer = initializer_he_normal(),
            kernel_regularizer = regularizer_l1(0.01)) %>%
   layer_dense(units = 1, activation = 'relu') 
  
summary(model)  
 
# Compile the model
model %>% compile(
   loss = loss_mean_squared_error(),
   optimizer = "adam",
   metrics = metric_mean_absolute_error()
 )

# Run the model on the training set 
# Stop this run by removing the 'Run_NN_Model_Flag' file in the Windows working directory. The latest run of epochs will finish smoothly.
# Likewise in Ubuntu, stop this run by putting R in the background (<ctl - z>), remove the flag in the Linux shell (rm Run_NN_Model_Flag) , and put R back in the foreground with 'fg'.  The latest run of epochs will finish smoothly.
# The number of old tabs created in the browser reflects the number of loops completed. (The axis in the different tabs may change as the model improves.)
# Excessive tabs may crash the browser, delete the old tabs as needed
file.create('Run_NN_Model_Flag', showWarnings = TRUE)
Loop <- 1
Cor <- CA_diag <- NULL
dev.set(2)

while(file.exists('Run_NN_Model_Flag')) {
   history <- fit(model, as.matrix(x.train), y.train, epochs = 3000, batch_size=32, validation_split = 0.2, view_metrics = TRUE)
   cat("\n\nLoop =", Loop, "\n")
   evaluate(model, as.matrix(x.test),  y.test, verbose = 2)
   cat("\n")
   Loop <- Loop + 1   

   print(summary(history))
   dev.new()
   print(plot(history))
       
   # Predict using the test set, plot, create statistics, and create an agreement table
   y.test.pred <- predict(model, as.matrix(x.test))
   dev.new(width = 20, height = 8)
   plot(y.test, y.test.pred)
   abline(0, 1, col = 'green', lty = 2)
   
   Cor <- c(Cor, cor(y.test, y.test.pred))
   cat("\nCorrelation =", rev(Cor)[1], "\n") # 0.9292393 (2 loops of 1000 with 777 seed),  0.9602671 (3 loops of 1000 with 747 seed)
   cat("\nSum of absolute difference =",     sum(abs(y.test - round(y.test.pred))), "\n") # 1,088
   
   CA_diag <- c(CA_diag, e1071::classAgreement(Table(round(y.test.pred), y.test), match.names = FALSE)$diag)
   cat("classAgreement diag", rev(CA_diag)[1], "\n") #  0.3053097, 0.283
   cat("\n\n")
   print(e1071::classAgreement(Table(round(y.test.pred), y.test), match.names = TRUE)$diag) # 0.300885, 0.283
   
   options(width = 300)
   print(Table(round(y.test.pred), y.test))
   
   dev.set(2)
   plot(Cor, col = 'green', type = 'b', ylim = c(-0.03, 1.03), ylab = "Correlation (green) & Diagonal of Class Agreement (red)", xlab = "Loop Number")
   lines(CA_diag, col = 'red', type = 'b')
}


# Save the NN models
save_model_weights_tf(model, './checkpoints/Sable_KR_2nd_Level_Seed_747')

load_model_weights_tf(model, './checkpoints/Sable_KR_2nd_Level_Seed_747')
evaluate(model, as.matrix(x.test),  y.test, verbose = 2)


Sable_KR_2nd_Level_Seed_747 <- serialize_model(model, include_optimizer = TRUE)
save(Sable_KR_2nd_Level_Seed_747, file = 'Sable_KR_2nd_Level_Seed_747.RData')

load('Sable_KR_2nd_Level_Seed_747.RData')
unserialize_model(Sable_KR_2nd_Level_Seed_747, custom_objects = NULL, compile = TRUE)


# kernel_regularizer only on input

  
Correlation = 0.9591366

Sum of absolute difference = 970
classAgreement diag 0.3384956


   y.test
      0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 34 35 36 38 39 41 44 47 49 50 51 55 56 57 58 61 63 65 66 67 71
  0   9  3                                                                                                                                                         
  1     63  2                                                                                                                                                      
  2      7  4 11  3                                                                                                                                                
  3      1    42 12  2  4  2     1                                                                                                                                 
  4      1    17 10  6  7  2  2        1                                                                                                                           
  5            4 13  4  9  3  1     4     1                                                                                                                        
  6            1  8  4  7  1  4  3  2  4        1                                                                                                                  
  7               2  2  8  4  4  4  2                                                                                                                              
  8               1  2  7     1  1     1     1                                                                                                                     
  9                  1  1     2  4     2                          1                                                                                                
  10              1     3  2  1  2        1        1     1     1                                                                                                   
  11              1  1     1  1     1     1     1  2        1           1                                                                                          
  12                       1     1           1  1  2  1  1  1     1                                                                                                
  13                          2                             1              1  1                                                                                    
  14                    1  2        1     1                       1                                                                                                
  15                                            1     1  1     2                                                                                                   
  16                                   1                             1                                                                                             
  17                                                                 1                                                                                             
  18                                                              1                                                                                                
  19                                         1        1     2  1  1        2  1  1                                                                                 
  20                                               1  1                                            1                                                               
  21                                                        1           1                 1                                                                        
  22                                                                    1              1                                                                           
  23                                                                                1  1                                                                           
  24                                                                    1  1                                      1                                                
  25                                                              1  1                    1                                                                        
  26                                                  1                                                                                                            
  27                                                                                   1  1  1                                                                     
  28                                                                                            1                                                                  
  30                                                                                                                    1                                          
  31                                                                                                     1                                                         
  32                                                                                                  1        1                                                   
  33                                                                                                                 1                                             
  35                                                                                                                       1                                       
  36                                                                             1                          1                                                      
  37                                                              1                                                                                                
  38                                                     1                                                                                                         
  39                                                                                                     1                    1                                    
  45                                                                                                                                            1                  
  47                                                                                                                                         1                     
  48                                                                                                                       1                                       
  52                                                                                                                             1                                 
  54                                                                                                                                                  1            
  55                                                                                                                                1                             1
  56                                                                                                                                                        1      
  57                                                                                                                                               1           1   
  58                                                                                                                                   1     1                    1
  60                                                                                                                       1                                       
  61                                                                                                                                                     1         
  62                                                                                                                                      1  1                     
  63                                                                                                                                   1                           





















