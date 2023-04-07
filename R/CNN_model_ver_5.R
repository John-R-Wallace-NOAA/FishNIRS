
CNN_model_ver_5 <- function(x.nrow = nrow(x.train),  x.ncol = ncol(x.train)) {
 # Follows 1D-FCN Soil Phosphorus Prediction in Madagascar paper
 # No activation = 'relu' for layer_conv_1d() nor padding="causal"
 # Drop layers are now up slighty to 0.3 & 0.15
 
   model <- keras_model_sequential() 
   model %>% 
       layer_dense(units = x.ncol + 1, activation = 'relu', kernel_initializer = initializer_he_normal(), 
             input_shape = c(x.ncol, 1), kernel_regularizer = regularizer_l1(0.01)) %>%               
       layer_conv_1d(filters = 10, kernel_size = 8) %>% 
          layer_max_pooling_1d(pool_size = 2, strides = 2) %>%          
       layer_conv_1d(filters = 10, kernel_size =16) %>% 
          layer_max_pooling_1d(pool_size = 2, strides = 2) %>% 
       # layer_conv_1d(filters = 10, activation = 'relu', kernel_size = 64) %>% 
       #   layer_max_pooling_1d(pool_size = 2, strides = 5) %>%
       # layer_dropout(0.3) %>%            
      layer_simple_rnn(units = 64, activation = 'relu', kernel_initializer = initializer_he_normal(),
             kernel_regularizer = regularizer_l1(0.01), return_sequences = TRUE) %>%
      layer_simple_rnn(units = 32, activation = 'relu', kernel_initializer = initializer_he_normal(),
            kernel_regularizer = regularizer_l1(0.01)) %>%
      layer_dense(units = floor(x.ncol/2), activation = 'relu', kernel_initializer = initializer_he_normal(),
               kernel_regularizer = regularizer_l1(0.01)) %>%
      layer_dropout(0.2) %>%         
      layer_dense(units = 1, activation = 'relu') 
       
   print(summary(model))
    
   # Compile the model 
   opt <- optimizer_adamax(learning_rate = learningRate)
   model %>% compile(
      loss = loss_mean_squared_error(),
      optimizer = list('adam', 'adamax', opt)[[3]],
      metrics = list(metric_mean_absolute_error(), "accuracy")[[2]]
   )
   model
}
 
 
 