FCNN_model <- function(layer_dropout_rate = NULL, numCol = ncol(x.train), activation_function = c('relu', 'elu', 'selu')[1]) {

   '  # Fully connected model from the grape texture paper:  '
   '  # Use of Artificial Neural Networks and NIR Spectroscopy for Non-Destructive Grape Texture Prediction. Basile et al. Foods 2022, 11, 281  '
   
   model <- keras_model_sequential() 
   
   if(is.null(layer_dropout_rate)) {
   
      # model %>% 
      #    layer_dense(units = numCol + 1, activation = activation_function, kernel_initializer = initializer_he_normal(), input_shape = numCol,
      #            kernel_regularizer = regularizer_l1(0.01)) %>%
      #    layer_dense(units = floor(numCol/2), activation = activation_function, kernel_initializer = initializer_he_normal(),
      #             kernel_regularizer = regularizer_l1(0.01)) %>%
      #    layer_dense(units = 1, activation = activation_function) 
         
       model %>% 
         layer_dense(units = numCol + 1, activation = activation_function, kernel_initializer = initializer_he_normal(), input_shape = numCol,
                 kernel_regularizer = regularizer_l1(0.01)) %>%
         layer_dense(units = floor(numCol/2), activation = activation_function, kernel_initializer = initializer_he_normal(),
                  kernel_regularizer = regularizer_l1(0.01)) %>%
         layer_dense(units = 1, activation = activation_function) 
         
   }  else  {
   
      model %>% 
         layer_dense(units = numCol + 1, activation = activation_function, kernel_initializer = initializer_he_normal(), input_shape = numCol,
                 kernel_regularizer = regularizer_l1(0.01)) %>%
         layer_dense(units = floor(numCol/2), activation = activation_function, kernel_initializer = initializer_he_normal(),
                  kernel_regularizer = regularizer_l1(0.01)) %>%
         layer_dropout(layer_dropout_rate) %>%         
         layer_dense(units = 1, activation = activation_function) 
   } 
   
   print(summary(model))
    
   # Compile the model 
   # https://machinelearningmastery.com/understand-the-dynamics-of-learning-rate-on-deep-learning-neural-networks/
   opt <- optimizer_adamax(learning_rate = 0.00088)
   model %>% compile(
      loss = loss_mean_squared_error(),
      optimizer = list('adam', 'adamax', opt)[[3]],
      metrics = list(metric_mean_absolute_error(), "accuracy")[[2]]
   )
   model
}
