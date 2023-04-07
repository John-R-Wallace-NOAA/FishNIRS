FCNN_model <- function(layer_dropout_rate = NULL, numCol = ncol(x.train)) {
   # Fully connected model from grape paper
   
   model <- keras_model_sequential() 
   
   if(is.null(layer_dropout_rate)) {
   
      # model %>% 
      #    layer_dense(units = numCol + 1, activation = 'relu', kernel_initializer = initializer_he_normal(), input_shape = numCol,
      #            kernel_regularizer = regularizer_l1(0.01)) %>%
      #    layer_dense(units = floor(numCol/2), activation = 'relu', kernel_initializer = initializer_he_normal(),
      #             kernel_regularizer = regularizer_l1(0.01)) %>%
      #    layer_dense(units = 1, activation = 'relu') 
         
       model %>% 
         layer_dense(units = numCol + 1, activation = 'relu', kernel_initializer = initializer_he_normal(), input_shape = numCol,
                 kernel_regularizer = regularizer_l1(0.01)) %>%
         layer_dense(units = floor(numCol/2), activation = 'relu', kernel_initializer = initializer_he_normal(),
                  kernel_regularizer = regularizer_l1(0.01)) %>%
         layer_dense(units = 1, activation = 'relu') 
         
   }  else  {
   
      model %>% 
         layer_dense(units = numCol + 1, activation = 'relu', kernel_initializer = initializer_he_normal(), input_shape = numCol,
                 kernel_regularizer = regularizer_l1(0.01)) %>%
         layer_dense(units = floor(numCol/2), activation = 'relu', kernel_initializer = initializer_he_normal(),
                  kernel_regularizer = regularizer_l1(0.01)) %>%
         layer_dropout(layer_dropout_rate) %>%         
         layer_dense(units = 1, activation = 'relu') 
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
