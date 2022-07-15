################################
## Function that takes some given train and test data and performs a full example,
## training the NN, obtaining the PR and comparing predictions with different plots.
###############################
perform_example_from_train_test<- function(train,
                                              test,
                                              af_string_list,
                                              h_neurons_vector,
                                              q_taylor_vector,
                                              my_max_norm,
                                              my_optimizer,
                                              my_loss,
                                              my_metrics,
                                              my_epochs,
                                              my_validation_split,
                                              my_verbose,
                                           all_partitions) {
  ###############################################
  # train and test need to be a matrix with the last column being the response.
  ###############################################
  
  # Obtain parameters:
  p <- ncol(train) - 1
  
  # Divide again in x and y
  train_x <- as.matrix(subset(train, select = -c(p+1)))
  train_y <- as.matrix(subset(train, select = c(p+1)))
  
  test_x <- as.matrix(subset(test, select = -c(p+1)))
  test_y <- as.matrix(subset(test, select = c(p+1)))
  
  # Build the nn
  nn <- build_keras_model(
    p,
    af_string_list,
    h_neurons_vector,
    my_max_norm
  )
  
  # Compile the model
  compile(nn,
          loss = my_loss,
          optimizer = my_optimizer,
          metrics = my_metrics
  )
  
  # Fit the model
  history <- fit(nn,
                 train_x,
                 train_y,
                 verbose = my_verbose,
                 epochs = my_epochs,
                 validation_split = my_validation_split,
                 batch_size = 500
  )
  
  
  print("NN training ended")
  
  # Save the history plot of the NN training
  plot_history <- plot(history)
  
  # Obtain the predicted values with the NN to compare them
  prediction_NN <- predict(nn, test_x)
  
  # plot that comparison between NN and original
  plot_NN_performance <- plot_NN_PR_comparison(test_y, prediction_NN)
  
  plot_NN_performance <- plot_NN_performance +
    ggplot2::labs(x = "Original Y") +
    theme_half_open()
  
  # Extract the weights:
  keras_weights <- keras::get_weights(nn)
  
  # En este caso solo hace falta cambiar lo de la ultima capa
  # ARREGLAR ESTO PARA QUE SEA CONSISTENTE
  
  n <- length(keras_weights)
  
  if(my_max_norm[[1]]=="no_constraint"){

    n2 <- n/2

    nn_weights <- vector(mode = "list", length = n2)
    for (i in 1:n2){
      nn_weights[[i]] <- rbind(keras_weights[[2*i]], keras_weights[[2*i-1]])
   }
    
  } else {
    nn_weights <- keras_weights[1:(n - 2)]
    
    nn_weights[[n - 1]] <- rbind(keras_weights[[n]], keras_weights[[n - 1]])
  }
  
  
  
  
  
  
  # use the the algorithm
  historical_coeffs <- nn2poly_algorithm(
    weights_list = nn_weights,
    af_string_list = af_string_list,
    q_taylor_vector = q_taylor_vector,
    all_partitions = all_partitions
  )
  
  coeffs <- historical_coeffs[[length(historical_coeffs)]][[1]]
  print(coeffs)
  
  # Obtain the predicted values for the test data with our Polynomial Regression
  n_test <- length(test_y)
  prediction_PR <- rep(0, n_test)
  
  for (i in 1:n_test) {
    prediction_PR[i] <- evaluate_PR(test[i, seq(p)], coeffs)
  }

  print("hola")
  # Plot the PR vs NN predictions
  plot_PR_vs_NN <- plot_NN_PR_comparison(prediction_PR, prediction_NN) +
    theme_half_open()
  
  print("adios")
  # Plot the taylor expansion at each layer:
  plot_taylor <- plot_taylor_and_synpatic_potentials(
    data = train,
    weights_list = nn_weights,
    af_string_list = af_string_list,
    q_taylor_vector = q_taylor_vector
  ) 
  
  print("que tal")
  for (i in 1:length(plot_taylor)){
    plot_taylor[[i]] <- plot_taylor[[i]] + 
      theme_half_open()
  }
  
  
  print("bien") 
  
  # Compute different metrics:
  
  # MSE:
  
  MSE_NN <- sum((test[, (p + 1)] - prediction_NN)^2) / n_test
  MSE_PR <- sum((test[, (p + 1)] - prediction_PR)^2) / n_test
  
  print("mas o menos")
  # print(prediction_NN)
  # print(prediction_PR)
  # print(n_test)
  # MSE between NN and PR (because PR is actually approximating the NN, not the actual response Y)
  MSE_NN_vs_PR <- sum((prediction_NN - prediction_PR)^2) / n_test
  
  print("yo igual")
  output <- vector(mode = "list", length = 12)
  output[[1]] <- nn
  output[[2]] <- coeffs
  output[[3]] <- prediction_NN
  output[[4]] <- prediction_PR
  output[[5]] <- MSE_NN
  output[[6]] <- MSE_PR
  output[[7]] <- MSE_NN_vs_PR
  output[[8]] <- plot_history
  output[[9]] <- plot_NN_performance
  output[[10]] <- plot_PR_vs_NN
  output[[11]] <- plot_taylor
  output[[12]] <- nn_weights
  
  names(output) <- c(
    "nn",
    "coeff",
    "prediction_NN",
    "prediction_PR",
    "MSE_NN",
    "MSE_PR",
    "MSE_NN_vs_PR",
    "plot_history",
    "plot_NN_performance",
    "plot_PR_vs_NN",
    "plot_taylor",
    "nn_weights"
  )
  
  return(output)
}
