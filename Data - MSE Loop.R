############################################################################
## Script that performs n simulations nn2poly examples, where random data 
## is generated and a NN trained with it to then apply nn2poly and compare
## NN and polynomial predictions with the MSE, changing over different
## hyperparameters that are:
##    - The activation function (softplus, tanh, sigmoid).
##    - The number of hidden neurons.
##    - The number of layers.
## Author: Pablo Morala
###########################################################################

####################################
# 1 - Load all needed packages and functions
####################################
library(nn2poly)
library(nn2poly.tools)
library(keras)
library(tensorflow)

library(future.apply)
plan(multisession)# Optional to use parallel computation

####################################
# 3 - Set up all  fixed parameters 
####################################

# Fixed Parameters for the data generation
# Refer to data_generation.R file to see the details.
source("data_generation.R")
my_seed <- 12345
n_sample <- 500
unif_range <- c(-1,1)
error_var <- 0.1
number_interactions <-5
p <- 5
q_original <- 2

# keras hyperparameters
my_loss <- "mse"
my_metrics <- "mse"
my_optimizer <- optimizer_rmsprop()
my_epochs <- 500
my_batch <- 100
my_validation_split <- 0.2
my_verbose <- 0
my_max_norm <- list("l1_norm", 1)


####################################
# 4 - Set up all parameters that change over in the loops
####################################

q_taylor_at_each_layer <- 8 # We can set a high value as we are going
# to limit it with a forced max Q.
forced_max_Q <- 3 # Choose 3 as data generated will be of order 2.

h_neurons_at_each_layer_vector <- c(50,100)

the_3_chosen_af <- c("tanh","softplus","sigmoid")

n_hidden_layers <- c(1,3,5,7)


# Set seeds
# Set random seed for reproducibility (only affects R, not keras?)
set.seed(my_seed)
tensorflow::tf$random$set_seed(12345) # Needed to have reproducible results with keras


####################################
# 5 - Define wrapper functions
####################################
perform_example_from_train_test<- function(train,
                                           test,
                                           af_string_list,
                                           h_neurons_vector,
                                           q_taylor_vector,
                                           forced_max_Q,
                                           my_max_norm,
                                           my_optimizer,
                                           my_loss,
                                           my_metrics,
                                           my_epochs,
                                           my_batch,
                                           my_validation_split,
                                           my_verbose,
                                           all_partitions) {

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
                 batch_size = my_batch
  )
  

  # Obtain the predicted values with the NN to compare them
  prediction_NN <- predict(nn, test_x)
  
  # MSE between NN and Poly
  n_test <- length(test_y)
  MSE_NN_vs_original<- sum((prediction_NN - test_y)^2) / n_test
  
  # Extract the weights:
  keras_weights <- keras::get_weights(nn)
  
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
  
  # use the  nn2poly algorithm
  coeffs <- nn2poly::nn2poly_algorithm(
    weights_list = nn_weights,
    af_string_list = af_string_list,
    q_taylor_vector = q_taylor_vector,
    forced_max_Q = forced_max_Q,
    all_partitions = all_partitions,
    store_coeffs = FALSE
  )
  
  labels <- coeffs[[length(coeffs)]][[1]]
  coeffs <- coeffs[[length(coeffs)]][[2]]
  
  # Obtain the predicted values for the test data with our polynomial
  n_test <- length(test_y)
  prediction_poly <- rep(0, n_test)
  
  for (i in 1:n_test) {
    prediction_poly[i] <- nn2poly::eval_poly(test[i, seq(p)], labels = labels, coeffs = coeffs)
  }
    
  # MSE between NN and Poly
  MSE_NN_vs_poly <- sum((prediction_NN - prediction_poly)^2) / n_test
  
  output <- vector(mode = "list", length = 0)
  output$MSE_NN_vs_poly <- MSE_NN_vs_poly
  output$MSE_NN_vs_original <- MSE_NN_vs_original
  
  return(output)
}



####################################
# 6 - Simulation
####################################


# Number of simulations for each combination of hyperparameters
n_simulation <- 20

library(tictoc)
tic.clearlog()
tic("total")

for (h_neurons_at_each_layer in h_neurons_at_each_layer_vector){
  for (L in n_hidden_layers){
    # q taylor with 1 at the end for regression
    q_taylor_vector <- rep(q_taylor_at_each_layer,L)
    q_taylor_vector <- c(q_taylor_vector,1)
    
    # h neurons with 1 at the end for regression
    h_neurons_vector <- rep(h_neurons_at_each_layer,L)
    h_neurons_vector <- c(h_neurons_vector,1)
    
    # We will store the simulations for the 3 AF at together
    # This is done because of how we will later plot the simulations
    simulations_MSE_all_AF_NN_vs_poly <- NULL
    simulations_MSE_all_AF_NN_vs_original<- NULL
    
    for (af in the_3_chosen_af){
      
      ###### Create first the list with the af at each layer:
      af_string_list <- vector(mode = "list", length = L+1)
      for (i in 1:L){
        af_string_list[i] <- af
      }
      # Add linear at the end so we have regression setting.
      af_string_list[L+1] <- "linear"
      
      ###### Loop in n_simulation:
      # Loop over number of simulations for a given combination of hyperparameters
      aux1 <- rep(0, n_simulation)
      aux2 <- rep(0, n_simulation)
      for (i in 1:n_simulation) {
        
        # Data generation:
        data_generated <- data_generation(n_sample, p, q_original, unif_range, number_interactions = number_interactions, error_var)
        data <- data_generated$data
        original_betas <- data_generated$original_betas
        
        # Scale the data in the desired interval and separate train and test
        scale_method <- "-1,1"
        data_scaled <- scale_data(data, scale_method)
        aux <- divide_train_test(data_scaled, train_proportion = 0.75)
        train <- aux$train
        test <- aux$test
        
        
        # Compute the MSE for this example
        aux <- perform_example_from_train_test(
          train = train,
          test = test,
          af_string_list = af_string_list,
          h_neurons_vector = h_neurons_vector,
          q_taylor_vector = q_taylor_vector,
          forced_max_Q = forced_max_Q,
          my_max_norm = my_max_norm,
          my_optimizer = my_optimizer,
          my_loss = my_loss,
          my_metrics = my_metrics,
          my_epochs = my_epochs,
          my_batch = my_batch,
          my_validation_split = my_validation_split,
          my_verbose
        )
        
        aux1[i] <- aux$MSE_NN_vs_poly
        aux2[i] <- aux$MSE_NN_vs_original
      }
      
      # Store MSE values for the given AF with the other AFs
      simulations_MSE_all_AF_NN_vs_poly <- rbind(simulations_MSE_all_AF_NN_vs_poly, aux1)
      simulations_MSE_all_AF_NN_vs_original <- rbind(simulations_MSE_all_AF_NN_vs_original, aux2)
      
      
    }
    
    # Generate a name to store the simulation file,
    simulation_name1 <- paste("temporal/Simulation_NN_vs_poly_uniform",
                             "Hidden_per_layer", h_neurons_at_each_layer,
                             "number_layers", L,
                             sep = "_")
    simulation_name2 <- paste("temporal/Simulation_NN_vs_original_uniform",
                              "Hidden_per_layer", h_neurons_at_each_layer,
                              "number_layers", L,
                              sep = "_")
    
    # Save simulation data
    saveRDS(simulations_MSE_all_AF_NN_vs_poly , simulation_name1)
    saveRDS(simulations_MSE_all_AF_NN_vs_original , simulation_name2)
  }
  
}

toc(log = TRUE, quiet = FALSE)

log.txt <- tic.log(format = TRUE)

# Save logs:
saveRDS(log.txt, "temporal/timelogs_original_and_poly_uniform_MSE_loop")



