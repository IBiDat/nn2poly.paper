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
library(cowplot)
library(ggplot2)
tensorflow::tf$random$set_seed(12345) # Needed to have reproducible results with keras

####################################
# 3 - Set up all  fixed parameters 
####################################

# Fixed Parameters for the data generation
source("data_generation.R")
my_seed <- 12345
n_sample <- 500
mean_range <- c(-10, 10)
unif_range <- c(-10, 10)
number_interactions <-4
beta_range <- c(-4, 4)
error_var <- 0
p <- 5
q_original <- 2

# Set random seed for reproducibility (only affects R, not keras?)
set.seed(my_seed)

# keras hyperparameters
my_loss <- "mse"
my_metrics <- "mse"
my_optimizer <- optimizer_rmsprop()
my_epochs <- 1000
my_batch <- 200
my_validation_split <- 0.2
my_verbose <- 0
my_max_norm <- list("l1_norm", 1)


####################################
# 4 - Set up all parameters that change over in the loops
####################################

q_taylor_at_each_layer <- 8 # We can set a high value as we are going
# to limit it with a forced max Q.
forced_max_Q <- 3 # Choose 3 as data generated will be of order 2.



#-------------------------------------------------------------

L <- 7
h_neurons_at_each_layer <- 32
af <- "tanh"

# q taylor with 1 at the end for regression
q_taylor_vector <- rep(q_taylor_at_each_layer,L)
q_taylor_vector <- c(q_taylor_vector,1)

# h neurons with 1 at the end for regression
h_neurons_vector <- rep(h_neurons_at_each_layer,L)
h_neurons_vector <- c(h_neurons_vector,1)


###### Create first the list with the af at each layer:
af_string_list <- vector(mode = "list", length = L+1)
for (i in 1:L){
  af_string_list[i] <- af
}
# Add linear at the end so we have regression setting.
af_string_list[L+1] <- "linear"


#-------------------------------------------------------------

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

plot(data_scaled)

#-----------------------------------------------------

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



plot(history)

# Obtain the predicted values with the NN to compare them
prediction_NN <- predict(nn, test_x)
plot_NN_performance <- plot_NN_PR_comparison(unname(test_y), prediction_NN)
plot_NN_performance

# MSE between NN and Poly
n_test <- length(test_y)
MSE_NN_vs_Y<- sum((prediction_NN - test_y)^2) / n_test

MSE_NN_vs_Y 


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
  store_coeffs = FALSE
)

labels <- coeffs[[length(coeffs)]][[1]]
coeffs <- coeffs[[length(coeffs)]][[2]]

# Obtain the predicted values for the test data with our polynomial

prediction_poly <- rep(0, n_test)

for (i in 1:n_test) {
  prediction_poly[i] <- nn2poly::eval_poly(test[i, seq(p)], labels = labels, coeffs = coeffs)
}
  
# MSE between NN and Poly
MSE_NN_vs_poly <- sum((prediction_NN - prediction_poly)^2) / n_test

MSE_NN_vs_poly 

# Plot the PR vs NN predictions
plot_PR_vs_NN <- plot_NN_PR_comparison(prediction_poly, prediction_NN) +
  theme_half_open()
plot_PR_vs_NN



# Plot the taylor expansion at each layer:
plot_taylor <- plot_taylor_and_synpatic_potentials(
  data = train,
  weights_list = nn_weights,
  af_string_list = af_string_list,
  q_taylor_vector = q_taylor_vector
) 

for (i in 1:length(plot_taylor)){
  plot_taylor[[i]] <- plot_taylor[[i]] + 
    theme_half_open()
}
plot_taylor

