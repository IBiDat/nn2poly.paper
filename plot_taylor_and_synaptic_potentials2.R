# Slightly modified version of the taylor plot function from the nn2poly package
# This function should be updated in the future in the package


plot_taylor_and_synpatic_potentials2 <- function(data,
                                                weights_list,
                                                af_string_list,
                                                q_taylor_vector,
                                                forced_max_Q) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("package 'ggplot2' is required for this functionality", call.=FALSE)
  
  # This function is currently only prepared for regression cases:
  if (af_string_list[[length(af_string_list)]] != "linear") {
    print("The NN is not a regression")
    return(NULL)
  }
  
  # The number of plots that we want to obtain is the number of hidden layers:
  n_plots <- length(weights_list) - 1
  
  # Initialize the list containing plots:
  plots_list <- vector(mode = "list", length = n_plots)
  
  # get the dimension
  p <- dim(data)[2] - 1
  
  # get the AF as R functions:
  af_function_list <- change_string_to_function(af_string_list)
  
  
  # We have to store the output of each layer to use it as input in the next one
  # and use it to compute the synaptic potentials.
  # Therefore, we initialize the variable "output" with data so the loop starts correctly.
  output <- data[, -(p + 1)]
  # The number of inputs is then:
  n_input <- dim(output)[1]
  
  for (k in 1:n_plots) {
    
    # Initialize input as output from previous layer
    input <- output
    
    # Obtain the weights at this layer
    weights <- weights_list[[k]]
    
    # We need to add a column with 1's to the input data to multiply them by the biases
    input <- cbind(rep(1, n_input), input)
    
    # Obtain number of neurons h at the desired layer
    h <- dim(weights)[2]
    
    # Compute the synaptic potentials first as a matrix, to compute each neuron separately
    synaptic_potentials <- matrix(0, n_input, h)
    for (j in 1:h) {
      for (i in 1:n_input) {
        synaptic_potentials[i, j] <- sum(weights[, j] * input[i, ])
      }
    }
    
    # Now join all the values in a vector to plot the density because
    # we don't care in which neuron we have the problems as long as there is one.
    synaptic_potentials_vectorized <- as.vector(synaptic_potentials)
    
    # However, we still need to use the matrix to compute the AF, so we obtain the
    # output to use in the next layer:
    fun <- af_function_list[[k]]
    output <- fun(synaptic_potentials)
    
    
    ################## Plot creation ########################
    
    
    # Depending on the function we need to obtain an adequate interval:
    if (af_string_list[[k]] == "tanh") {
      taylor_interval <- 1.1
    } else if (af_string_list[[k]] == "sigmoid") {
      taylor_interval <- 5
    } else if (af_string_list[[k]] == "softplus") {
      taylor_interval <- 5
    }
    
    # Create the x values for the Taylor plot
    x <- seq(-taylor_interval, taylor_interval, length.out = 1000)
    
    
    # create data frame and create an empty plot with only the density of those values
    df.density <- as.data.frame(synaptic_potentials_vectorized)
    names(df.density) <- c("value")
    
    #### Taylor graph ######
    
    # compute the true function
    yf <- fun(x)
    # compute the Taylor approximation
    pol <- pracma::taylor(fun, 0, min(q_taylor_vector[k],forced_max_Q))
    yp <- pracma::polyval(pol, x)
    # compute the error as the absolute value of the difference
    error <- abs(yf - yp)
    
    # Now we create the Taylor plot and add the density behind it.
    df.plot <- data.frame(x, yf, yp, error)
    
    plot.taylor.simple <- ggplot() +
      geom_line(data = df.plot, aes(x, yf, color = "black")) +
      # This line is only used to add the density color in the legend, and then
      # covered by the red line.
      geom_line(data = df.plot, aes(x, yp, color ="darkgreen")) +
      geom_line(data = df.plot, aes(x, yp, color ="red")) +
      geom_line(data = df.plot, aes(x, error, color = "blue")) +
      geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
      labs(x = "x") +
      labs(y = "y") +
      xlim(-1, 1) +
      theme(plot.title = element_text(hjust = 0.5, size = 10)) +
      theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10)) +
      scale_color_identity(name = "Legend",
                           breaks = c("black", "red", "blue", "darkgreen"),
                           labels = c("True function","Taylor approximation", "Error", "Activation potentials density"),
                           guide = "legend") +
      theme_minimal()
    
    # Using axis_canvas
    xdens4 <- axis_canvas(plot.taylor.simple, axis = "x") +
      xlim(-1, 1) +
      geom_density(data = df.density,
                   aes(x = value, y = after_stat(density)+1),
                   color = "darkgreen",
                   fill = "lightgreen", trim=TRUE) +
      scale_y_log10() +
      theme_void()
    
    plot.Taylor <- wrap_plots(xdens4, plot.taylor.simple, ncol=1, heights=c(0.1, 0.9))
    
    
    
    plots_list[[k]] <- plot.Taylor
  }
  
  return(plots_list)
}
