############################################################################
## Script that plots and saves the MSE simulation boxplots as eps files.
## Author: Pablo Morala
###########################################################################

library(ggplot2)
library(cowplot)
library(viridis)

####################################
# 2 - Define needed functions
####################################
reshapingMSESimulations=function(simulation,h_l,L){
  rownames(simulation)=c("tanh", "softplus", "sigmoid")
  n_sim=dim(simulation)[2]
  df=as.data.frame(t(simulation))
  df$Layers=as.factor(rep(L,n_sim))
  df$Neurons_per_layer=as.factor(rep(h_l,n_sim))
  df=reshape::melt(df,id.vars=c("Layers","Neurons_per_layer"))
  names(df)[c(3,4)]=c("Act.Function","MSE")
  return(df)
}

####################################
# 3 - Plot the simulations:
####################################

# Load the data
h_neurons_at_each_layer_vector <- c(50)
the_3_chosen_af <- c("tanh","softplus","sigmoid")
n_hidden_layers <- c(1,3,5)


simulation_NN_vs_poly <- vector(mode = "list", length = 0L)
simulation_NN_vs_original <- vector(mode = "list", length = 0L)
i <- 1
for (h_l in h_neurons_at_each_layer_vector){
  for (L in n_hidden_layers){
     aux <- readRDS(paste0("temporal/Simulation_NN_vs_poly_uniform_Hidden_per_layer_",
                                      h_l,
                                      "_number_layers_",
                                      L))
      simulation_NN_vs_poly[[i]] <- reshapingMSESimulations(aux, paste0("h_l = ",h_l) , L)
      aux <- readRDS(paste0("temporal/Simulation_NN_vs_original_uniform_Hidden_per_layer_",
                                                 h_l,
                                                 "_number_layers_",
                                                 L))
      simulation_NN_vs_original[[i]] <- reshapingMSESimulations(aux, paste0("h_l = ",h_l), L)
    
      i <- i+1
  }
}

df_NN_vs_poly <- NULL
df_NN_vs_original <- NULL
for (j in 1: length(simulation_NN_vs_poly)){
  df_NN_vs_poly <- rbind(df_NN_vs_poly, simulation_NN_vs_poly[[j]])
  df_NN_vs_original <- rbind(df_NN_vs_original, simulation_NN_vs_original[[j]])
}


# Y axis breakpoints
# my_breaks <- 10^c(-5,-3,-1,1,3,5,7,9,11)

# Create the plot
plot1 <- ggplot(df_NN_vs_poly, aes(x = Layers, y = MSE, fill = Act.Function)) +
  geom_boxplot() +
  facet_grid(Neurons_per_layer ~ .) +
  labs(fill = "Activation\n Function") +
  xlab("Number of Layers") +
  scale_y_continuous("MSE between NN and obtained PR", trans = "log10")+
  theme_half_open() +
  background_grid(major = "y")

plot1

# Create the plot
plot2 <- ggplot(df_NN_vs_original, aes(x = Layers, y = MSE, fill = Act.Function)) +
  geom_boxplot() +
  facet_grid(Neurons_per_layer ~ .) +
  labs(fill = "Activation\n Function") +
  xlab("Number of Layers") +
  scale_y_continuous("MSE between NN and obtained PR", trans = "log10")+
  theme_half_open() +
  background_grid(major = "y")

plot2



# Save the plot in temporal file
setEPS()
postscript("temporal/fig_MSE_boxplots.eps", width = 6, height = 5)
plot1
dev.off()

