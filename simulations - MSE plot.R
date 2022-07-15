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
reshapingMSESimulations=function(simulation,h_1,L){
  rownames(simulation)=c("tanh", "softplus", "sigmoid")
  n_sim=dim(simulation)[2]
  df=as.data.frame(t(simulation))
  df$Layers=as.factor(rep(L,n_sim))
  df$Neurons_per_layer=as.factor(rep(h_1,n_sim))
  df=reshape::melt(df,id.vars=c("Layers","Neurons_per_layer"))
  names(df)[c(3,4)]=c("Act.Function","MSE")
  return(df)
}

####################################
# 3 - Plot the simulations:
####################################

# Load the data
simulation1 <- readRDS("data/Simulation_uniform__Hidden_per_layer_32_number_layers_3")
simulation2 <- readRDS("data/Simulation_uniform__Hidden_per_layer_32_number_layers_5")
simulation3 <- readRDS("data/Simulation_uniform__Hidden_per_layer_32_number_layers_7")
simulation4 <- readRDS("data/Simulation_uniform__Hidden_per_layer_32_number_layers_3")
simulation5 <- readRDS("data/Simulation_uniform__Hidden_per_layer_32_number_layers_5")
simulation6 <- readRDS("data/Simulation_uniform__Hidden_per_layer_32_number_layers_7")


# Reshape Data using custom function 
df1 <- reshapingMSESimulations(simulation1, "h_l = 32", 3)
df2 <- reshapingMSESimulations(simulation2, "h_l = 32", 5)
df3 <- reshapingMSESimulations(simulation3, "h_l = 32", 7)

df4 <- reshapingMSESimulations(simulation4, "h_l = 64", 3)
df5 <- reshapingMSESimulations(simulation5, "h_l = 64", 5)
df6 <- reshapingMSESimulations(simulation6, "h_l = 64", 7)


# Joint dataframe
df.first <- rbind(df1, df2, df3, df4, df5, df6)

# Y axis breakpoints
# my_breaks <- 10^c(-5,-3,-1,1,3,5,7,9,11)

# Create the plot
plot1 <- ggplot(df.first, aes(x = Layers, y = MSE, fill = Act.Function)) +
  geom_boxplot() +
  facet_grid(Neurons_per_layer ~ .) +
  labs(fill = "Activation\n Function") +
  xlab("Number of Layers") +
  scale_y_continuous("MSE between NN and obtained PR", trans = "log10")+
  theme_half_open() +
  background_grid(major = "y")

plot1

# Save the plot in temporal file
setEPS()
postscript("temporal/fig_MSE_boxplots.eps", width = 6, height = 5)
plot1
dev.off()

