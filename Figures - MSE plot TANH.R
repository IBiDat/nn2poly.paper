############################################################################
## Script that plots and saves the MSE simulation boxplots as eps files.
## Author: Pablo Morala
###########################################################################

library(ggplot2)
library(cowplot)
library(viridis)
IEEE_width_inches <- 3.5
my_width <- IEEE_width_inches

####################################
# 2 - Define needed functions
####################################
reshapingMSESimulations=function(simulation,L){
  rownames(simulation)=c("40", "70", "100")
  n_sim=dim(simulation)[2]
  df=as.data.frame(t(simulation))
  df$Layers=as.factor(rep(L,n_sim))
  df=reshape::melt(df,id.vars=c("Layers"))
  names(df)[c(2,3)]=c("h_l","MSE")
  return(df)
}

####################################
# 3 - Plot the simulations:
####################################

# Load the data
simulation1 <- readRDS("data/Simulation_tanh_number_layers_1")
simulation2 <- readRDS("data/Simulation_tanh_number_layers_3")
simulation3 <- readRDS("data/Simulation_tanh_number_layers_5")
simulation4 <- readRDS("data/Simulation_tanh_number_layers_7")



# Reshape Data using custom function 
df1 <- reshapingMSESimulations(simulation1, 1)
df2 <- reshapingMSESimulations(simulation2, 3)
df3 <- reshapingMSESimulations(simulation3, 5)
df4 <- reshapingMSESimulations(simulation4, 7)


# Joint dataframe
df.plot <- rbind(df1, df2, df3, df4)

# Y axis breakpoints
# my_breaks <- 10^c(-5,-3,-1,1,3,5,7,9,11)

# Create the plot
plot1 <- ggplot(df.plot, aes(x = Layers, y = MSE, fill = h_l)) +
  geom_boxplot() +
  labs(fill = "h_l") +
  xlab("Number of Layers") +
  scale_y_continuous("MSE between NN and obtained PR", trans = "log10")+
  theme_half_open() +
  background_grid(major = "y")

plot1

# Save the plot in temporal file
setEPS()
postscript("temporal/fig_tanh_MSE_boxplots.eps", width = 2*my_width, height = my_width)
plot1
dev.off()

