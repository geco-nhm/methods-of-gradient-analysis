#######################################################
# P1 PCA of environmental variables
#######################################################

# Import libraries
library(vegan)
library(ggplot2)
library(dplyr)

# Loading and attaching environmental variable matrix (Oppkuven) -------------------------------------------

# Check and change the working directory if necessary
getwd()
#setwd("C:/Users/yourUserName/.../methods-of-gradient-analysis")

# Import data from file
env.var <- read.csv("P1_Oppkuven_environmental_variables") %>% 
	as.data.frame()

# Attach data to the R environment
attach(env.var)
names(env.var)
str(env.var)

# The correlation matrix can be calculated step by step by repeated commands:
cor.test(Mois, Litter, method = "kendall")
cor.test(Mois, BasalA, method = "kendall")

# Alternatively, we can make short-cuts as follows (returning only correlation coefficients):
symnum(cor(env.var, method = "kendall"))
cor(env.var, method = "kendall")

# Consider to paste and adapt the 'Kendallstauscript into this script (remember correspondence between object names!!)!



# PCA ordination of environmental variables -----------------------------------------------------------------------

pca.r <- rda(env.var, scale = TRUE) #The 'scaling' argument, which specifies the scaling of axes, is only needed in 'summary' and plot' commands
summary(pca.r) # Note that default is 'scaling = 2' (= correlation biplot scaling; optimising intervariable correlations)

# Base R PCA plot (not supported by ggplot):
plot(pca.r) 

# ggplot PCA plot

# Extract species scores 
pca_variable <- scores(pca.r, display = "species", scaling = 2) %>% as.data.frame() # Scaling parameter has to be set

# Create a column in the data frame for variable names
pca_variable$variable <- rownames(pca_variable)

# Create PCA plot
pca_plot <- ggplot() +
  
  # Add arrows for species scores
  geom_segment(data = pca_variable, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               color = "deepskyblue4") +
  
  # Add variable names
  geom_text(data = pca_variable, aes(x = PC1, y = PC2, label = variable),
            color = "navyblue", size = 3, nudge_x = 0.025, nudge_y = 0.025) +  # Dotted line to labels
  
  # Ensure equal axis length
  coord_fixed(ratio = 1) +
  
  # Label axes
  labs(x = "PC1", y = "PC2") +
  
  # Select theme
  theme_bw() + 
  
  # Exclude panel grid from the plot
  theme(panel.grid.minor = element_blank())



# Plot 'species' scores
pca_plot

# Extract site scores 
pca_sites <- scores(pca.r, display = "sites", scaling = 2) %>% as.data.frame() # Scaling parameter has to be set

# Create a column in the data frame for variable names
pca_sites$plots <- rownames(pca_sites)

# Add site scores to the plot
pca_plot + geom_point(data = pca_sites, aes(x = PC1, y = PC2)) +
  
  # Add plot names
  geom_text(data = pca_sites, aes(x = PC1, y = PC2, label = plots),
            color = "firebrick2", size = 3, nudge_x = 0.025, nudge_y = 0.025)
  
