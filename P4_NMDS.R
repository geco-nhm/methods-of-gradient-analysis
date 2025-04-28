#######################################################
# P4 GNMDS
#######################################################

# Script for GNMDS based upon Oppkuven data set

# Import libraries
library(vegan)
library(MASS)
library(stats)
library(mgcv)
library(ggplot2)
library(dplyr)

# Loading and attaching data -------------------------------------------

# Check and change the working directory if necessary
getwd()
#setwd("C:/Users/yourUserName/.../methods-of-gradient-analysis")

# Importing species data from Oppkuven
y <- read.csv("P1_Oppkuven_species.csv") %>% 
	as.data.frame() %>%
        select(-Stand)

# Importing environmental variable matrix:
env.var <- read.csv("P1_Oppkuven_environmental_variables.csv") %>% 
	as.data.frame()

# Attach variables to names
attach(y)
names(y)

attach(env.var)
names(env.var)


# Make dissimilarity matrix and make GNMDS's -----------------------------------
# Making Bray-Curtis dissimilarity matrix:
dist.y <- vegdist(y, method = "bray") 
dist.y 

# Replacing unreliable distances (B-C > 0.8 by geodesic distances,
# using stepacross; note that the optimal value for epsilon may be dataset-specific
geodist.y <- isomapdist(dist.y, epsilon = 0.8)
geodist.y

# Set the number of dimensions, k, in the ordination
k <- 2 

# Define a general, empty object called mds:
mds <- NULL

# Making 100 "mds"s from initial starting configurations, allocating them into the mds object:
for (i in 1:100) {
  mds[[i]] <- monoMDS(geodist.y, matrix(c(runif(dim(y)[1] * k)), 
                      nrow = dim(y)[1]), 
                      k = 2, 
                      model = "global", 
                      maxit = 200, 
                      smin = 1e-7, 
                      sfgrmin = 1e-7)
  }

# Alternative options: model = "local", "linear" or "hybrid" with threshold = [value]

# The mds object is now a list consisting of 100 "subobjects" being lists
# extracting the stress values as a vector - 
# stress is given as the 22th element in each "subobject list"
mds.stress <- unlist(lapply(mds, function(v) { v$stress })) 

# Looking at the stress values for 100 mds:
mds.stress

# Ordering the stress values for the 100 mds:
order(mds.stress)

# Saving the order in a vector
ordered <- order(mds.stress)
ordered

# Find the stress of the solutions with the lowest and second lowest stress:
mds.stress[ordered[1]]
mds.stress[ordered[2]]

# Scaling of axes to half change units and varimax rotation by postMDS
# Note that the original Bray-Curtis distances are used in calculation of H.C. scores!

mds.best <- postMDS(mds[[ordered[1]]], dist.y, pc = TRUE, halfchange = TRUE, threshold = 0.8)
mds.best
mds.secbest <- postMDS(mds[[ordered[2]]], dist.y, pc = TRUE, halfchange = TRUE, threshold = 0.8)
mds.secbest

# Procrustes comparisons
procrustes_fit <- procrustes(mds.best, mds.secbest, permutations = 999)
procrustes_fit
protest(mds.best, mds.secbest, permutations = 999)

# Create a data frame with the results to facilitate plotting
procrustes_data <- data.frame(
  Site = rownames(procrustes_fit$Yrot),
  X1_Orig = procrustes_fit$X[, 1],
  Y1_Orig = procrustes_fit$X[, 2],
  X1_Rot = procrustes_fit$Yrot[, 1],
  Y1_Rot = procrustes_fit$Yrot[, 2]
)

# Procrustes plot
ggplot(procrustes_data) +
  geom_segment(aes(x = X1_Orig, y = Y1_Orig, xend = X1_Rot, yend = Y1_Rot),
               color = "green") +
  geom_point(aes(x = X1_Orig, y = Y1_Orig), color = "blue", size = 1, alpha = 0.5) +
  geom_point(aes(x = X1_Rot, y = Y1_Rot), color = "red", size = 1, alpha = 0.5) +
  geom_text(aes(x = X1_Orig, y = Y1_Orig, label = Site), size = 3, hjust = -0.3) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  coord_fixed(ratio = 1) +
  labs(x = "MDS1",
       y = "MDS2") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


# Create a data frame for plotting
gnmds <- as.data.frame(mds.best$points)

#Create a column for plot ID
gnmds$plot <- 1:100

# Making the ordination diagram using the best solution:
gnmds_plot <- ggplot() +
  geom_text(data = gnmds, aes(x = MDS1, y = MDS2, label = plot),
            color = "black", size = 3) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  coord_fixed(ratio = 1) +
  labs(x = "GNMDS1 (scaling in H.C. units)", y = "GNMDS2 (scaling in H.C. units)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Display the plot
gnmds_plot

# Making Shepard-diagram, this is "optional"

# May be done with in-built function
stressplot(mds.best, dist.y)

# ... or manually
geodist.y
mds.best$points
dist.mds <- vegdist(mds.best$points, method = "euclidean")
dist.mds

# Create data frame with 
stressplot_data <- data.frame(observed_dissimilarity = as.vector(geodist.y), ordination_distance = as.vector(dist.mds))

# Order the data according to increasing values of geodist
stressplot_data <- stressplot_data[order(stressplot_data$observed_dissimilarity), ]

# Fit an isotonic regression model
iso_fit <- isoreg(stressplot_data$observed_dissimilarity, stressplot_data$ordination_distance)

# Create a column in the data frame for the fitted values
stressplot_data$fitted_values <- iso_fit$yf 

# Create Shepard diagram
ggplot(stressplot_data, aes(x = observed_dissimilarity, y = ordination_distance)) +
  geom_point(size = 2, alpha = 0.5, color = "deepskyblue4") +
  geom_line(aes(y = fitted_values), color = "coral", linewidth = 1.5) +
  labs(x = "Observed dissimilarity", y = "Ordination distance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



# Testing correlations (provided that both DCA and GNMDS results are available (retrieve the object "dca" from previous lab)
cor.test(dca1, gnmds$MDS1, method = "kendall")
cor.test(dca2, gnmds$MDS2, method = "kendall")

# Preparing 'envfit plot', with vectors fit to continuous variables and points fit to centroids
# for each level of factor type variables
mds.best$points
gnmds <- scores(mds.best, display = "sites", choices = 1:2, origin = TRUE)[, 1:2]
## Biplot with ordination of sites/plots and environmental variables
mds.z <- envfit(ord = as.data.frame(gnmds), env = env.var, permutations = 999)

# Create data frame for site scores (arrow coordinates)
envfit_arrows <- as.data.frame(mds.z$vectors$arrows)

# Create a column for labels
envfit_arrows$labels <- rownames(envfit_arrows)

# Plotting NMDS:
ggplot(gnmds) + 
  geom_text(data = gnmds, aes(x = MDS1, y = MDS2, label = rownames(gnmds)), size = 3) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_segment(data = envfit_arrows, aes(x = 0, y = 0, xend = MDS1, yend = MDS2), 
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               color = "deepskyblue4") +
  geom_text(data = envfit_arrows, aes(x = MDS1, y = MDS2, label = labels), size = 3, nudge_x = 0.025, nudge_y = 0.025) + 
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Isoline diagram on environmental variable:

# Choose the variable for which the isoline diagrasm is to be prepared (here 'TI1' and decide the number of digits you
# want the label for variable values to have. These values are later plotted onto the graph

TI1
TI1r <- round(TI1, digits = 2)
TI1r
mode(TI1r) <- "character"

# Fit isolines
ordisurf_fit <- ordisurf(mds.best, TI1, display = "sites", plot = FALSE)  

# Extract isoline grid
isoline_grid <- expand.grid(x = ordisurf_fit$grid$x, y = ordisurf_fit$grid$y)

# Convert the isoline grid from matrix to vector format
isoline_grid$z <- as.vector(ordisurf_fit$grid$z)

# Display GNMDS
gnmds_plot

# Add isolines
gnmds_plot + 
  geom_contour(data = isoline_grid, aes(x = x, y = y, z = z), color = "black") +
  geom_contour_filled(data = isoline_grid, aes(x = x, y = y, z = z), alpha = 0.25) +
  labs(fill = "TI1")

