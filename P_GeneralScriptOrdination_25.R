# General script for customised ordination diagrams and related illustrations

########################################
#*** Import and preparation of data ***#
########################################

# The script is generalised by opening for import of ordination results (ordination axes) from external files.
# Ordination axes may, of course, also be derived from R ordination objects. 
# In this script ordination axes, are given generic names.
# Customising the script to your data can be done in two ways; either by changing the names of your ordination
# axis (score) objects into the generic names (recommended), or by changing the generic names in this script.


# Importing libraries:
library(readxl)
library(writexl)
library(vegan)
library(graphics)  # To identify points on ordination diagrams
library(ggplot2)
library(tidyr)
library(dplyr)
library(corrplot)


# If R is not started anew for running this script, a complete cleanup (removal of all attached data frames) is recommended:

rm(list = ls())

# Data import and preparations:

dta <- read.table("clipboard", header = TRUE) # Raw species data
ev <- read.table("clipboard", header = TRUE) # Explanatory variables

ax <- read.table("clipboard", header = TRUE) # Optional, axes from a previous ordination
id <- read.table("clipboard", header = TRUE) # Optional, IDs of observation units
oi <- read.table("clipboard", header = TRUE) # Optional, supplementary information about the OI's (affiliation to 'nature type', subset etc.)


# Automatic import
setwd("C:/Users/"[insert the path to your working directory here]) #Set working directory

# Import excel sheets
dta_excel <- read_xlsx("X_SpFreq.xlsx", skip = 1, n_max = 100) %>% as.data.frame() # Exclude the first column and limit the number of columns to 100
dta <- dta_excel[, 3:ncol(dta_excel)] # Exclude column 1-2
ev_excel <- read_xlsx("X_EnvVar.xlsx") %>% as.data.frame()
ev <- ev_excel[, 5:ncol(ev_excel)] # Exclude column 1-4


# Attach variables to names
attach(ev)
names(ev)
attach(dta)
names(dta)

attach(ax)
names(ax)
attach(id)
names(id)
attach(oi)
names(oi)


# Optional control that ax is a data frame:

is.data.frame(ax)

# Optional generalisation of imported ordination axes

ax1 <- ax[, 1]
ax2 <- ax[, 2]
ax3 <- ax[, 3]
ax4 <- ax[, 4]


#############################################################################
#*** Standard script for between-variables correlation analyses analyses ***#
#############################################################################

# Making a vector of environmental variable names

evnames <- names(ev)

# Histograms for environmental variables displayed as a panel

# Reshape environmental data from 'short' to 'long' format to facilitate plotting 
long_ev <- ev %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

ev # 'Short' format
as.data.frame(long_ev) # 'Long' format

# Plot histograms side by side
ggplot(long_ev, aes(x = value)) +
  geom_histogram(bins = 8, color = "black", fill = "grey", alpha = 0.7) + # Select number of histogram bins
  facet_wrap(~ variable, scales = "free") +  # Facet to display histograms side by side
  labs(x = "Value",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend



## Correlation matrix

# Number of variables
n <- ncol(ev)

# Make empty datasets
# For p-values
pvalues <- matrix(data = NA, nrow = n, ncol = n)
pvalues <- as.data.frame(pvalues)
colnames(pvalues) <- names(ev)
rownames(pvalues) <- names(ev)

# For tau-values
tauvalues <- matrix(data = NA, nrow = n, ncol = n)
tauvalues <- as.data.frame(tauvalues)
colnames(tauvalues) <- names(ev)
rownames(tauvalues) <- names(ev)

# Loop over all variables
for(i in 1:n) {
  
  # Loop over all variables (for each variable)
  for(j in 1:n) {
    
    # Make an object of correlation coefficients and p-values
    corr <- cor.test(ev[, i], ev[, j], method = "kendall")
    
    # Save p-values associated with this object
    if(i != j) { 
      
      pvalues[i, j] <- corr$p.value 
      
    }
    
    # Save tau-values associated with this object
    if(i != j) { 
      
      tauvalues[i, j] <- corr$estimate
      
    }
  }
}


pvalues
tauvalues

## Simplified display of results (with symbols indicative of 'significance')

symnum(cor(ev, method = "kendall"), abbr = FALSE) # Alternatively: symnum(cor(ev, method = "kendall"), abbr = FALSE)


# Plot correlation matrix with corrplot
# corrplot requires that the data are stored in a matrix format
corr_matrix <- as.matrix(tauvalues)
p_matrix <- as.matrix(pvalues)

# Correlations are represented for each pair of variables by color and shape. More elongated ellipses equal higher correlations
# p-values are represented with symbols: '*' = 0 - 0.05, 'no symbol' = 0.1 - 1
corrplot(corr = corr_matrix, 
         method = "ellipse", 
         type = "upper", 
         diag = FALSE, 
         outline = TRUE, 
         order = "alphabet",
         tl.cex = 0.8,
         tl.col = "black", 
         tl.srt = 45, 
         tl.offset = 2, 
         p.mat = p_matrix, 
         sig.level = 0.05, 
         insig = "label_sig",
         pch.cex = 1)

## PCA ordination of environmental variables

pca.r <- rda(ev, scale = TRUE) #The 'scaling' argument, which specifies the scaling of axes, is only needed in 'summary' and plot' commands
summary(pca.r) # Note that default is 'scaling = 2' (= correlation biplot scaling; optimising intervariable correlations)

# Simple, 'raw' PCA plot (not supported by ggplot):
plot(pca.r) 

## Plotting environmental variables as vectors in the PCA ordination

# Focus on the relationships between environmental variable vectors
# Extract species scores 
pca_variable <- scores(pca.r, display = "species", scaling = 2) %>% as.data.frame() # Scaling parameter has to be set

# Create a column in the data frame for variable names
pca_variable$variable <- rownames(pca_variable)

# Create PCA plot
ggplot() +
  
  # Add arrows for species scores
  geom_segment(data = pca_variable, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               color = "deepskyblue4") +
  
  # Add variable names
  geom_text(data = pca_variable, aes(x = PC1, y = PC2, label = variable),
            color = "black", size = 3, nudge_x = 0.05, nudge_y = 0.05) +
  
  # Ensure equal axis length
  coord_fixed(ratio = 1) +
  
  # Set axis limits
  xlim(-1.5, 1.5) + ylim(-1.5, 1.5) +
  
  # Label axes
  labs(x = "PC1", y = "PC2") +
  
  # Select theme
  theme_bw() + 
  
  # Exclude panel grid from the plot
  theme(panel.grid.minor = element_blank())



#######################################################################
#*** Standard script for ordination analyses of species composition***#
#######################################################################

##*** DCA ***##

# Remove plots without any species and species that are not recorded in any plot
occupied_plots <- rowSums(dta) != 0
recorded_species <- colSums(dta) != 0
dta_dca <- dta[occupied_plots, recorded_species]

# DCA ordination
dca.av <- decorana(dta_dca)
dca.av


# Extract site scores 
# Note that origin = FALSE implies that origo of the ordination diagram is moved 
# from the centroid to the lower end of each axis
dca <- scores(dca.av, display = "sites", origin = FALSE) %>% as.data.frame()

# Write an xlsx file to the working directory with the site scores
getwd() # working directory
write_xlsx(dca, "dca.xlsx")

# Create a simple ordination diagram, showing samples with respect to axes 1 and 2

# Create a column in the data frame for plot names
dca$plot <- rownames(dca)

# Create DCA plot
ggplot(dca, aes(x = DCA1, y = DCA2)) +
  
  # Add points for site scores
  geom_point(size = 2, alpha = 1, color = "deepskyblue4") +
  
  # Add labels (for plot ID)
  geom_text(data = dca, aes(x = DCA1, y = DCA2, label = plot),
            color = "black", size = 3, nudge_x = 0.02, nudge_y = 0.02) +
  
  # Ensure equal axis length
  coord_fixed(ratio = 1) +
  
  # Add axis labels
  labs(x = "DCA1", y = "DCA2") +
  
  # Select theme
  theme_bw() +
  
  # Exclude panel grid from the plot
  theme(panel.grid.minor = element_blank())


# Extracting DCA axes as variables:

# Note that origin = FALSE implies that origo of the ordination diagram is moved 
# from the centroid to the lower end of each axis
# origin = TRUE retains the origin at the centroid

dca_var <- scores(dca.av, display = "species", origin = FALSE) %>% as.data.frame()

##*** GNMDS ***##

# GNMDS with geodetic correction of unreliable dissimilarities for PD > 0.8 [carefully consider which epsilon value to choose]

# Making proportional dissimilarity (=Bray-Curtis) dissimilarity matrix

dist.y <- vegdist(dta_dca, method = "bray") 
dist.y # Displays the dissimilarity matrix; optional
# Replacing unreliable distances (B-C > 0.85 by geodetic distances, using the stepacross algorithm 
# Note that the optimal value for epsilon is dataset-specific

geodist.y <- isomapdist(dist.y, epsilon = 0.85)

# For data sets in which one or more observations are weakly related to the rest (disjunct data sets), geodetic correction does not work unless
# ... a lower value for epsilon is chosen. In such cases, find the highest value for epsilon that provides results

geodist.y  # Displays the dissimilarity matrix; optional

# Select dimensionality for the GNMDS; k-dimensional GNMDS (k = 2, 3, ...)

k <- 2 # k determines the number of dimensions in the ordination; typically we start with the 4-dimensional solution, thereafter reduce the number of dimensions

# Define a general, empty object called mds:

mds <- NULL

# Make a number of MDSs of your choice (here by default set to 100) from random initial starting configurations, and allocate the solutions to the mds object:
# Remember to fit the right value of k into the statement 'k = ...' below

for (i in 1:100) {
  
  mds[[i]] <- monoMDS(geodist.y, matrix(c(runif(dim(dta_dca)[1] * k)), nrow = dim(dta_dca)[1]), 
                      k = k, model = "global", maxit = 200, smin = 1e-7, sfgrmin = 1e-7)
  
}

# The mds object is a list consisting of 100 "subobjects" which themselves are lists
# Extract the stress values as a vector 
mds.stress <- unlist(lapply(mds, function(x) { x$stress })) 

# Consider the stress values for the 100 MDSs:

mds.stress

# Order the stress values for the 100 MDSs:

order(mds.stress)

# Save the order in a vector

ordered <- order(mds.stress)
ordered

# Find the stress values for the solutions with the lowest and second lowest stress:

mds.stress[ordered[1]]
mds.stress[ordered[2]]

# Scale axes to half-change units and perform a varimax rotation by postMDS

# Note: In the most recent version of vegan, the postMDS function fails with dist = geodist because the corrected distances 
# do not have a fixed maximum value ("ceiling"). The error message says:
# "In postMDS(mds[[ordered[1]]], geodist.y, pc = TRUE, center = TRUE,  :  halfchange requested, but ceiling distance is unknown, using 0.8"
# No scores are produced
# On "https://rdrr.io/rforge/vegan/man/metaMDS.html" this is explained as follows:
# "Half-change scaling scales the configuration so that one unit means halving of community similarity from 
# replicate similarity. Half-change scaling is based on closer dissimilarities where the relation between 
# ordination distance and community dissimilarity is rather linear (the limit is set by argument threshold). 
# If there are enough points below this threshold (controlled by the parameter nthreshold), dissimilarities 
# are regressed on distances. The intercept of this regression is taken as the replicate dissimilarity, 
# and half-change is the distance where similarity halves according to linear regression. 
# Obviously the method is applicable only for dissimilarity indices scaled to 0-1, such as Kulczynski, 
# Bray-Curtis and Canberra indices. If half-change scaling is not used, the ordination is scaled to the same 
# range as the original dissimilarities."
# Solution: In the postMDS command, the distance matrix called must be dist.y and not geodist.y!!

mds.best <- postMDS(mds[[ordered[1]]], dist.y, pc = TRUE, center = TRUE, halfchange = TRUE, threshold = 0.8, nthreshold = 10)
mds.best
mds.secbest <- postMDS(mds[[ordered[2]]], dist.y, pc = TRUE, center = TRUE, halfchange = TRUE, threshold = 0.8, nthreshold = 10)
mds.secbest

# Extract axes
gnmds <- as.data.frame(mds.best$points)
gnmds


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

# Procrustes plot (plot ID next to the blue points, i.e., the best NMDS solution)
ggplot(procrustes_data) +
  geom_segment(aes(x = X1_Orig, y = Y1_Orig, xend = X1_Rot, yend = Y1_Rot),
               color = "green", linewidth = 1) +
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


##*** Correlations between GNMDS- and DCA-axes ***##

# Calculation of correlation coefficients:

cor.test(dca$DCA1, gnmds$MDS1, method = "kendall")
cor.test(dca$DCA1, gnmds$MDS2, method = "kendall")
cor.test(dca$DCA1, gnmds$MDS3, method = "kendall") 
cor.test(dca$DCA1, gnmds$MDS4, method = "kendall")

cor.test(dca$DCA2, gnmds$MDS1, method = "kendall")
cor.test(dca$DCA2, gnmds$MDS2, method = "kendall")
cor.test(dca$DCA2, gnmds$MDS3, method = "kendall") 
cor.test(dca$DCA2, gnmds$MDS4, method = "kendall")

cor.test(dca$DCA3, gnmds$MDS1, method = "kendall")
cor.test(dca$DCA3, gnmds$MDS2, method = "kendall")
cor.test(dca$DCA3, gnmds$MDS3, method = "kendall") 
cor.test(dca$DCA3, gnmds$MDS4, method = "kendall")

cor.test(dca$DCA4, gnmds$MDS1, method = "kendall")
cor.test(dca$DCA4, gnmds$MDS2, method = "kendall")
cor.test(dca$DCA4, gnmds$MDS3, method = "kendall") 
cor.test(dca$DCA4, gnmds$MDS4, method = "kendall")

#######################################################################################
#*** Interpretation of ordination results by correlation analyses and simple tests ***#
#######################################################################################

#### Choose axes to be analysed; these are named 'ax1' ... 'ax4'

# The general formula for the correlation between an ordination axis and an explanatory variable is:
# Choose ordination axis:

ax <- gnmds$MDS2  # etc.

cor.test(ax, ev[, 1], method = "kendall")
cor.test(ax, ev[, 2], method = "kendall")
cor.test(ax, ev[, 3], method = "kendall")
cor.test(ax, ev[, 4], method = "kendall")
cor.test(ax, ev[, 5], method = "kendall")
cor.test(ax, ev[, 6], method = "kendall")
cor.test(ax, ev[, 7], method = "kendall")
cor.test(ax, ev[, 8], method = "kendall")
cor.test(ax, ev[, 9], method = "kendall")
cor.test(ax, ev[, 10], method = "kendall")
cor.test(ax, ev[, 11], method = "kendall")
cor.test(ax, ev[, 12], method = "kendall")
cor.test(ax, ev[, 13], method = "kendall")
cor.test(ax, ev[, 14], method = "kendall")
cor.test(ax, ev[, 15], method = "kendall")
cor.test(ax, ev[, 16], method = "kendall")
cor.test(ax, ev[, 17], method = "kendall")
cor.test(ax, ev[, 18], method = "kendall")
cor.test(ax, ev[, 19], method = "kendall")
cor.test(ax, ev[, 20], method = "kendall")


##################################################
#*** Standard script for ordination diagrams ***##
##################################################

#### Choose which (two) axes to plot, and name these pax1 [plotting axis 1] and pax2

pax1 <- gnmds$MDS1
pax2 <- gnmds$MDS2
pax3 <- gnmds$MDS3
pax4 <- gnmds$MDS4

#### Simple diagram with points ####
# Create a baseline plot
# Remember to use the function coord_fixed() with 'ratio = 1' to make the units on the two axes equal
baseline_plot <- ggplot() + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  coord_fixed(ratio = 1) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Ordination diagram with black dots for the OU's:
baseline_plot + geom_point(data = gnmds, aes(x = MDS1, y = MDS2), size = 2)

# Create a column for terrain shape (MATer), a variable with 6 levels (0, ..., 5)
gnmds$MATer <- factor(MATer)
gnmds$MATer

# Select color, shape, and size for the points in the ordination diagram (for each of the levels in MATer)
colors <- c("0" = "lightblue4", "1" = "lightblue4", "2" = "blueviolet", "3" = "blueviolet", "4" = "red", "5" = "red")
shapes <- c("0" = 1, "1" = 16, "2" = 1, "3" = 16, "4" = 1, "5" = 16)  # 16 = open circle, 16 = closed circle
sizes <- c("0" = 2, "1" = 2, "2" = 3, "3" = 3, "4" = 3, "5" = 3) # Adjust size

# Plot ordination diagram
baseline_plot +
  geom_point(data = gnmds, aes(x = MDS1, y = MDS2, color = MATer, shape = MATer, size = MATer)) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  scale_size_manual(values = sizes) +
  theme(legend.position = "right")


###############################################
#*** Analyses for graphical interpretation ***#
###############################################

## Adding 'envfit vectors' to standard diagram (note that the r^2 given for the vectors are squared Pearson's r)

# Combine the ordination axes in question to a data frame 

ordx <- gnmds[, 1:2]
is.data.frame(ordx)
dim(ordx)

# Optional: remove explanatory variables that are not to be plotted
nvx <- ev[ , c(2:6)]
nvx

# Calculate envfit vectors
ordvec12 <- envfit(ord = ordx, env = nvx, permutations = 999)

ordvec12$vectors$arrows # Gives coordinates for vector arrowheads, standardised to length = 1
ordvec12$vectors$r # R-squared for variables
ordvec12 # Both, written as table

# Create data frame for site scores (arrow coordinates)
envfit_arrows <- as.data.frame(ordvec12$vectors$arrows)

# Create a column for labels
envfit_arrows$labels <- rownames(envfit_arrows)

# Plotting NMDS:
# Vector arrows plotted onto standard ordination diagram (note that this requires 'origin = TRUE' in DCA)
baseline_plot + 
  geom_point(data = ordx, aes(x = MDS1, y = MDS2), size = 3) +
  geom_segment(data = envfit_arrows, aes(x = 0, y = 0, xend = MDS1, yend = MDS2), 
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               color = "deepskyblue4") +
  geom_text(data = envfit_arrows, aes(x = MDS1, y = MDS2, label = labels), size = 3, nudge_x = 0.025, nudge_y = 0.025)


# Note that different colours for subsets of ev can be obtained by creating a new variable, e.g.:

# Specify colors for the arrows 
envfit_arrows$color <- c("blue", "firebrick2", "blue", "blue", "blue")

# Create ordination plot with arrows of different colors
gnmds_plot <- baseline_plot + 
  geom_point(data = ordx, aes(x = MDS1, y = MDS2), size = 3) +
  geom_segment(data = envfit_arrows, aes(x = 0, y = 0, xend = MDS1, yend = MDS2, color = color), 
               arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
  geom_text(data = envfit_arrows, aes(x = MDS1, y = MDS2, label = labels), size = 3, nudge_x = 0.025, nudge_y = 0.025) +
  scale_color_manual(values = c("blue", "firebrick2"),
                     name = "Variable group",
                     labels = c("Group 1", "Group 2"))

# Display the ordination plot
gnmds_plot

## Making isoline diagram, e.g., for MATer based on the standard ordination diagram using the ordisurf commmand; exemplified by axes 1 against 2
selected_variable <- MATer # Select variable

# Fit isolines
ordisurf_fit <- ordisurf(ordx, selected_variable, display = "sites", plot = FALSE)  

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
  labs(fill = "MATer")

# str(ordisurf_fit) shows the content of the complex object ordisurf_fit
df <- sum(ordisurf_fit$edf) - 1
df
df <- round(df, digits = 1)  # the number of degrees of freedom (1 for the Intercept needs to be subtracted), rounded off to 2 digits after decimal point
vf <- ordisurf_fit$deviance / ordisurf_fit$null.deviance # Fraction of variation explained by the GAM model implicit in the os function:
vf
vf <- signif(vf, digits = 3)  # Rounded to 3 decimals
dfc <- as.character(df) # Conversion to 'character'
vfc <- as.character(vf) # Conversion to 'character'
dfc <- paste("df =", dfc, sep = " ") # combines "df =" with the number of degrees of freedom
vfc <- paste("ve =", vfc, sep = " ") # combines "ve =" with the number of degrees of freedom
dfc
vfc

# Combine the strings into a single annotation
annotation_text <- paste("MATer\n", dfc, "\n", vfc, sep = "")

# Add variable name etc to the upper right corner of the ordination diagram
gnmds_plot + 
  geom_contour(data = isoline_grid, aes(x = x, y = y, z = z), color = "black") +
  geom_contour_filled(data = isoline_grid, aes(x = x, y = y, z = z), alpha = 0.25) +
  annotate("label", x = 1.3, y = 1, label = annotation_text, 
           hjust = 0, vjust = 1, size = 3, color = "black", fill = "white", 
           label.size = 1, fontface = "plain") +
  labs(fill = "MATer")

  