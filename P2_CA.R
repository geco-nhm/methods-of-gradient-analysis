#######################################################
# P2 Correspondence Analysis (CA)
#######################################################

# Importing libraries:
library(readxl)
library(vegan)
library(ggplot2)
library(dplyr)

# Loading and attaching data -------------------------------------------

# Check and change the working directory if necessary
getwd()
#setwd("C:/Users/yourUserName/.../methods-of-gradient-analysis")

# Import data from file
ca.tot <- read.csv("P1_Oppkuven_species.csv") %>% 
	as.data.frame() %>%
        select(-"Stand")# Remove first column of data (stand) 

# Attach variables to names
attach(ca.tot)
names(ca.tot)

# CA -------------------------------------------------------------------
# Run CA on species-plot matrix:
ca.r <- cca(ca.tot)
summary(ca.r)

# Extracting CA-axes for plot scores:
ca_sites <- scores(ca.r, display = "sites", scaling = 1) %>% 
            as.data.frame()

# The 'scores' command works just for axes 1 and 2
# If you want to extract axes beyond the second, you will have to 
# do that directly from the ca.r object in which it is found as the h-th column 
# of sublist u under list CA, e.g. 'ca.r$CA$u[, h]' where h is a number from 1 to n

ca.r$CA$u[, 3]

# Plotting -------------------------------------------------------------------------
# Plotting CA - with points:
ca_plot <- ggplot() + 
    geom_point(data = ca_sites, aes(x = CA1, y = CA2), size = 2) +  
    coord_fixed(ratio = 1) + 
    theme_bw()
ca_plot # Inspect plot

# - with plot numbers:
labels_sites <- c(1:100)
ca_plot + geom_text(data = ca_sites, aes(x = CA1, y = CA2, label = labels_sites), 
                    color = "black", size = 2.5, nudge_x = 0.025, nudge_y = 0.025)

# Extracting CA-axes for species scores:
ca_species <- scores(ca.r, display = "species", scaling = 1) %>% as.data.frame()

# Allocating all species names to "label":
labels_species <- names(ca.tot) 
labels_species

# Plotting species names:
ggplot(ca_species) + 
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_text(data = ca_species, aes(x = CA1, y = CA2, label = labels_species), color = "black", size = 2.5) +
  theme_bw()


# Note that the axes obtained by the cca command are, at the outset, 
# scaled to unit weighted variance of plot scores (the +1 scaling).
# This is the scaling of axes implemented in the commands above
# Alternatively, to extract CA scores with Hill's scaling to use 
# for plotting with a generic plotting function
# e.g., like plot(), use one of the following commands:
# (1) As above but with scaling = -1
# (2) By inserting an extra argument in the scores command like this: 
# ca11H <- scores(ca.r, display = "sites", hill = TRUE)
# This command provides scores along the first two axes for sites, 
# and with "species" it works for species 
# If you want to extract axes separately, this works: 
# ca1 <- scores(ca.r, display = "sites", hill = TRUE)[, 1]
# Again note that the scores command does not work with axes beyond the second
