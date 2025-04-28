#######################################################
# P1 Kendall's tau correlation testing
#######################################################

# Import libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(corrplot)

# Load data ----------------------------------------------------------------------
#' Load environmental variables from file P1_PONDenvvar.csv (variables as columns)

# Check and change the working directory if necessary
getwd()
#setwd("C:/Users/yourUserName/.../methods-of-gradient-analysis")

# Load data (environmental variables in several ponds)
envvar <- read.csv("P1_PONDenvvar.csv", sep = ",") %>% 
	as.data.frame() # make sure it is read as data frame

#' Column descripton for raw data:
#' PareaRAW = Pond area
#' AvgWidRAW = Average width of the pond
#' MaxDepRAW = Maximum depth of the pond
#' AltRAW = Altitude, where is the pond located
#' CndRAW = Conductivity (vannets ledningsevne)
#' pHRAW = pH

# Attach variables to names
attach(envvar)
ev <- names(envvar)
ev

# Number of variables
n <- ncol(envvar)
n

# Get correlation coefficients and p-values ----------------------------------
# Make empty datasets to be filled with data in an iterative loop below
# For p-values
pvalues <- matrix(data = NA, nrow = n, ncol = n)
pvalues <- as.data.frame(pvalues)
colnames(pvalues) <- ev
rownames(pvalues) <- ev

# For tau-values
tauvalues <- matrix(data = NA, nrow = n, ncol = n)
tauvalues <- as.data.frame(tauvalues)
colnames(tauvalues) <- ev
rownames(tauvalues) <- ev

# Loop over all variables
for(i in 1:n) {
  
  # Loop over all variables (for each variable)
  for(j in 1:n) {
      
      # Make an object of correlation coefficients and p-values
      corr <- cor.test(envvar[, i], envvar[, j], method = "kendall")
      
      # Insert p-value in data frame
      pvalues[i, j] <- corr$p.value 
      
      # Insert tau value in data frame
      tauvalues[i, j] <- corr$estimate
      
  }
}

# look at the resulting p estimates
pvalues

# look at the tau estimates
tauvalues

# From here: Alternative ways to display results --------------------------------

# Simplified display of results (with symbols indicative of 'significance')
symnum(cor(envvar, method = "kendall"), abbr = FALSE)

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
         tl.col = "black", 
         tl.srt = 0, 
         tl.offset = 2, 
         p.mat = p_matrix, 
         sig.level = 0.05, 
         insig = "label_sig")

# If you want to, run "?corrplot" for additional layout options

# Making a vector of environmental variable names
evnames <- names(ev)

# Histograms for environmental variables displayed as a panel

# Reshape environmental data from 'short' to 'long' format to facilitate plotting 
long_ev <- envvar %>%
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
