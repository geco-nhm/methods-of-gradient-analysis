#########################################################################
# P5: Variation partitioning
#########################################################################

#' This script exemplifies commands involved in variation partitioning.
#' We use the swamp forest data (three files; see P5_Task.doc for info).
#' In this script the three files are imported as SITES,
#' SPECIES and ENV. Note that when you are 
#' to do the exercise, you can use the BSK1E_8var.csv file, which just 
#' contains the 8 pre-selected environmental variables. We import the 
#' zero-skewness transformed environmental variables (lower matrix).
#' Use spreadsheet ZSDATA which contains the 53 zero-skewness transformed 
#' variables.

# Load libraries
library(vegan)
library(dplyr)

# Loading and attaching data -------------------------------------------

# Check and change the working directory if necessary
getwd()
#setwd("C:/Users/yourUserName/.../methods-of-gradient-analysis")

# Import data from file
SITES <- read.csv("P5_BSK1A.csv", header = TRUE) %>% as.data.frame()
SPECIES <- read.csv("P5_BSK1S.csv", header = TRUE) %>% as.data.frame()
ENV <- read.csv("P5_BSK1E_zsdata.csv", header = TRUE) %>% as.data.frame()

## Attach variables to names ----
attach(SITES)
attach(SPECIES)
attach(ENV)
names(SITES)
names(SPECIES)
names(ENV)

#' SITES contains one variable called 'AFF'. 
#' This is an factor variable with 11 levels. 
#' In order to tell R that this is a factor variable: 
SITE <- factor(AFF)

# CCA -------------------------------------------------------------------
# We find the variation attributed to SITE
CCA0 <- cca(SPECIES ~ SITE) # Use of the entire matrix of constr. vars seems perhaps not to work with vegan 2.0
CCA0

# The number we are looking for, is 'Constrained 1.597' which tells that the variable SITE
# explains 1.597 intertia units (out of a total of 6.767 units)
# We also note that since SITE is a factor variable with 11 levels, 10 constraining axes ('degrees of freedom'
# are needed)

# Finding if this is more than attributable to a random variable
testCCA <- permutest(CCA0, permutations = 999)  # permutest.cca replaced by permutest() in vegan 2.0
testCCA

# Commands that will be used:

# We find the variation attributable to the first variable, VertRan
CCA1 <- cca(SPECIES ~ VertRan)
CCA1

# The variable VertRan explains 0.1490 inertia units
testCCA1 <- permutest(CCA1, permutations = 999)
testCCA1

# In order to find significant 'additional' variation by other variables (e.g. SlopeAvg) after the
# variation attributable to VertRan has been explained, we perform a partial constrained ordination:
CCA2 <- cca(SPECIES ~ SlopeAvg + Condition(VertRan))
CCA2

#etc.

# The variable SlopeAvg explains 0.0667 inertia units not also explained by VertRan 
# (This means that the variation attributable to VertRan is first partialled out, and then the
# residual variation attributable to SlopeAvg is found)

# Variation partitioning works like building of a GLM model: For each variable group,
# in the 'first round' all variables are tested for significance one by one
# The 'best' significant variable is selected and used as 'Conditional variable' in
# 'round 2' when each of the individually significant variables are tested for significant
# additional variation explained
# The process is stopped when no variable adds significantly to the variation explained


# We now have all tools we need for variation partitioning including for forward selection of variables
# and for quantifying variation explained by significant variables in each group and variation explained by
# one group not shared by other groups
