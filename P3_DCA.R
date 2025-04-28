#######################################################
# P3 Detrended Correspondence Analysis (DCA)
#######################################################

# Import libraries:
library(vegan)
library(mgcv)
library(ggplot2)
library(dplyr)

# Loading and attaching data -------------------------------------------

# Check and change the working directory if necessary
getwd()
#setwd("C:/Users/yourUserName/.../methods-of-gradient-analysis")

# Import data from file
env.var <- read.csv("P1_Oppkuven_environmental_variables.csv") %>% 
	as.data.frame()

dca.tot <- read.csv("P1_Oppkuven_species.csv") %>% 
	as.data.frame() 

stand.data <- data.frame(Stand = dca.tot$Stand)

dca.tot <- select(-"Stand") # Remove first column of data (stand) 

# Attach variables to names
attach(dca.tot)
names(dca.tot)

attach(stand.data)
names(stand.data)

attach(env.var)
names(env.var)

# Converting variable from numeric to factor:
mode(Stand)
stand <- as.factor(Stand)
stand

# DCA ----------------------------------------------------------------
# Running DCA on species-plot matrix:
dca.r <- decorana(dca.tot)
summary(dca.r)

# Extracting DCA-axes for plot scores:
# Note that origin = FALSE implies that origo of the ordination diagram is moved 
# from the centroid to the lower end of each axis
dca_sites <- scores(dca.r, display = "sites", origin = FALSE) %>% as.data.frame()

# Saving DCA axes as a csv file
write.csv(dca_sites, "dca.csv", rownames = FALSE)


# Plotting -----------------------------------------------------------
# Plotting DCA - baseline plot:
dca_plot <- ggplot() + coord_fixed(ratio = 1) + theme_bw() + theme(panel.grid.major = element_blank())

# Plotting DCA - with points:
dca_plot + geom_point(data = dca_sites, aes(x = DCA1, y = DCA2))

# - with plot numbers:
dca_sites$labels <- c(1:100)
dca_plot + geom_text(data = dca_sites, aes(x = DCA1, y = DCA2, label = labels), size = 3)


# Subsets as different colours:
dca_sites$stand_level <- stand
dca_plot + geom_point(data = dca_sites, aes(x = DCA1, y = DCA2, color = stand_level))

# Subsets as different symbols:
dca_plot + geom_point(data = dca_sites, aes(x = DCA1, y = DCA2, shape = stand_level))

# Subsets with numbers and different colours:
dca_plot + geom_text(data = dca_sites, aes(x = DCA1, y = DCA2, label = labels, color = stand_level), size = 3)


# Extracting DCA-axes for species scores:
dca_species <- scores(dca.r, display = "species", origin = TRUE) %>% as.data.frame()

# Allocating all species names to "label":
dca_species$labels <- names(dca.tot)
dca_species$labels

# Plotting species names:
dca_plot + geom_text(data = dca_species, aes(x = DCA1, y = DCA2, label = labels), size = 3) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted")

# Create column for species groups
dca_species$species_groups <- factor(c(rep(x = 1, times = 22),
                                       rep(x = 2, times = 31),
                                       rep(x = 3, times = 21)))

# Plotting species groups in different colors:
dca_plot + geom_text(data = dca_species, aes(x = DCA1, y = DCA2, label = labels, color = species_groups), size = 3) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted")




# Extracting DCA-axes with origin = TRUE:
dca_origin <- scores(dca.r, display = "sites", origin = TRUE) %>% as.data.frame()

# Create a column for labels
dca_origin$labels <- c(1:100)

# Create a column for groups
dca_origin$stand <- factor(Stand)



# Inspecting the structure of the environmental data:
str(env.var)

## Biplot with DCA ordination of sites/plots and environmental variables
## both using origin = TRUE here
dca.z <- envfit(ord = dca_origin[, 1:2], env = env.var, permutations = 999)

# Create data frame for site scores (arrow coordinates)
envfit_arrows <- as.data.frame(dca.z$vectors$arrows)

# Create a column for labels
envfit_arrows$labels <- rownames(envfit_arrows)

# Plotting species names:
dca_plot + geom_text(data = dca_origin, aes(x = DCA1, y = DCA2, label = labels, color = stand), size = 3) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_segment(data = envfit_arrows, aes(x = 0, y = 0, xend = DCA1, yend = DCA2), 
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               color = "deepskyblue4") +
  geom_text(data = envfit_arrows, aes(x = DCA1, y = DCA2, label = labels), size = 3, nudge_x = 0.025, nudge_y = 0.025)



# Isoline diagram on environmental variable:
TI1
TI1r <- round(TI1, digits = 2)
TI1r
mode(TI1r) <- "character"
## NB, you may use untransformed data/environmental variables instead

# Fit isolines
ordisurf_fit <- ordisurf(dca.r, TI1, display ="sites", plot = FALSE)  

# Extract isoline grid
isoline_grid <- expand.grid(x = ordisurf_fit$grid$x, y = ordisurf_fit$grid$y)

# Convert the isoline grid from matrix to vector format
isoline_grid$z <- as.vector(ordisurf_fit$grid$z)

# Plotting species names:
dca_plot + 
  geom_text(data = dca_origin, aes(x = DCA1, y = DCA2, label = labels, color = stand), size = 3) + 
  geom_contour(data = isoline_grid, aes(x = x, y = y, z = z), color = "black") +
  geom_contour_filled(data = isoline_grid, aes(x = x, y = y, z = z), alpha = 0.25) +
  labs(fill = "TI1")

# Create vectors for DCA1 and DCA2
dca1 <- dca_sites$DCA1
dca2 <- dca_sites$DCA2


# Kendall's correlation test (non-parametric):
cor.test(dca1, Mois, method = "kendall")
cor.test(dca1, Litter, method = "kendall")
cor.test(dca1, BasalA, method = "kendall")
cor.test(dca1, Inclin, method = "kendall")
cor.test(dca1, HeatI, method = "kendall")
cor.test(dca1, SoilDMe, method = "kendall")
cor.test(dca1, RoughMe, method = "kendall")
cor.test(dca1, InclMax, method = "kendall")
cor.test(dca1, GapAvg, method = "kendall")
cor.test(dca1, LossOI, method = "kendall")
cor.test(dca1, pH, method = "kendall")
cor.test(dca1, Ca, method = "kendall")
cor.test(dca1, Mg, method = "kendall")
cor.test(dca1, TotN, method = "kendall")
cor.test(dca1, P.Al, method = "kendall")
cor.test(dca1, TI1, method = "kendall")
cor.test(dca1, TI2, method = "kendall")
cor.test(dca2, Mois, method = "kendall")
cor.test(dca2, Litter, method = "kendall")
cor.test(dca2, BasalA, method = "kendall")
cor.test(dca2, Inclin, method = "kendall")
cor.test(dca2, HeatI, method = "kendall")
cor.test(dca2, SoilDMe, method = "kendall")
cor.test(dca2, RoughMe, method = "kendall")
cor.test(dca2, InclMax, method = "kendall")
cor.test(dca2, GapAvg, method = "kendall")
cor.test(dca2, LossOI, method = "kendall")
cor.test(dca2, pH, method = "kendall")
cor.test(dca2, Ca, method = "kendall")
cor.test(dca2, Mg, method = "kendall")
cor.test(dca2, TotN, method = "kendall")
cor.test(dca2, P.Al, method = "kendall")
cor.test(dca2, TI1, method = "kendall")
cor.test(dca2, TI2, method = "kendall")


# or like this:
####### Summary of Kendall's tau for DCA axis 1#######
p.val <- NULL # create empty objects for the p.values
tau.val <- NULL # creates empty object for the t-values.
p <- NULL

# A Loop that calculates Kendall's tau and p-values for all the correlations tests.
for (i in 1:ncol(env.var)) {
  
  z <- cor.test(dca1, env.var[, i], method = "kendall")
  p.val[i] <- round(z$p.value, digits = 5)
  tau.val[i] <- round(z$estimate, digits = 5)
  
  # Prints a star when the p-value is less than 0.05
  if (p.val[i] < 0.05) {
    
    p[i] <- "***"
    
  } else {
    
    p[i] <- " "
    
  }
}

p.val
tau.val

test.summary.dca1 <- data.frame(variable = names(env.var), p.val, tau.val, p)
test.summary.dca1

# Summary of Kendall's tau correlation test for DCA axis 2
p.val2 <- NULL # create empty objects for the p.values
tau.val2 <- NULL # creates empty object for the t-values.
p2 <- NULL
for (i in 1:ncol(env.var)) {
	z <- cor.test(dca2, env.var[, i], method = "kendall")
	p.val2[i] <- round(z$p.value, digits = 4)
	tau.val2[i] <- round(z$estimate, digits = 4)
	if (p.val2[i] < 0.05) {
	  
	  p2[i] <- "***"
	
	  } else {
	    
	    p2[i] <- " "
	}
}

p.val2
tau.val2
test.summary.dca2 <- data.frame(variable = names(env.var), p.val2, tau.val2, p2)
test.summary.dca2



#Split-plot GLM:
spaMois <- aov(dca1 ~ + 1) # Calculating the variation (sum of squares) in DCA1
spaMois
spaMois <- aov(dca1 ~ Mois + Error(stand)) # Here the split-plot GLM starts
summary(spaMois)
coef(spaMois)
spaLitter <- aov(dca1 ~ Litter + Error(stand))
summary(spaLitter)
spaBasalA <- aov(dca1 ~ BasalA + Error(stand))
summary(spaBasalA)
spaInclin <- aov(dca1 ~ Inclin + Error(stand))
summary(spaInclin)
spaHeatI <- aov(dca1 ~ HeatI + Error(stand))
summary(spaHeatI)
spaSoilDMe <- aov(dca1 ~ SoilDMe + Error(stand))
summary(spaSoilDMe)
spaRoughMe <- aov(dca1 ~ RoughMe + Error(stand))
summary(spaRoughMe)

# Output can be organised like this:
## See SpliplotGLM.doc (NB this table shows GNMDS axis 2, not DCA1 which we use in the model here)
