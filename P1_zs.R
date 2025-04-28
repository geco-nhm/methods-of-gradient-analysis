#######################################################
# P1 Zero-skewness transformation
#######################################################

# Import libraries
library(e1071) # needed for skewness function
library(ggplot2)
library(dplyr)

# Check and change the working directory if necessary
getwd()
#setwd("C:/Users/yourUserName/.../methods-of-gradient-analysis")

# Import data from file
env.var <- read.csv("P1_Oppkuven_environmental_variables.csv") %>% 
	as.data.frame()

# Attach data to the R environment
attach(data)
names(data)

# x is the variable to be transformed. c is the unknown parameter to be estimated
# transformation constant that needs to be found.

# if skewness < 0 x is transformed by exp(c * x), if skewness > 0 x is transformed by log(c + x)
# Starts by defining x as the first variable
x <- [insert variable name here]  
scalex <- function(x, c) { 
	if(skewness(x, na.rm = TRUE) < 0) 
		return(exp(c * x)) 
	else 
		return(log(x + c))
}

minskew <- function(x) {
	cmin <- min(x) - 10 * (max(x) - min(x));
	cmax <- max(x) + 10 * (max(x) - min(x));
	if(skewness(x, na.rm = TRUE) >= 0 && cmin < -min(x))
		cmin <- -min(x) 	
	cmid <- (cmin + cmax) / 2;
	skew <- skewness(scalex(x, cmid), na.rm = TRUE);
	while (abs(skew) > 1 * 10^-05 && min(abs(c(cmax, cmin) - cmid)) > 10^-10) {
		print(c(cmin, cmid, cmax, skew));
		sleft <- skewness(scalex(x, (cmid + cmin) / 2), na.rm = TRUE)
		sright <- skewness(scalex(x, (cmid + cmax) / 2), na.rm = TRUE)
		if (abs(sleft) < abs(skew) && abs(sleft) < abs(sright)) {
			cmax <- cmid
			skew <- sleft
		} else if (abs(sright) < abs(skew)) {
			cmin <- cmid
			skew <- sright
		} else {
			cmin <- (cmin + cmid) / 2;
			cmax <- (cmax + cmid) / 2;
		}
		cmid <- (cmin + cmax) / 2;
	}
	return(list(c = cmid, skew = skew));
}

res <- minskew(x)

# Standardised skewness of the original (untransformed) variable
skewness(x) / (6 / length(x))^0.5

# c value and skewness after zero skewness transformation
res$c
res$skew

# standardised skewness of the transformed variable
(res$skew) / (6 / length(x))^0.5

# Ranging and printout of the ranged variable
standx <- scalex(x, res$c)
rangx <- (standx - min(standx)) / (max(standx) - min(standx))
rangx

# Plot a histogram
ggplot() + aes(rangx) + geom_histogram(bins = 8, color = "black", fill = "grey") + theme_minimal() 

# We obtain the zs-transformed version of the variable, 'zs[variable name]' as:
zs[Variable name] <- rangx
