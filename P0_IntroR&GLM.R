#######################################################
# P0 Introduction to R and generalized linear models
#######################################################

# Check your working directory - if you opened R through 
# clicking this script, R/RStudio will set the working 
# directory to its containing folder. Check if this is
# correct with getwd()
getwd()

# if necessary, change your working directory with setwd()
setwd() <- getwd() # replace with "C:/users/yourname/methods-of-gradient-analysis"

# Load and inspect data ----------------------------------------------------------
# Import data from P0_DamVar.csv
data <- read.csv("P0_DamVar.csv", sep = ",")

# For import of files, remember to include the whole path to the file, or ensure that the file is placed in the working directory

# Attach data to enable working with variables (data columns) 
# directly, instead of using the standard format (e.g. data$variable). 
# NB! Attaching data is not recommended if 
# you need to work with multiple datasets, or if any of the 
# variable names have the same name as a function or something else

attach(data)
names(data)
str(data)  # Gives overview of data structure
head(data)  # Gives the first observations of each variable

# R code is case sensitive, breaks such as "space" are not allowed, decimal sign may be changed
# from default '.' to ',' by adding 'dec = ","' in opening 'data' command. Norwegian computers use 
# semicolon as separator, while others use comma as separator, etc.
# Note that single quotes '' are used to delimit statements, double quotes are part of the R command

parea
Parea

# Look at the entire matrix, column/vector-subset and row
data

data[, 1]

data[1, ]

# Allocation by using the arrow, no header
area <- data[, 1]
area

# Extracting
area[13]
# Or from data matrix, equals
data[13, 1]
AvgWid[3]

# Mode, data types
mode(Alt)
mode(Fluct)	
	
# Change data type from numeric to factor
is.factor(Fluct)
Fluct <- as.factor(Fluct)
is.factor(Fluct)

# Plot data -----------------------------------------------------------------
# Import library
library(ggplot2)

#Create a baseline plot
my_ggplot <- ggplot(data, aes(x = Alt))
my_ggplot

# Add layers
my_ggplot + geom_histogram()
my_ggplot + geom_histogram(bins = 10)
my_ggplot + geom_point(aes(y = Species))

# Create variable for color
data$Color <- c(rep(x = 3, times = 10), rep(x = 4, times = 54))

# Plot different colors
my_ggplot <- ggplot(data, aes(x = Alt))
my_ggplot + geom_point(aes(y = Species, color = factor(Color)))
my_ggplot + geom_text(aes(y = Species, label = labels)) 

# Correlation test
cor.test(AvgWid, Species, method = "kendall")

# Libraries
library(zoo)
?library
?zoo	
# Returns no answer
help.search("zoo")
# Install zoo package from toolbar menu

# Make models --------------------------------------------------------------------
# Generalised linear modelling (GLM) of count data requires Poisson family errors
# We intend to build a GLM model for species richness as a function of the six 
# environmental variables Parea, AvgWid, MaxDeo, Alt, Cnd and pH
# Model selection method = stepwise forward selection of variables
library(MASS)
library(stats)
# Find null model as a reference for other models to be compared with
model0 <- glm(Species ~ + 1, family = poisson)
AIC(model0)

# 'Round 1': Checking the six environmental variables for significance
# by comparison with null model
model1a <- glm(Species ~ AvgWid, family = poisson)
anova(model0, model1a, test = "F")
summary(model1a)
model1b <- glm(Species ~ Alt, family = poisson)
anova(model0, model1b, test = "F")
summary(model1b)
model1c <- glm(Species ~ pH, family = poisson)
anova(model0, model1c, test = "F")
summary(model1c)
model1d <- glm(Species ~ MaxDep, family = poisson)
anova(model0, model1d, test = "F")
summary(model1d)
model1e <- glm(Species ~ Cnd, family = poisson)
anova(model0, model1e, test = "F")
summary(model1e)
model1f <- glm(Species ~ Parea, family = poisson)
anova(model0, model1f, test = "F")
summary(model1f)
# Significant variables are Parea,Alt,Cnd and pH
# We select the best Variable (the variable with highest F, lowest P and AIC)
# which is here Parea 

# 'Round' 2: We selectParea
# and test if other individually significant variables from 'round 1'
# improve the model compared to the model with Parea as the only variable
model2a <- glm(Species ~ Parea + Alt, family = poisson)
anova(model0, model1f, model2a, test = "F")
summary(model2a)
model2b <- glm(Species ~ Parea + Cnd, family = poisson)
anova(model0, model1f, model2b, test = "F")
summary(model2b)
model2c <- glm(Species ~ Parea + pH, family = poisson)
anova(model0, model1f, model2c, test = "F")
summary(model2c)
# We find that Alt is the best, while also Cnd and pH are significant

# 'Round' 3: We select Alt and check Cnd and pH
# At this stage, we also interactions between variables already in the model are 
# eligible for inclusion in the modelsummary(model3a)
model3a < -glm(Species ~ Parea + Alt + Cnd, family = poisson)
anova(model0, model1b, model2a, model3a, test = "F")
summary(model3a)
model3b <- glm(Species ~ Parea + Alt + pH, family = poisson)
anova(model0, model1b, model2a, model3b, test = "F")
summary(model3b)
model3c <- glm(Species ~ Parea + Alt + Parea:Alt, family = poisson)		
anova(model0, model1b, model2a, model3c, test = "F") #NS
# We find that pH is best and that Cnd is also significant


# 'Round' 4: We select pHand check Cnd and interactions
model4a <- glm(Species ~ Parea + Alt + pH + Cnd, family = poisson)
anova(model0, model1b, model2a, model3b, model4a, test = "F")	#NS
summary(model4a)
model4b <- glm(Species ~ Parea + Alt + pH + Parea:pH, family = poisson)
anova(model0, model1b, model2a, model3b, model4b, test = "F")
summary(model4b)							
model4c <- glm(Species ~ Parea + Alt + pH + Alt:pH, family = poisson)
anova(model0, model1b, model2a, model3b, model4c, test = "F")
summary(model4c)								
# We find that Parea:pH is best and that also Alt:pH is significant

# 'Round' 5: We check the interaction Alt:pH
model5a <- glm(Species ~ Parea + Alt + pH + Parea:pH + Alt:pH, family = poisson)
anova(model0, model1b, model2a, model3b, model4b, model5a, test = "F")
summary(model5a)								

# Conclusion:model5a is the best, three single variables and two interaction terms are significant


# Problems with models ----------------------------------------------------------------
# The modelling as carried out above is inadequate for two reasons: 
# (1) because the multiple-testing problem was not taken into account when
# models were built by forward selection, and 
# (2) because the data do not follow the poisson distribution!

# The first problem can be remedied by new analyses with a stricter criterion for selecting variables at each step.
# The second problem is more complex and is addressed as follows:

# According to the poisson distribution, the variance should be equal to the mean
# In our case the variance and the mean are:
var(Species) #129.0476
mean(Species) #9.25

# Here the variance is more than 13 times the mean!
# This phenomenon is called overdispersion
# and can be detected by the rule of the thumb that the residual deviance 
# is larger than the residual degrees of freedom. That is certainly the case here!
# In fact it will by more correct to assume that
# the data follow a distribution in which the variance is (theta)*mean
# where theta is given by

theta <- var(Species) / mean(Species)
theta  # theta = 13.95108

# This 'distribution' is not a proper distribution
# The solution to the problem is to repeat the analysis with 'quasipoisson errors',
# which compensates for the failure of the data to have theta = 1 (as assumed by a true poisson distribution)

# We can repeat the entire process using quasipoisson errors
# and by correcting for multiple testing:
# Note that with familiy = quasipoisson, AIC and BIC are not meaningful and will not be calculated

model0 <- glm(Species ~ + 1, quasipoisson)
model0
# 'Round' 1 
# We repeat the manual selection process, starting with Parea
model1a <- glm(Species ~ Parea, family = quasipoisson)
anova(model0, model1a, test = "F")
summary(model1a)	
# Jugded by the Pr(F) criterion alone, not even Parea is significant at the alpha=0.05 threshold
# This becomes even more clear if multiple testing is corrected for (e.g., by the Bonferroni correction)


# Model selection can also be executed by an automated procedure called 'addterm' 
# This procedure automatically takes into account that the data do not follow a poisson distribution
# i.e. it applies the quasipoisson even if we ask for poisson in cases of over- and underdispersion
# Create a full model
model0 <- glm(Species ~ + 1, family = poisson)
fullmodel <- glm(Species ~ AvgWid + Alt + pH + MaxDep + Cnd + Parea, family = poisson)
# AIC as evaluation criterion
addterm(model0, fullmodel, test = "F", sorted = TRUE)
# Note that a difference from the manual process arises because of the assumption here
# is quasipoisson errors 

# This procedure can be carried out to create increasingly complex models

# Instead, we show the procedure for the alternative penalised likelihood criterion, BIC

# BIC as evaluation criterion: "k = log(n)" where n is the number of observations)
addterm(model0, fullmodel, test = "F", sorted = TRUE, k = log(64))
# 'Round' 2 (given that Parea is accepted as significant)
model1 <- glm(Species ~ Parea, family = poisson)
addterm(model1, fullmodel, test = "F", sorted = TRUE, k = log(64))
# 'Round' 3 (given that Alt is accepted as significant)
model2 <- glm(Species ~ Parea + Alt, family = poisson)
addterm(model2, fullmodel, test = "F", sorted = TRUE, k = log(64))
# 'Round 4' (given that pH is also accepted as significant)
model3 <- glm(Species ~ Parea + Alt + pH, family = poisson)
addterm(model3, fullmodel, test = "F", sorted = TRUE, k = log(64))
# 'Round 5' (given that AvgWid is also accepted as significant)
model4 <- glm(Species ~ Parea + Alt + pH + AvgWid, family = poisson)
addterm(model4, fullmodel, test = "F", sorted = TRUE, k = log(64))
# 'Round 6' (given that MaxDep is also accepted as significant)
model5 <- glm(Species ~ Parea + Alt + pH + AvgWid + MaxDep, family = poisson)
addterm(model5, fullmodel, test = "F", sorted = TRUE, k = log(64))

# Which suggests that model 5 is the best model


# The 'ultimate test' of models is their ability to predict observations not used to parameterise the model
# A relevant question, then, is if the prediction error of the more complex models is lower than
# that of simpler models

# We set up a scheme for 4-fold cross-validation by defining a factor-type vector xv that divides the
# data into four subsets:

(xv <- as.factor(rep(x = 1:4, times = 16)))  # Note that putting the entire command in brackets implies that the result is printed on the screen

# We calculate prediction errors for the null model for 1/4 of the data in turn, 
# and use the remaining 3/4 of the data to train the model

Null.1 <- glm(Species[xv == 2 | xv == 3 | xv == 4] ~ + 1, family = quasipoisson)
summary(Null.1)$coefficients[1]  #Intercept = 2.157

# Which corresponds to an estimate of Species given by

Null.Species.Predict.1 <- exp(summary(Null.1)$coefficients[1])
Null.Species.Predict.1  #8.645163

# Calculate prediction error for xv == 1:

PENull.1 <- Species[xv == 1] - Null.Species.Predict.1
PENull.1

# Repeat for xv == 2 etc:

Null.2 <- glm(Species[xv == 1 | xv == 3 | xv == 4] ~ + 1, family = quasipoisson)
Null.Species.Predict.2 <- exp(summary(Null.2)$coefficients[1])
PENull.2 <- Species[xv == 2] - Null.Species.Predict.2
PENull.2

Null.3 <- glm(Species[xv == 1 | xv == 2 | xv == 4] ~ + 1, family = quasipoisson)
Null.Species.Predict.3 <- exp(summary(Null.3)$coefficients[1])
PENull.3 <- Species[xv == 3] - Null.Species.Predict.3
PENull.3

Null.4 <- glm(Species[xv == 1 | xv == 2 | xv == 3] ~ + 1, family = quasipoisson)
Null.Species.Predict.4 <- exp(summary(Null.4)$coefficients[1])
PENull.4 <- Species[xv == 4] - Null.Species.Predict.4
PENull.4

# Calculate total prediction error:

PENull <- sum(abs(PENull.1) + abs(PENull.2) + abs(PENull.3) + abs(PENull.4))
PENull  #534.3763


# Repeat the process for the model with Parea as the only predictor:

Model1.1 <- glm(Species[xv == 2 | xv == 3 | xv == 4] ~ Parea[xv == 2 | xv == 3 | xv == 4],quasipoisson)
summary(Model1.1)
summary(Model1.1)$coefficients[1]  #Intercept = 1.5386
summary(Model1.1)$coefficients[2]  #Parea coeff = 1.1120

# Which corresponds to an estimate of Species given by

Model1.Species.Predict.1 <- exp(summary(Model1.1)$coefficients[1] + summary(Model1.1)$coefficients[2] * Parea[xv == 1])
Model1.Species.Predict.1

# Calculate prediction error for xv == 1:

PEModel1.1 <- Species[xv == 1] - Model1.Species.Predict.1
PEModel1.1

# Repeat for xv == 2 etc:

Model1.2 <- glm(Species[xv == 1 | xv == 3 | xv == 4] ~ + Parea[xv == 1 | xv == 3 | xv == 4], family = quasipoisson)
Model1.Species.Predict.2 <- exp(summary(Model1.2)$coefficients[1] + summary(Model1.2)$coefficients[2] * Parea[xv==2])

PEModel1.2 <- Species[xv == 2] - Model1.Species.Predict.2
PEModel1.2

Model1.3 <- glm(Species[xv == 1 | xv == 2 | xv == 4] ~ + Parea[xv == 1 | xv == 2 | xv == 4], family = quasipoisson)
Model1.Species.Predict.3 <- exp(summary(Model1.3)$coefficients[1] + summary(Model1.3)$coefficients[2] * Parea[xv == 3])

PEModel1.3 <- Species[xv == 3]-Model1.Species.Predict.3
PEModel1.3

Model1.4 <- glm(Species[xv == 1 | xv == 2 | xv == 3] ~ + Parea[xv == 1 | xv == 2 | xv == 3], family = quasipoisson)
Model1.Species.Predict.4 <- exp(summary(Model1.4)$coefficients[1] + summary(Model1.4)$coefficients[2] * Parea[xv == 4])

PEModel1.4 <- Species[xv == 4] - Model1.Species.Predict.4
PEModel1.4
# Calculate total prediction error:

PEModel1 <- sum(abs(PEModel1.1) + abs(PEModel1.2) + abs(PEModel1.3) + abs(PEModel1.4))
PEModel1 

# Which shows that the model with Parea as the only predictor has lower prediction error than the null model.

# The cross-validation procedure can be applied to compare more complex models with the model with Parea as the only predictor



# Probability- or presence/absence variables are appropriately modelled by 
# binomial (or, in the case of over- or underdispersion quasibinomial) errors (i.e. logistic regression)

# We use manual forward selection to build a model for presence/absence of the first species
# Comarum palustre (myrhatt)

# Null model
model0 <- glm(ComaPal ~ + 1, family = binomial)
summary(model0) 

# The rule of the thumb, saying that the ratio of residual deviance to the residual degrees of freedom for the residuals can be used as an indicator of over- or underdispersion, is questioned for binomial models
# The reason for that is that the variance of a binomially distributed variables is not equal to the mean, but equal to mu * (1 - mu) where mu is the mean.
# Overdispersion implies that mu * (1 - mu) > var which is easily checked.

# We choose to use quasibinomial errors

model0 <- glm(ComaPal ~ + 1, family = quasibinomial)
summary(model0) 

#'Round 1': Testing each variable:
model1a <- glm(ComaPal ~ Parea, family = quasibinomial)
summary(model1a) 
anova(model0, model1a, test = "F")
model1b <- glm(ComaPal ~ AvgWid, family = quasibinomial)
summary(model1b) 
anova(model0, model1b, test = "F")
model1c <- glm(ComaPal ~ MaxDep, family = quasibinomial)
summary(model1c) 
anova(model0, model1c, test = "F")
model1d <- glm(ComaPal ~ pH, family = quasibinomial)
summary(model1d) 
anova(model0, model1d, test = "F")
model1e <- glm(ComaPal ~ Cnd, family = quasibinomial)
summary(model1e) 
anova(model0, model1e, test = "F")
# Which shows that none of the variables are significant

# Exercise: Build models for the other three species 
# RanuRep = Ranunculus repens, krypsoleie
# AlisPla = Alisma plantago-aquatica, vassgro
# LemnMin = Lemna minor, andmat

# Develop a 4-fold cross-validation scheme for one of the models
# Hint: Back-transformation of GLM models with logit link
# log(y / (1 - y)) = f(x) 
# is by way of
# y = exp(f(x)) / (1 + exp(f(x)))


