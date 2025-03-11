# Written by Bert van der Veen on 12-03-2020

################################### Information ###################################
# This exercise demonstrates use of the GLLVM package for model-based ordination.
# It has two examples: 1) using simulated data from Minchin 1987 and 2) using the Dune dataset from Jongman et al. 1995.
# gllvm() is the main function to run models
# To get a better idea of what the package has to offer, have a look at ?gllvm
# Distributions available in the package:
# 1) Poisson, 2) Negative Binomial, 3) Zero inflated poisson, 4) Binomial, 5) Ordinal, 6) Gaussian (normal), 7) Tweedie
# Some distributions are fit with LA, some with VA. It happens automatically and you don't have to worry about it
# VA is mostly faster than LA, but VA underestimates the variance explained by the latent variables frequently
# 1), 2) and 3) are for count data. 2) if count data is overdispersed, 3) if it has too many zeros
# 4) is for presence-absence data, 5) for categories
# 6) is for continuous, normal data
# 7) can be used for continuous, positive only data
# Though the package is used here in an ordination perspective, it is the same as a JSDM
# It can easily be used in combination with spatial prediction / maps
# Have a look at https://bertvanderveen.github.io/JSDM_gllvm_example, currently unfinished example
# If you want to use the package for your research, but have questions, e-mail me at bert.vanderveen@nibio.no
# I'm happy to help!
# Always carefully consider the number of observations you have (per site / per species)
# And how that compares to the number of parameters in the model
###################################################################################

# Install required packages:
require(vegan)
require(devtools)
require(corrplot)
# install gllvm developer version. DON'T USE THIS FOR REAL RESEARCH. Always use the CRAN published version
# which you can do by removing this installation and by install.packages("gllvm")
# Here we need this for an extension of the ordinal model, which will eventually be on CRAN
devtools::install_github("BertvanderVeen/gllvm-1", ref = "TMBOrdinal-Common")
library(gllvm)

# Download the minchin dataset
minchin <- read.csv("https://raw.githubusercontent.com/BertvanderVeen/Examples/master/Minchin87_2b_counts.csv") # Data frmo minchin 1987
# All missing values are zeros
minchin[is.na(minchin)] <- 0

# We will first do some classical ordination methods
# Correspondence analysis
CA_minchin <- cca(minchin)
# Detrended CA
DCA_minchin <- decorana(minchin)
# NMDS
NMDS_minchin <- metaMDS(minchin, k = 2)

# Let's have a look at the results
par(mfrow=c(1,3))
plot(CA_minchin, main="CA") # This has the arch effect
plot(DCA_minchin, main="DCA") # This has the tongue effect
plot(NMDS_minchin, main="NMDS") # This looks pretty OK!

# Now try a GLLVM with poisson distribution
mod <- gllvm(minchin, family="poisson", num.lv=2, starting.val="zero")

par(mfrow=c(1,1))
ordiplot(mod, biplot=T, main="GLLVM") # This just looks horrible.

# Always need to check residual diagnostic plots in regressions. Bad residuals = bad model!
# If the plot function here gives an error, just run it again (it can happen sometimes).
plot(mod) # Residuals plots don't look good, so it makes sense the biplot doesn't either.

# Let's fix this!
# Let's try a row-effect and some different starting values first.
# GLLVM can be sensitive to starting values, here we use the "res" option, which is recommended.
# Alternatives are "random" and "zero"
# Also we add some noise, to have a little different starting values, and do this 3 times
# This is the recommended setting according to Niku et al. (2019)
# We also add a site-specific effect (row.eff="random", row.eff="fixed" is the alternative, or no site effect)
mod <- gllvm(minchin, family="poisson", num.lv=2, row.eff="random", starting.val="res", n.init=3, jitter.var=0.2)
ordiplot(mod ,biplot=T) # This looks a little better, but I'm still not happy. Maybe we should try a different distribution

plot(mod) # Fan-shape pattern is indicative of overdispersion, try negative binomial

mod <- gllvm(minchin,family="negative.binomial",num.lv=2, row.eff="random",starting.val="res", n.init=3, jitter.var=0.2, trace=T)
ordiplot(mod, biplot=T) # This looks good! Seems like Minchin simulated in a grid. NMDS showed something similar I think

par(mfrow=c(1,2))
ordiplot(mod, biplot=T, main="GLLVM", predict.region = T) # Add prediction regions for the sites
plot(NMDS_minchin, type="t", main="NMDS")

# Let's try something else, ordinal data!
# The ordered multinomial distribution fits under the assumption that each higher category is more
# Likely to occur than the previous.
# How much more, is indicated by the cut-off values "zeta".
# The 2nd category is minimally as likely to occur as the maximum of the 1st category, and maximum and the minimum of the 3rd.
# We can currently only have cut-off values that are the different for all species (in the CRAN version of the package)
# This means each species needs to have at least 1, preferably more, observation per category and the categories have to be sequential per species (no missing in between)
# In the future (soon) there will be a version with species-common cut-offs to ease this data requirement (see github.com/BertvanderVeen/TMBOrdinal-Common
data(dune)

# Add full names
colnames(dune)<-c("A. millefolium",
                  "A. stolonifera",
                  "A. praecox",
                  "A. geniculatus",
                  "A. odoratum",
                  "B. perennis",
                  "B. hordaceus",
                  "C. album",
                  "C. arvense",
                  "E. palustris",
                  "E. repens",
                  "E. nigrum",
                  "H. radicata",
                  "J. articulatus",
                  "J. bufonius",
                  "S. autumnalis",
                  "L. perenne",
                  "P. lanceolata",
                  "P. pratensis",
                  "P. trivialis",
                  "P. palustris",
                  "R. flammula",
                  "R. acetosa",
                  "S. procumbens",
                  "S. repens",
                  "T. pratense",
                  "T. repens",
                  "V. lathyroides",
                  "B. rutabulum",
                  "C. cysouidata")

par(mfrow=c(1,3))
# Classical ordination first again
CA_dune <- cca(dune)
DCA_dune <- decorana(dune)
NMDS_dune <- metaMDS(dune)

par(mfrow=c(1,3))
plot(CA_dune, main="CA") # Kind of shows an arch, doesn't look very good
plot(DCA_dune, main="DCA") # This actually doesn't look too bad
plot(NMDS_dune, type="t", main="NMDS") # This looks best I think, but still a slight pattern

# fit a GLLVM to ordinal data
mod2 <- gllvm(dune, num.lv = 2, family = "ordinal", zeta.struc="common")

# Alternatively we can reclassify the data and fit with zeta.struc="species"
# Reclassify data per species
dune2 <- apply(dune,2,function(x){
  x<-as.factor(x)
  levels(x)<-1:max(as.integer(x))
  as.integer(x)
})
# If you'd try CA/DCA/NMDS on this reclassified dataset, you'll see things look a lot better
CA_dune2 <- cca(dune2)
DCA_dune2 <- decorana(dune2)
NMDS_dune2 <- metaMDS(dune2)

par(mfrow=c(1,3))
plot(CA_dune2, main="CA") # Kind of shows an arch, doesn't look very good
plot(DCA_dune2, main="DCA") # This actually doesn't look too bad
plot(NMDS_dune2, type="t", main="NMDS") # This looks best I think, but still a slight pattern

mod2_zeta <- gllvm(dune2, num.lv=2, family="ordinal", zeta.struc="species")

# Compare to NMDS again
par(mfrow=(c(1,2)))
ordiplot(mod2,biplot=T, main="GLLVM")
plot(NMDS_dune2,type="t", main="NMDS")

# Now include an environmental variable
# With an covariate included, the latent variables become a residual ordination
# Information accounted for by the covariates is not included
data(dune.env)

mod3 <- gllvm(dune, X = dune.env[,1, drop=F], num.lv = 2, family = "ordinal", zeta.struc = "common")

# Plot the effects of the A1 soil layer thickness per species
# Bars are 95% confidence intervals
par(mfrow=c(1,1))
coefplot(mod3)

# And we can get confidence intervals for the whole model
confint(mod3)

# We can get a model summary
summary(mod3)

# Maybe we should test if the model with one covariate is better than without?
anova(mod2, mod3) # seems like it but we get this annoying warning..
AIC(mod2, mod3) # AIC tells us the covariate does not actually add so much
BIC(mod2, mod3) # BIC tells us the covariate does not actually add so much
# Which we expect from the coefplot() as most effects are not different from zero (confidence intervals cross zero)

# Have a look at the residual correlation between species
resid_cor_dune <- getResidualCor(mod2)
corrplot(resid_cor_dune, type="lower", order = "AOE")

# This last part is to show you what happens if you fit a model that can't be estimated
# Because you have too little data
# Include more than one covariate
mod_bad <- gllvm(dune, X = dune.env, num.lv = 2, family = "ordinal", zeta.struc = "common")
# Doesn't work
# Include a lot of latent variables
mod_bad <- gllvm(dune, num.lv = 10, family = "ordinal", zeta.struc = "common")
# This might have given the same error, but it actually worked.
# As you will see, some latent variables + parameters are very small (1e-4 etc.)
# This is because they only account for noise, there is not enough information in the data to support them
# AIC or BIC can help you to select the best number of latent variables
mod_10LV <- gllvm(dune, num.lv = 10, family = "ordinal", zeta.struc = "common")
mod_1LV <- gllvm(dune, num.lv = 1, family = "ordinal", zeta.struc = "common")
AIC(mod_1LV,mod_10LV) # 1LV is better than 10 LVs
mod_2LV <- gllvm(dune, num.lv = 2, family = "ordinal", zeta.struc = "common")
AIC(mod_1LV,mod_2LV) # 2LVs is better than 1 LV

# If you get an error, you've probably formulated a model that is too complicated for your data,
# Or the starting values should be changed