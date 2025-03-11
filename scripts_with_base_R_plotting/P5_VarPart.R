#This script exemplifies the commands involved in the P5 exercise with variation partitioning
library(vegan)
#We use the swamp forest data (three files; see P5_Task.doc for info)
#In this script the three files are imported as SITES (P5_BSK1A.xls), ART (P5_BSK1S.xls) and ENV (P5_BSK1E.xls)
#Note that when you are to do the exercise, you can use the BSK1E8var.xls file,.
#which just contains the 8 selected environmental variables

SITES<-read.table("clipboard",header=T)
ART<-read.table("clipboard",header=T)

attach(SITES)
attach(ART)
names(SITES)
names(ART)

#SITES contains one variable called 'AFF'. This is a factor variable with 11 levels. In order to tell
#R that this is a factor variable: 
SITE<-factor(AFF)

#We find the variation attributed to SITE
CCA0<-cca(ART~SITE) #use of the entire matrix of constr. vars seems perhaps not to work with vegan 2.0
CCA0

#The number we are looking for, is 'Constrained 1.597' which tells that the variable SITE
#explains 1.597 intertia units (out of a total of 6.767 units)
#We also note that since SITE is a factor variable with 11 levels, 10 constraining axes ('degrees of freedom'
#are needed)

#Finding if this is more than attributable to a random variable
testCCA<-permutest(CCA0,permutations=999)  #permutest.cca replaced by permutest() in vegan 2.0
testCCA
#We import the zero skewness transformed environmental variables (lower matrix)
#Use spreadsheet ZSDATA which contains the 53 zero-skewness transformed variables

ENV<-read.table("clipboard",header=T)
attach(ENV)
names(ENV)

#Commands that will be used:

#We find the variation attributable to the first variable, VertRan
CCA1<-cca(ART~VertRan)
CCA1

#The variable VertRan explains 0.1490 inertia units
testCCA1<-permutest(CCA1,permutations=999)
testCCA1

#In order to find significant 'additional' variation by other variables (e.g. SlopeAvg) after the
#variation attributable to VertRan has been explained, we perform a partial constrained ordination:
CCA2<-cca(ART~SlopeAvg+Condition(VertRan))
CCA2

#etc.

#The variable SlopeAvg explains 0.0667 inertia units not also explained by VertRan 
#(This means that the variation attributable to VertRan is first partialled out, and then the
#residual variation attributable to SlopeAvg is found)

#Variation partitioning works like building of a GLM model: For each variable group,
#in the 'first round' all variables are tested for significance one by one
#The 'best' significant variable is selected and used as 'Conditional variable' in
#'round 2' when each of the individually significant variables are tested for significant
#additional variation explained
#The process is stopped when no variable adds significantly to the variation explained


#We now have all tools we need for variation partitioning including for forward selection of variables
#and for quantifying variation explained by significant variables in each group and variation explained by
#one group not shared by other groups
