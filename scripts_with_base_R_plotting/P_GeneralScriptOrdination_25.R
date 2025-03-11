#General script for customised ordination diagrams and related illustrations

########################################
#*** Import and preparation of data ***#
########################################

#The script is generalised by opening for import of ordination results (ordination axes) from external files.
#Ordination axes may, of course, also be derived from R ordination objects. 
#In this script ordination axes, are given generic names.
#Customising the script to your data can be done in two ways; either by changing the names of your ordination
#axis (score) objects into the generic names (recommended), or by changing the generic names in this script.

#If R is not started anew for running this script, a complete cleanup (removal of all attached dataframes) is recommended:

rm(list=ls())

#Data import and preparations:

dta<-read.table("clipboard",header=T) #raw species data
ev<-read.table("clipboard",header=T) #explanatory variables

ax<-read.table("clipboard",header=T) #Optional, axes from a previous ordination
id<-read.table("clipboard",header=T) #Optional, IDs of observation units
oi<-read.table("clipboard",header=T) #Optional, supplementary information about the OI's (affiliation to 'nature type', subset etc.)

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

#Importing libraries:

library(vegan)
library(graphics)  #to identify points on ordination diagrams

#Optional control that ax is a data frame:

is.data.frame(ax)

#Optional generalisation of imported ordination axes

ax1<-ax[,1]
ax2<-ax[,2]
ax3<-ax[,3]
ax4<-ax[,4]


#############################################################################
#*** Standard script for between-variables correlation analyses analyses ***#
#############################################################################

#Making a vector of environmental variable names

evnames<-names(ev)

##Histograms for environmental variables displayed as a panel

#Functions if you have 1-36 variables, otherwise parameters og q have to be changed

w<-ceiling(sqrt(n))
q<-ifelse(w<=5,w,6)

par(mfrow=c(q,q)) 
for(i in 1:n)

{
y<-i

hist(ev[,y],main=names(ev)[y] ,xlab="histogram"[y]
)}

##Correlation matrix

n<-ncol(ev)		

# Lager tomme datasett
for(i in 1:n)
{pverdier<-data.frame(rep(NA,n))}
for(i in 1:n)
{pverdier[names(ev)]<-data.frame(rep(NA,n))}

for(i in 1:n)
{tau<-data.frame(rep(NA,n))}
for(i in 1:n)
{tau[names(ev)]<-data.frame(rep(NA,n))}

pvalues<-pverdier[names(ev)]
tauvalues<-tau[names(ev)]

# Run through all variables
for(i in 1:n) {
	# correlate all variables
	for(j in 1:n) {
		# make an object of correlation coefficients and p-values
		korr <- cor.test(ev[,i], ev[,j], method="k")
		# save p-values associated with this object
		if(i != j) { pvalues[i,j] <- korr$p.value }
		# save tau-values associated with this object
		if(i != j) { tauvalues[i,j] <- korr$estimate }
	}
}


pvalues
tauvalues

##Simplified display of results

symnum(cor(ev,method="k"),abbr=F)#eventuelt symnum(cor(ev,method="k"),abbr=F)

##PCA ordination of environmental variables

pca.r<-rda(ev,scale=T) #The 'scaling' argument, which specifies the scaling of axes, is only needed in 'summary' and plot' commands
summary(pca.r) #Note that default is 'scaling = 2' (= correlation biplot scaling; optimising intervariable correlations)

#Simple, 'raw' PCA plot:
plot(pca.r) 

##Plotting environmental variables as vectors in the PCA ordination

#Focus on the relationships between environmental variable vectors
pca1sp<-scores(pca.r, display="species", choices=1, scaling=2) #Scaling parameter has to be set
pca1sp
pca2sp<-scores(pca.r, display="species", choices=2, scaling=2)
plot(pca1sp,pca2sp,type="n",xlim=c(-1.5,1.5),ylim=c(-1.5,2.0))
arrows(0,0,pca1sp,pca2sp,length=0.1,code=2,col=3)
text(pca1sp,pca2sp,names(ev),col=4,cex=0.6)

#The lengths of the x and y axes can be changed by xlim and ylim
#The position of the labels may be changed by multiplying, adding or substracting from each pca1sp and pca2sp value a given value,


#######################################################################
#*** Standard script for ordination analyses of species composition***#
#######################################################################

##*** DCA ***##

dca.av<-decorana(dta)
dca.av

#Extracting DCA-plot scores:
dca1<-scores(dca.av,display="sites",origin=FALSE)[,1]
#Note that origin=FALSE implies that origo of the ordination diagram is moved 
#from the centroid to the lower end of each axis
dca2<-scores(dca.av,display="sites",origin=FALSE)[,2]
dca3<-scores(dca.av,display="sites",origin=FALSE)[,3]
dca4<-scores(dca.av,display="sites",origin=FALSE)[,4]

#Simple ordination diagram, showing samples with respect to axes 1 and 2
plot(dca1,dca2)

#Eksporting extracted DCA scores to Excel
#By this method, axis scores are imported to Excel and pasted at an appropriate place in a spreadsheet one by one

writeClipboard(as.character(dca1))
writeClipboard(as.character(dca2))
writeClipboard(as.character(dca3))
writeClipboard(as.character(dca4))

#Extracting DCA axes as variables:

#Note that origin=FALSE implies that origo of the ordination diagram is moved 
#from the centroid to the lower end of each axis
#origin=TRUE retains the origin at the centroid

dca1var<-scores(dca.av,display="species",origin=FALSE)[,1]
dca2var<-scores(dca.av,display="species",origin=FALSE)[,2]
dca3var<-scores(dca.av,display="species",origin=FALSE)[,3]
dca4var<-scores(dca.av,display="species",origin=FALSE)[,4]

##*** GNMDS ***##

#GNMDS with geodetic correction of unreliable dissimilarities for PD > 0.8 [carefully consider which epsilon value to choose]

#Making proportional dissimilarity (=Bray-Curtis) dissimilarity matrix

dist.y<-vegdist(dta,method="bray") 
dist.y #displays the dissimilarity matrix; optional
#Replacing unreliable distances (B-C > 0.8 by geodetic distances, using the stepacross algorithm 
#Note that the optimal value for epsilon is dataset-specific

geodist.y<-isomapdist(dist.y, epsilon=0.8)

#For data sets in which one or more observations are weakly related to the rest (disjunct data sets), geodetic correction does not work unless
#... a lower value for epsilon is chosen. In such cases, find the highest value for epsilon that provides results

geodist.y  #displays the dissimilarity matrix; optional

#Select dimensionality for the GNMDS; k-dimensional GNMDS (k = 2, 3, ...)

k=2 #k determines the number of dimensions in the ordination; typically we start with the 4-dimensional solution, thereafter reduce the number of dimensions

#Define a general, empty object called mds:

mds<-NULL

#Make a number of MDSs of your choice (here by default set to 100) from random initial starting configurations, and allocate the solutions to the mds object:
#Remember to fit the right value of k into the statement 'k= ...' below

for(i in 1:100)
{mds[[i]]<-monoMDS(geodist.y, matrix(c(runif(dim(dta)[1]*k)),nrow=dim(dta)[1]), 
  k=2, model = "global", maxit=200, smin = 1e-7, sfgrmin = 1e-7)}

#The mds object is a list consisting of 100 "subobjects" which themselves are lists
#Extract the stress values as a vector 
#Stress values are provided by the 22th element in each "subobject list"

mds.stress<-unlist(lapply(mds,function(v){v[[22]]})) 

#Consider the stress values for the 100 MDSs:

mds.stress

#Order the stress values for the 100 MDSs:

order(mds.stress)

#Save the order in a vector

ordered<-order(mds.stress)
ordered

#Find the stress values for the solutions with the lowest and second lowest stress:

mds.stress[ordered[1]]
mds.stress[ordered[2]]

#Scale axes to half-change units and perform a varimax rotation by postMDS

#Note: In the most recent version of vegan, the postMDS function fails with dist = geodist because the corrected distances 
#do not have a fixed maximum value ("ceiling"). The error message says:
#"In postMDS(mds[[ordered[1]]], geodist.y, pc = TRUE, center = TRUE,  :  halfchange requested, but ceiling distance is unknown, using 0.8"
#No scores are produced
#On "https://rdrr.io/rforge/vegan/man/metaMDS.html" this is explained as follows:
#"Half-change scaling scales the configuration so that one unit means halving of community similarity from 
#replicate similarity. Half-change scaling is based on closer dissimilarities where the relation between 
#ordination distance and community dissimilarity is rather linear (the limit is set by argument threshold). 
#If there are enough points below this threshold (controlled by the parameter nthreshold), dissimilarities 
#are regressed on distances. The intercept of this regression is taken as the replicate dissimilarity, 
#and half-change is the distance where similarity halves according to linear regression. 
#Obviously the method is applicable only for dissimilarity indices scaled to 0 … 1, such as Kulczynski, 
#Bray-Curtis and Canberra indices. If half-change scaling is not used, the ordination is scaled to the same 
#range as the original dissimilarities."
#Solution: In the postMDS command, the distance matrix called must be dist.y and not geodist.y!!

mds.best<-postMDS(mds[[ordered[1]]],dist.y, pc = TRUE, center = TRUE, halfchange = TRUE, threshold = 0.8, nthreshold = 10)
mds.best
mds.secbest<-postMDS(mds[[ordered[2]]],dist.y, pc = TRUE, center = TRUE, halfchange = TRUE, threshold = 0.8, nthreshold = 10)
mds.secbest

#Procrustes comparisons
procrustes(mds.best,mds.secbest,permutations=999)
protest(mds.best,mds.secbest,permutations=999)

plot(procrustes(mds.best,mds.secbest,permutations=999)) #Optional Procrustes plot

#Extraction of axes, k = 2

gnmds2_1<-mds.best$points[,1]
gnmds2_2<-mds.best$points[,2]

plot(gnmds2_1,gnmds2_2)

#Extraction of axes, k = 3

gnmds3_1<-mds.best$points[,1]
gnmds3_2<-mds.best$points[,2]
gnmds3_3<-mds.best$points[,3]

#Extraction of axes, k = 4

gnmds4_1<-mds.best$points[,1]
gnmds4_2<-mds.best$points[,2]
gnmds4_3<-mds.best$points[,3]
gnmds4_4<-mds.best$points[,4]

##*** Correlations between GNMDS- and DCA-axes ***##

#Calculation of correlation coefficients:

cor.test(dca1,gnmds1,method="k")
cor.test(dca1,gnmds2,method="k")
cor.test(dca1,gnmds3,method="k") 
cor.test(dca1,gnmds4,method="k")

cor.test(dca2,gnmds1,method="k")
cor.test(dca2,gnmds2,method="k")
cor.test(dca2,gnmds3,method="k") 
cor.test(dca2,gnmds4,method="k")

cor.test(dca3,gnmds1,method="k")
cor.test(dca3,gnmds2,method="k")
cor.test(dca3,gnmds3,method="k") 
cor.test(dca3,gnmds4,method="k")

cor.test(dca4,gnmds1,method="k")
cor.test(dca4,gnmds2,method="k")
cor.test(dca4,gnmds3,method="k") 
cor.test(dca4,gnmds4,method="k")

#######################################################################################
#*** Interpretation of ordination results by correlation analyses and simple tests ***#
#######################################################################################

#### Choose axes to be analysed; these are named 'ax1' ... 'ax4'

#The general formula for the correlation between an ordination axis and an explanatory variable is:
#Choose ordination axis:

ax<-gnmds2_1  #etc.

cor.test(ax,ev[,1],method="k")
cor.test(ax,ev[,2],method="k")
cor.test(ax,ev[,3],method="k")
cor.test(ax,ev[,4],method="k")
cor.test(ax,ev[,5],method="k")
cor.test(ax,ev[,6],method="k")
cor.test(ax,ev[,7],method="k")
cor.test(ax,ev[,8],method="k")
cor.test(ax,ev[,9],method="k")
cor.test(ax,ev[,10],method="k")
cor.test(ax,ev[,11],method="k")
cor.test(ax,ev[,12],method="k")
cor.test(ax,ev[,13],method="k")
cor.test(ax,ev[,14],method="k")
cor.test(ax,ev[,15],method="k")
cor.test(ax,ev[,16],method="k")
cor.test(ax,ev[,17],method="k")
cor.test(ax,ev[,18],method="k")
cor.test(ax,ev[,19],method="k")
cor.test(ax,ev[,20],method="k")
...

##################################################
#*** Standard script for ordination diagrams ***##
##################################################

#### Choose which (two) axes to plot, and name these pax1 [plotting axis 1] and pax2

pax1 <- gnmds2_1
pax2 <- gnmds2_2

#### Simple diagram with points ####

#Ordination diagram with black dots for the OU's:

plot(pax1,pax2,xlab="",ylab="",type="n")
plot(pax1,pax2,pch=16)

#### Generalised ordination diagram with equal units on the axes, manually set up ####

#Find axis extreme values

min(pax1)
max(pax1)
min(pax2)
max(pax2)
min(pax3)
max(pax3)
min(pax4)
max(pax4)

#Calculate gradient lengths of ax1 ...

grl1<-max(pax1)-min(pax1)
grl2<-max(pax2)-min(pax2)
grl3<-max(pax3)-min(pax3)
grl4<-max(pax4)-min(pax4)
grl1
grl2
grl3
grl4

#Diagram for axis 1 against axis 2
#First we find the coordinates of corners in the 'plotting area' with coordinates (x1, x2, y1, y2) 
#... that make the units on the two axes equal
#x1, x2 and y1 are fixed and y2 is determined so that '(x2-x1)/(y2-y1)' in 'plt=c(x1,x2,y1,y2)'
#... is set equal to 'grl1/grl2'
#This implies that 'y2 = [(x2-x1)*grl2 + y1*grl1]/grl1'
#Keeping x1, x2 and y1 constant, we get:

y2<-(0.8*grl2+0.15*grl1)/grl1
y2

#Note that, if the gradient length of the vertical ordination axis is larger than of the horizontal axis,
#... it is x2 that has to be determined, keeping y1 and y2 constant

#Diagram for axis 1 against axis 3
#y1 and y2 are replaced by z1 and z2:

z2<-(0.8*grl3+0.15*grl1)/grl1
z2

#Diagram for axis 1 against axis 4
#y1 and y2 are replaced by w1 and w2:

w2<-(0.8*grl4+0.15*grl1)/grl1
w2

#Draw the diagram for axes 1 and 2
#Note that the number of tickmarks etc. in the 'xasp' and 'yaxp' arguments must be set individually by inspection of a preplot 
par(plt=c(0.15, 0.95, 0.15, y2))
plot(pax1,pax2,xlab="Axis 1",ylab="Axis 2",xlim=c(min(pax1)-0.01,max(pax1)+0.01),ylim=c(min(pax2)-0.01,max(pax2)+0.01),xaxp=c(0,6,6),yaxp=c(0,3,3),type="n") #Basis for plotting of symbols etc.

#Add points (simple diagram)
points(pax1,pax2,pch=16,cex=0.75)

#Add gridlines (example)
lines(c(-10,10),c(0,0),lty=2,col=8)
lines(c(0,0),c(-10,10),lty=2,col=8)

#Draw the diagram for axes 1 and 3
par(plt=c(0.15, 0.95, 0.15, z2))
plot(pax1,pax3,xlab="Axis 1",ylab="Axis 3",xlim=c(min(pax1)-0.01,max(pax1)+0.01),ylim=c(min(pax2)-0.01,max(pax2)+0.01),xaxp=c(0,6,6),yaxp=c(0,3,3),type="n") #Basis for plotting of symbols etc.

#Draw the diagram for axes 1 and 4
par(plt=c(0.15, 0.95, 0.15, w2))
plot(pax1,pax2,xlab="Axis 1",ylab="Axis 4",xlim=c(min(pax1)-0.01,max(pax1)+0.01),ylim=c(min(pax2)-0.01,max(pax2)+0.01),xaxp=c(0,6,6),yaxp=c(0,3,3),type="n") #Basis for plotting of symbols etc.

#### Standard ordination diagram with symbols, e.g., for different values of a discrete explanatory variables ####

#Set up colour palette, symbols etc. 
#cex=@ gives size as a fraction of full size (cex = 1) for each symbol
#pch=@ specifies symbol type (dot, ring, square etc.)
#0 = open square; 1 = open ring; 2 = open triangle with apex upwards; 3 = +; 4 = �; 5 = open diamond; 6 = open triangle with apex downwards; 7 = cross through open ring in square
#8 = *; 9 = + through diamond; 10 = + in ring; 11 = 'David star' (two superimposed triangles); 12 = + in open square;
#15 = filled square (pch=0 filled); 16 = dot (pch=1 filled); 17 = filled pch=2 etc.
#Subscripts are used to indicate which (combinations of) intervals along the explanatory variable that shall give which symbols and colours

#Example: MATer [terrain shape], a variable with 6 levels (0, ..., 5) in the X data set and ax1 against ax2
#Small light blue rings: MaTer = 0 (filled) and 1 (open); intermediate-sized violet rings for MaTer = 2 (filled) and 3 (open); large red rings for MaTer = 4 (filled) and 5 (open)

ax1<-pax1
ax2<-pax2

points(ax1[MATer==0],ax2[MATer==0],pch=16,cex=0.7,col="lightblue4")
points(ax1[MATer==1],ax2[MATer==1],pch=1,cex=0.7,col="lightblue4")
points(ax1[MATer==2],ax2[MATer==2],pch=16,cex=1,col="blueviolet")
points(ax1[MATer==3],ax2[MATer==3],pch=1,cex=1,col="blueviolet")
points(ax1[MATer==4],ax2[MATer==4],pch=16,cex=1.4,col="red")
points(ax1[MATer==5],ax2[MATer==5],pch=1,cex=1.4,col="red")
#Legend (positions of lines, symbols and text needs to ne set separately in each case
points(-0.1,2.55,pch=16,cex=0.7,col="lightblue4")
text(0.05,2.55,"MATer=0",adj=0,cex=0.75)
points(-0.1,2.38,pch=1,cex=0.7,col="lightblue4")
text(0.05,2.38,"MATer=1",adj=0,cex=0.75)
points(-0.1,2.21,pch=16,cex=1,col="blueviolet")
text(0.05,2.21,"MATer=2",adj=0,cex=0.75)
points(-0.1,2.04,pch=1,cex=1,col="blueviolet")
text(0.05,2.04,"MATer=3",adj=0,cex=0.75)
points(-0.1,1.87,pch=16,cex=1.4,col="red")
text(0.05,1.87,"MATer=4",adj=0,cex=0.75)
points(-0.1,1.7,pch=1,cex=1.4,col="red")
text(0.05,1.7,"MATer=5",adj=0,cex=0.75)
#Separation of legend from content by lines
lines(c(-0.3,0.75),c(1.55,1.55))
lines(c(0.75,0.75),c(1.55,3))


###############################################
#*** Analyses for graphical interpretation ***#
###############################################

##Adding 'envfit vectors' to standard diagram (note that the r^2 given for the vectors are squared Pearson's r)

#Combine the ordination axes in question to a dataframe 

ordx<-data.frame(ax1, ax2)
is.data.frame(ordx)
dim(ordx)

#Optional: remove explanatory variables that are not to be plotted
nvx<-ev[,-c(1,7:34)]
nvx

#Calculate envfit vectors
ordvek12<-envfit(ordx[,1:2], nvx, permutations = 999)

ordvek12$vectors$arrows #gives koordinater for vector arrowheads, standardised to length = 1
ordvek12$vectors$r #r-squared for variables
ordvek12 #both, written as table

#Vector arrows plotted onto standard ordination diagram (note that this requires 'origin=TRUE' in DCA)
plot(ordvek12,arrow.mul=.5,col="darkgreen",add=T,cex=0.6)

#Note that different colours for subsets of ev can be obtained by subsetting, e.g.:
vekA<-envfit(ordx[,1:2], nvx[,1:2], permutations = 999, col="blue")
vekB<-envfit(ordx[,1:2], nvx[,3:5], permutations = 999), col="red") #etc.

##Making isoline diagram, e.g., for MATer based on the standard ordination diagram using the ordisurf commmand; exemplified by axes 1 against 2

vark<-MATer #Select variable
os<-ordisurf(ordx[,1:2],vark,display="sites",col=8,add=T) #This command includes plotting on an existing, open, ordination diagram

# str(os) shows the content of the complex object os
df<-sum(os$edf)-1
df
df<-round(df,1)  #the number of degrees of freedom (1 for the Intercept needs to be subtracted), rounded off to 2 digits after decimal point
vf<-os$deviance/os$null.deviance #Fraction of variation explained by the GAM model implicit in the os function:
vf
vf<-signif(vf,3)  #Rounded to 3 decimals
dfc<-as.character(df) #Conversion to 'character'
vfc<-as.character(vf) #Conversion to 'character'
dfc<-paste("df =",dfc,sep=" ") #combines "df =" with the number of degrees of freedom
vfc<-paste("ve =",vfc,sep=" ") #combines "ve =" with the number of degrees of freedom
dfc
vfc

text(5.2,2.55,"MATer",adj=0,cex=0.9) #Adds variable name etc to the upper right corner of the ordination diagram
text(5.2,2.38,dfc,adj=0,cex=0.75) 
text(5.2,2.21,vfc,adj=0,cex=0.75) 

****
