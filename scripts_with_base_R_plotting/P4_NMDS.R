#Script for GNMDS based upon Oppkuven data set
#Load packages

library(vegan)
library(MASS)
library(stats)

#Importing species data from Oppkuven.xls/Species
#Note: Skip first column!

y<-read.table("clipboard",header=T)
attach(y)
names(y)

#making Bray-Curtis dissimilarity matrix:
dist.y<-vegdist(y,method="bray") 
dist.y 
#Replacing unreliable distances (B-C > 0.8 by geodesic distances,
#using stepacross; note that the optimal value for epsilon may be dataset-specific
geodist.y<-isomapdist(dist.y, epsilon=0.8)
geodist.y

k=2 #k determines the number of dimensions in the ordination
#define a general, empty object called mds:
mds<-NULL
#making 100 "mds"s from initial starting configurations, allocating them into the mds object:
for(i in 1:100)
{mds[[i]]<-monoMDS(geodist.y, matrix(c(runif(dim(y)[1]*k)),nrow=dim(y)[1]), 
  k=2, model = "global", maxit=200, smin = 1e-7, sfgrmin = 1e-7)}

#alternative options: model = "local", "linear" or "hybrid" with threshold = [value]

#the mds object is now a list consisting of 100 "subobjects" being lists
# extracting the stress values as a vector - 
# stress is given as the 22th element in each "subobject list"
mds.stress<-unlist(lapply(mds,function(v){v[[22]]})) 

# looking at the stress values for 100 mds:
mds.stress
#ordering the stress values for the 100 mds:
order(mds.stress)
#Saving the order in a vector
ordered<-order(mds.stress)
ordered

#find the stress of the solutions with the lowest and second lowest stress:
mds.stress[ordered[1]]
mds.stress[ordered[2]]

#scaling of axes to half change units and varimax rotation by postMDS
#Note that the original Bray-Curtis distances are used in calculation of H.C. scores!

mds.best<-postMDS(mds[[ordered[1]]], dist.y, pc = TRUE, halfchange = TRUE, threshold = 0.8)
mds.best
mds.secbest<-postMDS(mds[[ordered[2]]], dist.y, pc = TRUE, halfchange = TRUE, threshold = 0.8)
mds.secbest

#Procrustes comparisons
procrustes(mds.best,mds.secbest,permutations=999)
protest(mds.best,mds.secbest,permutations=999)

plot(procrustes(mds.best,mds.secbest,permutations=999))

#making variables from GNMDS axes 1 and 2 for plotting
gnmds1<-mds.best$points[,1]
gnmds2<-mds.best$points[,2]

#Making the ordination diagram using the best solution:
plot(gnmds1,gnmds2,xlab="gnmds1 (scaling in H.C. units)",ylab="gnmds2 (scaling in H.C. units",xlim=c(-1.5,1.5),ylim=c(-1.3,1.3),type="n")
labels<- c(1:100)
text(gnmds1,gnmds2,labels,cex=0.7) #The cex command adjusts the size of plot labels
lines(c(-2,2),c(0,0),lty=2,col=8)
lines(c(0,0),c(-2,2),lty=2,col=8)

#Making Shepard-diagram, this is "optional"

#May be done with in-built function
stressplot(mds.best,dist.y)

# ... or manually
geodist.y
mds.best$points
dist.mds<-vegdist(mds.best$points,method="euclidean")
dist.mds
#Turning a matrix into a vector:
vec.y<-as.vector(geodist.y)
vec.y
vec.mds.best<-as.vector(dist.mds)
plot(vec.mds.best,vec.y)
abline(lm(vec.y~vec.mds.best),col=2) 


#testing correlations (provided that both DCA and GNMDS results are available
cor.test(dca1,gnmds1,method="k")
cor.test(dca2,gnmds2,method="k")

#Preparing 'envfit plot', with vectors fit to continuous variables and points fit to centroids
#for each level of factor-typer variables
#Loading and attaching environmental variable matrix:
env.var<-read.table("clipboard",header=T)
attach(env.var)
names(env.var)


##Biplot with ordination of sites/plots and environmental variables
mds.z<-envfit(scores(mds.best,display="sites",choices=1:2,origin=TRUE)[,1:2],env.var,999)
plot(gnmds1,gnmds2,type="n")
plot(mds.z,arrow.mul=1.6,col=3,add=T,cex=0.75) #arrow.mul is a scaling factor that adjusts vector-arrow lengths to the size of the plot

#Isoline diagram on environmental variable:

library(mgcv)

#choose the variable for which the isoline diagrasm is to be prepared (here 'TI1' and decide the number of digits you
#want the label for variable values to have. These values are later plotted onto the graph

TI1
TI1r<-round(TI1,digits=2)
TI1r
mode(TI1r)<-"character"

plot(gnmds1,gnmds2,xlab="gnmds1 (scaling in H.C. units)",ylab="gnmds2 (scaling in H.C. units",type="n")
ordisurf(mds.best,TI1,display="sites",col=8,add=T)
text(-0.9,1.0,"TI1",col=2,cex=2) #adds text to the graph; co-ordinates for where tio place the text needs to be adjusted)


