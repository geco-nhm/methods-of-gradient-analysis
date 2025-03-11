#load and attach datasets:
#NB: Do not import first column (stand) of data spreadsheet
ca.tot<-read.table("clipboard",header=T)
attach(ca.tot)
names(ca.tot)

#Importing library:
library(vegan)

#Running CA on species-plot matrix:
ca.r<-cca(ca.tot)
summary(ca.r)

#Extracting CA-axes for plot scores:
ca1<-scores(ca.r,display="sites", scaling=1)[,1]
ca2<-scores(ca.r,display="sites", scaling=1)[,2]

#The 'scores' command works just for axes 1 and 2
#If you want to extract axes beyond the second, you will have to 
#do that directly from the ca.r object in which it is found as the h-th column 
#of sublist u under list CA, e.g. 'ca.r$CA$u[,h]' where h is a number from 1 to n

ca.r$CA$u[,3]

#Plotting CA - with points:
plot(ca1,ca2,xlab="CA1",ylab="CA2",type="n")
plot(ca1,ca2,xlab="CA1",ylab="CA2",pch=16)

# - with plot numbers:
plot(ca1,ca2,xlab="CA1",ylab="CA2",type="n")
labels<-c(1:100)
text(ca1,ca2,labels,cex=0.75)


#Extracting CA-axes for species scores:
ca11<-scores(ca.r,display="species", scaling=1)[,1]
ca22<-scores(ca.r,display="species", scaling=1)[,2]

#Allocating all species names to "label":
labels2<-names(ca.tot) 
labels2

#Plotting species names:
plot(ca11,ca22,xlab="CA1",ylab="CA2",type="n")
text(ca11,ca22,cex=0.75,labels2)
lines(c(0,0),c(-10,10),col=8,lty=2)
lines(c(-10,10),c(0,0),col=8,lty=2)

#Note that the axes obtained by the cca command are, at the outset, scaled to unit weighted variance of plot scores (the +1 scaling)
#This is the scaling of axes implemented in the commands above
#Alternatively, to extract CA scores with Hill's scaling to use for plotting with a generic plotting function
#e.g., like plot(), use one of the following commands:
#(1) As abovem but with scaling = -1
#(2) By inserting an extra argument in the scores command like this: ca11H<-scores(ca.r,display="sites",hill=T)
#This command provides scores along the first two axes for sites, and with "såecies" it works for species 
#If you want to extract axes separately, this works: ca1<-scores(ca.r,display="sites",hill=T)[,1]
#Again note that the scores command does not work with axes beyond the second