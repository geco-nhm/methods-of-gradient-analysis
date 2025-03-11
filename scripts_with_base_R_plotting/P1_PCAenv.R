#Loading and attaching environmental variable matrix (Oppkuven.xls), worksheet 'Env.var'
env.var<-read.table("clipboard",header=T)
attach(env.var)
names(env.var)
labels<-names(env.var)
str(env.var)

#Loading library vegan
library(vegan)

#The correlation matrix can be calculated step by step by repeated commands:
cor.test(Mois,Litter,method="k")
cor.test(Mois,BasalA,method="k")

#Alternatively, we can make short-cuts as follows (returning only correlation coefficients):
symnum(cor(env.var,method="k"))
cor(env.var,method="k")

#Consider to paste and adapt the 'Kendallstauscript into this script (remember correspondence between object names!!)!


#Running PCA on env.var. matrix:
pca.r<-rda(env.var,scale=T) #The 'scaling' argument, which specifies the scaling of axes, is only needed in 'summary' and plot' commands
summary(pca.r) #Note that default is 'scaling = 2' (= correlation biplot scaling; optimising intervariable correlations)

#A very first, 'raw' PCA plot:
plot(pca.r) 

##Plotting the env. var. as vectors in the PCA ordination (NB, only interested
##in the relationship between the environmental variables)
pca1sp<-scores(pca.r, display="species", choices=1, scaling=2) #Scaling parameter has to be set
pca1sp
pca2sp<-scores(pca.r, display="species", choices=2, scaling=2)
plot(pca1sp,pca2sp,type="n",xlim=c(-1.5,1.5),ylim=c(-1.5,2.0))
arrows(0,0,pca1sp,pca2sp,length=0.1,code=2,col=3)
text(pca1sp,pca2sp,labels,col=4,cex=0.6)


##You may change the lengths of the x and y axes by xlim and ylim
##The position of the labels may be changed by multiplying, adding or substracting
##from each pca1sp and pca2sp value a given value
