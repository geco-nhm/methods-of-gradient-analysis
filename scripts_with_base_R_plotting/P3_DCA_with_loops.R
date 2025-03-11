#load and attach datasets:
#NB: Do not import first column (stand) of data spreadsheet
dca.tot<-read.table("clipboard",header=T)
attach(dca.tot)
names(dca.tot)

stand.data<-read.table("clipboard",header=T)
attach(stand.data)
names(stand.data)

#Converting variable from numeric to factor:
mode(Stand)
stand<-as.factor(Stand)
stand
 
#Importing library:
library(vegan)

#Running DCA on species-plot matrix:
dca.r<-decorana(dca.tot)
summary(dca.r)

#Extracting DCA-axes for plot scores:
dca1<-scores(dca.r,display="sites",origin=FALSE)[,1]
dca2<-scores(dca.r,display="sites",origin=FALSE)[,2]

#Plotting DCA - with points:
plot(dca1,dca2,xlab="DCA1",ylab="DCA2",type="n")
plot(dca1,dca2,xlab="DCA1",ylab="DCA2",pch=16)

# - with plot numbers:
plot(dca1,dca2,xlab="DCA1",ylab="DCA2",type="n")
labels<-c(1:100)
text(dca1,dca2,labels,cex=0.75)


#Subsets as different colours:
plot(dca1,dca2,xlab="DCA1",ylab="DCA2",type="n")
points(dca1[stand==1],dca2[stand==1],pch=16,col=2)
points(dca1[stand==2],dca2[stand==2],pch=16,col=3)
points(dca1[stand==3],dca2[stand==3],pch=16,col=4)
points(dca1[stand==4],dca2[stand==4],pch=16,col=5) 

#Subsets as different symbols:
plot(dca1,dca2,xlab="DCA1",ylab="DCA2",type="n")
points(dca1[stand==1],dca2[stand==1],pch=16)
points(dca1[stand==2],dca2[stand==2],pch=1)
points(dca1[stand==3],dca2[stand==3],pch=17)
points(dca1[stand==4],dca2[stand==4],pch=6) 

#Subsets with numbers and different colours:
plot(dca1,dca2,xlab="DCA1",ylab="DCA2",type="n")
text(dca1[1:25],dca2[1:25],labels[1:25],cex=0.75,col=2)
text(dca1[26:50],dca2[26:50],labels[26:50],cex=0.75,col=3)
text(dca1[51:75],dca2[51:75],labels[51:75],cex=0.75,col=4)
text(dca1[76:100],dca2[76:100],labels[76:100],cex=0.75,col=5)


#Extracting DCA-axes for species scores:
dca11<-scores(dca.r,display="species",origin=TRUE)[,1]
dca22<-scores(dca.r,display="species",origin=TRUE)[,2]

#Allocating all species names to "label":
labels2<-names(dca.tot) 
labels2

#Plotting species names:
plot(dca11,dca22,xlab="DCA1",ylab="DCA2",type="n")
text(dca11,dca22,cex=0.75,labels2)
lines(c(0,0),c(-5,5),col=8,lty=2)
lines(c(-5,5),c(0,0),col=8,lty=2)

#Plotting species groups in different colours:
plot(dca11,dca22,xlab="DCA1",ylab="DCA2",type="n")
text(dca11[1:20],dca22[1:22],labels2[1:22],col=2,cex=0.75)
text(dca11[23:53],dca22[23:53],labels2[23:53],col=3,cex=0.75)
text(dca11[54:81],dca22[54:81],labels2[54:81],col=4,cex=0.75)
text(dca11[82:91],dca22[82:91],labels2[82:91],col=5,cex=0.75)
lines(c(0,0),c(-5,5),col=8,lty=2)
lines(c(-5,5),c(0,0),col=8,lty=2)


#Extracting DCA-axes with origin=T:
dca111<-scores(dca.r,display="sites",origin=TRUE)[,1]
dca222<-scores(dca.r,display="sites",origin=TRUE)[,2]

#Loading and attaching environmental variable matrix:
env.var<-read.table("clipboard",header=T)
attach(env.var)
names(env.var)


##Biplot with DCA ordination of sites/plots and environmental variables
##both using origin=TRUE here
dca.z<-envfit(scores(dca.r,display="sites",choices=1:4,origin=TRUE)[,1:2],env.var,999)
plot(dca111,dca222,type="n")
plot(dca.z,arrow.mul=1.6,col=3,add=T,cex=0.75)
text(dca111[1:25],dca222[1:25],labels[1:25],cex=0.75,col=2)
text(dca111[26:50],dca222[26:50],labels[26:50],cex=0.75,col=3)
text(dca111[51:75],dca222[51:75],labels[51:75],cex=0.75,col=4)
text(dca111[76:100],dca222[76:100],labels[76:100],cex=0.75,col=5)


#Isoline diagram on environmental variable:

library(mgcv)
library(akima)


TI1
TI1r<-round(TI1,digits=2)
TI1r
mode(TI1r)<-"character"
##NB, you may use untransformed data/environmental variables instead

plot(dca111,dca222,xlab="dca1",ylab="dca2",type="n")
text(dca111[Stand==1],dca222[Stand==1],TI1r[Stand==1],cex=0.75,col=2)
text(dca111[Stand==2],dca222[Stand==2],TI1r[Stand==2],cex=0.75,col=3)
text(dca111[Stand==3],dca222[Stand==3],TI1r[Stand==3],cex=0.75,col=4)
text(dca111[Stand==4],dca222[Stand==4],TI1r[Stand==4],cex=0.75,col=5)
ordisurf(dca.r,TI1,display="sites",col=8,add=T)
text(-0.9,1.0,"TI1",col=2,cex=2)

######## A loop that runs through several variables ########
par(mfrow=c(2,2)) # makes a 2x2 panel
labels3<-names(env.var)
names(env.var)
for (i in 1:4) # plots the 4 first isograms
	{
	lab<-labels3[i]
	envr<-round(env.var[,i],digits=2)
	mode(envr)<-"character"
	plot(dca111,dca222,xlab="dca1",ylab="dca2",type="n")
	text(dca111[Stand==1],dca222[Stand==1],envr[Stand==1],cex=0.75,col=2)
	text(dca111[Stand==2],dca222[Stand==2],envr[Stand==2],cex=0.75,col=3)
	text(dca111[Stand==3],dca222[Stand==3],envr[Stand==3],cex=0.75,col=4)
	text(dca111[Stand==4],dca222[Stand==4],envr[Stand==4],cex=0.75,col=5)
	ordisurf(dca.r,env.var[,i],lay="sites",col=8,add=T)
	text(-0.9,1.0,lab,col=2,cex=1)
	}



#Kendall's correlation test (non-parametric):

plot(dca1,dca2,xlab="DCA1",ylab="DCA2",type="n")
text(dca1[1:25],dca2[1:25],labels[1:25],cex=0.75,col=2)
text(dca1[26:50],dca2[26:50],labels[26:50],cex=0.75,col=3)
text(dca1[51:75],dca2[51:75],labels[51:75],cex=0.75,col=4)
text(dca1[76:100],dca2[76:100],labels[76:100],cex=0.75,col=5)


cor.test(dca1,Mois,method="k")
cor.test(dca1,Litter,method="k")
cor.test(dca1,BasalA,method="k")
cor.test(dca1,Inclin,method="k")
cor.test(dca1,HeatI,method="k")
cor.test(dca1,SoilDMe,method="k")
cor.test(dca1,RoughMe,method="k")
cor.test(dca1,InclMax,method="k")
cor.test(dca1,GapAvg,method="k")
cor.test(dca1,LossOI,method="k")
cor.test(dca1,pH,method="k")
cor.test(dca1,Ca,method="k")
cor.test(dca1,Mg,method="k")
cor.test(dca1,TotN,method="k")
cor.test(dca1,P.Al,method="k")
cor.test(dca1,TI1,method="k")
cor.test(dca1,TI2,method="k")
cor.test(dca2,Mois,method="k")
cor.test(dca2,Litter,method="k")
cor.test(dca2,BasalA,method="k")
cor.test(dca2,Inclin,method="k")
cor.test(dca2,HeatI,method="k")
cor.test(dca2,SoilDMe,method="k")
cor.test(dca2,RoughMe,method="k")
cor.test(dca2,InclMax,method="k")
cor.test(dca2,GapAvg,method="k")
cor.test(dca2,LossOI,method="k")
cor.test(dca2,pH,method="k")
cor.test(dca2,Ca,method="k")
cor.test(dca2,Mg,method="k")
cor.test(dca2,TotN,method="k")
cor.test(dca2,P.Al,method="k")
cor.test(dca2,TI1,method="k")
cor.test(dca2,TI2,method="k")

#######Summary of Kendall's tau for DCA axis 1#######
p.val<-NULL #create empty objects for the p.values
tau.val<-NULL #creates empty objetc for the t-values.
p<-NULL

for(i in 1:17) # A Loop that calculates Kendall's tau and p-values for all the correlations tests.
{
	z<-cor.test(dca1,env.var[,i], method="k")
	p.val[i]<-round(z$p.value, digits=5)
	tau.val[i]<-round(z$estimate,digits=5)
	if (p.val[i] < 0.05) #prints a star when the p-value is less than 0.05
	{p[i]<-"***"
      	}else{
  	p[i]<-" "}
}
#p.val
#tau.val
test.summary.dca1<-data.frame(cbind(names(env.var),p.val,tau.val,p))
test.summary.dca1

#Summary of Kendall's tau correlation test for DCA axis 2
p.val2<-NULL #create empty objects for the p.values
tau.val2<-NULL #creates empty objetc for the t-values.
p2<-NULL
for(i in 1:17) # 
{
	z<-cor.test(dca2,env.var[,i], method="k")
	p.val2[i]<-round(z$p.value,digits=4)
	tau.val2[i]<-round(z$estimate,digits=4)
	if (p.val2[i] < 0.05) 
	{p2[i]<-"***"
      	}else{
  	p2[i]<-" "
	}
}
#p.val2
#tau.val2
test.summary.dca2<-data.frame(cbind(names(env.var),p.val2,tau.val2,p2))
test.summary.dca2


#Split-plot GLM:
spaMois<-aov(dca1~Mois+Error(stand))
summary(spaMois)
coef(spaMois)
spaLitter<-aov(dca1~Litter+Error(stand))
summary(spaLitter)
coef(spaLitter)
spaBasalA<-aov(dca1~BasalA+Error(stand))
summary(spaBasalA)
spaInclin<-aov(dca1~Inclin+Error(stand))
summary(spaInclin)
spaHeatI<-aov(dca1~HeatI+Error(stand))
summary(spaHeatI)
spaSoilDMe<-aov(dca1~SoilDMe+Error(stand))
summary(spaSoilDMe)
spaRoughMe<-aov(dca1~RoughMe+Error(stand))

#Output can be organised like this:
##See SpliplotGLM.doc (NB this table shows GNMDS axis 2, not DCA1 which we use in the model here)


######### Another way to get a summary of some of the data #########
spa.cof<-1NULL
within<-NULL
Stand<-NULL
Intercept<-NULL
tsum<-NULL
pval.stand<-NULL
pval.within<-NULL
for (i in 1:17)
	{
	spax<-aov(dca1~env.var[,i]+Error(stand))
	test<-coef(spax)
	within[i]<-round(test$Within,digits=3)
	within[i]
	Stand[i]<-round(test$stand,digits=3)
	Intercept[i]<-round(test$"(Intercept)",digits=3)
	test2<-summary(spax)
	y1<-test2$"Error: stand"
	y11<-unlist(y1)
	y2<-test2$"Error: Within"
	y22<-unlist(y2)
	pval.stand[i]<-round(y11[9],digits=5)
	pval.stand
	pval.within[i]<-round(y22[9],digits=5)
	pval.within
	}
tsum<-data.frame(cbind(names(env.var),Intercept,within,Stand,pval.stand,pval.within))
tsum
