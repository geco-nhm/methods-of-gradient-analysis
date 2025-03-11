#Importing spreadsheet BASIC from data file P6OppkuvenGNMDS.xls
gnmdstot<-read.csv("P6OppkuvenGNMDS_basic.csv", header=T)
#or, via import from Clipboard
gnmdstot<-read.table("clipboard", header=T)
attach(gnmdstot)
names(gnmdstot)
library(vegan)

year<-as.factor(Year)
year
stand<-as.factor(Stand)
stand

labels97<-c(1:100)
labels05<-c(101:200)
plot(GNMDS1,GNMDS2,type="n")
plot(GNMDS1,GNMDS2,pch=16)

plot(GNMDS1,GNMDS2,type="n")
text(GNMDS1[1:100],GNMDS2[1:100],labels97,cex=0.75,col=2)
text(GNMDS1[101:200],GNMDS2[101:200],labels05,cex=0.75,col=3)


#GNMDS scores (sites) from 97 and 05
GNMDS1.97<-GNMDS1[1:100]
GNMDS2.97<-GNMDS2[1:100]  
GNMDS1.05<-GNMDS1[101:200]
GNMDS2.05<-GNMDS2[101:200]


#Plot with points, adding connective lines:
plot(GNMDS1,GNMDS2,xlim=c(0,2.6),ylim=c(0,1.8),xlab="GNMDS1",ylab="GNMDS2",type="n")
points(GNMDS1.97,GNMDS2.97,col=2,pch=16,cex=1)
points(GNMDS1.05,GNMDS2.05,col=3,pch=16,cex=0.5)
for(i in 1:100) 
{
  # arrows(c(GNMDS1.97[i],GNMDS1.05[i]),c(GNMDS2.97[i],GNMDS2.05[i]))
  arrows(x0 = GNMDS1.97, y0 = GNMDS2.97,
         x1 = GNMDS1.05, y1 = GNMDS2.05,
         length = 0.05)
}
#Plot with numbers, adding connective lines:
plot(GNMDS1,GNMDS2,xlim=c(0,2.6),ylim=c(0,1.8),xlab="GNMDS1",ylab="GNMDS2",type="n")
text(GNMDS1.97,GNMDS2.97,col=2,pch=16,labels97,cex=0.75)
text(GNMDS1.05,GNMDS2.05,col=3,pch=16,cex=0.5,labels05)
for(i in 1:100)lines(c(GNMDS1.97[i],GNMDS1.05[i]),c(GNMDS2.97[i],GNMDS2.05[i]))



#How to find mean displacement for some of the axes/stands
mean(GNMDS1.97)
mean(GNMDS1.05)
mean(GNMDS1.97[1:25])
mean(GNMDS1.05[1:25])
#...and so on

tot97<-(GNMDS2.97[1:100])
mean(tot97)
tot05<-(GNMDS2.05[1:100])
mean(tot05)
a97<-(GNMDS2.97[1:25])
mean(a97)
a05<-(GNMDS2.05[1:25])
mean(a05)
#...and so on


#Explanations for x1, x2 etc. in this table(A-D are the different stands):
		
#1997					
#		Tot	A	B	C	D	
#               Pink    Red     Green	Blue	Turquoise
#GNMDS1	x1	0.7925	0.6611	0.6729	0.9188	0.9171	
#GNMDS2	y1	1.0920	1.2488	0.9987	1.1038	0.9889	

				
#2005					
#GNMDS1	x2	0.7750	0.6421	0.7156	0.8514	0.8911	
#GNMDS2	y2	1.0975	1.2086	1.0049	1.1100	1.0353	


#Mean displacement from 1997 to 2005:
x1<-c(0.7925,0.6611,0.6729,0.9188,0.9171)
x2<-c(0.7750,0.6421,0.7156,0.8514,0.8911)
y1<-c(1.0920,1.2488,0.9987,1.1038,0.9889)
y2<-c(1.0975,1.2086,1.0049,1.1100,1.0353)


#Plot showing mean displacement (try to change arrow parameters: length, angle, 
#"code, linetype and linewidth, you may even try to change the length of the
#x and y axes by xlim and ylim)
plot(x1,y1,xlab="GNMDS1",ylab="GNMDS2",type="n",xlim=c(0.6,1.1),ylim=c(0.9,1.4))
points(x1,y1,xlab="GNMDS1",ylab="GNMDS2",pch=16)
points(x2,y2,xlab="GNMDS1",ylab="GNMDS2",pch=16)
arrows(0.7925,1.0920,0.7750,1.0975,length=0.05,angle=30,code=2,col=6,lty=3,lwd=1)
arrows(0.6611,1.2488,0.6421,1.2086,length=0.05,angle=30,code=2,col=2)
arrows(0.6729,0.9987,0.7156,1.0049,length=0.05,angle=10,code=2,col=3)
arrows(0.9188,1.1038,0.8514,1.1100,length=0.05,angle=70,code=2,col=4)
arrows(0.9171,0.9889,0.8911,1.0353,length=0.05,angle=30,code=2,col=5)
lines(c(-4,4),c(1.0920,1.0920),col=8)
lines(c(0.7925,0.7925),c(-4,4),col=8)

***

#PCA on change in species abundance from 1997 to 2005

#Importing species data
sp.change<-read.csv("P5OppkuvenGNMDS_change.csv",header=T)
#or, from clipboard:
sp.change<-read.table("clipboard",header=T)



attach(sp.change)
names(sp.change)
pca.sp<-rda(sp.change,scale=F)
summary(pca.sp,scaling=1)
pca.sp


#PCA ordination of species change matrix, site score vectors #Choose scaling=1 for optimising fit between distances between plots in diagram and floristic dissimilarity
pca.sp1<-scores(pca.sp,scaling=1,display="sites",choices=1)
pca.sp2<-scores(pca.sp,scaling=1,display="sites",choices=2)
plot(pca.sp1,pca.sp2,xlim=c(-2,2),ylim=c(-2,2),xlab="PCA1",ylab="PCA2",type="n")
text(pca.sp1,pca.sp2,,cex=0.75)


#PCA ordination of species change matrix, species score vectors #Choose scaling=2
pca.sp11<-scores(pca.sp,scaling=2,display="species",choices=1)
pca.sp22<-scores(pca.sp,scaling=2,display="species",choices=2)
SpName<-names(sp.change)
SpName
plot(pca.sp11,pca.sp22,xlim=c(-0.5,3),ylim=c(-2,2),xlab="PCA1",ylab="PCA2",type="n")
text(pca.sp11,pca.sp22,cex=0.75,SpName)
lines(c(-4,4),c(0,0),col=8)
lines(c(0,0),c(-4,4),col=8)

#### The absolute abundance change ####
sort(apply(sp.change, 2, function(x) sum(abs(x))))




#Exercise:
#Perform correlation tests of PCA-axes from species change against environmental 
#variables. Load the environmental variables and find 
#which of the variables are correlated with the PCA-axes?

