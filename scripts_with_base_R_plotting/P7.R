library(geoR)
library(sp)
#Importing data from BSKGEO08.xlsx (entire spreadsheet) as geoR object
MR.gd<- read.geodata("clipboard", header = TRUE, coords.col = 1:2, data.col = 3:75)
attach(MR.gd)
names(MR.gd)

#If this doesn't work, data van be imported via csv and clipboard:
#First save the Excel fila in .csv format. Excel uses semicolon to separate 
#... the vasues of a derived .csv file
#Then, this commend should work:
setwd("C:/filer/U/GA/dok21/pract")
MRx.gd<- read.geodata("P7_BSKGEO08.csv", header = TRUE, coords.col = 1:2, data.col = 3:75, sep = ";")
#If you use a desimal comma (and not a decimal point), you will also have to include in your command: dec = ","

#A "third way" to import data is the following:
#Convert the .xls file to .xlsx format
library(openxlsx)
d<-read.xlsx("P7_BSKGEO08.xlsx")
write.table(d, "d.csv")
MRy.gd<-read.geodata("d.csv", header = TRUE, coords.col = 1:2, data.col = 3:75)

***

#Calculate semivariogram for a selection of MR variables
#Inter-plot distances range from 2 m (minimum value) to slightly above 2000 m. 
#We divide the range of distances into equal intervals on a logarithmic scale (4, 8, ... 2048 m)
#Thus, 2-4 m is distance class 1, 4-8 m is distance class 2 etc.

#The semivariogram is calculated by use of the variog command with arguments that give break points between lag classes (bins)
vario.MR.gd<-variog(MR.gd,breaks=c(4,8,16,32,64,128,256,512,1024,2048), max.dist=2048)

#The variogram object is a list of many elements
vario.MR.gd$u #gives the midpoint of each bin
vario.MR.gd$v  #gives semivariances for each variable in each bin
vario.MR.gd$n #the number of observation pairs in each bin
vario.MR.gd$uvec #bin mid-points 
vario.MR.gd$var.mark #sample variance for each variable

#We calculate an envelope around the semivariances by use of the command variog.mc.env
#We find max and min values (lower and upper confidence limits) based upon s (here 999) permutations
#The variogram for variable 19 (Distance to the WaterTable):
vario.19WatTab50.env<-variog.mc.env(MR.gd,coords=MR.gd$coords, data =MR.gd$data[,19],vario.MR.gd,nsim=999)
vario.19WatTab50.env$u
vario.19WatTab50.env$v.lower
vario.19WatTab50.env$v.upper

#Note that the envelope command does not provide standardised envelope values
#We standardise the variogram by division with var.mark throughout
plot(c(4,8,16,32,64,128,256,512,1024,2048),vario.MR.gd$v[,19]/vario.MR.gd$var.mark[19],xlab="distance",ylab="standardised semivariance WatTab50, upper limit",ylim=c(0,2),type="n")
lines(c(4,8,16,32,64,128,256,512,1024,2048), vario.MR.gd$v[,19]/vario.MR.gd$var.mark[19],type="o")
lines(c(4,8,16,32,64,128,256,512,1024,2048), vario.19WatTab50.env$v.lower/vario.MR.gd$var.mark[19],lty=3,col=2)
lines(c(4,8,16,32,64,128,256,512,1024,2048), vario.19WatTab50.env$v.upper/vario.MR.gd$var.mark[19],lty=3,col=2) 

#This semivariogram has an inappropriate scaling of the x axis
vario.MR.gd$u

#We therefore convert the scale of lag distances to a 2-log scale
distlog2<-c(1,log2(6), log2(12), log2(24),log2(48),log2(96), log2(192),log2(384),log2(768),log2(1536))
distlog2

#Creating a new plotplot(distlog2,vario.MR.gd$v[,19]/vario.MR.gd$var.mark[19],xlab="distance (log2 scale)",ylab="standardised semivariance WatTab50",ylim=c(0,2),type="n")
lines(distlog2,vario.MR.gd$v[,19]/vario.MR.gd$var.mark[19],type="o")
lines(distlog2,vario.19WatTab50.env$v.lower/vario.MR.gd$var.mark[19],lty=3,col=2)
lines(distlog2,vario.19WatTab50.env$v.upper/vario.MR.gd$var.mark[19],lty=3,col=2) 
lines(c(6,6),c(-1,5),col=8) #line showing the approximate limit between within and between swamp forest scales
lines(c(-1,15),c(1,1),col=8,lty=3) #line showing semivariance = sample variance

#We repeat the process for a new variable, 40N
vario.40N.env<-variog.mc.env(MR.gd,coords=MR.gd$coords, data =MR.gd$data[,40],vario.MR.gd,nsim=999)
plot(distlog2,vario.MR.gd$v[,40]/vario.MR.gd$var.mark[40],xlab="distance (log2 scale)",ylab="standardised semivariance HumusN",ylim=c(0,2),type="n")
lines(distlog2,vario.MR.gd$v[,40]/vario.MR.gd$var.mark[40],type="o")
lines(distlog2,vario.40N.env$v.lower/vario.MR.gd$var.mark[40],lty=3,col=2)
lines(distlog2,vario.40N.env$v.upper/vario.MR.gd$var.mark[40],lty=3,col=2) 
lines(c(6,6),c(-1,5),col=8)
lines(c(-1,15),c(1,1),col=8,lty=3)

#### Optional: a loop for plotting several variables at the time 
x<-read.table("clipboard") 
#Import just the first row of the data spreadsheet, positions (3:75), i.e. variable names (names of coordinates vectors excluded)!
#Note that skipping this step results in numbers written over each plot!
x

par(mfrow=c(3,3)) 

for(i in 1:9) # for the first 9 variables VertRan,SlopeAvg,SlopeMa25,SlopeMa10,DistMSoil,SoilDMVL,SoilDMWT,CanopyCAvg,CanopyCMax

{
y<-i
z<-MR.gd$data[1,y]
vario.x.env<-variog.mc.env(MR.gd,coords=MR.gd$coords, data =MR.gd$data[,y],vario.MR.gd,nsim=999)
plot(distlog2,vario.MR.gd$v[,y]/vario.MR.gd$var.mark[y],xlab="distance (log2 scale)",ylab=x[,i],ylim=c(0,2),type="n")
lines(distlog2,vario.MR.gd$v[,y]/vario.MR.gd$var.mark[y],type="o")
lines(distlog2,vario.x.env$v.lower/vario.MR.gd$var.mark[y],lty=3,col=2)
lines(distlog2,vario.x.env$v.upper/vario.MR.gd$var.mark[y],lty=3,col=2) 
lines(c(6,6),c(-1,5),col=8)
lines(c(-1,15),c(1,1),col=8,lty=3) }


par(mfrow=c(3,3)) 

for(i in 1:9) # for the first 9 variables VertRan,SlopeAvg,SlopeMa25,SlopeMa10,DistMSoil,SoilDMVL,SoilDMWT,CanopyCAvg,CanopyCMax

{
y<-i
z<-MR.gd$data[1,y]
vario.x.env<-variog.mc.env(MR.gd,coords=MR.gd$coords, data =MR.gd$data[,y],vario.MR.gd,nsim=999)
plot(distlog2,vario.MR.gd$v[,y]/vario.MR.gd$var.mark[y],xlab="distance (log2 scale)",ylab=x[,i],ylim=c(0,2),type="n")
lines(distlog2,vario.MR.gd$v[,y]/vario.MR.gd$var.mark[y])
lines(distlog2,vario.x.env$v.lower/vario.MR.gd$var.mark[y],lty=3,col=2)
lines(distlog2,vario.x.env$v.upper/vario.MR.gd$var.mark[y],lty=3,col=2) 
lines(c(6,6),c(-1,5),col=8)
lines(c(-1,15),c(1,1),col=8,lty=3) }

