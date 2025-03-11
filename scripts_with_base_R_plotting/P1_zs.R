#Importing data 

data<-read.table("clipboard",header=T)
attach(data)
names(data)

#x is the variable to be transformed. c is the unknown parameter to be estimated
#transformation constant that needs to be found.
#library e1071 is needed for skewness function
library(e1071)
#if skewness <0 x is transformed by exp(c*x), if skewness >0 x is transformed by log(c+x)
#Starts by defining x as the first variable
x<-[insert variable name here]  
scalex <- function(x, c) { 
	if(skewness(x,na.rm=TRUE) < 0) 
		return(exp(c*x)) 
	else 
		return(log(x+c))
}

minskew <- function(x) {
	cmin <- min(x)-10*(max(x)-min(x));
	cmax <- max(x)+10*(max(x)-min(x));
	if(skewness(x,na.rm=TRUE) >= 0 && cmin < -min(x))
		cmin <- -min(x) 	
	cmid <- (cmin+cmax)/2;
	skew <- skewness(scalex(x, cmid), na.rm=TRUE);
	while (abs(skew) > 1*10^-05 && min(abs(c(cmax, cmin)-cmid)) > 10^-10) {
		print(c(cmin,cmid,cmax,skew));
		sleft <- skewness(scalex(x, (cmid+cmin)/2), na.rm=TRUE)
		sright <- skewness(scalex(x, (cmid+cmax)/2), na.rm=TRUE)
		if (abs(sleft) < abs(skew) && abs(sleft) < abs(sright)) {
			cmax <- cmid
			skew <- sleft
		} else if (abs(sright) < abs(skew)) {
			cmin <- cmid
			skew <- sright
		} else {
			cmin <- (cmin+cmid)/2;
			cmax <- (cmax+cmid)/2;
		}
		cmid <- (cmin+cmax)/2;
	}
	return(list(c=cmid, skew=skew));
}

res<-minskew(x)
#Standardised skewness of the original (untransformed) variable
skewness(x)/(6/length(x))^0.5
#c value and skewness after zero skewness transformation
res$c
res$skew
#standardised skewness of the transformed variable
(res$skew)/(6/length(x))^0.5
#Ranging and printout of the ranged variable
standx<-scalex(x,res$c)
rangx<-(standx-min(standx))/(max(standx)-min(standx))
rangx
hist(rangx)
#We obtain the zs-transformed version of the variable, 'zs[variable name]' as:
zs[Variable name]<-rangx
	