#load environmental variable data from file P1_PONDenvvar.xls (variables as columns)
#(variable names as header in first row)

envvar<-read.table("clipboard",header=T)
attach(envvar)
EV<-names(envvar)

n<-ncol(envvar)		

# Lager tomme datasett
for(i in 1:n)
{pverdier<-data.frame(rep(NA,n))}
for(i in 1:n)
{pverdier[EV]<-data.frame(rep(NA,n))}

for(i in 1:n)
{tau<-data.frame(rep(NA,n))}
for(i in 1:n)
{tau[EV]<-data.frame(rep(NA,n))}


pvalues<-pverdier[EV]
tauvalues<-tau[EV]

# Run through all variables
for(i in 1:n) {
	# correlate all variables
	for(j in 1:n) {
		# make an object of correlation coefficients and p-values
		korr <- cor.test(envvar[,i], envvar[,j], method="k")
		# save p-values associated with this object
		if(i != j) { pvalues[i,j] <- korr$p.value }
		# save tau-values associated with this object
		if(i != j) { tauvalues[i,j] <- korr$estimate }
	}
}


pvalues
tauvalues

#From here: Alternative ways to display results

symnum(cor(envvar,method="k"),abbr=F)#eventuelt symnum(cor(envvar,method="k"),abbr=F)

#environmental histograms (functions this way if you have 1-25 variables) 
w<-ceiling(sqrt(n))
q<-ifelse(w<=4,w,5)

par(mfrow=c(q,q)) 
for(i in 1:n)

{
y<-i

hist(envvar[,y]
,main=EV[y] ,xlab="histogram"[y]
)}


