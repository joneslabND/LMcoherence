# Lake metabolism coherence
# SEJ

rm(list=ls())

library("biwavelet")

rootDir="/Users/stuartjones/Documents/Research/UNDERC/LMcoherence/LMcoherence"

setwd(rootDir)


### using biwavelet coherence to identify cross-lake covariance/coherence
### takes a long time with 10 minute data, using hourly data

lake1="BO"
lake2="BA"

dox1=read.table(paste(lake1,'_DO.txt',sep=""),header=TRUE,sep="\t",colClasses=c(datetime="POSIXct"))
dox2=read.table(paste(lake2,'_DO.txt',sep=""),header=TRUE,sep="\t",colClasses=c(datetime="POSIXct"))
	
dox=merge(x=dox1,y=dox2,by='datetime')
minOfTS=as.numeric(format(dox$datetime,format="%s"))/(60)
minOfTS=minOfTS-min(minOfTS)
dox=cbind(dox,minOfTS)
	
table(dox[2:(nrow(dox)),4]-dox[1:(nrow(dox)-1),4])
	
doxApprox=cbind(seq(min(dox[,4]),max(dox[,4]),by=10),approx(dox[,4],dox[,2],seq(min(dox[,4]),max(dox[,4]),by=10))$y,approx(dox[,4],dox[,3],seq(min(dox[,4]),max(dox[,4]),by=10))$y)
names(doxApprox)=c("minOfTS","lake1dox","lake2dox")

# make time units hourly
doxApprox[,1]=doxApprox[,1]/(60)

# trim to hourly observations
doxApproxHourly=doxApprox[doxApprox[,1]%%1==0,]

# test normality
dev.new()
par(mfrow=c(2,1))
hist(doxApproxHourly[,2])
hist(doxApproxHourly[,3])

# standardize time series
doxApproxHourlySTD=doxApproxHourly
doxApproxHourlySTD[,2]=(doxApproxHourly[,2]-mean(doxApproxHourly[,2]))/sd(doxApproxHourly[,2])
doxApproxHourlySTD[,3]=(doxApproxHourly[,3]-mean(doxApproxHourly[,3]))/sd(doxApproxHourly[,3])


coher=wtc(doxApproxHourlySTD[,1:2],doxApproxHourlySTD[,c(1,3)],nrands=100)

dev.new()
par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
plot(coher,plot.cb=TRUE)

dev.new()
par(mfrow=c(2,1))
plot(coher$period,rowMeans(coher$rsq),type='l',xlab="period",ylab="mean Coherence")

daily=which(abs(coher$period-24)==min(abs(coher$period-24)))
plot(coher$t,coher$rsq[daily,],type='l',xlab="time",ylab="Coherence")

