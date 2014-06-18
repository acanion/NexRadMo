
#.Rdata= NexRadMon_allPix.Rdata
#
#Input variables:
#dens
#monthcorr
#bootstrap

#Generate histrogram of bootstrap results
hist(bootstrap,col=rgb(0,0,0,1/4),freq=F,breaks=200,ylim=c(0,22),xlim=c(0.55,0.75),main="Histogram of bootstrapped cross correlation (10,000 replicates)",xlab="Correlation coefficient (1 month lag)")

#generate plot of smoothed bootstrap density with actual correlation values as histogram
plot(dens$x,dens$y,type="l",xlab="Correlation coefficient (1 month lag)",ylab="Density",main="Actual distribution vs. bootstrapped",xlim=c(0.55,0.75),ylim=c(0,22))
hist(monthcorr$cc1,col=rgb(0,0,0,1/4),freq=F,breaks=50,add=T)