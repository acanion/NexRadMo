NexCorrM<-function(dir,outfile="out.csv"){

# dir is the directory name assuming data is located one directory below script directory
# Calculate monthly correlations
  
#----------Parameters to set------------
#Windows to make times series match
wstart= c(1995, 1)
wend= c(2013,12)

#CCF lag to output
#lnum=1
#----------------------------------------
  
#Pixel IDS
pixels= read.csv(paste("./",dir,"/","pixels.csv",sep=""))
pid=pixels[,1]

#Read Monthly discharge data
dis=read.csv("./data/SSmonth.csv",stringsAsFactors=F)
dis$Date=as.Date(dis$Date)
dists=ts(dis$cfs,c(1932,10),frequency=12)

#Differenced (1 month) discharge data
diffts=diff(dists)

#Window the differenced ts
wdists= window(diffts,start=wstart,end=wend)

#Create empty correlation data frame
corrdf=data.frame(pixID=integer(length(pid)),cc0=numeric(length(pid)),cc1=numeric(length(pid)),cc2=numeric(length(pid)),cc3=numeric(length(pid)))

#Loop through and calculate correlations
#Note that the start date for the precipitation time series must be specified
#Lags of 0-3 returned
for (i in 1:length(pid)){
  precip=read.csv(paste("./",dir,"/",pid[i],".csv",sep=""))
  precipts=ts(precip[,1],start=c(1995,1),frequency=12)
  wprecipts=window(precipts,start=wstart,end=wend)
  cc=ccf(wdists,wprecipts,lag.max=10,plot=F)
  lagcorr=c(cc$acf[11],cc$acf[11+1],cc$acf[11+2],cc$acf[11+3])
  corrdf[i,]=c(pid[i],lagcorr)
  
}


return(corrdf)

#write data frame to text file
#write.table(c(pid,lagcorr),outfile,row.names=F,col.names=F,sep=",",append=T)  





}