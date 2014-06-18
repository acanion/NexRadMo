BstrpNex<-function(list=NexMonth){
  
  #function to produce bootstrapped cross correlation from population of NexRad pixels
  #pixel data read in as a list of
  #Use dataset "./data/NexRadMon_allPix.RData"
  #Match data length of datasets
  wstart= c(1995, 1)
  wend= c(2013,12)
  
  #generate discharge time series windowed to match NexRad
  dis= read.csv("./data/SSmonth.csv",stringsAsFactors=F)
  dis$Date=as.Date(dis$Date)
  dists= ts(dis$cfs,c(1932,10),frequency=12)
  diffts=diff(dists)  
  wdists= window(diffts,start=wstart,end=wend)
  
  #Number of bootstrap replicates and length of time series
  reps=9999
  tsl=length(wdists)
  corrout=numeric(length(reps))
  
  #Calculate bootstrapped correlation coefficients
  for(i in 1:reps){
    set=sample(1:2536,size=tsl,replace=T)
    rain=numeric(tsl)
    
      for(j in 1:tsl){
        rain[j]=list[[set[j]]][[j,1]]
      }
    
    tsrain=ts(rain,start=wstart,frequency=12)
    cc=ccf(wdists,tsrain,lag.max=10,plot=F)
    corrout[i]=cc$acf[11+1]
       
  }
  
  return(corrout)
  
}