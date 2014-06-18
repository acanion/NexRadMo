NexSplit<-function(fid="./data/SSN.txt"){
  
  # Function to split NexRad data into individual files for easier processing
  #fid is NexRad input file
  # Put a slash after output directory
  
 
  #Read in Data
  rain.data=read.csv(fid,header=F)
  names(rain.data)=c("blank","Rain_in","PixID","Month","QA")
  rain.data$Month=as.character(rain.data$Month)
  
  #generate a list of pixel id's and write to a text file
  pix=sort(unique(rain.data$PixID))  
  newdir= substr(basename(fid),1,nchar(basename(fid))-4)
  dir.create(paste("./output/",newdir,sep=""))
  pixels=write.table(pix,paste("./output/",newdir,"/","pixels.csv",sep=""),sep=",")
  
  #Split Pixels and write csv files
  rain=split(rain.data[,2:4],rain.data$PixID)
  
  #Write individual datasets to csv files
  
  sapply(names(rain),function (x) write.table(rain[names(rain[x])],file=paste("./output/",newdir,"/",x,".csv",sep=""),sep=",",row.names=F)) 
  
}