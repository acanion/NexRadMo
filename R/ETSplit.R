ETSplit<-function(fid="./data/SSN_ET.txt"){
  
  # Function to split NexRad ET data into individual files for easier processing
  #fid is NexRad input file
  # Put a slash after output directory
  
  
  #Read in Data and convert mm ET to inches ET
  et.data=read.csv(fid,stringsAsFactors=F)
  et.data$RET_SUM=et.data$RET_SUM*0.0393701
  et.data$RET_AVG=as.numeric(et.data$RET_AVG)
  et.data$RET_AVG=et.data$RET_AVG*0.0393701
  et.data$Month=paste(et.data$YEAR,"/",et.data$MONTH,sep="")
  
  #generate a list of pixel id's and write to a text file
  pix=sort(unique(et.data$PIXEL_ID))  
  newdir= substr(basename(fid),1,nchar(basename(fid))-4)
  dir.create(paste("./output/",newdir,sep=""))
  pixels=write.table(pix,paste("./output/",newdir,"/","pixels.csv",sep=""),sep=",")
  
  #Split Pixels and write csv files
  et=split(et.data,et.data$PIXEL_ID)
  
  #Write individual datasets to csv files
  sapply(names(et),function (x) write.table(et[names(et[x])],file=paste("./output/",newdir,"/",x,"_ET",".csv",sep=""),sep=",",row.names=F)) 
  

  
}