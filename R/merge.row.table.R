merge.row.table<-function(Data1,Data2=NULL,wayout,starttime,filename){
  starttime<-ymd_hms(starttime)
  ifelse(is.null(Data2),Data<-Data1,Data<-rbind(Data1,Data2))
  Data$dmyhms<-ymd_hms(Data$dmyhms)
  Data$Timeabsolu<-duration(interval(starttime,Data$dmyhms))
  Data$Timeabsolu2<-as.numeric(as.POSIXct(Data$dmyhms)-as.POSIXct(starttime), units="secs")
  write.table(Data, paste(wayout, "/",filename,".csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")

}
