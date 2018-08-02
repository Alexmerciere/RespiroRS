#need lubridate
###Data needs to be in text file
###be sure all your data file are in the good order in the file
clean.mat <- function(waydata,newname,wayoutdata=NULL ){
  if (is.null(wayoutdata)) {wayoutdata <- waydata }

  fichiers<-list.files(waydata,pattern=".txt")
  tableclean<-data.frame(matrix(data=NA,ncol=2))
  colnames(tableclean)<- c("Date", "Time")

  for (i in fichiers){
    a<-paste(waydata,i, sep ="/")
    rowtab<- read.table(file=a, sep = "\t", fill=T)
    unit<-as.character(rowtab[5,3])
    unit
    tab <- read.table(file=a,skip=19, sep = "\t", fill=T,header=T)
    Table<-tab[,c(1:3,5:12)]
    file.position<-which(fichiers == i)
    chamber.position<-c(0,4,8,12,16)
    colnames(Table)<- c("Date", "Time", paste("FS",file.position,"Time.s",sep=""),paste("O2.Ch",chamber.position[file.position]+1,sep=""),paste("O2.Ch",chamber.position[file.position]+2,sep=""),paste("O2.Ch",chamber.position[file.position]+3,sep=""),paste("O2.Ch",chamber.position[file.position]+4,sep=""),paste("T.Ch",chamber.position[file.position]+1,sep=""),paste("T.Ch",chamber.position[file.position]+2,sep=""),paste("T.Ch",chamber.position[file.position]+3,sep=""),paste("T.Ch",chamber.position[file.position]+4,sep=""))
    tableclean<-merge(tableclean,Table,by=c("Date","Time"),all=T)
  }
  tableclean$dmyhms<-dmy_hms(paste(tableclean$Date,tableclean$Time))
  tableclean$Timeabsolu<-duration(interval(tableclean$dmyhms[[1]],tableclean$dmyhms))
  tableclean$Timeabsolu2<-as.numeric(as.POSIXct(tableclean$dmyhms)-as.POSIXct(tableclean$dmyhms[[1]]), units="secs")
  write.csv2(tableclean,paste(paste(wayoutdata,"/",sep=""),newname,".csv",sep=""),row.names = F)
  return(tableclean)
}
