clean.row.pyro <- function(waydata,waydatablank=NULL,newname,wayoutdata=NULL,Gap=NULL ){
  if (is.null(wayoutdata)) {wayoutdata <- waydata }
  if (is.null(Gap)) {Gap <- 20 }

  fichiers<-list.files(waydata,pattern=".txt")
  tableclean<-data.frame(matrix(nrow=0,ncol=2))
  colnames(tableclean)<- c("Date", "Time")

  for (i in fichiers){
    a<-paste(waydata,i, sep ="/")
    rowtab<- read.table(file=a, sep = "\t", fill=T)
    unit<-as.character(rowtab[5,3])
    unit
    tab <- read.table(file=a,skip=Gap, sep = "\t", fill=T,header=F)
    Table<-tab[,c(1:3,5,9,6,10,7,11,8,12)]
    file.position<-which(fichiers == i)
    chamber.position<-c(0,4,8,12,16)
    colnames(Table)<- c("Date", "Time", paste("FS",file.position,"Time.s",sep=""),paste("Ox.",chamber.position[file.position]+1,sep=""),paste("Temp.",chamber.position[file.position]+1,sep=""),paste("Ox.",chamber.position[file.position]+2,sep=""),paste("Temp.",chamber.position[file.position]+2,sep=""),paste("Ox.",chamber.position[file.position]+3,sep=""),paste("Temp.",chamber.position[file.position]+3,sep=""),paste("Ox.",chamber.position[file.position]+4,sep=""),paste("Temp.",chamber.position[file.position]+4,sep=""))
    tableclean<-merge(tableclean,Table,by=c("Date","Time"),all=T)
  }

  tableclean$DateTime<-dmy_hms(paste(tableclean$Date,tableclean$Time))
  tableclean$Time.s<-as.numeric(as.POSIXct(tableclean$DateTime)-as.POSIXct(tableclean$DateTime[1]), units="secs")

  tableclean<-arrange(tableclean,DateTime)
  write.table(tableclean, paste(paste(wayoutdata,"/",sep=""),newname,".csv",sep=""), sep = ";", dec = ".", row.names = F, qmethod = "double")

  if (!is.null(waydatablank)) {fichiers2<-list.files(waydatablank,pattern=".txt")
  tablecleanblank<-data.frame(matrix(data=NA,ncol=2))
  colnames(tablecleanblank)<- c("Date", "Time")

  for (i in fichiers2){
    a<-paste(waydatablank,i, sep ="/")
    rowtab<- read.table(file=a, sep = "\t", fill=T)
    unit<-as.character(rowtab[5,3])
    unit
    tab <- read.table(file=a,skip=Gap, sep = "\t", fill=T,header=T)
    Table<-tab[,c(1:3,5,9,6,10,7,11,8,12)]
    file.position<-which(fichiers2 == i)
    chamber.position<-c(0,4,8,12,16)
    colnames(Table)<- c("Date", "Time", paste("FS",file.position,"Time.s",sep=""),paste("Ox.",chamber.position[file.position]+1,sep=""),paste("Temp.",chamber.position[file.position]+1,sep=""),paste("Ox.",chamber.position[file.position]+2,sep=""),paste("Temp.",chamber.position[file.position]+2,sep=""),paste("Ox.",chamber.position[file.position]+3,sep=""),paste("Temp.",chamber.position[file.position]+3,sep=""),paste("Ox.",chamber.position[file.position]+4,sep=""),paste("Temp.",chamber.position[file.position]+4,sep=""))
    tablecleanblank<-merge(tablecleanblank,Table,by=c("Date","Time"),all=T)
  }
  tablecleanblank$DateTime<-dmy_hms(paste(tablecleanblank$Date,tablecleanblank$Time))
  tablecleanblank$Time.s<-as.numeric(as.POSIXct(tablecleanblank$DateTime)-as.POSIXct(tableclean$DateTime[1]), units="secs")
  write.table(tablecleanblank, paste(paste(wayoutdata,"/",sep=""),newname,"blank.csv",sep=""), sep = ";", dec = ".", row.names = F, qmethod = "double")
  }

}
