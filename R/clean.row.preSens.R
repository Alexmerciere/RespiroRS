clean.row.preSens <- function(waydata,waydatablank=NULL,newname,wayoutdata=NULL,Gap=NULL,dateformat=NULL){
  if (is.null(wayoutdata)) {wayoutdata <- waydata }
  if (is.null(Gap)) {Gap <- 1 }
  if (is.null(dateformat)) {dateformat <- "dmy" }

  fichiers<-list.files(waydata,pattern=".csv")
  tableclean<-data.frame(matrix(nrow=0,ncol=2))
  colnames(tableclean)<- c("Date", "Time")

  for (i in fichiers){
    a<-paste(waydata,i, sep ="/")
    tab <- read.table(file=a,skip=Gap, sep = ",", fill=T,header=T)
    unit<-as.character(tab[2,10])
    unit
    tab<-tab[1:(nrow(tab)-1),]
    tab[,3]<-as.numeric(as.character(tab[,3]))
    Table<-tab[,c(1,2,7,3,9,12)]
    nbrchannel<-max(Table[,4])
    dataox<-cast(Table,Date+Time+delta_t~Channel,value="Value")
    dataTemp<-cast(Table,Date+Time+delta_t~Channel,value="Phase")

    Tablef<-data.frame(matrix(ncol=11,nrow=nrow(dataox)))
    file.position<-which(fichiers == i)
    chamber.position<-c(0,4,8,12,16)
    colnames(Tablef)<- c("Date", "Time", paste("FS",file.position,"Time.s",sep=""),paste("Ox.",chamber.position[file.position]+1,sep=""),paste("Temp.",chamber.position[file.position]+1,sep=""),paste("Ox.",chamber.position[file.position]+2,sep=""),paste("Temp.",chamber.position[file.position]+2,sep=""),paste("Ox.",chamber.position[file.position]+3,sep=""),paste("Temp.",chamber.position[file.position]+3,sep=""),paste("Ox.",chamber.position[file.position]+4,sep=""),paste("Temp.",chamber.position[file.position]+4,sep=""))
    Tablef[,c(1:3)]<-dataTemp[,c(1:3)]
       for(j in 1:nbrchannel){
      Tablef[,2+(2*j)]<-dataox[,3+j]
      Tablef[,3+(2*j)]<-dataTemp[,3+j]
         }
    tableclean<-merge(tableclean,Tablef,by=c("Date","Time"),all=T)
  }
  if(dateformat=="dmy"){tableclean$DateTime<-dmy_hms(paste(tableclean$Date,tableclean$Time))}
  if(dateformat=="mdy"){tableclean$DateTime<-mdy_hms(paste(tableclean$Date,tableclean$Time))}

  tableclean$Time.s<-as.numeric(as.POSIXct(tableclean$DateTime)-as.POSIXct(tableclean$DateTime[1]), units="secs")

  tableclean<-arrange(tableclean,DateTime)
  write.table(tableclean, paste(paste(wayoutdata,"/",sep=""),newname,".csv",sep=""), sep = ";", dec = ".", row.names = F, qmethod = "double")

  if (!is.null(waydatablank)) {fichiers2<-list.files(waydatablank,pattern=".csv")
  tablecleanblank<-data.frame(matrix(data=NA,ncol=2))
  colnames(tablecleanblank)<- c("Date", "Time")

  for (i in fichiers2){
    a<-paste(waydatablank,i, sep ="/")
    tab <- read.table(file=a,skip=Gap, sep = ",", fill=T,header=T)
    unit<-as.character(tab[2,10])
    unit
    tab<-tab[1:(nrow(tab)-1),]
    tab[,3]<-as.numeric(as.character(tab[,3]))
    Table<-tab[,c(1,2,7,3,9,12)]
    nbrchannel<-max(Table[,4])
    dataox<-cast(Table,Date+Time+delta_t~Channel,value="Value")
    dataTemp<-cast(Table,Date+Time+delta_t~Channel,value="Phase")

    Tablef<-data.frame(matrix(ncol=11,nrow=nrow(dataox)))
    file.position<-which(fichiers2 == i)
    chamber.position<-c(0,4,8,12,16)
    colnames(Tablef)<- c("Date", "Time", paste("FS",file.position,"Time.s",sep=""),paste("Ox.",chamber.position[file.position]+1,sep=""),paste("Temp.",chamber.position[file.position]+1,sep=""),paste("Ox.",chamber.position[file.position]+2,sep=""),paste("Temp.",chamber.position[file.position]+2,sep=""),paste("Ox.",chamber.position[file.position]+3,sep=""),paste("Temp.",chamber.position[file.position]+3,sep=""),paste("Ox.",chamber.position[file.position]+4,sep=""),paste("Temp.",chamber.position[file.position]+4,sep=""))
    Tablef[,c(1:3)]<-dataTemp[,c(1:3)]
    for(j in 1:nbrchannel){
    Tablef[,2+(2*j)]<-dataox[,3+j]
    Tablef[,3+(2*j)]<-dataTemp[,3+j]
    }
    tablecleanblank<-merge(tablecleanblank,Tablef,by=c("Date","Time"),all=T)
  }
  if(dateformat=="dmy") {tablecleanblank$DateTime<-dmy_hms(paste(tablecleanblank$Date,tablecleanblank$Time))}
  if(dateformat=="mdy") {tablecleanblank$DateTime<-mdy_hms(paste(tablecleanblank$Date,tablecleanblank$Time))}
  tablecleanblank$DateTime<-dmy_hms(paste(tablecleanblank$Date,tablecleanblank$Time))
  tablecleanblank$Time.s<-as.numeric(as.POSIXct(tablecleanblank$DateTime)-as.POSIXct(tableclean$DateTime[1]), units="secs")
  write.table(tablecleanblank, paste(paste(wayoutdata,"/",sep=""),newname,"blank.csv",sep=""), sep = ";", dec = ".", row.names = F, qmethod = "double")

  }
}

