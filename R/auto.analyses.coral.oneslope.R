
auto.analyses.coral.oneslope<-function(Data,Volcoral,position,ID,wayout,startday=NULL,startnight=NULL,equalVolume=NULL){

  if (is.null(equalVolume)) {equalVolume<- T }

  #################Experiment Settings##############
  ##coral record
  closetime <- as.numeric(readline(prompt="closetime (s): "))  #s
  waittime <- as.numeric(readline(prompt="waittime (s): "))  #s
  enddiscard <- as.numeric(readline(prompt="enddiscard (s): "))  #s
  ifelse (equalVolume==F,{ChamberVolume <- as.numeric(unlist(strsplit(readline(prompt="list of Chambers Volumes (L): "), ","))) }, {ChamberVolume <- as.numeric(readline(prompt="Chamber Volume (L): "));ChamberVolume <- rep(ChamberVolume,length(position)+1)})

  unit<-"mg/L"
  O2column<-c(4,5,6,7,13,14,15,16,22,23,24,25,31,32,33,34)
  Tempcolumn<-c(8,9,10,11,17,18,19,20,26,27,28,29,35,36,37,38)

  timecolumn<-grep("Timeabsolu2", colnames(Data))
  numdmyhms<-grep("dmyhms", colnames(Data))
  Data<-arrange(Data, Timeabsolu2)
  ###TempGraph
  c<-ggplot(data,environment = environment())+geom_point(aes(x=Timeabsolu2,y=as.numeric(as.character(data[,Tempcolumn[1]]))))+ylab(paste(colnames(data[Tempcolumn[1]]),"Temp (°C)")) +xlab("Time (sec)")
  print(c)
  ggsave(c,filename="TempRun.pdf",path = wayout,width=20, height=4)

  ##Result Table
  Result<-data.frame(matrix(ncol=13,nrow=0))
  colnames(Result)<- c("ID","Date", "Chamber","RowDayvalue","DayRsquare","Dayvalue","Rownightvalue","NightRsquare","NightValue","VolCoral","ChamberVolume","Temp","sdTemp")
  ##Create Day and Night Table
  Daydata<-subset(Data,as.numeric(hms(Data[,"Time"]))>as.numeric(hms(startday))+waittime & as.numeric(hms(Data[,"Time"]))<as.numeric(hms(startday))+(closetime-enddiscard))
    Nightdata<-subset(Data,as.numeric(hms(Data[,"Time"]))>as.numeric(hms(startnight))+waittime & as.numeric(hms(Data[,"Time"]))<as.numeric(hms(startnight))+(closetime-enddiscard))

  for (l in position){
    Result[l,1]<-ID[[l]]
    Result[l,2]<-as.character(Data$Date[[1]])
    Result[l,3]<-position[[l]]

    ifelse(is.null(startday),NA,{
     ###DayResp###
  Daydata[,O2column[l]]<-as.numeric(as.character(Daydata[,O2column[l]]));
  c<-ggplot(Daydata,environment = environment())+geom_point(aes(x=Timeabsolu2,y=Daydata[,O2column[l]]))+ylab(paste(colnames(Daydata[O2column[l]]),unit)) +xlab("Time (sec)");
  print(c);
  ggsave(c,filename=paste("Dayslope",l,".pdf",sep=""),path = wayout,width=20, height=4);
  b<-lm(Daydata[,O2column[l]]~Daydata[,"Timeabsolu2"]);
  Result[l,4]<-b$coefficients[2];
  Result[l,5]<-summary(b)$r.squared;
  Result[l,6]<-(-b$coefficients[2]*(ChamberVolume[l]-Volcoral[l])*3600)})

    ifelse(is.null(startnight),NA,{
  ###NightResp###
  Nightdata[,O2column[l]]<-as.numeric(as.character(Nightdata[,O2column[l]]));
  c<-ggplot(Nightdata,environment = environment())+geom_point(aes(x=Timeabsolu2,y=Nightdata[,O2column[l]]))+ylab(paste(colnames(Nightdata[O2column[l]]),unit)) +xlab("Time (sec)");
  print(c);
  ggsave(c,filename=paste("Nightslope",l,".pdf",sep=""),path = wayout,width=20, height=4);
  b<-lm(Nightdata[,O2column[l]]~Nightdata[,"Timeabsolu2"]);
  Result[l,7]<-b$coefficients[2];
  Result[l,8]<-summary(b)$r.squared;
  Result[l,9]<-(-b$coefficients[2]*(ChamberVolume[l]-Volcoral[l])*3600);
  Result[l,10]<-Volcoral[[l]];
  Result[l,11]<-ChamberVolume[l]})

    Result[l,12]<-mean(na.omit(as.numeric(as.character(Data[,Tempcolumn[l]]))))
    Result[l,13]<-sd(na.omit(as.numeric(as.character(Data[,Tempcolumn[l]]))))
  }


    write.table(Result, paste(wayout, "/", "ResultRun.csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")
    ResultRun<<-Result

}

