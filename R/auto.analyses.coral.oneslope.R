
auto.analyses.coral.oneslope<-function(data,metadata,ID,day=NULL,night=NULL,wayout,position=NULL,vol=NULL,chambervol=NULL,startday=NULL,startnight=NULL,waittime=NULL,enddiscard=NULL,closetimeday=NULL,closetimenight=NULL){


  #################Experiment Settings##############
  ##coral record
  if (is.null(day)){day<-T}
  if (is.null(night)){night<-T}
  if (is.null(closetimeday)) { closetimeday <- metadata[ which(metadata$ID==ID[1]),"Close_Time_Day"]}
  if (is.null(closetimenight)) {closetimenight <- metadata[ which(metadata$ID==ID[1]),"Close_Time_Night"]}
  if (is.null(startday)) { startday <- metadata[ which(metadata$ID==ID[1]),"Start_Time_Day"]}
  if (is.null(startnight)) {startnight <- metadata[ which(metadata$ID==ID[1]),"Start_Time_Night"]}

  if (is.null(position)){position<-metadata[ which(metadata$ID==ID[1]),"Chamber"]}
  if (is.null(waittime)) {waittime<- 3600 } #s
  if (is.null(enddiscard)) {enddiscard<- 600 } #s
  if (is.null(chambervol)){chambervol<- metadata[ which(metadata$ID%in%ID),"chambervol"]}
  if (is.null(vol)){vol<- metadata[ which(metadata$ID%in%ID),"V.L"]}

  unit<-"mg/L"
  O2column<-c("Ox.1","Ox.2","Ox.3","Ox.4","Ox.5","Ox.6","Ox.7","Ox.8","Ox.9","Ox.10","Ox.11","Ox.12","Ox.13","Ox.14","Ox.15","Ox.16")
  Tempcolumn<-c("Temp.1","Temp.2","Temp.3","Temp.4","Temp.5","Temp.6","Temp.7","Temp.8","Temp.9","Temp.10","Temp.11","Temp.12","Temp.13","Temp.14","Temp.15","Temp.16")

  timecolumn<-grep("Time.s", colnames(data))
  numdmyhms<-grep("dmyhms", colnames(data))
  data<-arrange(data, Time.s)
  ###TempGraph
  c<-ggplot(data,environment = environment())+geom_point(aes(x=Time.s,y=as.numeric(as.character(data[,Tempcolumn[1]]))))+ylab(paste(colnames(data[Tempcolumn[1]]),"Temp (°C)")) +xlab("Time (sec)")
  print(c)
  ggsave(c,filename="TempRun.pdf",path = wayout,width=20, height=4)

  ##Result Table
  Result<-data.frame(matrix(ncol=13,nrow=0))
  colnames(Result)<- c("ID","Date", "Chamber","RowDayvalue","DayRsquare","Dayvalue","Rownightvalue","NightRsquare","NightValue","vol","chambervol","Temp","sdTemp")
  ##Create Day and Night Table
  Daydata<-subset(data,as.numeric(hms(data[,"Time"]))>as.numeric(hms(startday))+waittime & as.numeric(hms(data[,"Time"]))<as.numeric(hms(startday))+(closetimeday-enddiscard))

  Nightdata<-subset(data,as.numeric(hms(data[,"Time"]))>as.numeric(hms(startnight))+waittime & as.numeric(hms(data[,"Time"]))<as.numeric(hms(startnight))+(closetimenight-enddiscard))

  for (l in ID){
    Result[l,1]<-l
    Result[l,2]<-as.character(data$Date[[1]])
    Result[which(ID==l),3]<-metadata[ which(metadata$ID==l),"Chamber"]

    ifelse(day==F,NA,{
     ###DayResp###
  Daydata[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]<-as.numeric(as.character(Daydata[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]));
   c<-ggplot(Daydata,environment = environment())+geom_point(aes(x=Time.s,y=Daydata[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]))+ylab(paste(colnames(Daydata[O2column[metadata[which(metadata$ID==l),"Chamber"]]]),unit)) +xlab("Time (sec)");
  print(c);
  ggsave(c,filename=paste("Dayslope",l,".pdf",sep=""),path = wayout,width=20, height=4);
  b<-lm(Daydata[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]~Daydata[,"Time.s"]);
  Result[l,4]<-b$coefficients[2];
  Result[l,5]<-summary(b)$r.squared;
  Result[l,6]<-(-b$coefficients[2]*(metadata[which(metadata$ID==l),"chambervol"]-metadata[which(metadata$ID==l),"V.L"])*3600)})

    ifelse(night==F,NA,{
  ###NightResp###
  Nightdata[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]<-as.numeric(as.character(Nightdata[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]));
  c<-ggplot(Nightdata,environment = environment())+geom_point(aes(x=Time.s,y=Nightdata[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]))+ylab(paste(colnames(Nightdata[O2column[metadata[which(metadata$ID==l),"Chamber"]]]),unit)) +xlab("Time (sec)");
  print(c);
  ggsave(c,filename=paste("Nightslope",l,".pdf",sep=""),path = wayout,width=20, height=4);
  b<-lm(Nightdata[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]~Nightdata[,"Time.s"]);
  Result[l,7]<-b$coefficients[2];
  Result[l,8]<-summary(b)$r.squared;
  Result[l,9]<-(-b$coefficients[2]*(metadata[which(metadata$ID==l),"chambervol"]-metadata[which(metadata$ID==l),"V.L"])*3600);
  Result[l,10]<-metadata[which(metadata$ID==l),"V.L"];
  Result[l,11]<-metadata[which(metadata$ID==l),"chambervol"]})

    Result[l,12]<-mean(na.omit(as.numeric(as.character(data[,Tempcolumn[metadata[which(metadata$ID==l),"Chamber"]]]))))
    Result[l,13]<-sd(na.omit(as.numeric(as.character(data[,Tempcolumn[metadata[which(metadata$ID==l),"Chamber"]]]))))
  }


    write.table(Result, paste(wayout, "/", "ResultRun.csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")
    ResultRun<<-Result

}

