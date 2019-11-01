auto.analyses.coral.multislope<-function(data,metadata,ID,S1=NULL,S2=NULL,S3=NULL,S4=NULL,S5=NULL,S6=NULL,wayout,vol=NULL,chambervol=NULL,startS1=NULL,startS2=NULL,startS3=NULL,startS4=NULL,startS5=NULL,startS6=NULL,closetimeS1=NULL,closetimeS2=NULL,closetimeS3=NULL,closetimeS4=NULL,closetimeS5=NULL,closetimeS6=NULL,waittime=NULL,enddiscard=NULL){


  #################Experiment Settings##############
  ##coral record
  if (is.null(S1)){S1<-F}
  if (is.null(S2)){S2<-F}
  if (is.null(S3)){S3<-F}
  if (is.null(S4)){S4<-F}
  if (is.null(S5)){S5<-F}
  if (is.null(S6)){S6<-F}

  if (is.null(waittime)) {waittime<- 300 } #s
  if (is.null(enddiscard)) {enddiscard<- 10 } #s
  #if (is.null(chambervol)){chambervol<- metadata[ which(metadata$ID%in%ID),"chambervol"]}
  #if (is.null(vol)){vol<- metadata[ which(metadata$ID%in%ID),"V.L"]}

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
  Result<-data.frame(matrix(ncol=29,nrow=0))
  colnames(Result)<- c("ID","Date", "Chamber","RowS1value","S1Rsquare","S1value","TempS1","RowS2value","S2Rsquare","S2Value","TempS2","RowS3value","S3Rsquare","S3value","TempS3","RowS4value","S4Rsquare","S4Value","TempS4","RowS5value","S5Rsquare","S5Value","TempS5","RowS6value","S6Rsquare","S6Value","TempS6","vol","chambervol")

  for (l in ID){
    Result[l,1]<-l
    Result[l,2]<-as.character(data$Date[[1]])
    Result[which(ID==l),3]<-metadata[ which(metadata$ID==l),"Chamber"]


    ifelse(S1==F,NA,{
      startS1 <- metadata[ which(metadata$ID==l),"Start_Time_S1"]
      closetimeS1 <- metadata[ which(metadata$ID==l),"Close_Time_S1"]
      dataS1<-subset(data,as.numeric(hms(data[,"Time"]))>as.numeric(hms(startS1))+waittime & as.numeric(hms(data[,"Time"]))<as.numeric(hms(startS1))+(closetimeS1-enddiscard))

      ###S1Resp###
      dataS1[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]<-as.numeric(as.character(dataS1[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]));
      c<-ggplot(dataS1,environment = environment())+geom_point(aes(x=Time.s,y=dataS1[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]))+ylab(paste(colnames(dataS1[O2column[metadata[which(metadata$ID==l),"Chamber"]]]),unit)) +xlab("Time (sec)");
      print(c);
      ggsave(c,filename=paste("S1",l,".pdf",sep=""),path = wayout,width=20, height=4);
      b<-lm(dataS1[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]~dataS1[,"Time.s"]);
      Result[l,4]<-b$coefficients[2];
      Result[l,5]<-summary(b)$r.squared;
      Result[l,6]<-(-b$coefficients[2]*(metadata[which(metadata$ID==l),"chambervol"]-metadata[which(metadata$ID==l),"V.L"])*3600)
      Result[l,7]<-mean(na.omit(as.numeric(as.character(dataS1[,Tempcolumn[metadata[which(metadata$ID==l),"Chamber"]]]))))})

    ifelse(S2==F,NA,{
      startS2 <- metadata[ which(metadata$ID==l),"Start_Time_S2"]
      closetimeS2 <- metadata[ which(metadata$ID==l),"Close_Time_S2"]
      dataS2<-subset(data,as.numeric(hms(data[,"Time"]))>as.numeric(hms(startS2))+waittime & as.numeric(hms(data[,"Time"]))<as.numeric(hms(startS2))+(closetimeS2-enddiscard))

      ###S2Resp###
      dataS2[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]<-as.numeric(as.character(dataS2[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]));
      c<-ggplot(dataS2,environment = environment())+geom_point(aes(x=Time.s,y=dataS2[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]))+ylab(paste(colnames(dataS2[O2column[metadata[which(metadata$ID==l),"Chamber"]]]),unit)) +xlab("Time (sec)");
      print(c);
      ggsave(c,filename=paste("S2",l,".pdf",sep=""),path = wayout,width=20, height=4);
      b<-lm(dataS2[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]~dataS2[,"Time.s"]);
      Result[l,8]<-b$coefficients[2];
      Result[l,9]<-summary(b)$r.squared;
      Result[l,10]<-(-b$coefficients[2]*(metadata[which(metadata$ID==l),"chambervol"]-metadata[which(metadata$ID==l),"V.L"])*3600)
      Result[l,11]<-mean(na.omit(as.numeric(as.character(dataS2[,Tempcolumn[metadata[which(metadata$ID==l),"Chamber"]]]))))})

      ifelse(S3==F,NA,{

        startS3 <- metadata[ which(metadata$ID==l),"Start_Time_S3"]
        closetimeS3 <- metadata[ which(metadata$ID==l),"Close_Time_S3"]
        dataS3<-subset(data,as.numeric(hms(data[,"Time"]))>as.numeric(hms(startS3))+waittime & as.numeric(hms(data[,"Time"]))<as.numeric(hms(startS3))+(closetimeS3-enddiscard))

      ###S3Resp###
      dataS3[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]<-as.numeric(as.character(dataS3[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]));
      c<-ggplot(dataS3,environment = environment())+geom_point(aes(x=Time.s,y=dataS3[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]))+ylab(paste(colnames(dataS3[O2column[metadata[which(metadata$ID==l),"Chamber"]]]),unit)) +xlab("Time (sec)");
      print(c);
      ggsave(c,filename=paste("S3",l,".pdf",sep=""),path = wayout,width=20, height=4);
      b<-lm(dataS3[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]~dataS3[,"Time.s"]);
      Result[l,12]<-b$coefficients[2];
      Result[l,13]<-summary(b)$r.squared;
      Result[l,14]<-(-b$coefficients[2]*(metadata[which(metadata$ID==l),"chambervol"]-metadata[which(metadata$ID==l),"V.L"])*3600)
      Result[l,15]<-mean(na.omit(as.numeric(as.character(dataS3[,Tempcolumn[metadata[which(metadata$ID==l),"Chamber"]]]))))})


      ifelse(S4==F,NA,{

        startS4 <- metadata[ which(metadata$ID==l),"Start_Time_S4"]
        closetimeS4 <- metadata[ which(metadata$ID==l),"Close_Time_S4"]
        dataS4<-subset(data,as.numeric(hms(data[,"Time"]))>as.numeric(hms(startS4))+waittime & as.numeric(hms(data[,"Time"]))<as.numeric(hms(startS4))+(closetimeS4-enddiscard))

###S4Resp###
dataS4[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]<-as.numeric(as.character(dataS4[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]));
c<-ggplot(dataS4,environment = environment())+geom_point(aes(x=Time.s,y=dataS4[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]))+ylab(paste(colnames(dataS4[O2column[metadata[which(metadata$ID==l),"Chamber"]]]),unit)) +xlab("Time (sec)");
print(c);
ggsave(c,filename=paste("S4",l,".pdf",sep=""),path = wayout,width=20, height=4);
b<-lm(dataS4[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]~dataS4[,"Time.s"]);
Result[l,16]<-b$coefficients[2];
Result[l,17]<-summary(b)$r.squared;
Result[l,18]<-(-b$coefficients[2]*(metadata[which(metadata$ID==l),"chambervol"]-metadata[which(metadata$ID==l),"V.L"])*3600)
Result[l,19]<-mean(na.omit(as.numeric(as.character(dataS4[,Tempcolumn[metadata[which(metadata$ID==l),"Chamber"]]]))))})

      ifelse(S5==F,NA,{

        startS5 <- metadata[ which(metadata$ID==l),"Start_Time_S5"]
        closetimeS5 <- metadata[ which(metadata$ID==l),"Close_Time_S5"]
        dataS5<-subset(data,as.numeric(hms(data[,"Time"]))>as.numeric(hms(startS5))+waittime & as.numeric(hms(data[,"Time"]))<as.numeric(hms(startS5))+(closetimeS5-enddiscard))

        ###S5Resp###
        dataS5[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]<-as.numeric(as.character(dataS5[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]));
        c<-ggplot(dataS5,environment = environment())+geom_point(aes(x=Time.s,y=dataS5[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]))+ylab(paste(colnames(dataS5[O2column[metadata[which(metadata$ID==l),"Chamber"]]]),unit)) +xlab("Time (sec)");
        print(c);
        ggsave(c,filename=paste("S5",l,".pdf",sep=""),path = wayout,width=20, height=4);
        b<-lm(dataS5[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]~dataS5[,"Time.s"]);
        Result[l,20]<-b$coefficients[2];
        Result[l,21]<-summary(b)$r.squared;
        Result[l,22]<-(-b$coefficients[2]*(metadata[which(metadata$ID==l),"chambervol"]-metadata[which(metadata$ID==l),"V.L"])*3600)
        Result[l,23]<-mean(na.omit(as.numeric(as.character(dataS5[,Tempcolumn[metadata[which(metadata$ID==l),"Chamber"]]]))))})


      ifelse(S6==F,NA,{

        startS6 <- metadata[ which(metadata$ID==l),"Start_Time_S6"]
        closetimeS6 <- metadata[ which(metadata$ID==l),"Close_Time_S6"]
        dataS6<-subset(data,as.numeric(hms(data[,"Time"]))>as.numeric(hms(startS6))+waittime & as.numeric(hms(data[,"Time"]))<as.numeric(hms(startS6))+(closetimeS6-enddiscard))

        ###S6Resp###
        dataS6[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]<-as.numeric(as.character(dataS6[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]));
        c<-ggplot(dataS6,environment = environment())+geom_point(aes(x=Time.s,y=dataS6[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]))+ylab(paste(colnames(dataS6[O2column[metadata[which(metadata$ID==l),"Chamber"]]]),unit)) +xlab("Time (sec)");
        print(c);
        ggsave(c,filename=paste("S6",l,".pdf",sep=""),path = wayout,width=20, height=4);
        b<-lm(dataS6[,O2column[metadata[which(metadata$ID==l),"Chamber"]]]~dataS6[,"Time.s"]);
        Result[l,24]<-b$coefficients[2];
        Result[l,25]<-summary(b)$r.squared;
        Result[l,26]<-(-b$coefficients[2]*(metadata[which(metadata$ID==l),"chambervol"]-metadata[which(metadata$ID==l),"V.L"])*3600)
        Result[l,27]<-mean(na.omit(as.numeric(as.character(dataS6[,Tempcolumn[metadata[which(metadata$ID==l),"Chamber"]]]))))})



    Result[l,28]<-metadata[which(metadata$ID==l),"V.L"]
    Result[l,29]<-metadata[which(metadata$ID==l),"chambervol"]

  }


  write.table(Result, paste(wayout, "/", "ResultRun.csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")
  ResultRun<<-Result

}
