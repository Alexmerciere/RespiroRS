row.analyses.table<-function(Data,Wfish,fishposition,fishID,blankposition=NULL,wayout,Nnoise=NULL,Outliers=NULL,Samestart=NULL,Blank=NULL){

  if (is.null(Nnoise)) {Nnoise<- 5 }
  if (is.null(blankposition)) {deltaBlankposition<- 0}
  if (is.null(Outliers)) {Outliers<- 0 }
  if (is.null(Samestart)) {Samestart<- F }
  if (is.null(Blank)) {Blank<-T}
  ifelse(blankposition==1,deltaBlankposition<-1,deltaBlankposition<- 0)
  #################Experiment Settings##############
  ##record
  closetime <- as.numeric(readline(prompt="closetime (s): "))  #s
  opentime <- as.numeric(readline(prompt="opentime (s): "))  #s
  waittime <- as.numeric(readline(prompt="waittime (s): "))  #s
  enddiscard <- as.numeric(readline(prompt="enddiscard (s): "))  #s
  ChamberVolume <- as.numeric(readline(prompt="ChamberVolume (L): "))  #L

  nsdevsrsquared<-2
  unit<-"mg/L"
  O2column<-c(4,5,6,7,13,14,15,16,22,23,24,25,31,32,33,34)
  Tempcolumn<-c(8,9,10,11,17,18,19,20,26,27,28,29,35,36,37,38)

  resvolume<-c(ChamberVolume-Wfish[1],ChamberVolume-Wfish[2],ChamberVolume-Wfish[3],ChamberVolume-Wfish[4],ChamberVolume-Wfish[5],ChamberVolume-Wfish[6],ChamberVolume-Wfish[7],ChamberVolume)
  period<-opentime+closetime
  measureperiod<-closetime-waittime-enddiscard
  fishposition<-as.numeric(fishposition)

  ############################File with Fish record######################
  #########Moving average############
  testdata<-data.frame(subset(Data,Timeabsolu2<10000))
  testdata<-na.omit(testdata[,c(23,O2column[fishposition[1]])])
  testdata$movavg<-movavg(testdata[,2],13,type=c("s"))

  ##Calculate dif
  testdata$dif<-NA
  for (row in 1:nrow(testdata)){
    ifelse(testdata[row,1]>99 & testdata[row,1]<1900,testdata[row,4]<-(testdata[(row+50),3]-testdata[row,3]-(testdata[row,3]-testdata[(row-50),3])),testdata[row,4]<-NA)

  }
  ##graphic dif
  c<-ggplot(testdata)+geom_point(aes(x=Timeabsolu2,y=dif))
  print(c)

  ##noise
  noise<-sd(subset(testdata,Timeabsolu2>100&Timeabsolu2<200)[,4])
  print(noise)

  ##Find first endslope
  Toppos<-list()
  for (row in 1:nrow(testdata)){
    ifelse(testdata[row,1]>99 & testdata[row,1]<1900,ifelse(testdata[row,4]>testdata[(row-1),4] & testdata[row,4]>testdata[(row-5),4] & testdata[row,4]>testdata[(row-7),4] & testdata[row,4]>testdata[(row+1),4] & testdata[row,4]>testdata[(row+5),4] & testdata[row,4]>testdata[(row+7),4] & testdata[row,4]>(Nnoise*noise),Toppos<-list.append(Toppos,testdata[row,1]),NA),NA)
  }

  #Time of the first end slope
  Firstend<-as.numeric(Toppos[[1]])
  print(Firstend)

  #graph with end slope and mean temperature during experiment
  c<-ggplot(Data,environment = environment())+geom_point(aes(x=Timeabsolu2,y=Data[,O2column[fishposition[1]]]))+ylab(paste(colnames(Data[O2column[fishposition[1]]]),unit)) +xlab("Time (sec)")+xlim(0,10000) +
    geom_vline(aes(xintercept = Firstend),color="red")
  print(c)
  ggsave(c,filename="firstslope.pdf",path = wayout,width=20, height=4)
  question2 <- readline(prompt="First end slope is at the good position ?(YES or NO) : ")
  ifelse(question2=="YES",NA,{nperiod <-as.numeric(readline(prompt="How many periode do you want add (+) or delete (-) ? "));
  Firstend<-(nperiod*period)+Firstend;
  print(Firstend);
  c<-ggplot(Data,environment = environment())+geom_point(aes(x=Timeabsolu2,y=Data[,4]))+ylab(paste(colnames(Data[4]),unit)) +xlab("Time (sec)")+xlim(0,2000) +geom_vline(aes(xintercept = Firstend),color="red");
  ggsave(c,filename="firstslopecorrected.pdf",path = wayout,width=20, height=4);NA})




  firstmidpoint<-Firstend-enddiscard-(measureperiod/2)
  wholeperiods<-(max(na.omit(Data$Timeabsolu2))-firstmidpoint)/period
  numbperiods<-round(wholeperiods)

     ###########################################################################################################

  ####################################RESULT CHAMBER BLANK######################################################
if (Blank==T) {
  res<-data.frame(matrix(ncol=11,nrow=0));
  colnames(res)<- c("MidTime (sec)", "StartTime (sec)", "EndTime (sec)","linear coeff","MO2 (mg/h)","Temp(°C)","Date","SE","p-value","Rsquared","dmyhms");
  ifelse (Samestart==T,numbperiods<-as.numeric(round(wholeperiods)),numbperiods<-as.numeric(round(wholeperiods)-(blankposition-1) + deltaBlankposition));
  print(numbperiods);

  ##Create table with mid time measurment, start and end per chamber
  for (i in 1:numbperiods){
    ifelse (Samestart==T,res[i,1]<-firstmidpoint+(i-1)*period,res[i,1]<-firstmidpoint+(i-1+(blankposition-1))*period -(deltaBlankposition*period))#### blank chamber position to find start firstmid slope
    res[i,2]<-res[i,1]-(measureperiod/2)
    res[i,3]<-res[i,1]+(measureperiod/2)
    merge(res,Data[,c(21,23)],by.x="MidTime (sec)",by.y="Timeabsolu2",all.x = T)
    Datachamberindv<-na.omit(Data[,c(23,O2column[blankposition],Tempcolumn[blankposition],1)])
    linearreg<-subset(Datachamberindv, Datachamberindv$Timeabsolu2>=res[i,2] & Datachamberindv$Timeabsolu2<=res[i,3])
    b<-lm(linearreg[,2]~linearreg[,1])
    res[i,4]<-b$coefficients[2]
    res[i,5]<-(-b$coefficients[2]*(ChamberVolume)*3600)
    res[i,6]<-Datachamberindv[i,3]
    res[i,7]<-as.character(Datachamberindv[1,4])
    res[i,8]<-summary(b)$coefficients["linearreg[, 1]","Std. Error"]
    res[i,9]<-summary(b)$coefficients["linearreg[, 1]","Pr(>|t|)"]
    res[i,10]<-summary(b)$r.squared
    res[i,11]<-as.character(Data[as.numeric(which(Data$Timeabsolu2 == res[i,1])),"dmyhms"])
  };
  n<-nrow(res);
  res<-res[1:(n-Outliers),];
  Resultblank<-res;
  write.table(Resultblank, paste(wayout, "/rowresultchamberblank.csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double");
  print(c)
}

  ####################################RESULT CHAMBER FISH###############################################

  for (l in fishposition){
    res<-data.frame(matrix(ncol=11,nrow=0))
    colnames(res)<- c("MidTime (sec)", "StartTime (sec)", "EndTime (sec)","linear coeff","MO2 (mg/h)","Temp(°C)","Date","SE","p-value","Rsquared","dmyhms")
    ifelse (Samestart==T,numbperiods<-as.numeric(round(wholeperiods)),numbperiods<-as.numeric(round(wholeperiods)-(fishposition[l]-1) - deltaBlankposition))

    ##Record Fish Chamber
    ##Create table with mid time measurment, start and end per chamber
    for (i in 1:numbperiods){
      ifelse (Samestart==T,res[i,1]<-firstmidpoint+(i-1)*period,res[i,1]<-firstmidpoint+(i-1+(l-1))*period -(deltaBlankposition*period))####remove l-1 if you start all the chamber in the same time
      res[i,2]<-res[i,1]-(measureperiod/2)
      res[i,3]<-res[i,1]+(measureperiod/2)
      merge(res,Data[,c(21,23)],by.x="MidTime (sec)",by.y="Timeabsolu2",all.x = T)
      Datachamberindv<-na.omit(Data[,c(23,O2column[l],Tempcolumn[l],1)])
      linearreg<-subset(Datachamberindv, Datachamberindv$Timeabsolu2>=res[i,2] & Datachamberindv$Timeabsolu2<=res[i,3])
      b<-lm(linearreg[,2]~linearreg[,1])
      res[i,4]<-b$coefficients[2]
      res[i,5]<-(-b$coefficients[2]*(ChamberVolume-Wfish[l])*3600)
      res[i,6]<-Datachamberindv[i,3]
      res[i,7]<-as.character(Datachamberindv[1,4])
      res[i,8]<-summary(b)$coefficients["linearreg[, 1]","Std. Error"]
      res[i,9]<-summary(b)$coefficients["linearreg[, 1]","Pr(>|t|)"]
      res[i,10]<-summary(b)$r.squared
      res[i,11]<-as.character(Data[as.numeric(which(Data$Timeabsolu2 == res[i,1])),"dmyhms"])

    }
    n<-nrow(res)
    res<-res[1:(n-Outliers),]
    write.table(res, paste(wayout, "/", "rowresultchamber", l, ".csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")

  }
}

