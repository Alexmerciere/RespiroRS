auto.analyses<-function(Data,Fishbase,Datablank,fishID,wayout,Wfish=NULL,fishposition=NULL,blankposition=NULL,Nnoise=NULL,BlankNnoise=NULL,correctFirstendslope=NULL,Nnoisestartdev=NULL,waittime=NULL,enddiscard=NULL,waittimeblc=NULL,enddiscardblc=NULL,percentpoint=NULL,samestart=NULL){

  #if (is.null(Data)) {Data<- read.table(paste(way,Fishbase[ which(Fishbase$fishID==fishID[1]),"Data"],sep=""), sep = ";", dec = ".",header = T) }
 # if (is.null(Datablank)) {Datablank<- read.table(paste(way,Fishbase[ which(Fishbase$fishID==fishID[1]),"Datablank"],sep=""), sep = ";", dec = ".",header = T) }

  if (is.null(Nnoise)) {Nnoise<- 5 }
  if (is.null(BlankNnoise)) {BlankNnoise<- 5 }
  if (is.null(correctFirstendslope)) { chamberfirstslope <-Fishbase[ which(Fishbase$fishID==fishID[1]),"Nb_Ch"]  }
  if (!is.null(correctFirstendslope)) { chamberfirstslope <-correctFirstendslope }
  if (is.null(Nnoisestartdev)) {Nnoisestartdev<- 0.5 }
  if (is.null(percentpoint)) {percentpoint<- 10 }
  if (is.null(samestart)) {samestart<- F }

  if (is.null(Wfish)) {Wfish<- Fishbase[ which(Fishbase$fishID%in%fishID),"W"] }
  if (is.null(fishposition)) {fishposition<- Fishbase[ which(Fishbase$fishID%in%fishID),"Nb_Ch"] }
  if (is.null(blankposition)) {blankposition<- Fishbase[ which(Fishbase$fishID==fishID[1]),"Nb_Ch_Bl"] }

  ifelse (blankposition==1,deltaBlankposition<-1,deltaBlankposition<-0)
  ifelse (chamberfirstslope==1,corfirstslope<-0,corfirstslope<-chamberfirstslope-1)
  nbchanalyses<-length(fishID)

  #################Experiment Settings##############
  ##fish record
  closetime <- Fishbase[ which(Fishbase$fishID==fishID[1]),"Close_Time"]  #s
  opentime <- Fishbase[ which(Fishbase$fishID==fishID[1]),"Flush_Time"]  #s
  if (is.null(waittime)) {waittime<- 50 } #s
  if (is.null(enddiscard)) {enddiscard<- 22 } #s


  #ChamberVolume <- as.numeric(readline(prompt="ChamberVolume (L): "))  #L

  ##blank record
  closetimeblc <- Fishbase[ which(Fishbase$fishID==fishID[1]),"Close_Time_Blank"]  #s
  opentimeblc <- Fishbase[ which(Fishbase$fishID==fishID[1]),"Flush_Time_Blank"]  #s
  if (is.null(waittimeblc)) {waittimeblc<- 50 } #s
  if (is.null(enddiscardblc)) {enddiscardblc<- 22 } #s

  nsdevsrsquared<-0.5
  unit<-"mg/L"
  O2column<-c("Ox.1","Ox.2","Ox.3","Ox.4","Ox.5","Ox.6","Ox.7","Ox.8","Ox.9","Ox.10","Ox.11","Ox.12","Ox.13","Ox.14","Ox.15","Ox.16")
  Tempcolumn<-c("Temp.1","Temp.2","Temp.3","Temp.4","Temp.5","Temp.6","Temp.7","Temp.8","Temp.9","Temp.10","Temp.11","Temp.12","Temp.13","Temp.14","Temp.15","Temp.16")

  period<-opentime+closetime
  measureperiod<-closetime-waittime-enddiscard
  periodblc<-opentimeblc+closetimeblc
  measureperiodblc<-closetimeblc-waittimeblc-enddiscardblc
  ifelse(chamberfirstslope==1,adustfirstslope<-0, adustfirstslope<-chamberfirstslope*(period/2))

  numdmyhms<-grep("DateTime", colnames(Data))
  Data<-arrange(Data, Time.s)


  ############################File with Fish record######################
  #########Moving average############
  Data[,O2column[chamberfirstslope]]<-as.numeric(as.character(Data[,O2column[chamberfirstslope]]))
  Data[,Tempcolumn[chamberfirstslope]]<-as.numeric(as.character(Data[,Tempcolumn[chamberfirstslope]]))
  testdata<-data.frame(subset(Data,Time.s<100000))
  testdata<-na.omit(testdata[,c("Time.s",O2column[chamberfirstslope])])
  testdata$movavg<-movavg(as.numeric(testdata[,2]),13,type=c("s"))

  ##Calculate dif
  testdata$dif<-NA
  for (row in 1:nrow(testdata)){
    ifelse(testdata[row,1]>99+adustfirstslope & testdata[row,1]<4900+adustfirstslope,testdata[row,4]<-(testdata[(row+50),3]-testdata[row,3]-(testdata[row,3]-testdata[(row-50),3])),testdata[row,4]<-NA)

  }
  ##graphic dif
  c<-ggplot(testdata)+geom_point(aes(x=Time.s,y=dif))
  print(c)

  ##noise
  noise<-sd(na.omit(subset(Data,Time.s>100&Time.s<200)[,O2column[chamberfirstslope]]))
  print(noise)

  ##Find first endslope
  Toppos<-list()
  for (row in 1:nrow(testdata)){
    ifelse(testdata[row,1]>99+adustfirstslope & testdata[row,1]<1900+adustfirstslope,ifelse(testdata[row,4]>testdata[(row-1),4] & testdata[row,4]>testdata[(row-5),4] & testdata[row,4]>testdata[(row-7),4] & testdata[row,4]>testdata[(row+1),4] & testdata[row,4]>testdata[(row+5),4] & testdata[row,4]>testdata[(row+7),4] & testdata[row,4]>(Nnoise*noise),Toppos<-list.append(Toppos,testdata[row,1]),NA),NA)
  }

  #Time of the first end slope
  Firstend<-as.numeric(Toppos[[1]])
  print(Firstend)

  #graph with end slope

  c<-ggplot(Data,environment = environment())+geom_point(aes(x=Time.s,y=Data[,O2column[chamberfirstslope]]))+ylab(paste(colnames(Data[O2column[chamberfirstslope]]),unit)) +xlab("Time (sec)")+xlim(0,10000) +
    geom_vline(aes(xintercept = Firstend),color="red")
  print(c)
  ggsave(c,filename="firstslope.pdf",path = wayout,width=20, height=4)
  question2 <- readline(prompt="First end slope is at the good position ?(YES or NO) : ")
  if(question2=="NO"){startday <-as.character(readline(prompt="Time of start ?(ex:08:43:01) : "));
  Firstend<-Data[which(Data$Time==startday),"Time.s"]+closetime;
  print(Firstend[1]);
  c<-ggplot(Data,environment = environment())+geom_point(aes(x=Time.s,y=Data[,O2column[chamberfirstslope]]))+ylab(paste(colnames(Data[O2column[chamberfirstslope]]),unit)) +xlab("Time (sec)")+xlim(0,10000) +geom_vline(aes(xintercept = Firstend[1]),color="red") ;
  ggsave(c,filename="firstslopecorrected.pdf",path = wayout,width=20, height=4)}

  Firstend<-Firstend[1]-(corfirstslope*period)



  firstmidpoint<-Firstend-enddiscard-(measureperiod/2)
  wholeperiods<-(max(na.omit(Data$Time.s))-firstmidpoint)/period
  numberoflowvalues<-wholeperiods/percentpoint  #### pourcent of point to keep


  ###########################################################################################################
  ####################################File with Fishblank record#############################################
  #########Moving average############
  testdata<-Datablank
  testdata<-na.omit(testdata[,c("Time.s",O2column[fishposition[1]])])

  testdata$movavg<-movavg(as.numeric(testdata[,2]),13,type=c("s"))

  ##Calculate dif
  testdata$dif<-NA
  for (x in c(51:(nrow(testdata)-50))){
    testdata[x,4]<-(testdata[(x+50),3]-testdata[x,3]-(testdata[x,3]-testdata[(x-50),3]))

  }
  ##graphic dif
  ggplot(testdata)+geom_point(aes(x=Time.s,y=dif))

  ##noise
  noise<-sd(testdata[c(51:200),4])
  print(noise)
  ##Find first endslope
  Toppos<-list()
  for (row in c(51:950)){
    ifelse(testdata[row,4]>testdata[(row-1),4] & testdata[row,4]>testdata[(row-5),4] & testdata[row,4]>testdata[(row-7),4] & testdata[row,4]>testdata[(row+1),4] & testdata[row,4]>testdata[(row+5),4] & testdata[row,4]>testdata[(row+7),4] & testdata[row,4]>(BlankNnoise*noise),Toppos<-list.append(Toppos,testdata[row,1]),NA)
  }
  #Time of the first end slope
  Firstendblank<-as.numeric(Toppos[[1]])
  print(Firstendblank)
  #graph with end slope and mean temperature during experiment
  c<-ggplot(Datablank,environment = environment())+geom_point(aes(x=Time.s,y=Datablank[,O2column[fishposition[1]]]))+ylab(paste(colnames(Datablank[O2column[fishposition[1]]]),unit)) +xlab("Time (sec)") + geom_vline(aes(xintercept = Firstendblank,color="red"))
  print(c)
  ggsave(c,filename="firstslopeblank.pdf",path = wayout,width=20, height=4)

  question1 <- readline(prompt="First end slope blank is at the good position ?(YES or NO) : ")
  if(question1=="NO"){startday <-as.character(readline(prompt="Time of start ?(ex:08:43:01) : "));
  Firstendblank<-Datablank[which(Datablank$Time==startday),"Time.s"]+closetimeblc;
  print(Firstendblank);
  c<-ggplot(Datablank,environment = environment())+geom_point(aes(x=Time.s,y=Datablank[,O2column[fishposition[1]]]))+ylab(paste(colnames(Datablank[O2column[fishposition[1]]]),unit)) +xlab("Time (sec)") + geom_vline(aes(xintercept = Firstendblank,color="red"))
  print(c)
  ggsave(c,filename="firstslopeblankcorrected.pdf",path = wayout,width=20, height=4)}


  firstmidpointblank<-Firstendblank-enddiscardblc-(measureperiodblc/2)

  ###########################################################################################################

  ####################################RESULT CHAMBER BLANK######################################################

  res<-data.frame(matrix(ncol=10,nrow=0))
  colnames(res)<- c("MidTime (sec)", "StartTime (sec)", "EndTime (sec)","linear coeff","MO2 (mg/h)","Temp(°C)","Date","SE","p-value","Rsquared")
  numbperiods<-round(wholeperiods)-(blankposition-1) + deltaBlankposition

  ##Create table with mid time measurment, start and end per chamber
  for (i in 1:numbperiods){
    res[i,1]<-firstmidpoint+(i-1+(blankposition-1))*period -(deltaBlankposition*period) #### blank chamber position to find start firstmid slope
    res[i,2]<-res[i,1]-(measureperiod/2)
    res[i,3]<-res[i,1]+(measureperiod/2)
    Data[,O2column[blankposition]]<-as.numeric(as.character(Data[,O2column[blankposition]]))
    Data[,Tempcolumn[blankposition]]<-as.numeric(as.character(Data[,Tempcolumn[blankposition]]))
    merge(res,Data[,c("DateTime","Time.s")],by.x="MidTime (sec)",by.y="Time.s",all.x = T)
    Datachamberindv<-na.omit(Data[,c("Time.s",O2column[blankposition],Tempcolumn[blankposition])])
    linearreg<-subset(Datachamberindv, Datachamberindv$Time.s>=res[i,2] & Datachamberindv$Time.s<=res[i,3])
    b<-lm(linearreg[,2]~linearreg[,1])
    res[i,4]<-b$coefficients[2]
    res[i,5]<-(-b$coefficients[2]*(Fishbase[ which(Fishbase$fishID==fishID[1]),"Vol_Ch_Bl"])*3600)
    res[i,6]<-Datachamberindv[i,3]
    res[i,7]<-as.character(Data[1,"DateTime"])
    res[i,8]<-summary(b)$coefficients["linearreg[, 1]","Std. Error"]
    res[i,9]<-summary(b)$coefficients["linearreg[, 1]","Pr(>|t|)"]
    res[i,10]<-summary(b)$r.squared
  }
  e<-lm(res[,5] ~ poly(res[,1], 4, raw=TRUE))

  polyblank <- function(x) {e$coefficient[5]*x^4 + e$coefficient[4]*x^3 + e$coefficient[3]*x^2 + e$coefficient[2]*x + e$coefficient[1]}
  res$Poly<-polyblank(res[,1])
  c<-ggplot(res,environment = environment())+geom_point(aes(x=res[,1]/3600,y=res[,5]))+ylab(colnames(res[5])) +xlab("Time (h)")+ stat_smooth(aes(x=res[,1]/3600,y=res[,5]),method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, 4, raw=TRUE),colour="blue") +geom_point(aes(x=res[,1]/3600,y=res[,11],colour="red"))


  Resultblank<-res
  write.table(Resultblank, paste(wayout, "/resultchamberblank.csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")
  ggsave(c,filename="Blank.pdf",path = wayout,width=20, height=4)
  print(c)

  #################SLOPE BLANK RESPIRATION##############
  #######################################Find start of slope deviation##################################
  noise<-sd(Resultblank[c(1:10),5])
  print(noise)
  mean<-mean(Resultblank[c(1:10),5])
  Toppos<-list()
  for (row in 1:nrow(Resultblank)){
    ifelse(Resultblank[row,11]>(0+(Nnoisestartdev*noise)),Toppos<-list.append(Toppos,Resultblank[row,1]),NA)
  }
  #Time of the start slope deviation
  if (!length(Toppos) == 0) {Firststart<-as.numeric(Toppos[1]);
  print(Firststart);
  c<-ggplot(Resultblank,environment=environment())+geom_point(aes(x=Resultblank[,1]/3600,y=Resultblank[,5]))+ylab(paste(colnames(Resultblank[5]))) +xlab("Time (sec)")+geom_point(aes(x=Resultblank[,1]/3600,y=Resultblank[,11],colour="red"))+ geom_vline(aes(xintercept =Firststart/3600 ,color="red"));
  ggsave(c,filename="Blank_slope_start.pdf",path = wayout,width=20, height=4);
  print(c);
  Blankcorrection<-T
}
  if (length(Toppos) == 0) { print("No Blank Correction, blank consomption = 0");Blankcorrection<-F}
  ########################################################################################


  ####################################RESULT CHAMBER FISH###############################################
  Result<-data.frame(matrix(ncol=12,nrow=0))
  colnames(Result)<- c("fishID","Date", "Chamber","SMR","sdSMR","MaxMR","MinMR","FirstMR","MeanTemp (°C)","Weight (kg)","Chamber Volume (L)","BlankCorrection")

  for (l in fishID){
    res<-data.frame(matrix(ncol=11,nrow=0))
    colnames(res)<- c("MidTime (sec)", "StartTime (sec)", "EndTime (sec)","linear coeff","MO2 (mg/h)","Temp(°C)","Date","SE","p-value","Rsquared","MO2cor")
    if(samestart==F){numbperiods<-round(wholeperiods)-(Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"]-Fishbase[ which(Fishbase$fishID==fishID[1]),"Nb_Ch"])-deltaBlankposition}
    if(samestart==T){numbperiods<-round(wholeperiods)- deltaBlankposition}


    resblc<-data.frame(matrix(ncol=11,nrow=0))
    colnames(resblc)<- c("MidTime (sec)", "StartTime (sec)", "EndTime (sec)","linear coeff","MO2 (mg/h)","Temp(°C)","Date","SE","p-value","Rsquared","MO2cor")

    ##Record Fish Chamber
    ##Create table with mid time measurment, start and end per chamber
    for (i in 1:numbperiods){
      if(samestart==F){res[i,1]<-firstmidpoint+(i-1+(Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"]-Fishbase[ which(Fishbase$fishID==fishID[1]),"Nb_Ch"]))*period-(deltaBlankposition*period)}
      if(samestart==T){res[i,1]<-firstmidpoint+(i-1)*period-(deltaBlankposition*period)}
      res[i,2]<-res[i,1]-(measureperiod/2)
      res[i,3]<-res[i,1]+(measureperiod/2)
      Data[,O2column[Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"]]]<-as.numeric(as.character(Data[,O2column[Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"]]]))
      Data[,Tempcolumn[Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"]]]<-as.numeric(as.character(Data[,Tempcolumn[Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"]]]))
      merge(res,Data[,c("DateTime","Time.s")],by.x="MidTime (sec)",by.y="Time.s",all.x = T)
      Datachamberindv<-na.omit(Data[,c("Time.s",O2column[Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"]],Tempcolumn[Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"]])])
      linearreg<-subset(Datachamberindv, Datachamberindv$Time.s>=res[i,2] & Datachamberindv$Time.s<=res[i,3])
      b<-lm(linearreg[,2]~linearreg[,1])
      res[i,4]<-b$coefficients[2]
      res[i,5]<-(-b$coefficients[2]*(Fishbase[ which(Fishbase$fishID==l),"Vol_Ch"]-Fishbase[ which(Fishbase$fishID==l),"W"])*3600)
      res[i,6]<-Datachamberindv[i,3]
      res[i,7]<-as.character(Data[1,"DateTime"])
      res[i,8]<-summary(b)$coefficients["linearreg[, 1]","Std. Error"]
      res[i,9]<-summary(b)$coefficients["linearreg[, 1]","Pr(>|t|)"]
      res[i,10]<-summary(b)$r.squared
    }

    if(Blankcorrection==T) {
        ##Blank Fish Chamber

    for (i in c(1:3)){
      resblc[i,1]<-(firstmidpointblank+(i-1)*periodblc)
      resblc[i,2]<-resblc[i,1]-(measureperiodblc/2)
      resblc[i,3]<-resblc[i,1]+(measureperiodblc/2)
      Datablank[,O2column[Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"]]]<-as.numeric(as.character(Datablank[,O2column[Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"]]]))
      Datablank[,Tempcolumn[Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"]]]<-as.numeric(as.character(Datablank[,Tempcolumn[Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"]]]))

      merge(resblc,Datablank[,c("DateTime","Time.s")],by.x="MidTime (sec)",by.y="Time.s",all.x = T) ###A voir les colonnes datablank
      Datachamberindvblc<-na.omit(Datablank[,c("Time.s",O2column[Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"]],Tempcolumn[Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"]])])
      linearregblc<-subset(Datachamberindvblc, Datachamberindvblc$Time.s>=resblc[i,2] & Datachamberindvblc$Time.s<=resblc[i,3])
      b<-lm(linearregblc[,2]~linearregblc[,1])
      resblc[i,4]<-b$coefficients[2]
      resblc[i,5]<-(-b$coefficients[2]*(Fishbase[ which(Fishbase$fishID==l),"Vol_Ch"])*3600)
      resblc[i,6]<-Datachamberindvblc[i,3]
      resblc[i,7]<-as.character(Data[1,"DateTime"])
      resblc[i,8]<-summary(b)$coefficients["linearregblc[, 1]","Std. Error"]
      resblc[i,9]<-summary(b)$coefficients["linearregblc[, 1]","Pr(>|t|)"]
      resblc[i,10]<-summary(b)$r.squared
    }

    ##mean end point for fish blank O2 en Absolu Time
    Blanktailfish02<- mean(tail(resblc[,5],3))########### mean of three point in resblc
    BlanktailfishTime<- mean(tail(resblc[,1],3))

    ##Blank blank Chamber

    resblcblc<-data.frame(matrix(ncol=11,nrow=0))
    colnames(resblcblc)<- c("MidTime (sec)", "StartTime (sec)", "EndTime (sec)","linear coeff","MO2 (mg/h)","Temp(°C)","Date","SE","p-value","Rsquared","MO2cor")

    for (i in c(1:3)){
      resblcblc[i,1]<-(firstmidpointblank+(i-1)*periodblc)
      resblcblc[i,2]<-resblcblc[i,1]-(measureperiodblc/2)
      resblcblc[i,3]<-resblcblc[i,1]+(measureperiodblc/2)
      Datablank[,O2column[blankposition]]<-as.numeric(as.character(Datablank[,O2column[blankposition]]))
      Datablank[,Tempcolumn[blankposition]]<-as.numeric(as.character(Datablank[,Tempcolumn[blankposition]]))

      merge(resblcblc,Datablank[,c("DateTime","Time.s")],by.x="MidTime (sec)",by.y="Time.s",all.x = T) ###A voir les colonnes datablank
      Datachamberindvblc<-na.omit(Datablank[,c("Time.s",O2column[blankposition],Tempcolumn[blankposition])])
      linearregblc<-subset(Datachamberindvblc, Datachamberindvblc$Time.s>=resblcblc[i,2] & Datachamberindvblc$Time.s<=resblcblc[i,3])
      b<-lm(linearregblc[,2]~linearregblc[,1])
      resblcblc[i,4]<-b$coefficients[2]
      resblcblc[i,5]<-(-b$coefficients[2]*(Fishbase[ which(Fishbase$fishID==l),"Vol_Ch_Bl"])*3600)
      resblcblc[i,6]<-Datachamberindvblc[i,3]
      resblcblc[i,7]<-as.character(Data[1,"DateTime"])
      resblcblc[i,8]<-summary(b)$coefficients["linearregblc[, 1]","Std. Error"]
      resblcblc[i,9]<-summary(b)$coefficients["linearregblc[, 1]","Pr(>|t|)"]
      resblcblc[i,10]<-summary(b)$r.squared
    }
    ##mean end point for blank O2 en Absolu Time
    BlanktailblankO2<<-mean(tail(resblcblc[,5],3))
    BlanktailTime<-mean(tail(resblcblc[,1],3))

    ##Create linear regression with start and end point of blank slope
    blankcorrectionreg<-subset(Resultblank[,c(1,5)],Resultblank[,1]==Firststart)
    blankcorrectionreg[2,]<-c(BlanktailTime,BlanktailblankO2)
    reg1<-lm(blankcorrectionreg[,2]~blankcorrectionreg[,1])
    regblank <- function(x) reg1$coefficient[2]*x + reg1$coefficient[1]

    ##Create linear regression with start point of blank slope and end point blank fish chamber
    chambercorrectionreg<-subset(Resultblank[,c(1,5)],Resultblank[,1]==Firststart)
    chambercorrectionreg[2,]<-c(BlanktailfishTime,Blanktailfish02)
    reg2<-lm(chambercorrectionreg[,2]~chambercorrectionreg[,1])
    regchamber <- function(x) reg2$coefficient[2]*x + reg2$coefficient[1]

    d<-ggplot(res,environment=environment())+geom_point(aes(x=res[,1],y=regchamber(res[,1])),colour="red")+geom_point(aes(x=res[,1],y=regblank(res[,1]))) +xlab("Time (h)") +ylab("MO2 (mg/h)")  +xlim(Firststart,max(res[,1]))
    ggsave(d,filename=paste("Chamber",Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"],"Deltaregression.pdf",sep=""),path = wayout,width=20, height=4)

    res$deltablankfish<-ifelse(res[,1]>Firststart,regchamber(res[,1])-regblank(res[,1]),0)
    res$Poly<-polyblank(res[,1])
    res$MO2cor<-ifelse(res[,1]>Firststart,res[,5] - (res$Poly+res$deltablankfish),res[,5])
    }

    if(Blankcorrection==F) {
      res$deltablankfish<-0
      res$Poly<-NA
      res$MO2cor<-res[,5]
    }


    # layer SMR
    #Rsquared layer
    meanRsquared <- mean(res$Rsquared)
    SdRsquared <- sd(res$Rsquared)
    resstep1 <- subset(res, res[, 10] > (meanRsquared - (nsdevsrsquared * SdRsquared)))

    #lowest value layer
    LowestO2valueOutlier <- sort(resstep1[, 11], decreasing = F)[1:numberoflowvalues]
    resstep2 <- subset(resstep1, resstep1[, 11] %in% c(LowestO2valueOutlier))

    #Mean lowest value layer
    meanLowestO2values <- mean(resstep2$MO2cor)
    SdLowestO2values <- sd(resstep2$MO2cor)
    resstep3 <- subset(resstep2, resstep2[, 11] > (meanLowestO2values - (0.5 * SdLowestO2values)) & resstep2[, 11] < (meanLowestO2values + (2 * SdLowestO2values)))

    res$selection <- ifelse(res[, 11] %in% resstep3[, 11] & res[, 10] %in% resstep3[, 10], "SMR", ifelse(res[, 11] %in% resstep2[, 11] & res[, 10] %in% resstep2[, 10],"meanLowestO2valueOutlier", ifelse(res[, 11] %in% resstep1[, 11] & res[, 10] %in% resstep1[, 10], "NotUsedInSMRcalculation", "RsquaredOutlier")))
    #write Table
    write.table(res, paste(wayout, "/", "resultchamber", Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"], ".csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")
    write.table(resblc, paste(wayout, "/", "resultblankchamber", Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"],".csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")


    c<-ggplot(res,environment = environment())+geom_point(aes(x=res[,1]/3600,y=res[,5]),alpha=0.1)+ylab(colnames(res[5])) +xlab("Time (h)") +geom_point(aes(x=res[,1]/3600,y=MO2cor,colour=selection)) +scale_color_manual(values = c("NotUsedInSMRcalculation" = "black","RsquaredOutlier" = "red","meanLowestO2valueOutlier"="blue","SMR" = "green"))
    ggsave(c,filename=paste("Chamber",Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"],".pdf",sep=""),path = wayout,width=20, height=4)
    print(c)
    #####################################Final Table###################################################

    Result[which(fishID==l),1]<-l
    Result[which(fishID==l),2]<-res$Date[[1]]
    Result[which(fishID==l),3]<-Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"]
    Result[which(fishID==l),4]<-mean(subset(res,res$selection=="SMR")$MO2cor)
    Result[which(fishID==l),5]<-sd(subset(res,res$selection=="SMR")$MO2cor)
    Result[which(fishID==l),6]<-max(subset(res,!res$selection=="RsquaredOutlier")$MO2cor)
    Result[which(fishID==l),7]<-min(subset(res,res$selection=="SMR")$MO2cor)


    ###FirstMR###
    FMRmidpoint<-firstmidpoint+(1-1+(Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"]-1))*period
    FMRstartpoint<-res[1,1]-(measureperiod/2)
    linearregFMR<-subset(Datachamberindv, Datachamberindv$Time.s>=FMRstartpoint & Datachamberindv$Time.s<=FMRmidpoint)
    b<-lm(linearregFMR[,2]~linearregFMR[,1])
    Result[which(fishID==l),8]<-(-b$coefficients[2]*(Fishbase[ which(Fishbase$fishID==l),"Vol_Ch"]-Fishbase[ which(Fishbase$fishID==l),"W"])*3600)
    Result[which(fishID==l),9]<-mean(res[,6])
    Result[which(fishID==l),10]<-Fishbase[ which(Fishbase$fishID==l),"W"]
    Result[which(fishID==l),11]<-Fishbase[ which(Fishbase$fishID==l),"Vol_Ch"]
    Result[which(fishID==l),12]<-ifelse(Blankcorrection==T,"YES","NO")
  }
  write.table(Result, paste(wayout, "/", "ResultRun.csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")
  ResultRun<<-Result
}
