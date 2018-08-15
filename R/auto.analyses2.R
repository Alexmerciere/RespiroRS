auto.analyses2<-function(Data,Datablank,Wfish,fishposition,fishID,blankposition,wayout,Nnoise=NULL,correctFirstendslope=NULL,Nnoisestartdev=NULL,equalVolume=NULL){

  if (is.null(Nnoise)) {Nnoise<- 5 }

  if (is.null(correctFirstendslope)) { chamberfirstslope <-1 }
  if (!is.null(correctFirstendslope)) { chamberfirstslope <-correctFirstendslope }
  if (is.null(Nnoisestartdev)) {Nnoisestartdev<- 2 }
  if (is.null(equalVolume)) {equalVolume<- T }


  ifelse (blankposition==1,deltaBlankposition<-1,deltaBlankposition<-0)
  ifelse (chamberfirstslope==1,corfirstslope<-0,corfirstslope<-chamberfirstslope-1)


  #################Experiment Settings##############
  ##fish record
  closetime <- as.numeric(readline(prompt="closetime (s): "))  #s
  opentime <- as.numeric(readline(prompt="opentime (s): "))  #s
  waittime <- as.numeric(readline(prompt="waittime (s): "))  #s
  enddiscard <- as.numeric(readline(prompt="enddiscard (s): "))  #s
  ifelse (equalVolume==F,{ChamberVolume <- as.numeric(unlist(strsplit(readline(prompt="list of Chambers Volumes (L): "), ","))) }, {ChamberVolume <- as.numeric(readline(prompt="Chamber Volume (L): "));ChamberVolume <- rep(ChamberVolume,length(fishposition)+1)})

  #ChamberVolume <- as.numeric(readline(prompt="ChamberVolume (L): "))  #L

  ##blank record
  closetimeblc <- as.numeric(readline(prompt="closetimeblank (s): "))  #s
  opentimeblc <- as.numeric(readline(prompt="opentimeblank (s): "))  #s
  waittimeblc <- as.numeric(readline(prompt="waittimeblank (s): "))  #s
  enddiscardblc <- as.numeric(readline(prompt="enddiscardblank (s): "))  #s

  nsdevsrsquared<-2
  unit<-"mg/L"
  O2column<-c(4,5,6,7,13,14,15,16,22,23,24,25,31,32,33,34)
  Tempcolumn<-c(8,9,10,11,17,18,19,20,26,27,28,29,35,36,37,38)

  period<-opentime+closetime
  measureperiod<-closetime-waittime-enddiscard
  periodblc<-opentimeblc+closetimeblc
  measureperiodblc<-closetimeblc-waittimeblc-enddiscardblc
  ifelse(chamberfirstslope==1,adustfirstslope<-0, adustfirstslope<-chamberfirstslope*(period/2))

  ############################File with Fish record######################
  #########Moving average############
  testdata<-data.frame(subset(Data,Timeabsolu2<10000))
  testdata<-na.omit(testdata[,c(23,O2column[chamberfirstslope])])
  testdata$movavg<-movavg(testdata[,2],13,type=c("s"))

  ##Calculate dif
  testdata$dif<-NA
  for (row in 1:nrow(testdata)){
    ifelse(testdata[row,1]>99+adustfirstslope & testdata[row,1]<4900+adustfirstslope,testdata[row,4]<-(testdata[(row+50),3]-testdata[row,3]-(testdata[row,3]-testdata[(row-50),3])),testdata[row,4]<-NA)

  }
  ##graphic dif
  c<-ggplot(testdata)+geom_point(aes(x=Timeabsolu2,y=dif))
  print(c)

  ##noise
  noise<-sd(na.omit(subset(Data,Timeabsolu2>100&Timeabsolu2<200)[,O2column[chamberfirstslope]]))
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
  c<-ggplot(Data,environment = environment())+geom_point(aes(x=Timeabsolu2,y=Data[,O2column[chamberfirstslope]]))+ylab(paste(colnames(Data[O2column[chamberfirstslope]]),unit)) +xlab("Time (sec)")+xlim(0,10000) +
    geom_vline(aes(xintercept = Firstend),color="red")
  print(c)
  ggsave(c,filename="firstslope.pdf",path = wayout,width=20, height=4)
  question2 <- readline(prompt="First end slope is at the good position ?(YES or NO) : ")
  ifelse(question2=="YES",NA,{nperiod <-as.numeric(readline(prompt="How many periode do you want add (+) or delete (-) ? "));
  Firstend<-(nperiod*period)+Firstend;
  print(Firstend);
  c<-ggplot(Data,environment = environment())+geom_point(aes(x=Timeabsolu2,y=Data[,O2column[chamberfirstslope]]))+ylab(paste(colnames(Data[O2column[chamberfirstslope]]),unit)) +xlab("Time (sec)")+xlim(0,10000) +geom_vline(aes(xintercept = Firstend),color="red") ;
  ggsave(c,filename="firstslopecorrected.pdf",path = wayout,width=20, height=4);NA})

  Firstend<-Firstend-(corfirstslope*period)



  firstmidpoint<-Firstend-enddiscard-(measureperiod/2)
  wholeperiods<-(max(na.omit(Data$Timeabsolu2))-firstmidpoint)/period
  numberoflowvalues<-wholeperiods/10 ####10 pourcent of point
  numbperiods<-round(wholeperiods)


  ###########################################################################################################
  ####################################File with Fishblank record#############################################
  #########Moving average############
  testdata<-Datablank[1:1000,]
  testdata<-na.omit(testdata[,c(23,O2column[fishposition[1]])])
  library("pracma", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
  testdata$movavg<-movavg(testdata[,2],13,type=c("s"))

  ##Calculate dif
  testdata$dif<-NA
  for (x in c(51:950)){
    testdata[x,4]<-(testdata[(x+50),3]-testdata[x,3]-(testdata[x,3]-testdata[(x-50),3]))

  }
  ##graphic dif
  ggplot(testdata)+geom_point(aes(x=Timeabsolu2,y=dif))

  ##noise
  noise<-sd(testdata[c(51:100),4])
  print(noise)
  ##Find first endslope
  Toppos<-list()
  for (row in c(51:950)){
    ifelse(testdata[row,4]>testdata[(row-1),4] & testdata[row,4]>testdata[(row-5),4] & testdata[row,4]>testdata[(row-7),4] & testdata[row,4]>testdata[(row+1),4] & testdata[row,4]>testdata[(row+5),4] & testdata[row,4]>testdata[(row+7),4] & testdata[row,4]>(3*noise),Toppos<-list.append(Toppos,testdata[row,1]),NA)
  }
  #Time of the first end slope
  Firstendblank<-as.numeric(Toppos[[1]])
  print(Firstendblank)
  #graph with end slope and mean temperature during experiment
  c<-ggplot(Datablank,environment = environment())+geom_point(aes(x=Timeabsolu2,y=Datablank[,O2column[fishposition[1]]]))+ylab(paste(colnames(Datablank[O2column[fishposition[1]]]),unit)) +xlab("Time (sec)") + geom_vline(aes(xintercept = Firstendblank,color="red"))
  print(c)
  ggsave(c,filename="firstslopeblank.pdf",path = wayout,width=20, height=4)

  ifelse(question1<-"YES",NA,{Firstendblank <-as.numeric(readline(prompt="Give a new value of First end "))+Firstendblank;c<-ggplot(Datablank,environment = environment())+geom_point(aes(x=Timeabsolu2,y=Datablank[,4]))+ylab(paste(colnames(Datablank[4]),unit)) +xlab("Time (sec)") + geom_vline(aes(xintercept = Firstendblank,color="red"));ggsave(c,filename="firstslopeblankcorrected.pdf",path = wayout,width=20, height=4)})
  question1 <- readline(prompt="First end slope blank is at the good position ?(YES or NO) : ")
  ifelse(question1=="YES",NA,{nperiod <-as.numeric(readline(prompt="How many periode do you want add (+) or delete (-) ? "));
  Firstendblank<-(nperiod*periodblc)+Firstendblank;
  print(Firstendblank);
  c<-ggplot(Datablank,environment = environment())+geom_point(aes(x=Timeabsolu2,y=Datablank[,4]))+ylab(paste(colnames(Datablank[4]),unit)) +xlab("Time (sec)") + geom_vline(aes(xintercept = Firstendblank,color="red"))
  ggsave(c,filename="firstslopeblankcorrected.pdf",path = wayout,width=20, height=4);NA})


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
    merge(res,Data[,c(21,23)],by.x="MidTime (sec)",by.y="Timeabsolu2",all.x = T)
    Datachamberindv<-na.omit(Data[,c(23,O2column[blankposition],Tempcolumn[blankposition],1)])
    linearreg<-subset(Datachamberindv, Datachamberindv$Timeabsolu2>=res[i,2] & Datachamberindv$Timeabsolu2<=res[i,3])
    b<-lm(linearreg[,2]~linearreg[,1])
    res[i,4]<-b$coefficients[2]
    res[i,5]<-(-b$coefficients[2]*(ChamberVolume[blankposition])*3600)
    res[i,6]<-Datachamberindv[i,3]
    res[i,7]<-as.character(Datachamberindv[1,4])
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
  mean<-mean(Resultblank[c(1:10),5])
  Toppos<-list()
  for (row in 1:nrow(Resultblank)){
    ifelse(Resultblank[row,11]>mean+Nnoisestartdev*noise,Toppos<-list.append(Toppos,Resultblank[row,1]),NA)
  }
  #Time of the start slope deviation
  Firststart<-as.numeric(Toppos[1])
  print(Firststart)
  c<-ggplot(Resultblank,environment=environment())+geom_point(aes(x=Resultblank[,1]/3600,y=Resultblank[,5]))+ylab(paste(colnames(Resultblank[5]))) +xlab("Time (sec)")+geom_point(aes(x=Resultblank[,1]/3600,y=Resultblank[,11],colour="red"))+ geom_vline(aes(xintercept =Firststart/3600 ,color="red"))


  ggsave(c,filename="Blank_slope_start.pdf",path = wayout,width=20, height=4)
  print(c)


  ########################################################################################


  ####################################RESULT CHAMBER FISH###############################################
  Result<-data.frame(matrix(ncol=11,nrow=0))
  colnames(Result)<- c("fishID","Date", "Chamber","SMR","sdSMR","MaxMR","MinMR","FirstMR","MeanTemp (°C)","Weight (g)","Chamber Volume (L)")

  for (l in fishposition){
    res<-data.frame(matrix(ncol=11,nrow=0))
    colnames(res)<- c("MidTime (sec)", "StartTime (sec)", "EndTime (sec)","linear coeff","MO2 (mg/h)","Temp(°C)","Date","SE","p-value","Rsquared","MO2cor")
    numbperiods<-round(wholeperiods)-(fishposition[l]-1) - deltaBlankposition
    resblc<-data.frame(matrix(ncol=11,nrow=0))
    colnames(resblc)<- c("MidTime (sec)", "StartTime (sec)", "EndTime (sec)","linear coeff","MO2 (mg/h)","Temp(°C)","Date","SE","p-value","Rsquared","MO2cor")

    ##Record Fish Chamber
    ##Create table with mid time measurment, start and end per chamber
    for (i in 1:numbperiods){
      res[i,1]<-firstmidpoint+(i-1+(l-1))*period -(deltaBlankposition*period)###remove l-1 if you start all the chamber in the same time
      res[i,2]<-res[i,1]-(measureperiod/2)
      res[i,3]<-res[i,1]+(measureperiod/2)
      merge(res,Data[,c(21,23)],by.x="MidTime (sec)",by.y="Timeabsolu2",all.x = T)
      Datachamberindv<-na.omit(Data[,c(23,O2column[l],Tempcolumn[l],1)])
      linearreg<-subset(Datachamberindv, Datachamberindv$Timeabsolu2>=res[i,2] & Datachamberindv$Timeabsolu2<=res[i,3])
      b<-lm(linearreg[,2]~linearreg[,1])
      res[i,4]<-b$coefficients[2]
      res[i,5]<-(-b$coefficients[2]*(ChamberVolume[l]-Wfish[l])*3600)
      res[i,6]<-Datachamberindv[i,3]
      res[i,7]<-as.character(Datachamberindv[1,4])
      res[i,8]<-summary(b)$coefficients["linearreg[, 1]","Std. Error"]
      res[i,9]<-summary(b)$coefficients["linearreg[, 1]","Pr(>|t|)"]
      res[i,10]<-summary(b)$r.squared
    }
    ##Blank Fish Chamber
    for (i in c(1:3)){
      resblc[i,1]<-(firstmidpointblank+(i-1)*periodblc)
      resblc[i,2]<-resblc[i,1]-(measureperiodblc/2)
      resblc[i,3]<-resblc[i,1]+(measureperiodblc/2)
      merge(resblc,Datablank[,c(21,23)],by.x="MidTime (sec)",by.y="Timeabsolu2",all.x = T) ###A voir les colonnes datablank
      Datachamberindvblc<-na.omit(Datablank[,c(23,O2column[l],Tempcolumn[l],1)])
      linearregblc<-subset(Datachamberindvblc, Datachamberindvblc$Timeabsolu2>=resblc[i,2] & Datachamberindvblc$Timeabsolu2<=resblc[i,3])
      b<-lm(linearregblc[,2]~linearregblc[,1])
      resblc[i,4]<-b$coefficients[2]
      resblc[i,5]<-(-b$coefficients[2]*(ChamberVolume[l])*3600)
      resblc[i,6]<-Datachamberindvblc[i,3]
      resblc[i,7]<-as.character(Datachamberindv[1,4])
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
      merge(resblcblc,Datablank[,c(21,23)],by.x="MidTime (sec)",by.y="Timeabsolu2",all.x = T) ###A voir les colonnes datablank
      Datachamberindvblc<-na.omit(Datablank[,c(23,O2column[blankposition],Tempcolumn[blankposition],1)])
      linearregblc<-subset(Datachamberindvblc, Datachamberindvblc$Timeabsolu2>=resblcblc[i,2] & Datachamberindvblc$Timeabsolu2<=resblcblc[i,3])
      b<-lm(linearregblc[,2]~linearregblc[,1])
      resblcblc[i,4]<-b$coefficients[2]
      resblcblc[i,5]<-(-b$coefficients[2]*(ChamberVolume[blankposition])*3600)
      resblcblc[i,6]<-Datachamberindvblc[i,3]
      resblcblc[i,7]<-as.character(Datachamberindvblc[1,4])
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
    ggsave(d,filename=paste("Chamber",l,"Deltaregression.pdf",sep=""),path = wayout,width=20, height=4)

    res$deltablankfish<-ifelse(res[,1]>Firststart,regchamber(res[,1])-regblank(res[,1]),0)
    res$Poly<-polyblank(res[,1])
    res$MO2cor<-ifelse(res[,1]>Firststart,res[,5] - (res$Poly+res$deltablankfish),res[,5])

    # layer SMR
#Rsquared layer
    meanRsquared <- mean(res$Rsquared)
    SdRsquared <- sd(res$Rsquared)
    resstep1 <- subset(res, res[, 10] > (meanRsquared - (nsdevsrsquared * SdRsquared)))

#lowest value layer
    LowestO2valueOutlier <- sort(res[, 11], decreasing = F)[1:numberoflowvalues]
    resstep2 <- subset(resstep1, resstep1[, 11] %in% c(LowestO2valueOutlier))

#Mean lowest value layer
    meanLowestO2values <- mean(resstep2$MO2cor)
    SdLowestO2values <- sd(resstep2$MO2cor)
    resstep3 <- subset(resstep2, resstep2[, 11] > (meanLowestO2values - (2 * SdLowestO2values)) & resstep2[, 11] < (meanLowestO2values + (2 * SdLowestO2values)))

    res$selection <- ifelse(res[, 11] %in% resstep3[, 11] & res[, 10] %in% resstep3[, 10], "SMR", ifelse(res[, 11] %in% resstep2[, 11] & res[, 10] %in% resstep2[, 10],"meanLowestO2valueOutlier", ifelse(res[, 11] %in% resstep1[, 11] & res[, 10] %in% resstep1[, 10], "NotUsedInSMRcalculation", "RsquaredOutlier")))
   #write Table
    write.table(res, paste(wayout, "/", "resultchamber", l, ".csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")
    write.table(resblc, paste(wayout, "/", "resultblankchamber", l,".csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")


    c<-ggplot(res,environment = environment())+geom_point(aes(x=res[,1]/3600,y=res[,5]),alpha=0.1)+ylab(colnames(res[5])) +xlab("Time (h)") +geom_point(aes(x=res[,1]/3600,y=MO2cor,colour=selection)) +scale_color_manual(values = c("NotUsedInSMRcalculation" = "black","RsquaredOutlier" = "red","meanLowestO2valueOutlier"="blue","SMR" = "green"))
    ggsave(c,filename=paste("Chamber",l,".pdf",sep=""),path = wayout,width=20, height=4)
    print(c)
    #####################################Final Table###################################################
    Result[l,1]<-fishID[[l]]
    Result[l,2]<-res$Date[[1]]
    Result[l,3]<-fishposition[[l]]
    Result[l,4]<-mean(subset(res,res$selection=="SMR")$MO2cor)
    Result[l,5]<-sd(subset(res,res$selection=="SMR")$MO2cor)
    Result[l,6]<-max(subset(res,!res$selection=="RsquaredOutlier")$MO2cor)
    Result[l,7]<-min(subset(res,res$selection=="SMR")$MO2cor)


    ###FirstMR###
    FMRmidpoint<-firstmidpoint+(1-1+(l-1))*period ####remove l-1 if you start all the chamber in the same time
    FMRstartpoint<-res[1,1]-(measureperiod/2)
    linearregFMR<-subset(Datachamberindv, Datachamberindv$Timeabsolu2>=FMRstartpoint & Datachamberindv$Timeabsolu2<=FMRmidpoint)
    b<-lm(linearregFMR[,2]~linearregFMR[,1])
    Result[l,8]<-(-b$coefficients[2]*(ChamberVolume[l]-Wfish[l])*3600)
    Result[l,9]<-mean(res[,6])
    Result[l,10]<-Wfish[[l]]
    Result[l,11]<-ChamberVolume[l]
  }
  write.table(Result, paste(wayout, "/", "ResultRun.csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")
  ResultRun<<-Result
}
