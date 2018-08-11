blank.correction<-function(Datachamber,Datablankchamber,Datablankfish,Numfishchamb,NumBlankchamb,fishID,Wfish,wayout,NnoiseFirstslope=NULL){
  if (is.null(NnoiseFirstslope)) {NnoiseFirstslope<- 2 }

  #################Experiment Settings##############
  ##fish record

  ChamberVolume <- as.numeric(readline(prompt="ChamberVolume (L): "))  #L

  ##blank record
  closetimeblc <- as.numeric(readline(prompt="closetimeblank (s): "))  #s
  opentimeblc <- as.numeric(readline(prompt="opentimeblank (s): "))  #s
  waittimeblc <- as.numeric(readline(prompt="waittimeblank (s): "))  #s
  enddiscardblc <- as.numeric(readline(prompt="enddiscardblank (s): "))  #s

  nsdevsrsquared<-2
  unit<-"mg/L"
  O2column<-c(4,5,6,7,13,14,15,16,22,23,24,25,31,32,33,34)
  Tempcolumn<-c(8,9,10,11,17,18,19,20,26,27,28,29,35,36,37,38)

  periodblc<-opentimeblc+closetimeblc
  measureperiodblc<-closetimeblc-waittimeblc-enddiscardblc

  #################################

  e<-lm(Datablankchamber[,5] ~ poly(Datablankchamber[,13], 4, raw=TRUE))

  polyblank <- function(x) {e$coefficient[5]*x^4 + e$coefficient[4]*x^3 + e$coefficient[3]*x^2 + e$coefficient[2]*x + e$coefficient[1]}
  Datablankchamber$Poly<<-polyblank(Datablankchamber[,13])
  c<-ggplot(Datablankchamber,environment = environment())+geom_point(aes(x=Datablankchamber[,13]/3600,y=Datablankchamber[,5]))+ylab(colnames(Datablankchamber[5])) +xlab("Time (h)")+ stat_smooth(aes(x=Datablankchamber[,13]/3600,y=Datablankchamber[,5]),method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, 4, raw=TRUE),colour="blue")
  Resultblank<-Datablankchamber
  write.table(Resultblank, paste(wayout, "/resultchamberblank.csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")
  print(c)
  ggsave(c,filename="Blank.pdf",path = wayout,width=20, height=4)


  ##mean end point for O2 en Absolu Time
  BlanktailO2<-mean(tail(Resultblank[,5],3))
  BlanktailTime<-mean(tail(Resultblank[,1],3))

  #################SLOPE BLANK RESPIRATION##############
  #######################################Find start of slope deviation##################################
  noise<-sd(Resultblank[c(1:10),5])
  mean<-mean(Resultblank[c(1:10),5])
  Toppos<-list()
  for (row in 1:nrow(Resultblank)){
    ifelse(Resultblank[row,"Poly"]>mean+NnoiseFirstslope*noise,Toppos<-list.append(Toppos,Resultblank[row,1]),NA)
  }
  #Time of the start slope deviation
  Firststart<-as.numeric(Toppos[1])
  print(Firststart)
  c<-ggplot(Resultblank,environment=environment())+geom_point(aes(x=Resultblank[,1]/3600,y=Resultblank[,5]))+ylab(paste(colnames(Resultblank[5]))) +xlab("Time (sec)")+geom_point(aes(x=Resultblank[,1]/3600,y=Resultblank[,14],colour="red"))+ geom_vline(aes(xintercept =Firststart/3600 ,color="red"))


  ggsave(c,filename="Blank_slope_start.pdf",path = wayout,width=20, height=4)
  print(c)


  ###########################################################################################################
  ####################################File with Fishblank record#############################################
  #########Moving average############
  Datablank<-Datablankfish
  testdata<-Datablank[1:1000,]
  testdata<-na.omit(testdata[,c(23,O2column[Numfishchamb])])
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
    ifelse(testdata[row,4]>testdata[(row-1),4] & testdata[row,4]>testdata[(row-5),4] & testdata[row,4]>testdata[(row-7),4] & testdata[row,4]>testdata[(row+1),4] & testdata[row,4]>testdata[(row+5),4] & testdata[row,4]>testdata[(row+7),4] & testdata[row,4]>(5*noise),Toppos<-list.append(Toppos,testdata[row,1]),NA)
  }
  #Time of the first end slope
  Firstendblank<-as.numeric(Toppos[[1]])
  print(Firstendblank)
  #graph with end slope and mean temperature during experiment
  c<-ggplot(Datablank,environment = environment())+geom_point(aes(x=Timeabsolu2,y=Datablank[,O2column[Numfishchamb]]))+ylab(paste(colnames(Datablank[O2column[Numfishchamb]]),unit)) +xlab("Time (sec)") + geom_vline(aes(xintercept = Firstendblank,color="red"))
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


  ####################################RESULT CHAMBER FISH###############################################
  Result<-data.frame(matrix(ncol=11,nrow=0))
  colnames(Result)<- c("fishID","Date", "Chamber","SMR","sdSMR","MaxMR","MinMR","FirstMR","MeanTemp (°C)","Weight (g)","Chamber Volume (L)")

  res<-Datachamber
  wholeperiods<-nrow(res)
  numberoflowvalues<-wholeperiods/10 ####10 pourcent of point
  numbperiods<-round(wholeperiods)

  resblc<-data.frame(matrix(ncol=11,nrow=0))
  colnames(resblc)<- c("MidTime (sec)", "StartTime (sec)", "EndTime (sec)","linear coeff","MO2 (mg/h)","Temp(°C)","Date","SE","p-value","Rsquared","MO2cor")


    ##Blank Fish Chamber
    for (i in c(1:3)){
      resblc[i,1]<-(firstmidpointblank+(i-1)*periodblc)
      resblc[i,2]<-resblc[i,1]-(measureperiodblc/2)
      resblc[i,3]<-resblc[i,1]+(measureperiodblc/2)
      merge(resblc,Datablank[,c(21,23)],by.x="MidTime (sec)",by.y="Timeabsolu2",all.x = T) ###A voir les colonnes datablank
      Datachamberindvblc<-na.omit(Datablank[,c(23,O2column[Numfishchamb],Tempcolumn[Numfishchamb],1)])
      linearregblc<-subset(Datachamberindvblc, Datachamberindvblc$Timeabsolu2>=resblc[i,2] & Datachamberindvblc$Timeabsolu2<=resblc[i,3])
      b<-lm(linearregblc[,2]~linearregblc[,1])
      resblc[i,4]<-b$coefficients[2]
      resblc[i,5]<-(-b$coefficients[2]*(ChamberVolume)*3600)
      resblc[i,6]<-Datachamberindvblc[i,3]
      resblc[i,7]<-as.character(Datachamberindvblc[1,4])
      resblc[i,8]<-summary(b)$coefficients["linearregblc[, 1]","Std. Error"]
      resblc[i,9]<-summary(b)$coefficients["linearregblc[, 1]","Pr(>|t|)"]
      resblc[i,10]<-summary(b)$r.squared
    }
    ##mean end point for fish blank O2 en Absolu Time
    Blanktailfish02<<- mean(tail(resblc[,5],3))########### mean of three point in resblc
    BlanktailfishTime<- mean(tail(resblc[,1],3))

    ##Blank blank Chamber

    resblcblc<-data.frame(matrix(ncol=11,nrow=0))
    colnames(resblcblc)<- c("MidTime (sec)", "StartTime (sec)", "EndTime (sec)","linear coeff","MO2 (mg/h)","Temp(°C)","Date","SE","p-value","Rsquared","MO2cor")

    for (i in c(1:3)){
      resblcblc[i,1]<-(firstmidpointblank+(i-1)*periodblc)
      resblcblc[i,2]<-resblcblc[i,1]-(measureperiodblc/2)
      resblcblc[i,3]<-resblcblc[i,1]+(measureperiodblc/2)
      merge(resblcblc,Datablank[,c(21,23)],by.x="MidTime (sec)",by.y="Timeabsolu2",all.x = T) ###A voir les colonnes datablank
      Datachamberindvblc<-na.omit(Datablank[,c(23,O2column[NumBlankchamb],Tempcolumn[NumBlankchamb],1)])
      linearregblc<-subset(Datachamberindvblc, Datachamberindvblc$Timeabsolu2>=resblcblc[i,2] & Datachamberindvblc$Timeabsolu2<=resblcblc[i,3])
      b<-lm(linearregblc[,2]~linearregblc[,1])
      resblcblc[i,4]<-b$coefficients[2]
      resblcblc[i,5]<-(-b$coefficients[2]*(ChamberVolume)*3600)
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
    ggsave(d,filename=paste("Chamber",Numfishchamb,"Deltaregression.pdf",sep=""),path = wayout,width=20, height=4)

    res$deltablankfish<-ifelse(res[,1]>Firststart,regchamber(res[,1])-regblank(res[,1]),0)
    res$Poly<-polyblank(res[,1])
    res$MO2cor<-ifelse(res[,1]>Firststart,res[,5] - (res$Poly+res$deltablankfish),res[,5])

    # layer SMR
    #Rsquared layer
    meanRsquared <- mean(res$Rsquared)
    SdRsquared <- sd(res$Rsquared)
    resstep1 <- subset(res, res[, "Rsquared"] > (meanRsquared - (nsdevsrsquared * SdRsquared)))

    #lowest value layer
    LowestO2valueOutlier <- sort(res[, "MO2cor"], decreasing = F)[1:numberoflowvalues]
    resstep2 <- subset(resstep1, resstep1[, "MO2cor"] %in% c(LowestO2valueOutlier))

    #Mean lowest value layer
    meanLowestO2values <- mean(resstep2$MO2cor)
    SdLowestO2values <- sd(resstep2$MO2cor)
    resstep3 <- subset(resstep2, resstep2[, "MO2cor"] > (meanLowestO2values - (2 * SdLowestO2values)) & resstep2[, "MO2cor"] < (meanLowestO2values + (2 * SdLowestO2values)))

    res$selection <- ifelse(res[, "MO2cor"] %in% resstep3[, "MO2cor"] & res[, "Rsquared"] %in% resstep3[, "Rsquared"], "SMR", ifelse(res[, "MO2cor"] %in% resstep2[, "MO2cor"] & res[, "Rsquared"] %in% resstep2[, "Rsquared"],"meanLowestO2valueOutlier", ifelse(res[, "MO2cor"] %in% resstep1[, "MO2cor"] & res[, "Rsquared"] %in% resstep1[, "Rsquared"], "NotUsedInSMRcalculation", "RsquaredOutlier")))
    #write Table
    write.table(res, paste(wayout, "/", "resultchamber", Numfishchamb, ".csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")
    write.table(resblc, paste(wayout, "/", "resultblankchamber", Numfishchamb,".csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")


    c<-ggplot(res,environment = environment())+geom_point(aes(x=res[,"Timeabsolu2"]/3600,y=res[,5]),alpha=0.1)+ylab(colnames(res[5])) +xlab("Time (h)") +geom_point(aes(x=res[,"Timeabsolu2"]/3600,y=MO2cor,colour=selection)) +scale_color_manual(values = c("NotUsedInSMRcalculation" = "black","RsquaredOutlier" = "red","meanLowestO2valueOutlier"="blue","SMR" = "green"))
    ggsave(c,filename=paste("Chamber",Numfishchamb,".pdf",sep=""),path = wayout,width=20, height=4)
    print(c)
    #####################################Final Table###################################################
    Result[1,1]<-fishID
    Result[1,2]<-as.character(res$Date[1])
    Result[1,3]<-Numfishchamb
    Result[1,4]<-mean(subset(res,res$selection=="SMR")$MO2cor)
    Result[1,5]<-sd(subset(res,res$selection=="SMR")$MO2cor)
    Result[1,6]<-max(subset(res,!res$selection=="RsquaredOutlier")$MO2cor)
    Result[1,7]<-min(subset(res,res$selection=="SMR")$MO2cor)


    ###FirstMR###
    Result[1,8]<-res$MO2cor[1]
    Result[1,9]<-mean(res[,6])
    Result[1,10]<-Wfish
    Result[1,11]<-ChamberVolume

  write.table(Result, paste(wayout, "/", "ResultRunchamber",Numfishchamb,".csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")
  ResultRun<<-Result
}
