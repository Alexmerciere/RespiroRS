row.analyses.table<-function(Data,Fishbase,fishID,wayout,Wfish=NULL,fishposition=NULL,Nnoise=NULL,correctFirstendslope=NULL,waittime=NULL,enddiscard=NULL,samestart=NULL){

    if (is.null(Nnoise)) {Nnoise<- 5 }
    if (is.null(correctFirstendslope)) { chamberfirstslope <-Fishbase[ which(Fishbase$fishID==fishID[1]),"Nb_Ch"]  }
    if (!is.null(correctFirstendslope)) { chamberfirstslope <-correctFirstendslope }
    if (is.null(samestart)) {samestart<- F }


    if (is.null(Wfish)) {Wfish<- Fishbase[ which(Fishbase$fishID%in%fishID),"W"] }
    if (is.null(fishposition)) {fishposition<- Fishbase[ which(Fishbase$fishID%in%fishID),"Nb_Ch"] }

    ifelse (chamberfirstslope==1,corfirstslope<-0,corfirstslope<-chamberfirstslope-1)
    nbchanalyses<-length(fishID)

    #################Experiment Settings##############
    ##fish record
    closetime <- Fishbase[ which(Fishbase$fishID==fishID[1]),"Close_Time"]  #s
    opentime <- Fishbase[ which(Fishbase$fishID==fishID[1]),"Flush_Time"]  #s
    if (is.null(waittime)) {waittime<- 50 } #s
    if (is.null(enddiscard)) {enddiscard<- 22 } #s

    nsdevsrsquared<-0.5
    unit<-"mg/L"
    O2column<-c("Ox.1","Ox.2","Ox.3","Ox.4","Ox.5","Ox.6","Ox.7","Ox.8","Ox.9","Ox.10","Ox.11","Ox.12","Ox.13","Ox.14","Ox.15","Ox.16")
    Tempcolumn<-c("Temp.1","Temp.2","Temp.3","Temp.4","Temp.5","Temp.6","Temp.7","Temp.8","Temp.9","Temp.10","Temp.11","Temp.12","Temp.13","Temp.14","Temp.15","Temp.16")

    period<-opentime+closetime
    measureperiod<-closetime-waittime-enddiscard
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
      ifelse(testdata[row,1]>99+adustfirstslope & testdata[row,1]<99900+adustfirstslope,testdata[row,4]<-(testdata[(row+50),3]-testdata[row,3]-(testdata[row,3]-testdata[(row-50),3])),testdata[row,4]<-NA)

    }
    ##graphic dif
    c<-ggplot(testdata)+geom_point(aes(x=Time.s,y=dif))+xlim(0,99900)
    print(c)

    ##noise
    noise<-sd(na.omit(subset(Data,Time.s>100&Time.s<200)[,O2column[chamberfirstslope]]))
    print(noise)

    ##Find first endslope
    Toppos<-list()
    for (row in 1:nrow(testdata)){
      ifelse(testdata[row,1]>99+adustfirstslope & testdata[row,1]<99900+adustfirstslope,ifelse(testdata[row,4]>testdata[(row-1),4] & testdata[row,4]>testdata[(row-5),4] & testdata[row,4]>testdata[(row-7),4] & testdata[row,4]>testdata[(row+1),4] & testdata[row,4]>testdata[(row+5),4] & testdata[row,4]>testdata[(row+7),4] & testdata[row,4]>(Nnoise*noise),Toppos<-list.append(Toppos,testdata[row,1]),NA),NA)
    }

    #Time of the first end slope
    Firstend<-as.numeric(Toppos[[1]])
    print(Firstend)

    #graph with end slope

    c<-ggplot(Data,environment = environment())+geom_point(aes(x=Time.s,y=Data[,O2column[chamberfirstslope]]))+ylab(paste(colnames(Data[O2column[chamberfirstslope]]),unit)) +xlab("Time (sec)")+xlim(0,Firstend+5000) +
      geom_vline(aes(xintercept = Firstend),color="red")
    print(c)
    ggsave(c,filename="firstslope.pdf",path = wayout,width=20, height=4)
    question2 <- readline(prompt="First end slope is at the good position ?(YES or NO) : ")
    if(question2=="NO"){startday <-as.character(readline(prompt="Time of start ?(ex:08:43:01) : "));
    Firstend<-Data[which(Data$Time==startday),"Time.s"]+closetime;
    print(Firstend[1]);
    c<-ggplot(Data,environment = environment())+geom_point(aes(x=Time.s,y=Data[,O2column[chamberfirstslope]]))+ylab(paste(colnames(Data[O2column[chamberfirstslope]]),unit)) +xlab("Time (sec)")+xlim(0,Firstend+5000) +geom_vline(aes(xintercept = Firstend[1]),color="red") ;
    ggsave(c,filename="firstslopecorrected.pdf",path = wayout,width=20, height=4)}

    Firstend<-Firstend[1]-(corfirstslope*period)



    firstmidpoint<-Firstend-enddiscard-(measureperiod/2)
    wholeperiods<-(max(na.omit(Data$Time.s))-firstmidpoint)/period

      ###########################################################################################################

    ####################################RESULT CHAMBER FISH###############################################

    for (l in fishID){
      res<-data.frame(matrix(ncol=10,nrow=0))
      colnames(res)<- c("MidTime (sec)", "StartTime (sec)", "EndTime (sec)","linear coeff","MO2 (mg/h)","Temp(°C)","Date","SE","p-value","Rsquared")
      if(samestart==F){numbperiods<-round(wholeperiods)-(Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"]-Fishbase[ which(Fishbase$fishID==fishID[1]),"Nb_Ch"])}
      if(samestart==T){numbperiods<-round(wholeperiods)}

      ##Record Fish Chamber
      ##Create table with mid time measurment, start and end per chamber
      for (i in 1:numbperiods){
        if(samestart==F){res[i,1]<-firstmidpoint+(i-1+(Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"]-Fishbase[ which(Fishbase$fishID==fishID[1]),"Nb_Ch"]))*period}
        if(samestart==T){res[i,1]<-firstmidpoint+(i-1)*period}
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
        res[i,6]<-mean(linearreg[,3])
        res[i,7]<-as.character(Data[1,"DateTime"])
        res[i,8]<-summary(b)$coefficients["linearreg[, 1]","Std. Error"]
        res[i,9]<-summary(b)$coefficients["linearreg[, 1]","Pr(>|t|)"]
        res[i,10]<-summary(b)$r.squared
      }

      #write Table per chamber
      write.table(res, paste(wayout, "/", "resultchamber", Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"], ".csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")

      c<-ggplot(res,environment = environment())+geom_point(aes(x=res[,1]/3600,y=res[,5]))+ylab(colnames(res[5])) +xlab("Time (h)")
      ggsave(c,filename=paste("Chamber",Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"],".pdf",sep=""),path = wayout,width=20, height=4)
      print(c)

  }
}
