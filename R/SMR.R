SMR<-function(data,Fishbase,fishID,wayout,nsdevsrsquared=NULL,percentpoint=NULL){
res<-data
  if (is.null(nsdevsrsquared)) {nsdevsrsquared<-0.5}
if (is.null(percentpoint)) {percentpoint<-10}
numberoflowvalues<-nrow(res)*percentpoint/100
firstmidpoint<-data[1,1]

####################################RESULT CHAMBER FISH###############################################
Result<-data.frame(matrix(ncol=13,nrow=0))
colnames(Result)<- c("fishID","Date", "Chamber","SMR","sdSMR","MaxMR","MinMR","FirstMR","MeanTemp (°C)","Weight (kg)","Chamber Volume (L)","BlankCorrection","Mean all data")


  # layer SMR
  #Rsquared layer
  meanRsquared <- mean(res$Rsquared)
  SdRsquared <- sd(res$Rsquared)
  resstep1 <- subset(res, res[, 10] > (meanRsquared - (nsdevsrsquared * SdRsquared)))

  #lowest value layer
  LowestO2valueOutlier <- sort(resstep1[, 5], decreasing = F)[1:numberoflowvalues]
  resstep2 <- subset(resstep1, resstep1[, 5] %in% c(LowestO2valueOutlier))

  #Mean lowest value layer
  meanLowestO2values <- mean(resstep2[, 5])
  SdLowestO2values <- sd(resstep2[, 5])
  resstep3 <- subset(resstep2, resstep2[, 5] > (meanLowestO2values - (0.5 * SdLowestO2values)) & resstep2[, 5] < (meanLowestO2values + (2 * SdLowestO2values)))

  res$selection <- ifelse(res[, 5] %in% resstep3[, 5] & res[, 10] %in% resstep3[, 10], "SMR", ifelse(res[, 5] %in% resstep2[, 5] & res[, 10] %in% resstep2[, 10],"meanLowestO2valueOutlier", ifelse(res[, 5] %in% resstep1[, 5] & res[, 10] %in% resstep1[, 10], "NotUsedInSMRcalculation", "RsquaredOutlier")))
  #write Table
  write.table(res, paste(wayout, "/", "resultchamber", Fishbase[ which(Fishbase$fishID==fishID),"Nb_Ch"], ".csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")

  c<-ggplot(res,environment = environment()) +xlab("Time (h)") +geom_point(aes(x=res[,1]/3600,y=res[,5],colour=selection)) +scale_color_manual(values = c("NotUsedInSMRcalculation" = "black","RsquaredOutlier" = "red","meanLowestO2valueOutlier"="blue","SMR" = "green"))
  ggsave(c,filename=paste("Chamber",Fishbase[ which(Fishbase$fishID==fishID),"Nb_Ch"],".pdf",sep=""),path = wayout,width=20, height=4)
  print(c)
  #####################################Final Table###################################################

  Result[which(fishID==fishID),1]<-fishID
  Result[which(fishID==fishID),2]<-res$Date[[1]]
  Result[which(fishID==fishID),3]<-Fishbase[ which(Fishbase$fishID==fishID),"Nb_Ch"]
  Result[which(fishID==fishID),4]<-mean(subset(res,res$selection=="SMR")[,5])
  Result[which(fishID==fishID),5]<-sd(subset(res,res$selection=="SMR")[,5])
  Result[which(fishID==fishID),6]<-max(subset(res,!res$selection=="RsquaredOutlier")[,5])
  Result[which(fishID==fishID),7]<-min(subset(res,res$selection=="SMR")[,5])


  ###FirstMR###
  Result[which(fishID==fishID),8]<- NA
  Result[which(fishID==fishID),9]<-mean(res[,6])
  Result[which(fishID==fishID),10]<-Fishbase[ which(Fishbase$fishID==fishID),"W"]
  Result[which(fishID==fishID),11]<-Fishbase[ which(Fishbase$fishID==fishID),"Vol_Ch"]
  Result[which(fishID==fishID),12]<-"NO"
  Result[which(fishID==fishID),13]<-mean(subset(res,!res$selection=="RsquaredOutlier")[, 5])


write.table(Result, paste(wayout, "/", "Result",fishID,".csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")
ResultRun<<-Result
}
