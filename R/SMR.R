SMR<-function(data,metadata,fishID,wayout){


  # layer SMR
  #Rsquared layer
  meanRsquared <- mean(data$Rsquared)
  SdRsquared <- sd(data$Rsquared)
  resstep1 <- subset(data, data[, 10] > (meanRsquared - (nsdevsrsquared * SdRsquared)))

  #lowest value layer
  LowestO2valueOutlier <- sort(resstep1[, 11], decreasing = F)[1:numberoflowvalues]
  resstep2 <- subset(resstep1, resstep1[, 11] %in% c(LowestO2valueOutlier))

  #Mean lowest value layer
  meanLowestO2values <- mean(resstep2$MO2cor)
  SdLowestO2values <- sd(resstep2$MO2cor)
  resstep3 <- subset(resstep2, resstep2[, 11] > (meanLowestO2values - (0.5 * SdLowestO2values)) & resstep2[, 11] < (meanLowestO2values + (2 * SdLowestO2values)))

  data$selection <- ifelse(data[, 11] %in% resstep3[, 11] & data[, 10] %in% resstep3[, 10], "SMR", ifelse(data[, 11] %in% resstep2[, 11] & data[, 10] %in% resstep2[, 10],"meanLowestO2valueOutlier", ifelse(data[, 11] %in% resstep1[, 11] & data[, 10] %in% resstep1[, 10], "NotUsedInSMRcalculation", "RsquaredOutlier")))
  #write Table
  write.table(data, paste(wayout, "/", "resultchambercorrect", Fishbase[ which(Fishbase$fishID==l),"Nb_Ch"], ".csv", sep = ""), sep = ";", dec = ".", row.names = F, qmethod = "double")


#####################################Final Table###################################################
l<-fishID
Result[which(fishID==l),1]<-l
Result[which(fishID==l),2]<-date(data$Date[[1]])
Result[which(fishID==l),3]<-Fishbase[ which(metadata$fishID==l),"Nb_Ch"]
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
