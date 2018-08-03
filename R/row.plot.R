
###Rowgraph O2 for all the chamber
row.plot<-function(data,chamber.nber,unit,wayout){
  x<-c(4,5,6,7,13,14,15,16,22,23,24,25,31,32,33,34)
 colchamber<-x[1:chamber.nber]
  for(i in colchamber){
    c<-ggplot(data,environment = environment())+geom_point(aes(x=Timeabsolu2,y=data[,i]))+ylab(paste(colnames(data[i]),unit,sep="")) +xlab("Time (sec)")
    ggsave(c,filename=paste(colnames(data[i]),".pdf",sep=""),path =wayout,width=20, height=4)
    }
  }
