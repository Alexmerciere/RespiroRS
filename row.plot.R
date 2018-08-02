###needs ggplot
###Rowgraph O2 for all the chamber
row.plot<-function(data,chamber.nber,unit,path){
  x<-c(4,5,6,7,13,14,15,16,22,23,24,25,31,32,33,34)
 nbchamber<-x[1:chamber.nber]
  for(i in nbchamber){
    c<-ggplot(data)+geom_point(aes(x=Timeabsolu2,y=data[,i]))+ylab(paste(colnames(data[i]),unit)) +xlab("Time (sec)")
    ##ggsave(c,filename=paste(colnames(data[i]),".pdf",sep=""),path = way3,width=20, height=4)}
    print(c)
  }
}
