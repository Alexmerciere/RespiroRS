###Rowgraph O2 for all the chamber
row.plot<-function(data,chamber.nber,unit,wayout){
  x<-c("Ox.1","Ox.2","Ox.3","Ox.4","Ox.5","Ox.6","Ox.7","Ox.8","Ox.9","Ox.10","Ox.11","Ox.12","Ox.13","Ox.14","Ox.15","Ox.16")

 colchamber<-x[1:chamber.nber]
  for(i in colchamber){
    c<-ggplot(data,environment = environment())+geom_point(aes(x=Time.s,y=as.numeric(as.character(data[,i]))))+ylab(paste(colnames(data[i]),unit,sep="")) +xlab("Time (sec)")
    ggsave(c,filename=paste(colnames(data[i]),".pdf",sep=""),path =wayout,width=20, height=4)
    }
  }

