library(ggplot2)
library(reshape2)

data1<-read.csv("C:\\Users\\shine\\Desktop\\Kapsarc\\asset plots\\horizon.csv",header=F)
data1[is.na(data1)] <- 0

year<-1990:2090
d<-data.frame(year,data1)
d1<-melt(d,id.vars="year")

f <- ggplot(d1,aes(x=year,y=value,color=variable))+geom_line()+theme_minimal()+
     ylab("Production (bcm)")+
     scale_x_continuous(limits=c(1990, 2090),breaks=seq(1990, 2090, 20))+
     theme(legend.position='none')
ggsave(filename="assets production.png")