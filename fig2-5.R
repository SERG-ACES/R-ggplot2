library(ggplot2)
library(plyr)
library(grid)

#all files read
path <- "C:\\Users\\shine\\Desktop\\IA\\代码和数据" 
fileNames <- dir(path)  
filePath <- sapply(fileNames, function(x){ 
  paste(path,x,sep='/')})  
data <- lapply(filePath, function(x){
  read.table(x,sep=",",header=T)})  

#stretch
subjoin<-function(x){
  x$successful.join.rate
}
subprofit<-function(x){
  x$profit
}
subdefault<-function(x){
  x$deault.rate
}

jora<-sapply(data,subjoin)
dera<-sapply(data,subdefault)
pfra<-sapply(data,subprofit)

pfra<-pfra/10

#j:join ratio,d:default ratio,p:profit
#1:N-,2:N+,3:alpha,4:penalty,5:dividend
j1<-as.vector(jora[,1:6])
j2<-as.vector(jora[,19:24])
j3<-as.vector(jora[,7:12])
j4<-as.vector(jora[,13:18])
j5<-as.vector(jora[,25:30])
d1<-as.vector(dera[,1:6])
d2<-as.vector(dera[,19:24])
d3<-as.vector(dera[,7:12])
d4<-as.vector(dera[,13:18])
d5<-as.vector(dera[,25:30])
p1<-as.vector(pfra[,1:6])
p2<-as.vector(pfra[,19:24])
p3<-as.vector(pfra[,7:12])
p4<-as.vector(pfra[,13:18])
p5<-as.vector(pfra[,25:30])

n1<-c(rep(0.4,100),rep(0.45,100),rep(0.5,100),rep(0.55,100),rep(0.6,100),rep(0.65,100))
n2<-c(rep(0.7,100),rep(0.75,100),rep(0.8,100),rep(0.85,100),rep(0.9,100),rep(0.95,100))
n3<-c(rep(0,100),rep(0.05,100),rep(0.1,100),rep(0.15,100),rep(0.2,100),rep(0.25,100))
n4<-c(rep(0,100),rep(0.15,100),rep(0.3,100),rep(0.45,100),rep(0.6,100),rep(0.75,100))
n5<-c(rep(0.2,100),rep(0.3,100),rep(0.4,100),rep(0.5,100),rep(0.6,100),rep(0.7,100))

g1<-data.frame(j1,j2,j3,j4,j5,d1,d2,d3,d4,d5,p1,p2,p3,p4,p5)
g2<-data.frame(n1,n2,n3,n4,n5,n1,n2,n3,n4,n5,n1,n2,n3,n4,n5)
f1<-data.frame(j1,j2)
for(i in 1:15){
  f1[[i]]<-data.frame(g1[i],g2[i])
}

# fig.join, darkslategray,steelblue4,shape=1,size=6,alpha=0.2
# fig.default, plum4, rosybrown4, shape=23,size=4,alpha=0.2
# fig.profit, darkseagreen4, aquamarine4, shape=24,size=4,alpha=0.2

#figure 1: 2*3,n-,n+
f13 <- ggplot(f1[[1]],aes(x=n1,y=j1))+stat_smooth(fill="gray70",color="darkslategray",method="loess",size=1,weight=1)+theme_minimal()+
     geom_point(shape=1,size=5,alpha=0.2,aes(shape=fl),color="steelblue4") +xlab("")+ylab("Join ratio")+
     theme(axis.text.x = element_text(size = 11),axis.text.y = element_text(size = 11),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
     scale_y_continuous(limits=c(0.4, 1),breaks=seq(0.4, 1, 0.2))
f11 <- ggplot(f1[[6]],aes(x=n1,y=d1))+stat_smooth(fill="gray70",color="plum4",method="loess",size=1,weight=1)+theme_minimal()+
     geom_point(shape=23,size=4,alpha = 0.2,aes(shape=fl),color="rosybrown4") +xlab("")+ylab("Default ratio")+
  theme(axis.text.x = element_text(size = 11),axis.text.y = element_text(size = 11),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  scale_y_continuous(limits=c(0.4, 1),breaks=seq(0.4, 1, 0.2))
f15 <- ggplot(f1[[11]],aes(x=n1,y=p1))+stat_smooth(fill="gray70",color="darkseagreen4",method="loess",size=1,weight=1)+theme_minimal()+
  geom_point(shape=24,size=4,alpha = 0.2,aes(shape=fl),color="aquamarine4") +xlab("Lower threshold")+ylab("Profit (million RMB)")+
  theme(axis.text.x = element_text(size = 11),axis.text.y = element_text(size = 11),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  scale_y_continuous(limits=c(-5, 35),breaks=seq(0, 30, 10))
f12 <- ggplot(f1[[7]],aes(x=n2,y=d2))+stat_smooth(fill="gray70",color="plum4",method="loess",size=1,weight=1)+theme_minimal()+
  geom_point(shape=23,size=4,alpha = 0.2,aes(shape=fl),color="rosybrown4") +xlab("")+ylab("Default ratio")+
  theme(axis.text.x = element_text(size = 11),axis.text.y = element_text(size = 11),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  scale_y_continuous(limits=c(0.4, 1),breaks=seq(0.4, 1, 0.2))
f14 <- ggplot(f1[[2]],aes(x=n2,y=j2))+stat_smooth(fill="gray70",color="darkslategray",method="loess",size=1,weight=1)+theme_minimal()+
  geom_point(shape=1,size=5,alpha=0.2,aes(shape=fl),color="steelblue4") +xlab("")+ylab("Join ratio")+
  theme(axis.text.x = element_text(size = 11),axis.text.y = element_text(size = 11),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  scale_y_continuous(limits=c(0.4, 1),breaks=seq(0.4, 1, 0.2))
f16 <- ggplot(f1[[12]],aes(x=n2,y=p2))+stat_smooth(fill="gray70",color="darkseagreen4",method="loess",size=1,weight=1)+theme_minimal()+
  geom_point(shape=24,size=4,alpha = 0.2,aes(shape=fl),color="aquamarine4") +xlab("Higher threshold")+ylab("Profit (million RMB)")+
  theme(axis.text.x = element_text(size = 11),axis.text.y = element_text(size = 11),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  scale_y_continuous(limits=c(-5, 35),breaks=seq(0, 30, 10))

#group1
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(f11, vp = vplayout(1, 1))
print(f12, vp = vplayout(1, 2))
print(f13, vp = vplayout(2, 1))
print(f14, vp = vplayout(2, 2))
print(f15, vp = vplayout(3, 1))
print(f16, vp = vplayout(3, 2))

#############################################################################
#figure 2:alpha
f21 <- ggplot(f1[[8]],aes(x=n3,y=j3))+stat_smooth(fill="gray70",color="plum4",method="loess",size=1,weight=1)+theme_minimal()+
  geom_point(shape=23,size=4,alpha=0.2,aes(shape=fl),color="rosybrown4") +xlab("Slope of yield forecast")+ylab("Deault ratio")+ 
  theme(axis.text.x = element_text(size = 11),axis.text.y = element_text(size = 11),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  scale_y_continuous(limits=c(0.4, 1),breaks=seq(0.4, 1, 0.2))
f22 <- ggplot(f1[[3]],aes(x=n3,y=d3))+stat_smooth(fill="gray70",color="darkslategray",method="loess",size=1,weight=1)+theme_minimal()+
  geom_point(shape=1,size=6,alpha = 0.2,aes(shape=fl),color="steelblue4") +xlab("Slope of yield forecast")+ylab("Join ratio")+
  theme(axis.text.x = element_text(size = 11),axis.text.y = element_text(size = 11),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  scale_y_continuous(limits=c(0, 1),breaks=seq(0, 1, 0.2))
f23 <- ggplot(f1[[13]],aes(x=n3,y=p3))+stat_smooth(fill="gray70",color="darkseagreen4",method="loess",size=1,weight=1)+theme_minimal()+
  geom_point(shape=24,size=4,alpha=0.2,aes(shape=fl),color="aquamarine4") +xlab("Slope of yield forecast")+ylab("Profit (million RMB)")+
  theme(axis.text.x = element_text(size = 11),axis.text.y = element_text(size = 11),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  scale_y_continuous(limits=c(-5, 35),breaks=seq(0, 30, 10))

# fig.join, darkslategray,steelblue4,shape=1,size=6,alpha=0.2
# fig.default, plum4, rosybrown4, shape=23,size=4,alpha=0.2
# fig.profit, darkseagreen4, aquamarine4, shape=24,size=4,alpha=0.2

#group2
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(f21, vp = vplayout(1, 1))
print(f22, vp = vplayout(1, 2))
print(f23, vp = vplayout(1, 3))

#############################################################################
#figure 3:penalty
f31 <- ggplot(f1[[9]],aes(x=n4,y=d4))+stat_smooth(fill="gray70",color="plum4",method="loess",size=1,weight=1)+theme_minimal()+
  geom_point(shape=23,size=4,alpha=0.2,aes(shape=fl),color="rosybrown4") +xlab("Penalty ratio")+ylab("Default ratio")+
  theme(axis.text.x = element_text(size = 11),axis.text.y = element_text(size = 11),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  scale_y_continuous(limits=c(0, 1),breaks=seq(0, 1, 0.2))+
  scale_x_continuous(limits=c(0, 0.75),breaks=seq(0, 0.75, 0.15))
f32 <- ggplot(f1[[4]],aes(x=n4,y=j4))+stat_smooth(fill="gray70",color="darkslategray",method="loess",size=1,weight=1)+theme_minimal()+
  geom_point(shape=1,size=6,alpha = 0.2,aes(shape=fl),color="steelblue4") +xlab("Penalty ratio")+ylab("Join ratio")+
  theme(axis.text.x = element_text(size = 11),axis.text.y = element_text(size = 11),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  scale_y_continuous(limits=c(0.4, 1),breaks=seq(0.4, 1, 0.2))+
  scale_x_continuous(limits=c(0, 0.75),breaks=seq(0, 0.75, 0.15))
f33 <- ggplot(f1[[14]],aes(x=n4,y=p4))+stat_smooth(fill="gray70",color="darkseagreen4",method="loess",size=1,weight=1)+theme_minimal()+
  geom_point(shape=24,size=4,alpha=0.2,aes(shape=fl),color="aquamarine4") +xlab("Penalty ratio")+ylab("Profit (million RMB)")+
  theme(axis.text.x = element_text(size = 11),axis.text.y = element_text(size = 11),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  scale_y_continuous(limits=c(-5, 35),breaks=seq(0, 30, 10))+
  scale_x_continuous(limits=c(0, 0.75),breaks=seq(0, 0.75, 0.15))

#group3
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(f31, vp = vplayout(1, 1))
print(f32, vp = vplayout(1, 2))
print(f33, vp = vplayout(1, 3))

#############################################################################
#figure 4:dividend ratio
f41 <- ggplot(f1[[10]],aes(x=n5,y=d5))+stat_smooth(fill="gray70",color="plum4",method="loess",size=1,weight=1)+theme_minimal()+
  geom_point(shape=23,size=4,alpha=0.2,aes(shape=fl),color="rosybrown4") +xlab("Dividend ratio")+ylab("Default ratio")+
  theme(axis.text.x = element_text(size = 11),axis.text.y = element_text(size = 11),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  scale_y_continuous(limits=c(0.4, 1),breaks=seq(0.4, 1, 0.2))
f42 <- ggplot(f1[[5]],aes(x=n5,y=j5))+stat_smooth(fill="gray70",color="darkslategray",method="loess",size=1,weight=1)+theme_minimal()+
  geom_point(shape=1,size=6,alpha = 0.2,aes(shape=fl),color="steelblue4") +xlab("Dividend ratio")+ylab("Join ratio")+
  theme(axis.text.x = element_text(size = 11),axis.text.y = element_text(size = 11),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  scale_y_continuous(limits=c(0.4, 1),breaks=seq(0.4, 1, 0.2))
f43 <- ggplot(f1[[15]],aes(x=n5,y=p5))+stat_smooth(fill="gray70",color="darkseagreen4",method="loess",size=1,weight=1)+theme_minimal()+
  geom_point(shape=24,size=4,alpha=0.2,aes(shape=fl),color="aquamarine4") +xlab("Dividend ratio")+ylab("Profit (million RMB)")+
  theme(axis.text.x = element_text(size = 11),axis.text.y = element_text(size = 11),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  scale_y_continuous(limits=c(-5, 35),breaks=seq(0, 30, 10))

#group4
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(f41, vp = vplayout(1, 1))
print(f42, vp = vplayout(1, 2))
print(f43, vp = vplayout(1, 3))

# fig.join, darkslategray,steelblue4,shape=1,size=6,alpha=0.2
# fig.default, plum4, rosybrown4, shape=23,size=4,alpha=0.2
# fig.profit, darkseagreen4, aquamarine4, shape=24,size=4,alpha=0.2

#############################################################################
#trial
#sam <- ggplot(f1[[15]],aes(x=n5,y=p5))+stat_smooth(fill="gray80",color="darkseagreen4",method="loess",size=1,weight=1)+theme_minimal()+
#  geom_point(shape=24,size=4,alpha = 0.2,aes(shape=fl),color="aquamarine4") +xlab("Dividend ratio")+ylab("Initiator's profit")+
#  ggtitle("sample")+
#  theme(axis.text.x = element_text(size = 11),axis.text.y = element_text(size = 11),axis.title.x = element_text(size = 12,face="bold"),axis.title.y = element_text(size = 12,face="bold"))+
#  scale_y_continuous(breaks=seq(0, 300, 50))
#sam



