library(ggplot2)
library(dplyr)


SHD<-function(device1,device2)                 ## take 2 data frame as input 
{
  colnames(device1)<-c("T","Y","W")            ## Rename coloum_name
  colnames(device2)<-c("T","Y","W")
  
  device1<-device1[order(device1$T), ]         ## sorting
  device2<-device2[order(device2$T), ]
  
  idx1<-which.max(device1$Y)                   ## amplitude normalizing
  mx1<-device1$Y[idx1]
  idx2<-which.max(device2$Y)
  mx2<-device2$Y[idx2]
  mx<-(mx1+mx2)/2
  device1$Y<-device1$Y*(mx/mx1)
  device2$Y<-device2$Y*(mx/mx2)
  
  
  idx1<-which.max(device1$Y)               ## spliting with respect to peak
  device_11<-device1[1:idx1, ]
  device_12<-device1[idx1+1:(length(device1$T)-idx1), ]
  idx2<-which.max(device2$Y)
  device_21<-device2[1:idx2, ]
  device_22<-device2[idx2+1:(length(device2$T)-idx2), ]
  
  
  
  res_t1<-0                       ## mean for left part of each device
  res_y1<-0
  res_w1<-0
  if(device_11$Y[1]>device_21$Y[1]){
    for(i in 1:length(device_21$T))       
    {
      pos<- which.min(abs(device_21$Y[i]-device_11$Y))
      res_t1[i]<- ((device_21$T[i]*device_21$W[i])+(device_11$T[pos]*device_11$W[pos]))/(device_21$W[i]+device_11$W[pos])
      res_y1[i]<- ((device_21$Y[i]*device_21$W[i])+(device_11$Y[pos]*device_11$W[pos]))/(device_21$W[i]+device_11$W[pos])
      res_w1[i]<- device_21$W[i]+device_11$W[pos]
    }
  }else{
    for(i in 1:length(device_11$T))       
    {
      pos<- which.min(abs(device_11$Y[i]-device_21$Y))
      res_t1[i]<- ((device_11$T[i]*device_11$W[i])+(device_21$T[pos]*device_21$W[pos]))/(device_11$W[i]+device_21$W[pos])
      res_y1[i]<- ((device_11$Y[i]*device_11$W[i])+(device_21$Y[pos]*device_21$W[pos]))/(device_11$W[i]+device_21$W[pos])
      res_w1[i]<- device_11$W[i]+device_21$W[pos]
    }
  }
  res1<-cbind.data.frame(res_t1,res_y1,res_w1)
  colnames(res1)<-c("T","Y","W")      ## to change coloum name from res_t11 to T
                                      ## & res_y11 to Y
  
  
  res_t2<-0                 ## mean for right part of each device
  res_y2<-0
  res_w2<-0
  if(device_12$Y[length(device_12$T)]>device_22$Y[length(device_22$T)]){
    for(i in 1:length(device_22$T))
    {
      pos<-which.min(abs(device_22$Y[i]-device_12$Y))
      res_t2[i]<-((device_22$T[i]*device_22$W[i])+(device_12$T[pos]*device_12$W[pos]))/(device_22$W[i]+device_12$W[pos])
      res_y2[i]<-((device_22$Y[i]*device_22$W[i])+(device_12$Y[pos]*device_12$W[pos]))/(device_22$W[i]+device_12$W[pos])
      res_w2[i]<- device_22$W[i]+device_12$W[pos]
    }
  }else{
    for(i in 1:length(device_12$T))
    {
      pos<-which.min(abs(device_12$Y[i]-device_22$Y))
      res_t2[i]<-((device_12$T[i]*device_12$W[i])+(device_22$T[pos]*device_22$W[pos]))/(device_12$W[i]+device_22$W[pos])
      res_y2[i]<-((device_12$Y[i]*device_12$W[i])+(device_22$Y[pos]*device_22$W[pos]))/(device_12$W[i]+device_22$W[pos])
      res_w2[i]<- device_12$W[i]+device_22$W[pos]
    }
  }
  res2<-cbind.data.frame(res_t2,res_y2,res_w2)
  colnames(res2)<-c("T","Y","W")     ## to change coloum name from res_t11 to T
                                     ## & res_y11 to Y
  
  res<-rbind(res1,res2)         ## bind two half in row wise
  
  return(res)
}


N<- 8       ## Num of device
point<-500  ## Num of point of each device
aa<-read_excel(choose.files())

dev1<-cbind.data.frame(aa[[1]],aa[[2]],rep(1,point))
colnames(dev1)<-c("T","Y","W")
res<-dev1

for(i in seq(from=4, to= (3*N-2), by=3)){
  dev2<-cbind.data.frame(aa[[i]],aa[[i+1]],rep(1,point))
  colnames(dev2)<-c("T","Y","W")
  res<-SHD(res,dev2)
}



dev1<-cbind.data.frame(aa[[1]],aa[[2]],rep(1,point))
colnames(dev1)<-c("T","Y","W")
dev2<-cbind.data.frame(aa[[4]],aa[[5]],rep(1,point))
colnames(dev2)<-c("T","Y","W")
dev3<-cbind.data.frame(aa[[7]],aa[[8]],rep(1,point))
colnames(dev3)<-c("T","Y","W")
dev4<-cbind.data.frame(aa[[10]],aa[[11]],rep(1,point))
colnames(dev4)<-c("T","Y","W")
dev5<-cbind.data.frame(aa[[13]],aa[[14]],rep(1,point))
colnames(dev5)<-c("T","Y","W")
dev6<-cbind.data.frame(aa[[16]],aa[[17]],rep(1,point))
colnames(dev6)<-c("T","Y","W")
dev7<-cbind.data.frame(aa[[19]],aa[[20]],rep(1,point))
colnames(dev7)<-c("T","Y","W")
dev8<-cbind.data.frame(aa[[22]],aa[[23]],rep(1,point))
colnames(dev8)<-c("T","Y","W")
ggplot()+
  geom_line(dev1, mapping = aes(x=T,y=Y), size=1, color="green")+
  geom_line(dev2, mapping = aes(x=T,y=Y), size=1, color="blue")+
  geom_line(dev3, mapping = aes(x=T,y=Y), size=1, color="magenta")+
  geom_line(dev4, mapping = aes(x=T,y=Y), size=1, color="black")+
  geom_line(dev5, mapping = aes(x=T,y=Y), size=1, color="cyan")+
  geom_line(dev6, mapping = aes(x=T,y=Y), size=1, color="darkorchid1")+
  geom_line(dev7, mapping = aes(x=T,y=Y), size=1, color="deeppink")+
  geom_line(dev8, mapping = aes(x=T,y=Y), size=1, color="darkorange")+
  geom_line(res,  mapping = aes(x=T,y=Y),  size=2, color="red")







        ### Manually generated ###

# N<- 500            # num of point in device
# 
# a1<- -5
# a2<- -3            # shifting factor
# a3<- 0
# a4<-3
# a5<-4
# a6<-5
# a7<-6
# a8<-7
# 
# b1<-1.5
# b2<-1.75           # amplitude factor
# b3<-1.7
# b4<-1.9
# b5<-1.85
# b6<-1.6
# b7<-1.95
# b8<-1.56
# 
# t1<-runif(N, -10, 10)
# t2<-runif(N, -10, 10)
# t3<-runif(N, -10, 10)
# t4<-runif(N, -10, 10)
# t5<-runif(N, -10, 10)
# t6<-runif(N, -10, 10)
# t7<-runif(N, -10, 10)
# t8<-runif(N, -10, 10)
# 
# y1=b1*exp(-(t1-a1)^2)
# y2=b2*exp(-(t2-a2)^2)
# y3=b3*exp(-(t3-a3)^2)
# y4=b4*exp(-(t4-a4)^2)
# y5=b5*exp(-(t5-a5)^2)
# y6=b6*exp(-(t6-a6)^2)
# y7=b7*exp(-(t7-a7)^2)
# y8=b8*exp(-(t8-a8)^2)           ## have to add weight
# 
# w1<-1^t1
# w2<-1^t2
# w3<-1^t3
# w4<-1^t4
# w5<-1^t5
# w6<-1^t6
# w7<-1^t7
# w8<-1^t8
# 
# device1<-cbind.data.frame(t1,y1,w1)
# device2<-cbind.data.frame(t2,y2,w2)
# device3<-cbind.data.frame(t3,y3,w3)
# device4<-cbind.data.frame(t4,y4,w4)
# device5<-cbind.data.frame(t5,y5,w5)
# device6<-cbind.data.frame(t6,y6,w6)
# device7<-cbind.data.frame(t7,y7,w7)
# device8<-cbind.data.frame(t8,y8,w8)
# 
# #data<-cbind.data.frame(t1,y1,w1, t2,y2,w2, t3,y3,w3, t4,y4,w4, t5,y5,w5, t6,y6,w6, t7,y7,w7, t8,y8,w8)
# 
# 
# ggplot()+
#   geom_line(mapping = aes(x=t1,y=y1), size=1, color="black")+
#   geom_line(mapping = aes(x=t2,y=y2), size=1, color="blue")+
#   geom_line(mapping = aes(x=t3,y=y3), size=1, color="red")+
#   geom_line(mapping = aes(x=t4,y=y4), size=1, color="green")+
#   geom_line(mapping = aes(x=t5,y=y5), size=1, color="darkorchid1")+
#   geom_line(mapping = aes(x=t6,y=y6), size=1, color="magenta")+
#   geom_line(mapping = aes(x=t7,y=y7), size=1, color="deeppink")+
#   geom_line(mapping = aes(x=t8,y=y8), size=1, color="cyan")
# 
# 
# 
# 
# res1<-SHD(device1,device2)
# ggplot()+
#   geom_line(device1, mapping = aes(x=t1,y=y1), size=1, color="green")+
#   geom_line(device2, mapping = aes(x=t2,y=y2), size=1, color="blue")+
#   geom_line(res1, mapping = aes(x=T,y=Y),      size=2, color="red")
# 
# res2<-SHD(res1,device3)
# ggplot()+
#   geom_line(device1, mapping = aes(x=t1,y=y1), size=1, color="green")+
#   geom_line(device2, mapping = aes(x=t2,y=y2), size=1, color="blue")+
#   geom_line(device3, mapping = aes(x=t3,y=y3), size=1, color="black")+
#   geom_line(res2, mapping = aes(x=T,y=Y),      size=2, color="red")
# 
# res3<-SHD(res2,device4)
# ggplot()+
#   geom_line(device1, mapping = aes(x=t1,y=y1), size=1, color="green")+
#   geom_line(device2, mapping = aes(x=t2,y=y2), size=1, color="blue")+
#   geom_line(device3, mapping = aes(x=t3,y=y3), size=1, color="black")+
#   geom_line(device4, mapping = aes(x=t4,y=y4), size=1, color="darkorchid1")+
#   geom_line(res3, mapping = aes(x=T,y=Y),      size=2, color="red")
# 
# res4<-SHD(res3,device5)
# ggplot()+
#   geom_line(device1, mapping = aes(x=t1,y=y1), size=1, color="green")+
#   geom_line(device2, mapping = aes(x=t2,y=y2), size=1, color="blue")+
#   geom_line(device3, mapping = aes(x=t3,y=y3), size=1, color="black")+
#   geom_line(device4, mapping = aes(x=t4,y=y4), size=1, color="darkorchid1")+
#   geom_line(device5, mapping = aes(x=t5,y=y5), size=1, color="magenta")+
#   geom_line(res4, mapping = aes(x=T,y=Y),      size=2, color="red")
# 
# res5<-SHD(res4,device6)
# ggplot()+
#   geom_line(device1, mapping = aes(x=t1,y=y1), size=1, color="green")+
#   geom_line(device2, mapping = aes(x=t2,y=y2), size=1, color="blue")+
#   geom_line(device3, mapping = aes(x=t3,y=y3), size=1, color="black")+
#   geom_line(device4, mapping = aes(x=t4,y=y4), size=1, color="darkorchid1")+
#   geom_line(device5, mapping = aes(x=t5,y=y5), size=1, color="magenta")+
#   geom_line(device6, mapping = aes(x=t6,y=y6), size=1, color="deeppink")+
#   geom_line(res5, mapping = aes(x=T,y=Y),      size=2, color="red")
# 
# res6<-SHD(res5,device7)
# ggplot()+
#   geom_line(device1, mapping = aes(x=t1,y=y1), size=1, color="green")+
#   geom_line(device2, mapping = aes(x=t2,y=y2), size=1, color="blue")+
#   geom_line(device3, mapping = aes(x=t3,y=y3), size=1, color="black")+
#   geom_line(device4, mapping = aes(x=t4,y=y4), size=1, color="darkorchid1")+
#   geom_line(device5, mapping = aes(x=t5,y=y5), size=1, color="magenta")+
#   geom_line(device6, mapping = aes(x=t6,y=y6), size=1, color="deeppink")+
#   geom_line(device7, mapping = aes(x=t7,y=y7), size=1, color="cyan")+
#   geom_line(res6, mapping = aes(x=T,y=Y),      size=2, color="red")
# 
# res7<-SHD(res6,device8)
# ggplot()+
#   geom_line(device1, mapping = aes(x=t1,y=y1), size=1, color="green")+
#   geom_line(device2, mapping = aes(x=t2,y=y2), size=1, color="blue")+
#   geom_line(device3, mapping = aes(x=t3,y=y3), size=1, color="black")+
#   geom_line(device4, mapping = aes(x=t4,y=y4), size=1, color="darkorange")+
#   geom_line(device5, mapping = aes(x=t5,y=y5), size=1, color="magenta")+
#   geom_line(device6, mapping = aes(x=t6,y=y6), size=1, color="deeppink")+
#   geom_line(device7, mapping = aes(x=t7,y=y7), size=1, color="cyan")+
#   geom_line(device8, mapping = aes(x=t8,y=y8), size=1, color="darkorchid1")+
#   geom_line(res7, mapping = aes(x=T,y=Y),      size=2, color="red")
# 
# 






####################### Take input from file ###########################
library(ggplot2)
library(dplyr)
library(readr)


a<-read_rds(choose.files()) ## load file row=50, coloum=250& page=100 in "a" varable
N<-250                      ## num of point each curve



## Shortest y deviation function......
SHD<-function(device1,device2)                 ## take 2 data frame as input 
{
  colnames(device1)<-c("T","Y","W")            ## Rename coloum_name
  colnames(device2)<-c("T","Y","W")
  
  device1<-device1[order(device1$T), ]         ## sorting
  device2<-device2[order(device2$T), ]
  
  idx1<-which.max(device1$Y)                   ## amplitude normalizing
  mx1<-device1$Y[idx1]
  idx2<-which.max(device2$Y)
  mx2<-device2$Y[idx2]
  mx<-(mx1+mx2)/2
  device1$Y<-device1$Y*(mx/mx1)
  device2$Y<-device2$Y*(mx/mx2)
  
  
  idx1<-which.max(device1$Y)               ## spliting with respect to peak
  device_11<-device1[1:idx1, ]
  device_12<-device1[idx1+1:(length(device1$T)-idx1), ]
  idx2<-which.max(device2$Y)
  device_21<-device2[1:idx2, ]
  device_22<-device2[idx2+1:(length(device2$T)-idx2), ]
  
  
  
  res_t1<-0                       ## mean for left part of each device
  res_y1<-0
  res_w1<-0
  if(device_11$Y[1]>device_21$Y[1]){
    for(i in 1:length(device_21$T))       
    {
      pos<- which.min(abs(device_21$Y[i]-device_11$Y))
      res_t1[i]<- ((device_21$T[i]*device_21$W[i])+(device_11$T[pos]*device_11$W[pos]))/(device_21$W[i]+device_11$W[pos])
      res_y1[i]<- ((device_21$Y[i]*device_21$W[i])+(device_11$Y[pos]*device_11$W[pos]))/(device_21$W[i]+device_11$W[pos])
      res_w1[i]<- device_21$W[i]+device_11$W[pos]
    }
  }else{
    for(i in 1:length(device_11$T))       
    {
      pos<- which.min(abs(device_11$Y[i]-device_21$Y))
      res_t1[i]<- ((device_11$T[i]*device_11$W[i])+(device_21$T[pos]*device_21$W[pos]))/(device_11$W[i]+device_21$W[pos])
      res_y1[i]<- ((device_11$Y[i]*device_11$W[i])+(device_21$Y[pos]*device_21$W[pos]))/(device_11$W[i]+device_21$W[pos])
      res_w1[i]<- device_11$W[i]+device_21$W[pos]
    }
  }
  res1<-cbind.data.frame(res_t1,res_y1,res_w1)
  colnames(res1)<-c("T","Y","W")      ## to change coloum name from res_t11 to T
  ## & res_y11 to Y
  
  
  res_t2<-0                 ## mean for right part of each device
  res_y2<-0
  res_w2<-0
  if(device_12$Y[length(device_12$T)]>device_22$Y[length(device_22$T)]){
    for(i in 1:length(device_22$T))
    {
      pos<-which.min(abs(device_22$Y[i]-device_12$Y))
      res_t2[i]<-((device_22$T[i]*device_22$W[i])+(device_12$T[pos]*device_12$W[pos]))/(device_22$W[i]+device_12$W[pos])
      res_y2[i]<-((device_22$Y[i]*device_22$W[i])+(device_12$Y[pos]*device_12$W[pos]))/(device_22$W[i]+device_12$W[pos])
      res_w2[i]<- device_22$W[i]+device_12$W[pos]
    }
  }else{
    for(i in 1:length(device_12$T))
    {
      pos<-which.min(abs(device_12$Y[i]-device_22$Y))
      res_t2[i]<-((device_12$T[i]*device_12$W[i])+(device_22$T[pos]*device_22$W[pos]))/(device_12$W[i]+device_22$W[pos])
      res_y2[i]<-((device_12$Y[i]*device_12$W[i])+(device_22$Y[pos]*device_22$W[pos]))/(device_12$W[i]+device_22$W[pos])
      res_w2[i]<- device_12$W[i]+device_22$W[pos]
    }
  }
  res2<-cbind.data.frame(res_t2,res_y2,res_w2)
  colnames(res2)<-c("T","Y","W")     ## to change coloum name from res_t11 to T
  ## & res_y11 to Y
  
  res<-rbind(res1,res2)         ## bind two half in row wise
  
  return(res)
}


p <- ggplot()              ########## print multiple plot ############
plot_mul <- function(df){
  p <<- p + geom_line(data=df, aes(x=T, y=Y),size=1)
  return(p)
}




a1<-a[ , ,100]       ## first page row=50 & coloum=250
t<-(1:N)             ## in x-axis point
w<-rep(1,N)          ## intial weight = 1
res<-data.frame(t,a1[1, ],w)    ## first device data frame
colnames(res)<-c("T","Y","W")   ## Rename coloum

for(i in 2:50){         ## second to last device loop
  tmp<-a1[i, ]
  tmp<-data.frame(t,tmp,w)
  colnames(tmp)<-c("T","Y","W")
  res<-SHD(res,tmp)     ## restltant device curve
}

for(i in 1:50){         ## make all device plot in one graph
  tmp<-a1[i, ]
  tmp<-data.frame(t,tmp)
  colnames(tmp)<-c("T","Y")
  plot_mul(tmp)
}

p <<- p + geom_line(data=res, aes(x=T, y=Y),size=2,color="red")
p

