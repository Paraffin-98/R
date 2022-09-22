#####################################################
library("readxl")
library("ggplot2")

data<- read_excel(file.choose())
x<-0
y<-0

for(i in 1:length(data$x1))
{
  pos<-which.min(abs(data$y1[i]-data$y2))
  x[i]<-(data$x1[i]+data$x2[pos])/2
  y[i]<-(data$y1[i]+data$y2[pos])/2
}

ggplot(data = cbind.data.frame(x, y, data$x1, data$y1, data$x2, data$y2))+ 
  geom_point(mapping = aes(x = data$x1, y = -data$y1), color = "blue", size = 1) + 
  geom_point(mapping = aes(x = data$x2, y = -data$y2), color = "green", size = 1) +
  geom_point(mapping = aes(x = x, y = -y), color = "red", size = 1) 

#########################################################


library(ggplot2)
library(ggpmisc)
library(data.table)
library(dplyr)

N<- 500
t<- runif(N, -10, 10)
a1<- -4  ## shaifting factor
a2<- 3  ## shaifting factor
b<- 1  ## amplitude factor

y1=1*exp(-(t-a1)^2)
y2=0.5*exp(-(t-a2)^2)

data1<-cbind.data.frame(t,y1)
data1<-data1[order(data1$t), ]

data2<-cbind.data.frame(t,y2)
data2<-data2[order(data2$t), ]

idx1<-which.max(data1$y1)
data_11<-data1[1:idx1, ]
data_12<-data1[idx1+1:(N-idx1) ,]

idx2<-which.max(data2$y2)
data_21<-data2[1:idx2, ]
data_22<-data2[idx2+1:(N-idx2) ,]


res_t1<-0
res_y1<-0
if(length(data_11$t)>length(data_21$t)){
  for(i in 1:length(data_11$t))
  {
    pos<-which.min(abs(data_11$y1[i]-data_21$y2))
    res_t1[i]<-(data_11$t[i]+data_21$t[pos])/2
    res_y1[i]<-(data_11$y1[i]+data_21$y2[pos])/2
  }
}else{
  for(i in 1:length(data_21$t))
  {
    pos<-which.min(abs(data_21$y2[i]-data_11$y1))
    res_t1[i]<-(data_21$t[i]+data_11$t[pos])/2
    res_y1[i]<-(data_21$y2[i]+data_11$y1[pos])/2
  }
}
res1<-cbind.data.frame(res_t1,res_y1)
colnames(res1)<-c("T","Y")


res_t2<-0
res_y2<-0
if(length(data_12$t)>length(data_22$t)){
  for(i in 1:length(data_12$t))
  {
    pos<-which.min(abs(data_12$y1[i]-data_22$y2))
    res_t2[i]<-(data_12$t[i]+data_22$t[pos])/2
    res_y2[i]<-(data_12$y1[i]+data_22$y2[pos])/2
  }
}else{
  for(i in 1:length(data_22$t))
  {
    pos<-which.min(abs(data_22$y2[i]-data_12$y1))
    res_t2[i]<-(data_22$t[i]+data_12$t[pos])/2
    res_y2[i]<-(data_22$y2[i]+data_12$y1[pos])/2
  }
}
res2<-cbind.data.frame(res_t2,res_y2)
colnames(res2)<-c("T","Y")

res<-rbind(res1,res2)


ggplot()+
  geom_line(data1, mapping= aes(x=t, y=y1), color="red")+
  geom_line(data2, mapping= aes(x=t, y=y2), color="blue")+
  geom_line(res, mapping = aes(x=T, y=Y), color="green")

plot(res$T,res$Y)





########################### Final code ######################################



library(ggplot2)
library(ggpmisc)
library(data.table)
library(dplyr)

N<- 500
t<- runif(N, -10, 10)
a1<- -5  ## shifting factor for device 1
a2<- 5  ## shifting factor for device 2
b1<- 1  ## amplitude factor for device 1
b2<- 0.4  ## amplitude factor for device 2

y1=b1*exp(-(t-a1)^2)
y2=b2*exp(-(t-a2)^2)

data1<-cbind.data.frame(t,y1)
data1<-data1[order(data1$t), ]

data2<-cbind.data.frame(t,y2)
data2<-data2[order(data2$t), ]


idx1<-which.max(data1$y1)
mx1<-data1$y1[idx1]
idx2<-which.max(data2$y2)
mx2<-data2$y2[idx2]
mx<-(mx1+mx2)/2
data1$y1<-data1$y1*(mx/mx1)
data2$y2<-data2$y2*(mx/mx2)



idx1<-which.max(data1$y1)
data_11<-data1[1:idx1, ]
data_12<-data1[idx1+1:(N-idx1) ,]

idx2<-which.max(data2$y2)
data_21<-data2[1:idx2, ]
data_22<-data2[idx2+1:(N-idx2) ,]


ggplot()+
  geom_point(data_11,mapping = aes(x=t,y=y1),color="blue")+
  geom_point(data_21,mapping = aes(x=t,y=y2),color="red")


res_t11<-0
res_y11<-0
for(i in 1:length(data_11$t))
{
  pos<-which.min(abs(data_11$y1[i]-data_21$y2))
  res_t11[i]<-(data_11$t[i]+data_21$t[pos])/2
  res_y11[i]<-(data_11$y1[i]+data_21$y2[pos])/2
}
res11<-cbind.data.frame(res_t11,res_y11)
colnames(res11)<-c("T","Y")
ggplot()+
  geom_point(data_11,mapping = aes(x=t,y=y1),color="blue")+
  geom_point(data_21,mapping = aes(x=t,y=y2),color="red")+
  geom_point(res11, mapping = aes(x=T,y=Y),color="green")

res_t21<-0
res_y21<-0
for(i in 1:length(data_21$t))
{
  pos<-which.min(abs(data_21$y2[i]-data_11$y1))
  res_t21[i]<-(data_21$t[i]+data_11$t[pos])/2
  res_y21[i]<-(data_21$y2[i]+data_11$y1[pos])/2
}
res21<-cbind.data.frame(res_t21,res_y21)
colnames(res21)<-c("T","Y")

res1<-rbind(res11,res21)

ggplot()+
  geom_point(data_11,mapping = aes(x=t,y=y1),color="blue")+
  geom_point(data_21,mapping = aes(x=t,y=y2),color="red")+
  geom_point(res1, mapping = aes(x=T,y=Y),color="magenta")+
  geom_point(res21, mapping = aes(x=T,y=Y),color="green")



ggplot()+
  geom_point(data_12,mapping = aes(x=t,y=y1),color="blue")+
  geom_point(data_22,mapping = aes(x=t,y=y2),color="red")


res_t12<-0
res_y12<-0
for(i in 1:length(data_12$t))
{
  pos<-which.min(abs(data_12$y1[i]-data_22$y2))
  res_t12[i]<-(data_12$t[i]+data_22$t[pos])/2
  res_y12[i]<-(data_12$y1[i]+data_22$y2[pos])/2
}
res12<-cbind.data.frame(res_t12,res_y12)
colnames(res12)<-c("T","Y")
ggplot()+
  geom_point(data_12,mapping = aes(x=t,y=y1),color="blue")+
  geom_point(data_22,mapping = aes(x=t,y=y2),color="red")+
  geom_point(res12, mapping = aes(x=T,y=Y),color="green")

res_t22<-0
res_y22<-0
for(i in 1:length(data_22$t))
{
  pos<-which.min(abs(data_22$y2[i]-data_12$y1))
  res_t22[i]<-(data_22$t[i]+data_12$t[pos])/2
  res_y22[i]<-(data_22$y2[i]+data_12$y1[pos])/2
}
res22<-cbind.data.frame(res_t22,res_y22)
colnames(res22)<-c("T","Y")

res2<-rbind(res12,res22)

ggplot()+
  geom_point(data_12,mapping = aes(x=t,y=y1),color="blue")+
  geom_point(data_22,mapping = aes(x=t,y=y2),color="red")+
  geom_point(res2, mapping = aes(x=T,y=Y),color="magenta")+
  geom_point(res22, mapping = aes(x=T,y=Y),color="green")



res<-rbind(res1,res2)

ggplot()+
  geom_line(data1,mapping = aes(x=t,y=y1/(mx/mx1)),size=1,color="green")+
  geom_line(data2,mapping = aes(x=t,y=y2/(mx/mx2)),size=1,color="blue")+
  geom_line(res, mapping = aes(x=T,y=Y),size=1,color="red")

