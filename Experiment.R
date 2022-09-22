
library("readxl")
library("ggplot2")

data1<- read_excel(file.choose())

ggplot(data = cbind.data.frame(data1$'Time(s)', data1$'Disp(um)', data1$'Load(N)'),aes(x=data1$`Disp(um)`,y=data1$`Load(N)`))+
  geom_point(shape = 4,color="red",size=1)+
  geom_smooth(method=lm, se=FALSE)



#set.seed(6)
x <- runif(100, -3, 3) 
y <- 2 + x + rnorm(100) 

model1<-lm(y ~ x) 
plot(x,y)
abline(model1,col="red")
abline(coef(model1),0,col="dark green")

b1 <- 1.5
a <- mean(y - b1 * x)

mod2 <- lm(I(y-1.5*x)~1)
plot(x, y)
abline(mod2$coefficients, 0)


#https://stackoverflow.com/questions/33292969/linear-regression-with-specified-slope
#how to plot a best fit line with specific slope in R
#https://stackoverflow.com/questions/27192929/drawing-line-segments-in-r
#segment line plot in R

#########################################################


library(ggplot2)
library(ggpmisc)
library(data.table)
library(dplyr)

N<- 500
t<- runif(N, -10, 10)
a1<- -5  ## shaifting factor
a2<- 5  ## shaifting factor
b<- 1  ## amplitude factor

y1=1*exp(-(t-a1)^2)
y2=1.5*exp(-(t-a2)^2)

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
  geom_line(data1, mapping= aes(x=t, y=y1),size=1, color="red")+
  geom_line(data2, mapping= aes(x=t, y=y2),size=1, color="blue")+
  geom_line(res, mapping = aes(x=T, y=Y),size=1, color="green")

#+
# stat_smooth(res, mapping=aes(y=Y, x=T), method=lm, formula=y~poly(x,15),se=F)


ggplot()+
  geom_point(res1, mapping = aes(x=T,y=Y))+
  geom_point(data_11, mapping = aes(x=t,y=y1),color="red")+
  geom_point(data_21, mapping = aes(x=t,y=y2),color="blue")

ggplot()+
  geom_point(res2, mapping = aes(x=T,y=Y))+
  geom_point(data_12, mapping = aes(x=t,y=y1),color="red")+
  geom_point(data_22, mapping = aes(x=t,y=y2),color="blue")

## subplot
pos<-which.min(abs(data_11$y1[i]-data_21$y2))



################################ weighted single peak ################################

library(ggplot2)
library(ggpmisc)
library(data.table)
library(dplyr)


SHD<-function(device1,device2)
{
  colnames(device1)<-c("T","Y","W")            # Rename coloum_name
  colnames(device2)<-c("T","Y","W")
  
  device1<-device1[order(device1$T), ]         # sorting
  device2<-device2[order(device2$T), ]
  
  idx1<-which.max(device1$Y)                   # amplitude normalizing
  mx1<-device1$Y[idx1]
  idx2<-which.max(device2$Y)
  mx2<-device2$Y[idx2]
  mx<-(mx1+mx2)/2
  device1$Y<-device1$Y*(mx/mx1)
  device2$Y<-device2$Y*(mx/mx2)
  
  
  idx1<-which.max(device1$Y)               # spliting with respect to peak
  device_11<-device1[1:idx1, ]
  device_12<-device1[idx1+1:(N-idx1), ]
  idx2<-which.max(device2$Y)
  device_21<-device2[1:idx2, ]
  device_22<-device2[idx2+1:(N-idx2), ]
  
  
  res_t1<-0                       # mean for left part of each device
  res_y1<-0
  res_w1<-0
  for(i in 1:length(device_11$T))       
  {
    pos<- which.min(abs(device_11$Y[i]-device_21$Y))
    res_t1[i]<- ((device_11$T[i]*device_11$W[i])+(device_21$T[pos]*device_21$W[pos]))/(device_11$W[i]+device_21$W[pos])
    res_y1[i]<- ((device_11$Y[i]*device_11$W[i])+(device_21$Y[pos]*device_21$W[pos]))/(device_11$W[i]+device_21$W[pos])
    res_w1[i]<- device_11$W[i]+device_21$W[pos]
  }
  res1<-cbind.data.frame(res_t1,res_y1,res_w1)
  colnames(res1)<-c("T","Y","W")      # to change coloum name from res_t11 to T
                                   # & res_y11 to Y
  
  res_t2<-0                       # mean for right part of each device
  res_y2<-0
  res_w2<-0
  for(i in 1:length(device_12$T))
  {
    pos<-which.min(abs(device_12$Y[i]-device_22$Y))
    res_t2[i]<-((device_12$T[i]*device_12$W[i])+(device_22$T[pos]*device_22$W[pos]))/(device_12$W[i]+device_22$W[pos])
    res_y2[i]<-((device_12$Y[i]*device_12$W[i])+(device_22$Y[pos]*device_22$W[pos]))/(device_12$W[i]+device_22$W[pos])
    res_w2[i]<- device_12$W[i]+device_22$W[pos]
  }
  res2<-cbind.data.frame(res_t2,res_y2,res_w2)
  colnames(res2)<-c("T","Y","W")     # to change coloum name from res_t11 to T
                                  # & res_y11 to Y
  
  res<-rbind(res1,res2)         # bind two half in row wise
  
  return(res)
}




N<- 500
t1<- runif(N, -10, 10)
t2<- runif(N, -10, 10)
a1<- 0    ## shaifting factor
a2<- 5    ## shaifting factor
b1<- 1    ## amplitude factor
b2<- 1.4  ## amplitude factor

y1<-0.6*exp(-(t1-a1)^2)
y2<-0.8*exp(-(t2-a2)^2)
w1<-1^t1
w2<-1^t2
# w1[5]<-10.5      ## For checking weight correctness
# w1[100]<-5.875

data1<-cbind.data.frame(t1,y1,w1)
#data1<-data1[order(data1$t), ]

data2<-cbind.data.frame(t2,y2,w2)
#data2<-data2[order(data2$t), ]



result<-SHD(data1,data2)
ggplot()+
  geom_line(data1,mapping = aes(x=t1,y=y1),size=1,color="green")+
  geom_line(data2,mapping = aes(x=t2,y=y2),size=1,color="blue")+
  geom_line(result, mapping = aes(x=T,y=Y),size=1,color="red")+
  geom_line(result, mapping = aes(x=T,y=W),size=1,color="magenta")

######################################################################

for(i in 1:length(y8)){
  print(y8[i])
}
#######################################################################

library(xlsx)
library(readxl)
aa<-read.xlsx("C:/Users/HP/Documents/RStudio/SNL/code/Data(8-device).xlsx", "sheet_name", rowIndex = 5:700, colIndex = 1:10)
bb<- read_excel(choose.files())
for(i in 1:4){
  zz<- bb[[i]]
  print(zz)
}

for(i in seq(from=1, to=4, by=2)){
  print("bokachoda JKP")
}

ss<-rep(1,500)
tt<-cbind.data.frame(t1,y1,ss)


##############################################

library(readr)
library(ggplot2)
library(dplyr)
a<-read_rds(choose.files()) ## load row=50, coloum=250& page=100 in "a" varable
# print(a[ , , ])

p <- ggplot()
plot_mul <- function(df){
  p <<- p + geom_line(data=df, aes(x,y),size=1)
  return(p)
}


a1<-a[ , ,1]       ## first page row=50 & coloum=250
t<-(1:250)

for(i in 1:50){
  tmp<-a1[i, ]
  tmp<-data.frame(t,tmp)
  colnames(tmp)<-c("x","y")
  plot_mul(tmp)
}
p


##################### Write data in xlsx ############################

library(readxl)
write.xlsx(data1, C:\Users\HP\Documents\RStudio\SNL\code\'Data double peak.xlsx')

write_excel_csv(data1, C:/Users/HP/Documents/RStudio/SNL/code/'Data double peak.xlsx')
