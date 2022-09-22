# f(t)=e^((-(t+1.5)^2)/2)+ e^((-(t-1.5)^2)/2)  ;  t∈[-12,12]. 


library(ggplot2)
library(ggpmisc)
library(data.table)
library(dplyr)

N<- 500
t1<- runif(N, -12, 12)
t2<- runif(N, -12, 12)
b1<- 1     # first peak amplitude
b2<- 0.7   # second peak amplitude
a1<- 5    # shifting factor for first device
a2<- -5     # shifting factor for second device
c <- 0.85  # amplitude factor for 2nd device compared to 1st one.

y1=b1*exp((-(t1+1.5-a1)^2)/2)+b2*exp((-(t1-1.5-a1)^2)/2)
y2=c*b1*exp((-(t2+1.5-a2)^2)/2)+c*b2*exp((-(t2-1.5-a2)^2)/2)

data1<-cbind.data.frame(t1,y1)
data1<-data1[order(data1$t1), ]

data2<-cbind.data.frame(t2,y2)
data2<-data2[order(data2$t2), ]

ggplot()+
  geom_point(data1, mapping = aes(x=t1, y=y1),color="green",size=1)+
  geom_point(data2, mapping = aes(x=t2, y=y2), color= "blue",size=1)


# peak & valley finder function...

peak_finder <- function(val)
{
  len<- length(val)
  tmp<- 0
  idx<- 1
  for (i in 2:(len-1) ) {
    if( (val[i-1]<val[i]) && (val[i]>val[i+1]) ) {
      tmp[idx]<-i
      idx<-idx+1
    }
  }
  return(tmp)
}

valley_finder <- function(val)
{
  len<- length(val)
  tmp<- 0
  idx<- 1
  for (i in 2:(len-1) ) {
    if( (val[i-1]>val[i]) && (val[i]<val[i+1]) ) {
      tmp[idx]<-i
      idx<-idx+1
    }
  }
  return(tmp)
}

peak_1 <- peak_finder(data1$y1)
#print(peak_1)
valley_1 <- valley_finder(data1$y1)
#print(valley_1)

ggplot()+
  geom_point(data1, mapping = aes(x=t1, y=y1),color="green",size=1)+
  geom_point(data2, mapping = aes(x=t2, y=y2), color= "blue",size=1)+
  geom_point(data1, mapping = aes(x=t1[peak_1[1]],y=y1[peak_1[1]]),shape=3,size=5,color="magenta",stroke=2)+
  geom_point(data1, mapping = aes(x=t1[peak_1[2]],y=y1[peak_1[2]]),shape=3,size=5,color="magenta",stroke=2)+
  geom_point(data1, mapping = aes(x=t1[valley_1],y=y1[valley_1]),shape=3,size=5,color="black",stroke=2)

peak_2<-peak_finder(data2$y2)
#print(peak_2)
valley_2 <- valley_finder(data2$y2)
#print(valley_2)

ggplot()+
  geom_point(data1, mapping = aes(x=t1, y=y1),color="green",size=1)+
  geom_point(data2, mapping = aes(x=t2, y=y2), color= "blue",size=1)+
  geom_point(data1, mapping = aes(x=t1[peak_1[1]],y=y1[peak_1[1]]),shape=3,size=5,color="magenta",stroke=2)+
  geom_point(data1, mapping = aes(x=t1[peak_1[2]],y=y1[peak_1[2]]),shape=3,size=5,color="magenta",stroke=2)+
  geom_point(data1, mapping = aes(x=t1[valley_1],y=y1[valley_1]),shape=3,size=5,color="black",stroke=2)+
  geom_point(data2, mapping = aes(x=t2[peak_2[1]],y=y2[peak_2[1]]),shape=3,size=5,color="magenta",stroke=2)+
  geom_point(data2, mapping = aes(x=t2[peak_2[2]],y=y2[peak_2[2]]),shape=3,size=5,color="magenta",stroke=2)+
  geom_point(data2, mapping = aes(x=t2[valley_2],y=y2[valley_2]),shape=3,size=5,color="black",stroke=2)



# Make both device amplitude same...

idx1<-which.max(data1$y1)
mx1<-data1$y1[idx1]
idx2<-which.max(data2$y2)
mx2<-data2$y2[idx2]
mx<-(mx1+mx2)/2
data1$y1<-data1$y1*(mx/mx1)
data2$y2<-data2$y2*(mx/mx2)
ggplot()+
  geom_point(data1, mapping = aes(x=t1, y=y1),color="green",size=1)+
  geom_point(data2, mapping = aes(x=t2, y=y2), color= "blue",size=1)



# Split curve according to peak and valley...

# idx1<-which.max(data1$y1)
data_11<-data1[1:peak_1[1], ]
data_12<-data1[peak_1[1]+1:(valley_1-peak_1[1]), ]
data_13<-data1[valley_1+1:(peak_1[2]-valley_1), ]
data_14<-data1[peak_1[2]+1:(N-peak_1[2]), ]

# idx2<-which.max(data2$y2)
data_21<-data2[1:peak_2[1], ]
data_22<-data2[peak_2[1]+1:(valley_2-peak_2[1]), ]
data_23<-data2[valley_2+1:(peak_2[2]-valley_2), ]
data_24<-data2[peak_2[2]+1:(N-peak_2[2]), ]

ggplot()+
  geom_point(data_11,mapping = aes(x=t1,y=y1),color="green")+
  geom_point(data_21,mapping = aes(x=t2,y=y2),color="green")+
  geom_point(data_12,mapping = aes(x=t1,y=y1),color="black")+
  geom_point(data_22,mapping = aes(x=t2,y=y2),color="black")+
  geom_point(data_13,mapping = aes(x=t1,y=y1),color="magenta")+
  geom_point(data_23,mapping = aes(x=t2,y=y2),color="magenta")+
  geom_point(data_14,mapping = aes(x=t1,y=y1),color="blue")+
  geom_point(data_24,mapping = aes(x=t2,y=y2),color="blue")


# shortest y deviation calculation

                 ## 1St part ##
res_t11<-0
res_y11<-0
for(i in 1:length(data_11$t1))
{
  pos<-which.min(abs(data_11$y1[i]-data_21$y2))
  res_t11[i]<-(data_11$t1[i]+data_21$t2[pos])/2
  res_y11[i]<-(data_11$y1[i]+data_21$y2[pos])/2
}
res11<-cbind.data.frame(res_t11,res_y11)
colnames(res11)<-c("T","Y")
ggplot()+
  geom_point(data_11,mapping = aes(x=t1,y=y1),color="green")+
  geom_point(data_21,mapping = aes(x=t2,y=y2),color="green")+
  geom_point(res11, mapping = aes(x=T,y=Y),color="red")


                     ## 2nd part ##
res_t12<-0
res_y12<-0
for(i in 1:length(data_12$t1))
{
  pos<-which.min(abs(data_12$y1[i]-data_22$y2))
  res_t12[i]<-(data_12$t1[i]+data_22$t2[pos])/2
  res_y12[i]<-(data_12$y1[i]+data_22$y2[pos])/2
}
res12<-cbind.data.frame(res_t12,res_y12)
colnames(res12)<-c("T","Y")
ggplot()+
  geom_point(data_11,mapping = aes(x=t1,y=y1),color="green")+
  geom_point(data_21,mapping = aes(x=t2,y=y2),color="green")+
  geom_point(res11, mapping = aes(x=T,y=Y),color="red")+
  geom_point(data_12,mapping = aes(x=t1,y=y1),color="blue")+
  geom_point(data_22,mapping = aes(x=t2,y=y2),color="blue")+
  geom_point(res12, mapping = aes(x=T,y=Y),color="black")


                          ## 3rd part ##
res_t13<-0
res_y13<-0
for(i in 1:length(data_13$t1))
{
  pos<-which.min(abs(data_13$y1[i]-data_23$y2))
  res_t13[i]<-(data_13$t1[i]+data_23$t2[pos])/2
  res_y13[i]<-(data_13$y1[i]+data_23$y2[pos])/2
}
res13<-cbind.data.frame(res_t13,res_y13)
colnames(res13)<-c("T","Y")
ggplot()+
  geom_point(data_11,mapping = aes(x=t1,y=y1),color="green")+
  geom_point(data_21,mapping = aes(x=t2,y=y2),color="green")+
  geom_point(res11, mapping = aes(x=T,y=Y),color="red")+
  geom_point(data_12,mapping = aes(x=t1,y=y1),color="cyan")+
  geom_point(data_22,mapping = aes(x=t2,y=y2),color="cyan")+
  geom_point(res12, mapping = aes(x=T,y=Y),color="purple")+
  geom_point(data_13,mapping = aes(x=t1,y=y1),color="yellow")+
  geom_point(data_23,mapping = aes(x=t2,y=y2),color="yellow")+
  geom_point(res13, mapping = aes(x=T,y=Y),color="magenta")


                        ## 4th part ##
res_t14<-0
res_y14<-0
for(i in 1:length(data_14$t1))
{
  pos<-which.min(abs(data_14$y1[i]-data_24$y2))
  res_t14[i]<-(data_14$t1[i]+data_24$t2[pos])/2
  res_y14[i]<-(data_14$y1[i]+data_24$y2[pos])/2
}
res14<-cbind.data.frame(res_t14,res_y14)
colnames(res14)<-c("T","Y")
ggplot()+
  geom_point(data_11,mapping = aes(x=t1,y=y1),color="green",size=1)+
  geom_point(data_21,mapping = aes(x=t2,y=y2),color="green",size=1)+
  geom_point(res11, mapping = aes(x=T,y=Y),color="green",size=1)+
  geom_point(data_12,mapping = aes(x=t1,y=y1),color="red",size=1)+
  geom_point(data_22,mapping = aes(x=t2,y=y2),color="red",size=1)+
  geom_point(res12, mapping = aes(x=T,y=Y),color="red",size=1)+
  geom_point(data_13,mapping = aes(x=t1,y=y1),color="blue",size=1)+
  geom_point(data_23,mapping = aes(x=t2,y=y2),color="blue",size=1)+
  geom_point(res13, mapping = aes(x=T,y=Y),color="blue",size=1)+
  geom_point(data_14,mapping = aes(x=t1,y=y1),color="magenta",size=1)+
  geom_point(data_24,mapping = aes(x=t2,y=y2),color="magenta",size=1)+
  geom_point(res14, mapping = aes(x=T,y=Y),color="magenta",size=1)


res<- rbind(res11,res12,res13,res14)
ggplot()+
  geom_line(data1, mapping = aes(x=t1,y=y1/(mx/mx1)),color="green",size=1)+
  geom_line(data2, mapping = aes(x=t2,y=y2/(mx/mx2)),color="blue",size=1)+
  geom_line(res, mapping = aes(x=T,y=Y),color="red",size=1)

