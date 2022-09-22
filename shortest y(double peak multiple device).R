library(ggplot2)
library(dplyr)
library(readr)


a<-read_rds(choose.files()) ## load file row=50, coloum=250& page=100 in "a" varable
N<-250                      ## num of point each curve



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

SHD_D_Peak<-function(device1,device2){
  
  peak_1 <- peak_finder(data1$y1)        ## peak & valley for device1
  #print(peak_1)
  valley_1 <- valley_finder(data1$y1)
  #print(valley_1)
  
  peak_2<-peak_finder(data2$y2)          ## peak & valley for device2
  #print(peak_2)
  valley_2 <- valley_finder(data2$y2)
  #print(valley_2)
  
  idx1<-which.max(data1$y1)              ## make both device amplitude same
  mx1<-data1$y1[idx1]
  idx2<-which.max(data2$y2)
  mx2<-data2$y2[idx2]
  mx<-(mx1+mx2)/2
  data1$y1<-data1$y1*(mx/mx1)
  data2$y2<-data2$y2*(mx/mx2)
  
  
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
  
  
  res<- rbind(res11,res12,res13,res14)    ## resultant curve
  
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

