# Equation: Y=e^(-(t-a)^2/b)

library(ggplot2)
library(ggpmisc)

t<-runif(200, -10, 10)
a1=-3
a2=5
b=1
y1=exp(-(t-a1)^2/b)
y2=exp(-(t-a2)^2/b)
data1<-cbind.data.frame(t,y1,y2)

ggplot( )+
  geom_point(aes(x=t, y=y1),shape = 4,color="red",size=1)+
  geom_line(data = data1, aes(x=t, y=y1),color="red",size=0.5)+
  stat_peaks(data=data1, aes(x=t, y=y1), colour="blue",shape=3,stroke=1,size=5)+
  
  geom_point(aes(x=t, y=y2),shape = 4,color="blue",size=1)+
  geom_line(data = data1, aes(x=t, y=y2),color="blue",size=0.5)+
  stat_peaks(data=data1, aes(x=t, y=y2), colour="red",shape=3,stroke=1,size=5)


print(which.max(sort(data1)))





print(find_peaks(y1))
sort(y1)
