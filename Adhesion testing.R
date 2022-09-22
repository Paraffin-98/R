
library("readxl")
library("ggplot2")

##/////////////// for data set-1 \\\\\\\\\\\\\\\\\\##

data1<- read_excel(file.choose())
#attach(data1)  ### It allow us to access data without using "$" sign.###

ggplot(data = cbind.data.frame(data1$'Time(s)', data1$'Disp(um)', data1$'Load(N)'),aes(x=data1$`Disp(um)`,y=data1$`Load(N)`))+
  geom_line(mapping = aes(x = data1$'Disp(um)', y = data1$'Load(N)'), color = "green", size = 1)+
  geom_point(shape = 4,color="blue",size=1)+
  geom_point(aes(x=564.2,	y=19.8278, size = 1),col="red",shape=3,stroke= 2)+
  geom_point(aes(x=644.4,	y=20.0966, size = 1),col="red",shape=3,stroke= 2)+
  geom_hline(yintercept = 19.6867,col="Magenta",size=0.5)+theme_get()+
  xlab("Distance...")+ylab("Load...")+
  ggtitle("For dataset-1")



##/////////////// for data set-2 \\\\\\\\\\\\\\\\\\##

data2<- read_excel(file.choose())

ggplot(data = cbind.data.frame(data2$'Time(s)', data2$'Disp(um)', data2$'Load(N)'),aes(x=data2$`Disp(um)`,y=data2$`Load(N)`))+
  geom_line(mapping = aes(x = data2$'Disp(um)', y = data2$'Load(N)'), color = "green", size = 1)+
  geom_point(shape = 4,color="blue",size=1)+
  geom_point(aes(x=566.1,	y=19.8726, size = 1),col="red",shape=3,stroke= 2)+
  geom_point(aes(x=653.4,	y=20.2291, size = 1),col="red",shape=3,stroke= 2)+
  geom_hline(yintercept = 19.84824664,col="magenta",size=0.5)+theme_get()+
  xlab("Distance...")+ylab("Load...")+
  ggtitle("For dataset-2")



##/////////////// for data set-3 \\\\\\\\\\\\\\\\\\##

data3<- read_excel(file.choose())

ggplot(data = cbind.data.frame(data3$'Time(s)', data3$'Disp(um)', data3$'Load(N)'),aes(x=data3$`Disp(um)`,y=data3$`Load(N)`))+
  geom_line(mapping = aes(x = data3$'Disp(um)', y = data3$'Load(N)'), color = "green", size = 1)+
  geom_point(shape = 4,color="blue",size=1)+
  geom_point(aes(x=586.1,	y=19.0938, size = 1),col="red",shape=3,stroke= 2)+
  geom_point(aes(x=644.2,	y=19.5982, size = 1),col="red",shape=3,stroke= 2)+
  geom_hline(yintercept = 18.90978319,col="magenta",size=0.5)+theme_get()+
  xlab("Distance...")+ylab("Load...")+
  ggtitle("For dataset-3")


