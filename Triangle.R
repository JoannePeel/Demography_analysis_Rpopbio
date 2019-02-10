

library(ggtern)
library(ggplot2)

base.elasticidades=read.csv(file.choose(), head=T)	 

###Plot para mangle##
str(base.elasticidades)
base.elasticidades
breaks = seq(0,1,by=0.2)
plot <- ggtern(data=base.elasticidades,aes(x=Fecunidad,y=Crecimiento,z=Stasis))+
  scale_color_manual(values=c('black','grey70'))+
  limit_tern(breaks=breaks,labels=1*breaks)+
  theme_bw()+
  geom_point(aes(color=Matriz),size=5)+
  labs(x= "",xarrow  = "Fecundidad", y= "", yarrow  = "Crecimiento", z = "",zarrow  = "Estasis")+
  theme_showarrows()+
  theme_clockwise()
plot 


png(filename="TrianguloMS.png",width=700,height=480)

plot <- ggtern(data=base.elasticidades,aes(x=F,y=C,z=S)) +
   geom_point(aes(color=Matrix),size=2)+scale_color_manual(values=c('black','grey50'))+geom_point(aes(shape=Species),size=3.5)+scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)) +labs(x="F",y="C",z="S")+ theme_bw()
plot+tern_limits(T=.4,L=.4,R=1)
plot
dev.off()
