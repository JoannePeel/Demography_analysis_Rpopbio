##limpiar memoria##
rm(list=ls(all=TRUE))

##Cargar popbio##
library(popbio)


##datosA-dap2014-15##
datosA<-read.csv(file.choose ())
head(datosA, n=2)

##Clasificar##Clases->Dap 2014-2015 ###
##t0##
datosA$clase1.t0<-ifelse(datosA$t0<=1 &datosA$t0>0,1,0)
datosA$clase2.t0<-ifelse(datosA$t0<=4 &datosA$t0>1,2,0)
datosA$clase3.t0<-ifelse(datosA$t0<=6 &datosA$t0>4,3,0)
datosA$clase4.t0<-ifelse(datosA$t0<=9 &datosA$t0>6,4,0)
datosA$clase5.t0<-ifelse(datosA$t0<=29 &datosA$t0>9,5,0)
datosA$clase6.t0<-ifelse(datosA$t0 >29,6,0)
datosA$staget0<-with(datosA,clase1.t0+clase2.t0+clase3.t0+clase4.t0+clase5.t0+clase6.t0)
head(datosA,n=10)
datosA$staget0
table(datosA$staget0)

##Clasificar##Clases->Dap 2014-2015 ###
##t1##

datosA$clase1.t1<-ifelse(datosA$t1<=1 &datosA$t1>0,1,0)
datosA$clase2.t1<-ifelse(datosA$t1<=4 &datosA$t1>1,2,0)
datosA$clase3.t1<-ifelse(datosA$t1<=6 &datosA$t1>4,3,0)
datosA$clase4.t1<-ifelse(datosA$t1<=9 &datosA$t1>6,4,0)
datosA$clase5.t1<-ifelse(datosA$t1<=29 &datosA$t1>9,5,0)
datosA$clase6.t1<-ifelse(datosA$t1 >29,6,0)
datosA$staget1<-with(datosA,clase1.t1+clase2.t1+clase3.t1+clase4.t1+clase5.t1+clase6.t1)
head(datosA,n=10)
datosA$staget1
table(datosA$staget1)

##histograma##
hist(datosA$staget0)
hist(datosA$staget1)

###crear tabla de frecuencia:importante primero va año t+1, luego t0####
table(datosA$staget1,datosA$staget0)

###reproducción###

###producción de propágulo/mes(censo)###
datosA$rept0<-(datosA$F)
datosA$rept0
###renombrar columnas###
str(datosA)
datosA.matriz<-as.data.frame(cbind(datosA$ind, datosA$staget0, datosA$staget1, datosA$rept0))
datosA.matriz
str(datosA.matriz)
colnames(datosA.matriz)[1]<-c("Individuo")
colnames(datosA.matriz)[2]<-c("staget0")
colnames(datosA.matriz)[3]<-c("staget1")
colnames(datosA.matriz)[4]<-c("rept0")

str(datosA.matriz)

###Transformar estados a characteres###
datosA.matriz$staget0<-as.character(datosA.matriz$staget0)
datosA.matriz$staget1<-as.character(datosA.matriz$staget1)



colnames(datosA.matriz)[4]<-c("rept0")
stages<-c("1","2","3","4","5","6")


str(datosA.matriz)


###Generar una matriz de transición###
matrizA<-projection.matrix(datosA.matriz,staget0,staget1,rept0, sort=stages, TF=FALSE)
matrizA




###análisis###
analisisA<-eigen.analysis(matrizA)



lambda(matrizA)
stable.stage(matrizA)
reproductive.value(matrizA)
sensitivity(matrizA)
elasticity(matrizA)

######################################################################################################################################
######################################################################################################################################

##Generar la segunda matriz##
##formas de abrir tabla##
datosB<-read.csv(file.choose ())
head(datosB, n=2)

##Clasificar##Clases->Dap 2015-2016 ###

datosB$clase1.t0<-ifelse(datosB$t0<=1 &datosB$t0>0,1,0)
datosB$clase2.t0<-ifelse(datosB$t0<=4 &datosB$t0>1,2,0)
datosB$clase3.t0<-ifelse(datosB$t0<=6 &datosB$t0>4,3,0)
datosB$clase4.t0<-ifelse(datosB$t0<=9 &datosB$t0>6,4,0)
datosB$clase5.t0<-ifelse(datosB$t0<=29 &datosB$t0>9,5,0)
datosB$clase6.t0<-ifelse(datosB$t0 >29,6,0)
datosB$staget0<-with(datosB,clase1.t0+clase2.t0+clase3.t0+clase4.t0+clase5.t0+clase6.t0)
head(datosB,n=10)
datosB$staget0
table(datosB$staget0)

datosB$clase1.t1<-ifelse(datosB$t1<=1 &datosB$t1>0,1,0)
datosB$clase2.t1<-ifelse(datosB$t1<=4 &datosB$t1>1,2,0)
datosB$clase3.t1<-ifelse(datosB$t1<=6 &datosB$t1>4,3,0)
datosB$clase4.t1<-ifelse(datosB$t1<=9 &datosB$t1>6,4,0)
datosB$clase5.t1<-ifelse(datosB$t1<=29 &datosB$t1>9,5,0)
datosB$clase6.t1<-ifelse(datosB$t1 >29,6,0)
datosB$staget1<-with(datosB,clase1.t1+clase2.t1+clase3.t1+clase4.t1+clase5.t1+clase6.t1)
head(datosB,n=10)
datosB$staget1
table(datosB$staget1)

hist(datosB$staget0)
hist(datosB$staget1)

###crear tabla de frecuencia:importante primero va año t+1, luego t0####
table(datosB$staget1,datosB$staget0)

###reproducción###

###producción anual de propágulos/año###
#anual_props<-12
#datosB$rept0<-(datosB$F*anual_props)
datosB$rept0


###producción de propágulo/mes(censo)###
datosB$rept0<-(datosB$F)
datosB$rept0
###renombrar columnas###
str(datosB)
datosB.matriz<-as.data.frame(cbind(datosB$ind, datosB$staget0, datosB$staget1, datosB$rept0))
datosB.matriz
str(datosB.matriz)
colnames(datosB.matriz)[1]<-c("Individuo")
colnames(datosB.matriz)[2]<-c("staget0")
colnames(datosB.matriz)[3]<-c("staget1")
colnames(datosB.matriz)[4]<-c("rept0")

str(datosB.matriz)

###Transformar estados a characteres###
datosB.matriz$staget0<-as.character(datosB.matriz$staget0)
datosB.matriz$staget1<-as.character(datosB.matriz$staget1)



colnames(datosB.matriz)[4]<-c("rept0")
stages<-c("1","2","3","4","5","6")


str(datosB.matriz)


###Generar una matriz de transición###
matrizB<-projection.matrix(datosB.matriz,staget0,staget1,rept0, sort=stages, TF=FALSE)
matrizB


###reemplazar un valor manualmente###

matrizB[[2, 1]] <- 0.0000001

###análisis###
analisisB<-eigen.analysis(matrizB)



lambda(matrizB)
stable.stage(matrizB)
reproductive.value(matrizB)
sensitivity(matrizB)
elasticity(matrizB)


n <- c(5, 5, 5, 5, 5, 5)
p <- pop.projection(matrizB, n, 20)
p

stage.vector.plot(p$stage.vectors, col = 2:4)

plot(analisisB$stable.stage)
barplot(analisisB$stable.stage)
barplot(analisisB$repro.value)


############################################################################################################


##Chisqare test para daps##

row1 = c(86,352.69)
row2 = c(187,139.14) 
row3 = c(166,39.20) 
row4 = c(60,9.19)
row5 = c(32,2.41)
row6 = c(12,0.36)

data.table = rbind(row1,row2,row3, row4, row5, row6)
data.table

####Chi cuadrada###
chisq.test(data.table)
### Calcular valores esperados###
 chisq.test(data.table)$expected
###Calcular residuales####
R<-chisq.test(data.table)$residuals
###Calcular residulaes estandarizados##
S<-chisq.test(data.table)$stdres


############################################################################################################
#1.Simulaciones#General#
#that the numbers in the for loop must be the same length as the size of k
#can be checked with the command length(k)
bootstrap<-NULL
for(i in 1:26)
{
  k<-c(0.0001,0.0005,0.001,0.003,0.005,0.009,0.01,0.015,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  #length(k)
  matriz_trabajo <- matrizA
  matriz_trabajo[1,1]<-k[i]
  sim<-eigen.analysis(matriz_trabajo)
  lambdasim<-sim$lambda1
  bootstrap[i]<-lambdasim
}
#png(filename="Simulacion_mangle.png",width=480, height=480)
plot(k,bootstrap,ylab="Finite rate of population increase",xlab="Value of cell",cex=0.1,type="l")
text(0.15,3.5,"Mangle",cex=2)
abline(h=1,lty=2)
#dev.off()
#####a22 2014-2015###
#2.Simulaciones
#that the numbers in the for loop must be the same length as the size of k
#can be checked with the command length(k)
bootstrap<-NULL
for(i in 1:26)
{
  k<-c(0.0001,0.0005,0.001,0.003,0.005,0.009,0.01,0.015,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.2,0.3,0.4,0.5,0.6,0.65, 0.7,0.7135)
  #length(k)
  matriz_trabajo <- matrizA
  matriz_trabajo[2,2]<-k[i]
  sim<-eigen.analysis(matriz_trabajo)
  lambdasim<-sim$lambda1
  bootstrap[i]<-lambdasim
}
#png(filename="Simulacion_mangle.png",width=480, height=480)
plot(k,bootstrap,ylab="Finite rate of population increase",xlab="Value of cell",cex=0.1,type="l")
text(0.15,3.5,"Mangle",cex=2)
abline(h=1,lty=2)
#dev.off()

#####3. para a55 2014-2015###
bootstrap<-NULL
for(i in 1:26)
{
  k<-c(0.0001,0.0005,0.001,0.003,0.005,0.009,0.01,0.015,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.2,0.3,0.4,0.5,0.6,0.65, 0.7,0.75)
  #length(k)
  matriz_trabajo <- matrizA
  matriz_trabajo[5,5]<-k[i]
  sim<-eigen.analysis(matriz_trabajo)
  lambdasim<-sim$lambda1
  bootstrap[i]<-lambdasim
}
#png(filename="Simulacion_mangle.png",width=480, height=480)
plot(k,bootstrap,ylab="Finite rate of population increase",xlab="Value of cell",cex=0.1,type="l")
text(0.15,3.5,"Mangle",cex=2)
abline(h=1,lty=2)
#dev.off()

##### 4. para a66 2014-2015###
bootstrap<-NULL
for(i in 1:26)
{
  k<-c(0.0001,0.0005,0.001,0.003,0.005,0.009,0.01,0.015,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.2,0.3,0.4,0.5,0.6, 0.7,0.8, 0.9167)
  #length(k)
  matriz_trabajo <- matrizA
  matriz_trabajo[6,6]<-k[i]
  sim<-eigen.analysis(matriz_trabajo)
  lambdasim<-sim$lambda1
  bootstrap[i]<-lambdasim
}
#png(filename="Simulacion_mangle.png",width=480, height=480)
plot(k,bootstrap,ylab="Finite rate of population increase",xlab="Value of cell",cex=0.1,type="l")
text(0.15,3.5,"Mangle",cex=2)
abline(h=1,lty=2)
#dev.off()

##### 5. para a22 2015-2016###
bootstrap<-NULL
for(i in 1:26)
{
  k<-c(0.0001,0.0005,0.001,0.003,0.005,0.009,0.01,0.015,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.095, 0.1, 0.15, 0.2, 0.25, 0.3,0.35, 0.4,0.4651)
  #length(k)
  matriz_trabajo <- matrizB
  matriz_trabajo[2,2]<-k[i]
  sim<-eigen.analysis(matriz_trabajo)
  lambdasim<-sim$lambda1
  bootstrap[i]<-lambdasim
}
#png(filename="Simulacion_mangle.png",width=480, height=480)
plot(k,bootstrap,ylab="Finite rate of population increase",xlab="Value of cell",cex=0.1,type="l")
text(0.15,3.5,"Mangle",cex=2)
abline(h=1,lty=2)
#dev.off()

##### 6. para a55 2015-2016###
bootstrap<-NULL
for(i in 1:26)
{
  k<-c(0.0001,0.0005,0.001,0.003,0.005,0.009,0.01,0.015,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.875)
  #length(k)

  matriz_trabajo <- matrizB
  matriz_trabajo[5,5]<-k[i]
  sim<-eigen.analysis(matriz_trabajo)
  lambdasim<-sim$lambda1
  bootstrap[i]<-lambdasim
}
#png(filename="Simulacion_mangle.png",width=480, height=480)
plot(k,bootstrap,ylab="Finite rate of population increase",xlab="Value of cell",cex=0.1,type="l")
text(0.15,3.5,"Mangle",cex=2)
abline(h=1,lty=2)
#dev.off()

##### 7. para a66 2015-2016###
bootstrap<-NULL
for(i in 1:25)
{
  k<-c(0.0001,0.0005,0.001,0.003,0.005,0.009,0.01,0.015,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)
  #length(k)

  matriz_trabajo <- matrizB
  matriz_trabajo[6,6]<-k[i]
  sim<-eigen.analysis(matriz_trabajo)
  lambdasim<-sim$lambda1
  bootstrap[i]<-lambdasim
}
#png(filename="Simulacion_mangle.png",width=480, height=480)
plot(k,bootstrap,ylab="Finite rate of population increase",xlab="Value of cell",cex=0.1,type="l")
text(0.15,3.5,"Mangle",cex=2)
abline(h=1,lty=2)
#dev.off()



############################################################################################################
##iNTERVALOS DE CONFIANZA##

bootstrap1 <- NULL
for (i in 1:10000) 
{n<-nrow(C)
kk<-sample(n,replace=TRUE)
bt<-C[kk,]
bt1 <- bt
pm<-projection.matrix(bt1,staget0,staget1,rept0,sort=stages)
x<-eigen.analysis(pm)
lambda<-x$lambda1
bootstrap1[i]<-lambda
}
summary(bootstrap)
#postscript("bootstrap_lambdas_0809")
hist(bootstrap,ylab="Frequency",main="",xlab=expression(paste(lambda)))
#dev.off()


#####a22 2014-2015###
#Simulaciones
#that the numbers in the for loop must be the same length as the size of k
#can be checked with the command length(k)
bootstrap<-NULL
for(i in 1:26)
{
  k<-c(0.0001,0.0005,0.001,0.003,0.005,0.009,0.01,0.015,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.2,0.3,0.4,0.5,0.6,0.65, 0.7,0.7135)
  #length(k)
  matriz_trabajo <- matrizA
  matriz_trabajo[2,2]<-k[i]
  sim<-eigen.analysis(matriz_trabajo)
  lambdasim<-sim$lambda1
  bootstrap[i]<-lambdasim
}
#png(filename="Simulacion_mangle.png",width=480, height=480)
plot(k,bootstrap,ylab="Finite rate of population increase",xlab="Value of cell",cex=0.1,type="l")
text(0.15,3.5,"Mangle",cex=2)
abline(h=1,lty=2)
#dev.off()

##### para a55 2014-2015###
bootstrap<-NULL
for(i in 1:26)
{
  k<-c(0.0001,0.0005,0.001,0.003,0.005,0.009,0.01,0.015,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.2,0.3,0.4,0.5,0.6,0.65, 0.7,0.75)
  #length(k)
  matriz_trabajo <- matrizA
  matriz_trabajo[5,5]<-k[i]
  sim<-eigen.analysis(matriz_trabajo)
  lambdasim<-sim$lambda1
  bootstrap[i]<-lambdasim
}
#png(filename="Simulacion_mangle.png",width=480, height=480)
plot(k,bootstrap,ylab="Finite rate of population increase",xlab="Value of cell",cex=0.1,type="l")
text(0.15,3.5,"Mangle",cex=2)
abline(h=1,lty=2)
#dev.off()

##### para a66 2014-2015###
bootstrap<-NULL
for(i in 1:26)
{
  k<-c(0.0001,0.0005,0.001,0.003,0.005,0.009,0.01,0.015,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.2,0.3,0.4,0.5,0.6, 0.7,0.8, 0.9167)
  #length(k)
  matriz_trabajo <- matrizA
  matriz_trabajo[6,6]<-k[i]
  sim<-eigen.analysis(matriz_trabajo)
  lambdasim<-sim$lambda1
  bootstrap[i]<-lambdasim
}
#png(filename="Simulacion_mangle.png",width=480, height=480)
plot(k,bootstrap,ylab="Finite rate of population increase",xlab="Value of cell",cex=0.1,type="l")
text(0.15,3.5,"Mangle",cex=2)
abline(h=1,lty=2)
#dev.off()



############################################################################################################
##iNTERVALOS DE CONFIANZA##

bootstrap1 <- NULL
for (i in 1:10000) 
{n<-nrow(C)
kk<-sample(n,replace=TRUE)
bt<-C[kk,]
bt1 <- bt
pm<-projection.matrix(bt1,staget0,staget1,rept0,sort=stages)
x<-eigen.analysis(pm)
lambda<-x$lambda1
bootstrap1[i]<-lambda
}
summary(bootstrap)
#postscript("bootstrap_lambdas_0809")
hist(bootstrap,ylab="Frequency",main="",xlab=expression(paste(lambda)))
#dev.off()


############################################################################################################

##Modelo estochastico##

matrices<-list(matrizA, matrizB)

stoch.growth.mangle <-stoch.growth.rate(matrices, prob=c(0.75,0.25), maxt=5000, verbose=TRUE)
exp(stoch.growth.mangle$approx)
exp(stoch.growth.mangle$sim)
exp(stoch.growth.mangle$sim.CI)







