install.packages("mfp")
install.packages("freqparcoord")

library(mfp)
library(freqparcoord)


data(mlb)
names(mlb)
mean(mlb$Weight)
attach(mlb)
mlb.reg=lm(Weight~Height, data=mlb)
#informaci�n de todo
summary(mlb.reg)

#calculadno para 72"
-151.1333+4.7833*72

#-----------analisis no param�trico------------

#tapply hace que se dividan la variable peso en grupos segun los valores de la variable Altura
#y luego calcula el peso promedio en cada grupo. 
#Ej: para una altura espec�fico, el promedio del peso de todos los que mida esa altura

muhats <- tapply(mlb$Weight, mlb$Height, mean)
muhats

plot(67:83,muhats)


#length: conteo 

muhatsl <- tapply(mlb$Weight, mlb$Height, length)
muhatsl

plot(67:83,muhats1)

#gr�fica de peso conrta altura y l�nea de regresi�n

plot(Weight, main ="Peso vs Altura", xlab = "Peso", ylab = "Altura")
abline(lm(Weight~Height))

#Calcular la desviaci�n estandar  (tomar en cuenta que es 1 desviaci�n est�ndar)
muhatss <- tapply(mlb$Weight, mlb$Height, sd)
muhatss


#Prediccion param�trica

attach(mlb)

mlb.reg=lm(Weight~Height,data=mlb)
summary(mlb.reg)

#u*(t)=c+d*t

coef(mlb.reg)


#calcular el peso para una altura de 72"

coef(mlb.reg)%*%c(1,72)


#Se calcula la desv. est. de el vector 

matriz <-c(1,72)

sqrt(matriz%*%vcov(mlb.reg)%*%matriz)


#--------------regresion multiple----------

attach(mlb)
mlb.regm=lm(Weight~Height+Age,data=mlb)
summary(mlb.regm)




