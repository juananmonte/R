library(ggplot2)
library(corrplot)
State1
names(State1)

mean(State1[["Population"]])
#la media recortada queexcluye a los cinco estados más grandes. trim =0.1 calculo que quitá el 10% de los datos (5% arriba y 5% abajo)
mean(State1[["Population"]], trim=0.1)
#la mediana
median(State1[["Population"]])
#promedio ponderado
weighted.mean(State1[["Murder.Rate"]], w=State1[["Population"]])
#resumen datos
summary(State1[["Population"]])
#desviacion estandar
sd(State1[["Population"]])
sum(sd(State1$Population))
#grafica
ggplot(data=State1)+geom_point(mapping = aes(x=Abbreviation, y=Murder.Rate))
                                                                               

#quantiles
quantile(State1[["Murder.Rate"]], p=c(.05, .25, .5, .75, .95))
quantile(State1[["Population"]], p=c(.05, .25, .5, .75, .95))
#boxplot. idea es tener una tasa de homicidios por cada millon de habitantes. Cambiael eje y y su mediciòn. Más fácil de interpretar
boxplot(State1[["Population"]]/1000000, ylab="Population(millones)")

#ejercicio 2

library (ggplot2)
library(corrplot)
head (Cajas3)
#ya que campos no es numérica en sí, entonces la quitamos del análisis
Cajas3$campos = NULL
#se define una nueva variable Cajas3.cor para la correlacion
Cajas3.cor = cor(Cajas3,method="pearson")
#redondear los resultados a 2 digitos
round(Cajas3.cor,digits=2)
corrplot(Cajas3.cor)

corrplot(Cajas3.cor,method="shade",shade.col=NA, tl.col = "black", tl.srt = 45)

corrplot(Cajas3.cor,method="shade",shade.col=NA, tl.col = "black", tl.srt = 45, addCoef.col = "white", order ="AOE", type = "upper", diag = FALSE, addshade ="all")


# ejercicio 3co

library(corrplot)
#Aquí se recomienda que el inversionista invierta en ADS (tiene el mejor promedio)
summary(sp503)
#Sacar correlación, pero hay que quitar la fecha porque no es numérica
sp503$Fecha = NULL
sp503.cor = cor(sp503,method="pearson")
#hacer la correlacion y graficarla
round(sp503.cor, digits=2)
corrplot(sp503.cor)

corrplot(sp503.cor,method="shade",shade.col=NA, tl.col = "black", tl.srt = 45)
corrplot(sp503.cor,method="shade",shade.col=NA, tl.col = "black", tl.srt = 45, addCoef.col = "white", order ="AOE",type = "upper", diag = FALSE, addshade ="all")
#Matriz completa
corrplot(sp503.cor,method="shade",shade.col=NA, tl.col = "black", tl.srt = 45, addCoef.col = "white", order ="AOE", addshade ="all")


#ejercicio 4

data("mtcars")
head(mtcars)
mtcars


summary(mtcars)
#Sacar correlación, pero hay que quitar la fecha porque no es numérica
mtcars$X= NULL
mtcars.cor = cor(mtcars,method="pearson")
round(mtcars.cor, digits=2)
corrplot(mtcars.cor)
corrplot(mtcars.cor,method="shade",shade.col=NA, tl.col = "black", tl.srt = 45, addCoef.col = "white", order ="AOE", addshade ="all")
corrplot(mtcars.cor,method="shade",shade.col=NA, tl.col = "black", tl.srt = 45, addCoef.col = "white", order ="AOE",type = "upper", diag = FALSE, addshade ="all")
names(mtcars)
?mtcars



#--------------------------REGRESIÓN LINEAL-----------------------------------
#LM ES LINEAL REGRESION
attach(EPG)
#variable independiente ~ variable dependiente
#variable1 depende de (~) variable2
regresion1=lm(peso~edad, data = EPG)
#x es la variable independiente que sería edad (aparece en los resultados)
#nuestro error estándar es .2103 que es un 21% de error ¿el cual se divide en 2 por el más/menos?
summary(regresion1)
plot(edad~peso, main="Edad vrs Peso", xlab ="Edad", ylab = "Grasas")
abline(lm(edad~peso))


regresion1=lm(grasas~edad, data = EPG)
summary(regresion1)
plot(grasas~edad, main="grasas vrs Edad", xlab ="Edad", ylab = "Grasas")
abline(lm(grasas~edad))

regresion1=lm(grasas~peso, data = EPG)
summary(regresion1)
plot(grasas~peso, main=" Peso vrs Grasas", xlab ="Pesos", ylab = "Grasas")
abline(lm(grasas~peso))