
#elobjetivo  del ejercicio es el de estimar la cantidad de bicis a rentar

#dimension de la data filas*columnas
dim(dia)
names(dia)
#si hubiera datos nulos, es necesario seguir la polìtica de la empresa para rellenar estos datos
is.null(dia)
#
is.integer(dia)
#crear un data frame
dia<- data.frame(dia)
#ver su estructura
str(dia)

#contabilizar los registros por estación
View(dia)

#%A es una abreviaciòn para la variable dia 

dia$season <- factor(format(dia$season, format="%A"), levels = c("1", "2", "3", "4"), labels = c("Springs", "Summer", "Fall","Winter"))
table(dia$season)

#contabilizar los registros por día de trabajo

#vemos que hasta el momento se rentan más en otoño pero el porcentaje es mayor para los trabajadores

dia$holiday <- factor(format(dia$holiday, format="%A"), levels = c("0","1"), labels = c("Working day","Holiday"))
table(dia$holiday)

#contabilizar los registros por el weathersit 

dia$weathersit <- factor(format(dia$weathersit, format="%A"), levels = c("1", "2", "3", "4"), 
                         labels = c("Good:Clear/Sunny","Moderate:Cloudy/Mist", "Bad:Rain/Snow/Fog", "Worse: Heavyrain/Snow/Fog"))
table(dia$weathersit)

#Contabilizar los registros por año

dia$yr <- factor(format(dia$yr), levels = c("0", "1"), labels = c("2011", "2012"))
table(dia$yr)

#--------------visualizar el data frame---------------
str(dia)

summary(dia)

#Agregar nuevas columnas a la data
names(dia)
dia$actual_temp<-dia$temp*41
dia$actual_feel_temp <- dia$atemp*50
dia$actual_windspeed <-dia$windspeed*67
dia$actual_hum <-dia$hum*100
dia$mean_acttemp_feeltemp<-(dia$actual_temp+dia$actual_feel_temp)/2
names(dia)

#----------PLOTS-------------

#breaks es la divisiòn de los grupos de valores (intervalos)
#frecuencia de renta es la cantidad de veces que se renta una bici al dìa
h <- hist(dia$cnt, breaks = 30, ylab ="Frecuencia de renta", xlab = "Total de bicicletas rentadas", main ="Distribuciòn de la Renta Total de Bicicletas", col = "blue")

#elocuencia de la gráfica

hi <- ggplot(data=dia)+geom_histogram(mapping = aes(x=cnt), binwidth = 1000, color = "white")
hi
#cambiar los nombres de los ejes yel título de la gráfica

hi+xlab ("cantidad de Bicicletas Rentadas")+ylab("frecuencia de Renta")+ggtitle("histograma de Venta de Bicicletas")

#Calcular la media

mean(dia[["cnt"]])

#-----------AJUSTAR VALORES GRÀFICA----------

#agregar la curva de normailidad basicamente 

h <- hist(dia$cnt, breaks = 30, ylab ="Frecuencia de renta", xlab = "Total de bicicletas rentadas", main ="Distribuciòn de la Renta Total de Bicicletas", col = "blue")
xfit<-seq(min(dia$cnt),max(dia$cnt), length=50)
yfit <- dnorm(xfit,mean =mean(dia$cnt), sd=sd(dia$cnt))
yfit<-yfit*diff(h$mids[1:2])*length(dia$cnt)
lines(xfit,yfit,col="red",lwd=3)


#gràfica por estaciòn Y -----USANDO BOXPLOT---------

#boxplot de temporada 
#con esta gráfica veo que a pesar de que habìa visto que el conteo de winter no era bueno,
#realemente vendo menos en spring
#Ya que en summer y fall vendo más  les hago mantenimiento a a las bicis en spring y winter
S=ggplot(data=dia)+geom_boxplot(mapping=aes(x=season, y = cnt))

S+xlab("Estación")+ylab("Volumen de renta")+ggtitle("Renta de Bicicletaspor Estación")

#Gráfica para tipo de día

D=ggplot(data=dia)+geom_boxplot(mapping=aes(x=holiday, y = cnt))

D+xlab("Dia")+ylab("Volumen de renta")+ggtitle("Renta de Bicicletas por tipo de dia")


#Graficar por estado del tiempo

ws=ggplot(data=dia)+geom_boxplot(mapping=aes(x=weathersit, y = cnt))

ws+xlab("Estado del tiempo")+ylab("Volumen de renta")+ggtitle("Renta de Bicicletas por estado del tiempo")

#Graficar por el año 

wa=ggplot(data=dia)+geom_boxplot(mapping=aes(x=yr, y = cnt))

wa+xlab("año")+ylab("Volumen de renta")+ggtitle("Renta de Bicicletas por año")

#GRÁFICA POR MES

boxplot(dia$cnt~dia$mnth, data=dia, main="Renta total Mensual", xlab="Mes", 
        ylab="Total de bicicletas Rentadas",col=c("yellow"))

#Gràfica por dia

boxplot(dia$cnt~dia$weekday,data=dia,main="Renta Total Diaria", 
        xlab="Dia",ylab = "Total de Bicicletas Rentadas", col=c("red"))


#----------Gráficas de variables continuas-------------

#por temperatura

ta <- ggplot(dia)+geom_line(mapping=aes(x=actual_temp,y=cnt))

ta+xlab("Temperatura grados centigrados")+ylab("Volumen de renta")+ggtitle("Renta de bicicletas segun temperatura")

names(dia)
#Por sensación térmica

st <- ggplot(dia)+geom_line(mapping=aes(x=actual_feel_temp,y=cnt))

st+xlab("Temperatura grados centigrados")+ylab("Volumen de renta")+ggtitle("Renta de bicicletas según sensación térmica")

#Según velocidad del viento

vv <-ggplot(dia)+geom_line(mapping=aes(x=actual_windspeed,y=cnt), col="red")

vv+xlab("Velocidad del viento")+ylab("Volumen de renta")+ggtitle("Renta de bicicletas según velocidad del viento")

#Según la humedad

hh <-ggplot(dia)+geom_line(mapping=aes(x=actual_hum,y=cnt),col = dia$season)

hh+xlab("Humedad")+ylab("Volumen de renta")+ggtitle("Renta de bicicletas según humedad")

#según la humedad pero con smooth

hh1 <-ggplot(dia, mapping=aes(x=actual_hum,y=cnt))+geom_point(col=("Red"))+geom_smooth(col=("green"))

hh1+xlab("Humedad")+ylab("Volumen de renta")+ggtitle("Renta de bicicletas según humedad")

#---------------CORRELACIONES--------------------------

#Entre temperatura (actual_temp) y volumen de rentas (cnt)

diara <- lm(cnt~actual_temp, data=dia)
summary(diara)
plot(dia$cnt~dia$actual_temp, main="renta de bicicletas", xlab="Temperatura", ylab="Volumen de Rentas", col= dia$season)
abline(lm(cnt~actual_temp, data=dia))


#modelo de predicciòn
rentas <- 1214.642+161.969*25
rentas


#Entre sensación térmica y volumen de rentas

diarb <-lm(cnt~actual_feel_temp, data=dia)
summary(diarb)
plot(dia$cnt~dia$actual_feel_temp, main="renta de bicicletas", xlab="Sensa Térmica", ylab="Volumen de Rentas", col ="blue")
abline(lm(cnt~actual_feel_temp, data=dia))

#modelo de predicciòn
rentas <- 945.824+150.037*27
rentas


#Entre velocidad del viento y volumen de ventas

diarc <-lm(cnt~actual_windspeed, data=dia)
summary(diarc)
plot(dia$cnt~dia$actual_windspeed, main="renta de bicicletas", xlab="velocidad viento", ylab="Volumen de Rentas", col ="blue")
abline(lm(cnt~actual_windspeed, data=dia))

5621.2-87.51*25


#Entre humedad y volumen de ventas

diard <-lm(cnt~actual_hum, data=dia)
summary(diard)
plot(dia$cnt~dia$actual_hum, main="renta de bicicletas", xlab="humedad", ylab="Volumen de Rentas", col ="blue")
abline(lm(cnt~actual_hum, data=dia))

5363.986-13.691*25
5363.986-13.691*30
5363.986-13.691*10


#--------------CORRELACION LINEAL--------------------

dialm<-lm(formula=cnt~actual_temp+actual_feel_temp+actual_windspeed+actual_hum, data=dia)
summary(dialm)

#Queremos llegar a 5000 que es nuestra media 

3860.37+51.51*20+102.78*32-67.59*10-31.49*80

#ya veremos si este valo calculado bajoestas condiciones, nos da lo mismo

3860.37+51.51*25+102.78*25-67.59*25-31.49*25

3860.37+51.51*30+102.78*30-67.59*30-31.49*30