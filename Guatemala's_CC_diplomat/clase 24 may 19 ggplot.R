library(datasets)
data("women")

ggplot(data=women) + geom_point(mapping = aes(x = weight, y = height))

?datasets
library(help = "datasets")


#mbase de datos mpg

data(mpg)
mpg
?mpg
#graficamos el tamaño del motor vrs el desplazamiento en carretera

ggplot(data=mpg)+ geom_point(mapping = aes(x= displ, y = hwy))

ggplot(data=mpg)+ geom_point(mapping = aes(x= hwy, y = cyl))

ggplot(data=mpg)+ geom_point(mapping = aes(x= class, y = hwy),

#complementos de ggplot (asteríscos)
#el color será la clase de carro (la leyenda)
ggplot(data=mpg)+ geom_point(mapping = aes(x= displ, y = hwy, color = class)),
#size, tamaño del punto
ggplot(data=mpg)+ geom_point(mapping = aes(x= displ, y = hwy, color = class, size = class)),
#alpha: transparencia del punto
ggplot(data=mpg)+ geom_point(mapping = aes(x= displ, y = hwy, color = class, size = class, alpha=class)),
#shape, para diferentes figuras por categorìa. NOTA solo son 6 figuras
ggplot(data=mpg)+ geom_point(mapping = aes(x= displ, y = hwy, color = class, shape=class)),
#filtro    
ggplot(data=mpg)+ geom_point(mapping = aes(x= displ, y = hwy, color = displ <6)),

#facetas: 
#las facetas hacen una pequeña gráfica por la clase de carro y que estas se ordenen en 2 filas
ggplot(data=mpg)+ geom_point(mapping = aes(x= displ, y = hwy))+facet_wrap(~class,nrow =2),
#faceta con face grid
ggplot(data=mpg)+ geom_point(mapping = aes(x= displ, y = hwy))+facet_wrap(~class,nrow =2)+facet_grid(drv ~cyl),
#con facet grid con punto
ggplot(data=mpg)+ geom_point(mapping = aes(x= displ, y = hwy))+facet_wrap(~class,nrow =2)+facet_grid(.~cyl),

#OBJETOS GEOMETRICOS
#Aquí podemos ver el alto rendimiento que conduce los carros de 2 asientos. La sombre es el error estadístico
ggplot(data=mpg)+ geom_smooth(mapping = aes(x= displ, y = hwy))
ggplot(data=mpg)+ geom_smooth(mapping = aes(x= displ, y = hwy, linetype=drv)),
ggplot(data=mpg)+ geom_smooth(mapping = aes(x= displ, y = hwy, color = drv))+geom_point(mapping = aes(x= displ, y = hwy, color = drv))

#agrupar las gráficas
ggplot(data=mpg)+ geom_smooth(mapping = aes(x= displ, y = hwy, group = drv))

#combinar tipos de graficas
ggplot(data=mpg)+ geom_smooth(mapping = aes(x= displ, y = hwy)) +geom_point(mapping = aes(x= displ, y = hwy))
#también puede ser descrito así
ggplot(data=mpg, mapping = aes(x= displ, y = hwy)) +geom_point()+geom_smooth()
#grafica smooth pero sin la franja de error
ggplot(data=mpg, mapping = aes(x= displ, y = hwy)) +geom_point()+geom_smooth(se = FALSE)
ggplot(data=mpg, mapping = aes(x= displ, y = hwy, group = drv, color = drv )) +geom_point()+geom_smooth(se = FALSE)

ggplot(data=mpg, mapping = aes(x= displ, y = hwy)) +geom_point()+geom_smooth()

#BOXPLOT
ggplot(data=mpg)+ geom_boxplot(mapping = aes(x= class, y = hwy))

###TRANSFORMACIONES ESTADÍSTICAS###
?diamonds,
data(diamonds),
ggplot(data=diamonds)+geom_bar(mapping = aes(x = cut)),
ggplot(data=diamonds)+stat_summary(mapping = aes(x = cut, y =depth)),fun.ymin=min,fun.ymax=max
#agregándole proporcion
ggplot(data=diamonds)+geom_bar(mapping = aes(x = cut, y=..prop..)),
ggplot(data=diamonds)+geom_bar(mapping = aes(x = cut, color, y=..prop..))
#3 variables y la tercera (claridad) distribuida por color
ggplot(data=diamonds)+geom_bar(mapping = aes(x = cut, fill=clarity))


                                   