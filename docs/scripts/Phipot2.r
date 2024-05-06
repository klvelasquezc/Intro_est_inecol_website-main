##%######################################################%##
#                                                          #
####        Pruebas de hip�tesis dos poblaciones        ####
#                                                          #
##%######################################################%##


# R.3 ---------------------------------------------------------------------

# Volvamos al ejemplo de las concentraciones de ozono en los invernaderos y vamos a preguntarnos si el promedio de sus concentraciones de ozono es significativamente distinto

ozono<-read.table("gardens.txt",header=T)
attach(ozono)
names(ozono)

# Veamos un gr�fico

ozonoAB<-c(gardenA,gardenB)
ozonoAB
etiqueta<-factor(c(rep("A",10),rep("B",10)))
etiqueta
boxplot(ozonoAB~ etiqueta, notch=T,
        xlab="Invernadero",ylab="Ozono")

#Si usamos el ojimetro, parece ser que no sus medianas no son distintas porque los intervalos intercuartil no se sobrelapan. Ahora hagamos una prueba de t (para comparar las medias) a pie.

# �Que necesitamos?

# 1. los grados de libertad. dijimos que para una prueba de dos poblaciones calculamos el total de observaciones (20) menos el num. de par�metros estimados antes de realizar la prueba (dos medias). As� que tenemos 18 g.l.

#2. Necesitamos calcualar las varianzas individuales  de cada invernadero, para poder calcular la diferencia de EE.

s2A<-var(gardenA)
s2B<-var(gardenB)

# �Luego que sigue?

#3.Calculamos la t de student para la diferencia de medias

(mean(gardenA)-mean(gardenB))/sqrt(s2A/10+s2B/10)

# Noten que podemos ignorar el signo de la t. Porque solo depende que cual media pusimos primero. El valor absoluto es lo que importa.

# 4.Ahora necesitamos el valor cr�tico de la distribuci�n de t de referencia para determinar si aceptamos o rechazamos la hip�tesis de que no hay diferenicas entre medias (que vienen de la misma poblaci�n). Se trata de un problema de una o dos colas?


# Como no me interesa cual es mayor o menor, ni tengo ninguna hip�tesis de que invernadero deb�a tener mas o menos ozono, entonces es de dos colas, por lo tanto uso una probabilidad de?

qt(0.975,18)

# �Acepto o rechazo la hip�tesis de que son iguales?

# En virtud de que el valor calculado es mayor que el valor cr�tico se rechaza la hip�tesis nula.   (recuerden m�s alto=rechazo Ho m�s bajo=acepto)

# Finalmente necesito saber cual es la probabilidad de que encuentre yo la diferencia entre estas dos muestras (o una m�s extrema) a pesar de que provienen de poblaciones con la misma media.  Recuerden que es un problema de dos colas y por esa raz�n necesito calcular pt y despu�s multiplicarlos por dos (para los dos extremos).

p<-2*pt(-3.872983,18)
p

# para calcular los intervalos de confianza para la diferencia medias

difmed<-mean(gardenA)-mean(gardenB)
EEdifmed<-sqrt(s2A/10+s2B/10)
t.de.tablas.alfa.05.gl.18<-qt(0.975,18)
t.de.tablas.alfa.05.gl.18
IC95<-(EEdifmed)*(t.de.tablas.alfa.05.gl.18)
Cotasup<- difmed+IC95
Cotasup
Cotainf<- difmed-IC95 
Cotainf
IC95t<-(sqrt(s2A/10+s2B/10))*(qt(0.975,18))
  IC95t

# �Cual es la probabilidad de que suceda?

# Ahora el autom�tico

pruebat<-t.test(gardenA,gardenB)
pruebat

#Entonces reporto la concentraci�n de ozono fue significativamente m�s alta en el invernadero B (5.0 ppm) que en el A(3.0ppm; t=3.87, p=0.001(dos colas). gl.=18)

# Ahora, pudi�ramos tener el problema de que las varianzas no son iguales. entonces antes de hacer una prueba de t, necesitamos preguntarnos si las varianzas son significativamente distintas. Comparemos las de los invernaderos B y C.

var(gardenB)
var(gardenC)

# �se acuerdan que la distribuci�n de F era la adecuada para comparar varianzas? la pregunta es, �cual es la probabilidad de que estas dos muestras hayan sido sacada de poblaciones que tienen la misma varianza? El valor de F es simplemente el cociente de las varianzas

CocienteF<-var(gardenC)/var(gardenB)
CocienteF

# Ahora comparo con la distribuci�n de probabilidad de F para los grados de libertad correspondientes, y como no tengo ninguna idea de cual de las muestras debiera tener una varianza m�s alta, entonces es una prueba de dos colas.

# Como F no es sim�trica, necesito calcular ambos lados. 

qf(0.975,9,9)
qf(0.025,9,9)

# �Que concluyo?
# �Cual es la probabilidad de que estas dos muestras provengan de poblaciones con la misma varianza?

2*(1-pf(CocienteF,9,9))

# Ahora el autom�tico

var.test(gardenB,gardenC)

# Ahora, si encontr�ramos que suponer que las poblaciones de las que provienen las muestras no se distribuyen de manera normal entonces tenemos la alternativa de la prueba de Wilcoxon de suma de rangos. Esta es id�ntica en su sistema a la de los rangos signados. Veamos como

# Lo primero que hago es poner todas las observaciones en un mismo vector

ozone<-c(gardenA,gardenB)
ozone

#Ahora les hago sus etiquetas para que no se me pierdan

etiqueta<-c(rep("A",10),rep("B",10))
etiqueta

#Ahora hago un vector de los rangos con la funci�n rank

rangoscomb<-rank(ozone)
rangoscomb

#Noten que para todos los valores repetidos se hace un promedio de los rangos que les tocan.

tapply(rangoscomb,etiqueta,sum)

# Finalmente uso el valor m�s peque�o (66) para compararlo con el valor de tablas para el etad�stico W para n de 10 y 10 y un alfa del 5% (W=78). Como nuestro valor es *****menor*****(porque uso como referencia el mas peque�o del par que obtuve), entonces rechazo mi H0. Las Medias son significativamente distintas.

# ahora el autom�tico.

wilcox.test(gardenA,gardenB)

# Las diferencias se deben a que R utiliza un algoritmo de aproximaci�n para calcular valores de z. Es ligeramente distinto del tradicional que hicimos arriba. La mec�nica es la misma. Noten la diferencia en las p de t y W.
# Wilcoxon es menos poderoso (95%). Pero es el correcto si las distribuciones son sesgadas. Tambi�n es correcto si no lo son (aunque poco menos poderoso). t no es correcta si hay desviaciones sustanciales de la normalidad.

# Fin