##%######################################################%##
#                                                          #
####                Script Estimaci�n 2                 ####
####     Introducci�n a la estad�stica inferencial      ####
#                                                          #
##%######################################################%##


# R.1 ---------------------------------------------------------------------
# Estimaci�n de par�metros de tendencia central ---------------------------

# Sabemos que aunque los datos no se agrupen alrededor de un valor t�pico si hacemos muestras consecutivas los estad�sticos de estas van a tener una tendencia central.

# Veamos un cuerpo de datos. Una variable y.

yvals <- read.table("yvalues.txt",header=T)
attach(yvals)
yvals

#Una manera muy simple de medir la tendencia central es ver cual es el valor m�s frecuente. Este se denomina MODA

yord<-sort(y)
yord

windows()
hist(y)

# �cual es la clase modal aqu�?

# Pero ahora queremos saber la media aritm�tica (el promedio) que es la suma de todos los valores dividido por n. �Que hago?

total<-sum(y)
sum(y)

# pero ahora necesito saber cuantos valores son

n<-length(y)
n
media<- total/n
media

# pero ahora quiero tener una funci�n que me sirva para siempre

media.aritmetica <- function(x) {
	sum(x)/length(x) }

#ya est�, ahora prob�mosla

data<-c(3,4,6,7)
media.aritmetica(data)

media.aritmetica(y)

#�Que bien! �R es fant�stico! puedo calcular la media siempre que me plazca.

# En realidad la mayor parte de las funciones estad�sticas est�n ya construidas en R. Por supuesto que la media es una de ellas. Solo quer�a mostrarles que no hay nada obscuro detr�s de los objetos ya creados en R

mean(y)

# La media como medida de tendencia central tiene el serio problema de que es muy sensible a valores at�picos. vean lo siguiente.

dataat<-c(data,100)
dataat
mean(dataat)

#comparado con

mean(data)

# Una alternativa es la mediana, que es el valor de en medio, una vez que todos los valores han sido ordenados. Veamos dataat

dataat

# �Cual es la mediana?

median(dataat)

# es mucho mejor estimaci�n de el centro que 24.

#y de data?

data

median(data)

# y para y?

median(y)
mean(y)

# Se parecen mucho porque no hay valores at�picos y porque la distribuci�n es sim�trica.

#Ahora pensemos en fen�menos que cambian multiplicativamente. �Conocen alguno?

# Uno de los m�s comunes en ecolog�a es el crecimiento poblacional y por lo tanto la dispersi�n de organismos de una poblaci�n. En dichos casos la media aritm�tica y/o la mediana suelen ser p�simos estimadores de la tendencia central. Veamos un ejemplo.

#El n�mero de insectos en una serie de plantas vecinas es

insectos<-c(1,10,1000,10,1)

# �Cual es la mejor estimaci�n de tendencia central?

windows()
hist(insectos)

mean(insectos)
median(insectos)

# C.1


# R.2 ---------------------------------------------------------------------

#Lo que se usa es la media geom�trica que si se acuerdan hay dos maneras de calcularla. Para el ejemplo de los insectos �cual es la mas simple?

100000^0.2

# �y la otra?

exp(mean(log(insectos)))

detach(yvals)
rm(insectos)
ls()

#C.2

# R.3 ---------------------------------------------------------------------

# Medidas de dispersi�n

# Veamos un cuerpo de datos cualquiera y pregunt�monos c�mo podemos medir su dispersi�n.

y<-c(13,7,5,12,9,15,6,11,9,7,12)

# veamos c�mo se ve

windows()
plot(y,ylim=c(0,20))

#Lo m�s f�cil es decir de donde a donde va (el intervalo).

range(y)

# Pero esto tiene sus problemas.

# 1. No tiene relaci�n con el par�metro poblaci�n de intervalo
# 2.incrementa con la n.
# 3.Adem�s es muy susceptible a valores at�picos
# 4. no considera a todos lo valores.

# Otra medida de dispersi�n muy importante es la varianza. Que est� fundamentada en las desviaciones (o residuales) de cada valor con la media

y<-c(13,7,5,12,9,15,6,11,9,7,12)
abline(mean(y),0)
for (i in 1:11) lines(c(i,i),c(y[i],mean(y)))

# se usa la suma de cuadrados de la diferencia de cada valor con la media general como base. �C�mo lo calculamos?

y-mean(y)
(y-mean(y))^2
sum((y-mean(y))^2)

# Fant�sitico, pero que sucede a SC cada vez que yo adiciono una nueva observaci�n. �Que tenemos que hacer?

# Si divido entre n, se llama la desviaci�n media de los cuadrados.

#C.3

# R.4 ---------------------------------------------------------------------

variance <- function (x)   sum((x-mean(x))^2)/(length(x)-1)
variance(y)

# Pero claro, ya est� definido.

var(y)

#La relaci�n entre la varianza de la muestra y el tama�o de muestra (n)

windows()
plot(c(0,32),c(0,15),type="n",xlab="Tama�o de muestra",ylab="Varianza")

# Lo que vamos a hacer es seleccionar aleatoriamente n�meros de una poblaci�n que tiene una distribuci�n normal (media 10 y var 4). Esto lo vamos a hacer repetidas veces pero nuestra muestra va a ir incrementando su n desde 3 hasta 31. Vamos a sacar 30 muestras de cada tama�o de muestra. Es decir 30 muestras de 3 n�meros, 30 muestras de 5 n�meros etc. A cada muestra le vamos a calcular su varianza y las vamos a graficar.

for (tm in seq(3,31,2)) {
for( i in 1:30){
x<-rnorm(tm,mean=10,sd=2)
points(tm,var(x)) }}

# Ahora pueden ver que la varianza poblacional puede estar muy mal estimada con tama�os de muestra peque�os. Y a medida que aumentamos el tama�o de muestra la probabilidad de que la estimaci�n est� muy lejos del par�metro disminuye. Esta es una raz�n m�s para elegir muy cuidadosamente el tama�o de muestra. En unas clases vamos a hablar de esto en el contexto de pruebas de hip�tesis.

#C.4

# R.5 ---------------------------------------------------------------------

#Hagamos el mismo proceso que hicimos antes para elegir muestras con tama�os de muestra que incrementan pero ahora veamos que sucede con nuestra medida de desconfianza de la estimaci�n a medida que aumenta n

windows()
plot(c(0,32),c(0,2),type="n",xlab="Tama�o de muestra",ylab="Error estandar")
 for (tm in seq(3,31,2)) {
for( i in 1:30){
x<-rnorm(tm,mean=10,sd=2)
points(tm,sqrt(var(x)/tm)) }}

# Veamos un ejemplo de la concentraci�n de ozono en unos invernaderos

ozono<- read.table("gardens.txt",header=T)
attach(ozono)
ozono

windows()
par(mfrow=c(1,3))
plot (gardenA)
plot (gardenB)
plot (gardenC)

MediaA<- mean(gardenA)
EEA<- sqrt(var(gardenA)/10)
MediaB<- mean(gardenB)
EEB<- sqrt(var(gardenB)/10)
MediaC<- mean(gardenC)
EEC<- sqrt(var(gardenC)/10)

MediaA
EEA
MediaB
EEB
MediaC
EEC

# �Para cual invernadero puedo yo confiar m�s de la estimaci�n de la media?

# C.5

# R.6 ---------------------------------------------------------------------

# Vamos a calcular los intervalos de confianza para la media de los invernaderos usando la distribuci�n de t. qt nos d? el valor de t para el cual hay cierta proporci�n a la izquierda.

qt(.975,9)

#calculemos los intervalos de confianza al 95% para el invernadero B

qt(.975,9)*sqrt(1.3333/10)

# el reporte diario, la concentraci�n promedio de ozono en el invernadero B fue de 5.0?0.826(I.C.95%, n=10)

windows()
plot(gardenB)
abline(mean(gardenB),0)
abline((mean(gardenB)+0.826),0, lty=2)
abline((mean(gardenB)-0.826),0, lty=2)

# los dejo que ustedes calculen aquellos de los invernaderos A y C.

#C.6

# Fin ---------------------------------------------------------------------

