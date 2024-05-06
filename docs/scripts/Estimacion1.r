
##%######################################################%##
#                                                          #
####        Script Estimaci�n 1- El papel de la         ####
####            probabilidad - Introducci�n             ####
####            a la estad�stica inferencial            ####
#                                                          #
##%######################################################%##


# El teorema de tendencia central -----------------------------------------

# R.1 ---------------------------------------------------------------------

# #Pid�mosle a R que tome 10000 n�meros al azar (0-10) y que grafique con que frecuencia eligi� cada n�mero  que esperan ver?

numeros<-(runif(10000)*10)
numeros
hist(runif(10000)*10,main="")

# Tomemos 5 n�meros al azar del 0 al 10 y saquemos el promedio  de los cinco n�meros �cual creen que sea el promedio t�pico?  Hag�moslo

clase<-c((mean(,,,,)), (mean(,,,,)))
clase

#Ahora pid�mosle a R que lo haga 10000 veces (�10000 muestras de n�meros del 0 al 10!)

means<-numeric(10000)
for (i in 0:10000){ 
  means[i]<- mean(runif(5)*10)}
  
hist(means,ylim=c(0,1600))
 
#Noten que existe una tendencia central. Lo que significa que el mero hecho de muestrear genera una tendencia central en
#el valor promedio.

#C.1

# R.2 ---------------------------------------------------------------------

#El histograma de los n�meros se ve como una distribuci�n normal. �porque no dibujamos una funci�n normal sobre este histograma para ver que tan normal es? �Que necesitamos saber?

mean(means)
sd(means)

# y meterlo en la ecuaci�n f(y) = 1/(sqrt(2 pi) sigma) e^-((y - mu)^2/(2 sigma^2)) para cada valor de means verdad?  R lo hace por ustedes

# Primero generamos una serie de n�meros para el eje x que le permita a R saber cada cuando poner un puntito de la l�nea. Si queremos una l�nea suave una buena regla es 100 n�meros. Como queremos n�meros entre 0 y 10:

xv<-seq(0,10,0.1)
  
# Como la funci�n normal tiene un �rea bajo la curva de 1 y nosotros tenemos 10000 valores, necesitamos escalar la curva multiplic�ndola por  el n�mero de valores que se encuentran de cada lado de el valor de en medio (mediana).

yv<-dnorm(xv,mean=4.993275,sd=1.291482)*5000
lines(xv,yv)

# �el ajuste es perfecto! Lo interesante del teorema de tendencia central #es que no importa cual sea la distribuci�n real, si se toma una muestra de ella, se comportar� normalmente.

#C.2


# R.3 ---------------------------------------------------------------------

# El estandarizar la curva normal tiene muchas ventajas. Nos permite por ejemplo saber cual es el �rea hasta cualquier valor del eje X (llamados #desviaciones est�ndar).
#Pid�mosle a R que nos dibuje una Distribuci�n Normal Estandarizada. Recuerden que lo primero que hay que hacer cuando se dibujan l�neas

nd<-seq(-3,3,0.01)

#cuando no le damos la media y a la var, significa que queremos la estandarizada (el default), solo le dimos las x.

y<-dnorm(nd)
y
plot(nd,y,type="l")

#Ahora utilicemos pnorm para determinar que proporci�n de valores caen por debajo de dos desviaciones est�ndar.
 
pnorm(-2)

# que proporci�n? y ahora por debajo de 1 ds

pnorm(-1)

# �Que pasa si queremos saber que proporci�n de muestras caen A LA DERECHA de la desviaci�n 3?

# esta es una opci�n   

 1-pnorm(3)
 
 # �y la otra?
 
pnorm(-3)

#es claro que un valor de 3 cuando la media es 0 en una dn es muy poco probable (0.13% de las veces cuando se saca una muestra de la pob.)

# Ahora veamos un uso muy com�n de la distribuci�n de z. Este es preguntarse bajo una situaci�n aleatoria (solo por el hecho de muestrear una poblaci�n) entre que desviaciones est�ndar podemos encontrar el 95% de las muestras. (noten que al ser sim�trica, tenemos que alojar 2.5% de los casos"excedentes" de cada lado y para ello definimos un vector dentro de la funci�n qnorm (cuantiles normales)
 
qnorm(c(0.025,0.975))

# ahora dibuj�moslo

y<-dnorm(nd)

plot(nd,y,type="l")
  abline(v=-1.96)
  abline(v=+1.96)

#Esto es muy importante porque si encontramos que nuestro muestreo no se ajusta a estos valores esperados entonces posiblemente pertenezcan a dos poblaciones distintas. Este es el principio b�sico de todas las pruebas de significancia (p=0.05), el origen de el error est�ndar (1.96 desv. est�ndar), los l�mites de confianza etc.


# Tarea de hoy ------------------------------------------------------------

# Supongamos que tenemos una muestra de 100 colibr�es de una especie A. Les hemos medido la extensi�n de alas y encontramos que la media es 17 cm  y la desviaci�n est�ndar es de 0.8 cm. 
  
#1.Dibujen la distribuci�n de probabilidad bajo un supuesto de normalidad
  
#2.�que probabilidad hay de encontrar un caso con una extensi�n mayor a 18cm?
  
# Noten que cualquier valor de y en una distribuci�n normal se puede convertir en un valor de z. Recuerden que z=(y-media(y))/desv.stand. 
  
#3.�y la de encontrar un ave con una extensi�n menor a 15?
  
#4.�ustedes creen que un ave con una extensi�n de 15cm puede decirse que con una confianza del 95% pertenece a la misma poblaci�n?
  
#5. �entre que medidas de extensi�n de alas se puede decir con un 95% de confianza que las aves pertenecen a esa poblaci�n?


# �Fin! -------------------------------------------------------------------

  #�Fin!
 
