##%######################################################%##
#                                                          #
####                       ANDEVA                       ####
#                                                          #
##%######################################################%##

#R.1----------------------------------------------------

#  Conceptos b�sicos del an�lisis de la varianza

# La hip�tesis nula en un an�lisis de la varianza tipo I com�n es:

#         H0: m1 = m2 = m3 = ... = mk

# �C�mo es que esta hip�tesis se pone a prueba en un ANDEVA?
# Por cierto esta es una prueba "omnibus", es decir �prueba muchas cosas de un jal�n!

# Para ver como es que opera el anova veamos el ejemplo que sigue: Tomemos un solo factor, "f", con dos niveles y pongamos los datos en una gr�fica simple, seg�n el orden en el que fueron obtenidas las mediciones.

anova<-read.table("anova.data.txt",header=T)
attach(anova)
names(anova)
plot(y)
abline(mean(y), 0, col=4)
for (i in 1:length(y)) lines (c(i,i), c(mean(y), y[i]), col=5)

# �Qu� muestra esta gr�fica? 
# �a que equivale la suma de los trazos verticales?
 
#                     SCT=Sigma(y-ymedia)^2    

# Ahora vamos a hacer exactamente lo mismo, pero ahora dividiendo por cada uno de los niveles del factor F. Incorporemos la informaci�n del factor "f".  Para esto hay que calcular los promedios de "y" que corresponden a los niveles de "f"

promedios <- tapply(y, f, mean)

# Grafiquemos esta nueva estructura de datos sobre la  gr�fica que ya tenemos

plot(y)
lines(c(1, 7), c(promedios[1], promedios[1]), col = 2)
lines(c(7, 14), c(promedios[2], promedios[2]), col = 5)
for (i in 1:7 ) lines (c(i,i), c(promedios[1], y[i]), col = 1, lty=6)
for (i in 8:14) lines (c(i,i), c(promedios[2], y[i]), col = 1, lty=6)
  

# �Qu� muestra esta gr�fica? �a que equivale la suma de los trazos punteados verticales?

#         SCE=Sigma(y1-y1gorrito)^2 + Sigma(y2-y2gorrito)^2


# Si las dos medias fueran iguales �c�mo comparar�an estas dos gr�ficas?

# Si lo piensan tendr�an que ser iguales porque las medias de los niveles del tratamiento se nivelar�an a la misma altura. Si las medias son  significativamente distintas �cual varianza ser�a mayor? la calculada con SCT o la calculada con SCE? Esta es la raz�n por la cual el ANDEVA compara medias a trav�s de la comparaci�n de varianzas!!!!

# �Qu� interpretaci�n tiene la diferencia entre las dos sumas mencionadas arriba?
# Pues es precisamente la varianza explicada por el modelo. Esta diferencia se asocia con la siguiente gr�fica:

modelo <- lm(y~f)
plot (y)
abline (mean(y), 0, col = 4)
points(predict(modelo), pch = 16, col = 5)
for (i in 1:14) lines(c(i, i), c(mean(y), predict(modelo)[i]), col = 6)

# C.1


# R.2 ---------------------------------------------------------------------

# �Que implica el ajuste del modelo ANOVA del factor "f"?

SCT<-sum((y-mean(y))^2)
SCT

# La pregunta es cuanto de esta variaci�n es explicada por diferencias entre las medias de A y B (niveles del factor F) y cuanto por el error

SCEa<-sum((y[f=="a"]-mean(y[f=="a"]))^2)
SCEb<-sum((y[f=="b"]-mean(y[f=="b"]))^2)

#Entonces la SCE es la suma de estas dos cantidades 

 SCE<-SCEa+SCEb
 SCE

 #Finalmente la SCA es SCT-SCE
 
 SCA<-SCT-SCE
 SCA
 
#Entonces ya podemos llenar la tabla de ANOVA
 
#C.2


# R.3 ---------------------------------------------------------------------

# Ahora calculemos la F

31.5/2
 
#y la p
 
1-pf(15.75,1,12)

#Ahora el automatico
 
modelo<-aov(y~f)

summary(modelo)
boxplot(y~f,xlab="factor F",ylab="y")

# �Cual es la conclusi�n?
# Ahora hacemos la cr�tica del modelo

windows()
par(mfrow=c(2,2))
plot(modelo)

# y ahora lo ultimo

A<-c(6,8,5,9,7,8,6)
B<-c(9,11,8,12,10,11,9)
t.test(A,B)
summary(modelo)

# La anova es una "Generalizaci�n" de t para poder comparar mas de dos medias. En realidad F=t^2

# Fin

