##%######################################################%##
#                                                          #
####        Pruebas de hip�tesis dos poblaciones bis    ####
#                                                          #
##%######################################################%##


# R.3 ---------------------------------------------------------------------

# Entonces �cual es la probabilidad de que mi muestra de frecuencias (o una mas extrema) pertenezca a una poblaci�n donde el aislamiento de la planta y el plumaje del colibr� defensor son independientes?

1-pchisq(35.34,1)

# �Cual es el valor cr�tico para rechazar la H0 con un alfa de 0.05?

qchisq(0.95,1)

# Ahora vamos a hacerlo de manera autom�tica en R

colibries<-matrix(c(38,14,11,51),nrow=2)
colibries
chisq.test(colibries)

# Noten que estos valores son un poco distintos porque se aplic� una correcci�n de Yates. Esta fue dise�ada para frecuencias peque�as, pero ahora existen pruebas mejores para frecuencias peque�as (20% o m�s frecuencias menores a 5) como La Prueba exacta de Fisher Crawley p. 90 QyK p.388.

#Entonces le quito la correcci�n

chisq.test(colibries,correct=F)

# me da exactamente lo que calculamos a mano.

#C.3

#*****Aqu� va la segunda parte de su tarea*****
# El problema se trata de un investigador que esta interesado en si las hormigas construyen preferentemente sus nidos en �rboles de una de dos especies. Entonces muestre� 100 �rboles de cada una de las  especies. Encontr� que 60 �rboles de la especie A  y 20 de la especie B ten�an nidos.  �Esta muestra apoya la hip�tesis de preferencia?


