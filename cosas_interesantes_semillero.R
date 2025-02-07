mu <- c(3, -1, 5)  # mu contiene un valor negativo
if (any(mu <= 0)) stop("Parameter mu has to be positive!")
# Error: Parameter mu has to be positive!

#notese que stop es una función para dar a conocer que hay
#una restricción y lo que hace es que genera un error con 
#ese y aparece ese mensaje, por otro lado, el any lo que
#hace es que evalúa por cada valor que puede tomar ese 
#vector, es decir, mu puede ser un vector, entonces
#si alguna de sus entradas es negativa o cero, aparecera
#este mensaje

#para la función de distribución acumulada hay que poner el 
#parámetro log.p pues se usa en funciones de probabilidad en 
#caso que deban devolver su forma logarítmica y la idea
#en que en caso que sea true, la función devuelva el logaritmo
#de su probabilidad

#Sacar números aleatorios
#Tiene sentido, la función de distribución acumulada
#calcula la probabilidad de que una Variable aleatoria este
#a la derecha o a la izquierda de un número que pertenece a 
#la distribución, entonces, si se toma un número aleatorio entre
# 0 y 1, ya tenemos ese valor de la distribución, y con esa probabilidad
#se puede hallar ese número dentro de la distribución que genera
#esa región a la derecha o a la izquierda, lo importante 
#es generar números aleatorios entre 0 y 1 y me dará los cuantiares aleatorios
#que son los valores que hacen que se tenga una probabilidad a la 
#derecha o a la izquierda


