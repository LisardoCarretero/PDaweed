#Equipo P(Daweed)
#install.packages("e1071")
library(e1071)
setwd("F:/Users/Scaar/Desktop/Informatica/MIO/1º Ingenieria/ESTADISTICA/PRACTICA 1")
datos <- read.csv("Notas.csv", sep=";", dec=",", header=T)
grupo <- datos$ï..Grupo

datos$NOTA.FINAL.SEPTIEMBRE..0.10. <- NULL
datos$EXAMEN.SEPTIEMBRE..0.7.<-NULL

for(i in c(2:4)){(datos[,i])*3*5}
datos$TOTAL.prÃ.cticas..0.2.<-  datos$TOTAL.prÃ.cticas..0.2.*5
datos$Total.cuestionarios..0.1. <- datos$Total.cuestionarios..0.1.*10
datos$EXAMEN.JUNIO..0.7.<- datos$EXAMEN.JUNIO..0.7.*(10.0/7.0)
datos$NOTA.FINAL.JUNIO...0.10.<- datos$NOTA.FINAL.JUNIO...0.10.
datos$PrÃ.ctica.1..0.2.3. <-  datos$PrÃ.ctica.1..0.2.3.*(10/(2/3))
datos$PrÃ.ctica.2...0.2.3. <- datos$PrÃ.ctica.2...0.2.3.*(10/(2/3))
datos$PrÃ.ctica.3...0.2.3. <- datos$PrÃ.ctica.3...0.2.3.*(10/(2/3))
datos


#Objetivo 1 
#Ejercicio 1
#Conversion de variables cuantitativas en variables cualitativas 
for(i in c(2:11)){datos[is.na(datos[,i]),i]<-0}
for(i in c(2:11)){
  datos[,i] <- cut(datos[,i], breaks = c(0, 5, 10), labels = c("Suspenso", "Aprobado"))}
  datos[,i]

#Ejercicio 2
#Naive Bayes
#notas finales
m <- naiveBayes(NOTA.FINAL.JUNIO...0.10. ~. , data = datos)
m
predictions <- predict(m, datos)
table(predictions, datos$NOTA.FINAL.JUNIO...0.10.)
table(datos$NOTA.FINAL.JUNIO...0.10.)

#Ejercicio 3

n <- naiveBayes(NOTA.FINAL.JUNIO...0.10. ~ datos$ï..Grupo, data = datos)
n
predictions <- predict(n, datos)
table(predictions, datos$NOTA.FINAL.JUNIO...0.10.)


o <- naiveBayes(NOTA.FINAL.JUNIO...0.10. ~ Cuestionario.temas.1.y.2..0.10. + PrÃ.ctica.1..0.2.3. , data = datos)
o
predictions <- predict(o, datos)
table(predictions, datos$NOTA.FINAL.JUNIO...0.10.)


p <- naiveBayes(NOTA.FINAL.JUNIO...0.10. ~ Cuestionario.temas.1.y.2..0.10.+ Cuestionario.tipo.test.temas.3.y.4...0.10.+datos$PrÃ.ctica.1..0.2.3.+datos$PrÃ.ctica.2...0.2.3., data = datos)
p
predictions <- predict(p, datos)
table(predictions, datos$NOTA.FINAL.JUNIO...0.10.)


m <- naiveBayes(NOTA.FINAL.JUNIO...0.10. ~ datos$ï..Grupo+datos$TOTAL.prÃ.cticas..0.2.+datos$Total.cuestionarios..0.1., data = datos)
m
predictions <- predict(m, datos)
table(predictions, datos$NOTA.FINAL.JUNIO...0.10.)

#Objetivo 2

datos <- read.csv("Notas1.csv", sep=";", dec=",", header=T)
datos$NOTA.FINAL.SEPTIEMBRE..0.10. <- NULL
datos$EXAMEN.SEPTIEMBRE..0.7.<-NULL

for(i in c(2:4)){(datos[,i])*3*5}
datos$TOTAL.prÃ.cticas..0.2.<-  datos$TOTAL.prÃ.cticas..0.2.*5
datos$Total.cuestionarios..0.1. <- datos$Total.cuestionarios..0.1.*10
datos$EXAMEN.JUNIO..0.7.<- datos$EXAMEN.JUNIO..0.7.*(10.0/7.0)
datos$NOTA.FINAL.JUNIO...0.10.<- datos$NOTA.FINAL.JUNIO...0.10.
datos$PrÃ.ctica.1..0.2.3. <-  datos$PrÃ.ctica.1..0.2.3.*(10/(2/3))
datos$PrÃ.ctica.2...0.2.3. <- datos$PrÃ.ctica.2...0.2.3.*(10/(2/3))
datos$PrÃ.ctica.3...0.2.3. <- datos$PrÃ.ctica.3...0.2.3.*(10/(2/3))
datos

#Ejercicio 1
#Consideramos los no presentados de forma explicita
for(i in c(2:11)){datos[is.na(datos[,i]),i]<- -1}
for(i in c(2:11)){datos[,i] <- cut(datos[,i],breaks = c(-1, 0, 5, 10), labels = c("No presentado","Suspenso", "Aprobado"))}
datos$TOTAL.prÃ.cticas..0.2.


#Ejercicio 2
datos <- read.csv("Notas.csv", sep=";", dec=",", header=T)
datos$NOTA.FINAL.SEPTIEMBRE..0.10. <- NULL
datos$EXAMEN.SEPTIEMBRE..0.7.<-NULL
for(i in c(2:4)){(datos[,i])*3*5}
datos$TOTAL.prÃ.cticas..0.2.<-  datos$TOTAL.prÃ.cticas..0.2.*5
datos$Total.cuestionarios..0.1. <- datos$Total.cuestionarios..0.1.*10
datos$EXAMEN.JUNIO..0.7.<- datos$EXAMEN.JUNIO..0.7.*(10.0/7.0)
datos$NOTA.FINAL.JUNIO...0.10.<- datos$NOTA.FINAL.JUNIO...0.10.
datos$PrÃ.ctica.1..0.2.3. <-  datos$PrÃ.ctica.1..0.2.3.*(10/(2/3))
datos$PrÃ.ctica.2...0.2.3. <- datos$PrÃ.ctica.2...0.2.3.*(10/(2/3))
datos$PrÃ.ctica.3...0.2.3. <- datos$PrÃ.ctica.3...0.2.3.*(10/(2/3))
datos
for(i in c(2:11)){datos[,i] <- cut(datos[,i],breaks = c(0, 4.99, 6.99, 8.99, 11.0), 
labels = c("Suspenso", "Aprobado", "Notable", "Sobresaliente"))}
datos[,i]

#Ejercicio 3
b <- naiveBayes(NOTA.FINAL.JUNIO...0.10. ~ ., data = datos)
b


#Ejercicio 4

tabla1 <- naiveBayes(datos$NOTA.FINAL.JUNIO...0.10. ~. , data = datos[,c(1:9,11)])
tabla1
table(predict(tabla1, datos[,c(1:9,11)]), datos$NOTA.FINAL.JUNIO...0.10.)


#Objetivo 3
#Ejercicio 1
datos <- read.csv("Notas.csv", sep=";", dec=",", header=T)
datos$NOTA.FINAL.SEPTIEMBRE..0.10. <- NULL
datos$EXAMEN.SEPTIEMBRE..0.7.<-NULL

for(i in c(2:4)){(datos[,i])*3*5}
datos$TOTAL.prÃ.cticas..0.2.<-  (datos$TOTAL.prÃ.cticas..0.2.)*(5)
datos$Total.cuestionarios..0.1. <- (datos$Total.cuestionarios..0.1.)*(10)
datos$EXAMEN.JUNIO..0.7.<- (datos$EXAMEN.JUNIO..0.7.)*(10.0/7.0)
datos$NOTA.FINAL.JUNIO...0.10.<- datos$NOTA.FINAL.JUNIO...0.10.
datos

m <- naiveBayes(NOTA.FINAL.JUNIO...0.10. ~. , data = datos)
m
predictions <- predict(m, datos)
table(predictions, datos$NOTA.FINAL.JUNIO...0.10.)
table(datos$NOTA.FINAL.JUNIO...0.10.)

#Ejercicio 2
datos$NOTA.FINAL.JUNIO...0.10.<-cut(datos$NOTA.FINAL.JUNIO...0.10., breaks = c(0,5,10))
m<-naiveBayes(datos$NOTA.FINAL.JUNIO...0.10.~datos$ï..Grupo+datos$PrÃ.ctica.1..0.2.3.,data=datos[,c(1:9,10)])
m

#Ejercicio 3

datos <- read.csv("Notas1.csv", sep=";", dec=",", header=T)

for(i in c(2:4)){(datos[,i])*3*5}
datos$PrÃ.ctica.1..0.2.3. <-  datos$PrÃ.ctica.1..0.2.3.*(10/(2/3))
datos$Cuestionario.temas.1.y.2..0.10.<-datos$Cuestionario.temas.1.y.2..0.10.
datos$Cuestionario.tipo.test.temas.3.y.4...0.10.<-datos$Cuestionario.tipo.test.temas.3.y.4...0.10.


datos$Cuestionario.tipo.test.temas.5.y.6...0.10.<-NULL
datos$PrÃ.ctica.2...0.2.3.<-NULL
datos$PrÃ.ctica.3...0.2.3.<-NULL
datos$Total.cuestionarios..0.1.<-NULL
datos$EXAMEN.JUNIO..0.7.<-NULL
datos$NOTA.FINAL.SEPTIEMBRE..0.10. <- NULL
datos$EXAMEN.SEPTIEMBRE..0.7.<-NULL
datos

datos$NOTA.FINAL.JUNIO...0.10. <- cut(datos$NOTA.FINAL.JUNIO...0.10., breaks = c(0, 5, 10), labels = c("Suspenso", "Aprobado"))
datos$NOTA.FINAL.JUNIO...0.10.

notasFuturo <- naiveBayes(NOTA.FINAL.JUNIO...0.10.~ ., data = datos)
notasFuturo
predictions <- predict(notasFuturo, datos)
predictions
table(predictions, datos$NOTA.FINAL.JUNIO...0.10.)
