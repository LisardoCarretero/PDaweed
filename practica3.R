#Equipo P(Daweed)
getwd()
setwd("/Users/fatimapaternaroda/RStudio")
datos <- read.csv("Notas.csv", sep=";", dec=",", header=T)
grupo <- datos$Grupo


for(i in c(2:4)){(datos[,i])*3*5}
datos$TOTAL.prácticas..0.2.<-  datos$TOTAL.prácticas..0.2.*5
datos$Total.cuestionarios..0.1. <- datos$Total.cuestionarios..0.1.*10
datos$EXAMEN.JUNIO..0.7.<- datos$EXAMEN.JUNIO..0.7.*(10.0/7.0)
datos$NOTA.FINAL.JUNIO...0.10.<- datos$NOTA.FINAL.JUNIO...0.10.
datos$Práctica.1..0.2.3. <-  datos$Práctica.1..0.2.3. 
datos$Práctica.2...0.2.3. <- datos$Práctica.2...0.2.3. 
datos$Práctica.3...0.2.3.  <- datos$Práctica.3...0.2.3. 
datos$EXAMEN.SEPTIEMBRE..0.7.<-datos$EXAMEN.SEPTIEMBRE..0.7.*(10.0/7.0)
datos$NOTA.FINAL.SEPTIEMBRE..0.10.<-datos$NOTA.FINAL.SEPTIEMBRE..0.10.

#Objetivo 1
#Conversion de variables cuantitativas en variables cualitativas 

for(i in c(2:13)){(datos[is.na(datos[,i]),i]<- 0)}
datos[,i]
for(i in c(2:13)){
  datos[,i] <- cut(datos[,i],breaks = c(-1, 4.99, 10.0), labels = c("Suspenso", "Aprobado"))}
datos[,i]

juniofinal<- datos$NOTA.FINAL.JUNIO...0.10.
junio<-datos$EXAMEN.JUNIO..0.7.
practicas<-datos$TOTAL.prácticas..0.2.
cuestionarios <- datos$Total.cuestionarios..0.1.
septiembre <- datos$EXAMEN.SEPTIEMBRE..0.7.
septiembrefinal <- datos$NOTA.FINAL.SEPTIEMBRE..0.10.

tabla<-table(practicas, junio)

t=chisq.test (tabla)
t$observed
t$expected
t$p.value

#Alpha=0.05
#H0= Independiente
#H1=No Independiente 
# Como el p-valor es menor que alpha (0.05),rechazamos la hipotesis H0, por lo que son dependientes

tabla2<-table(junio, cuestionarios)

t=chisq.test (tabla2)
t$observed
t$expected
t$p.value

#Alpha=0.05
#H0= Independiente
#H1=No Independiente 
# Como el p-valor es menor que alpha (0.05),rechazamos la hipotesis H0, por lo que son dependientes


tabla3<-table(junio, septiembre)

t=chisq.test (tabla3)
t$observed
t$expected
t$p.value

#Alpha=0.05
#H0= Independiente
#H1=No Independiente 
# Como el p-valor es mayor que alpha (0.05),aceptamos la hipotesis H0, por lo que son independientes

tabla4<-table(junio, septiembrefinal)

t=chisq.test (tabla4)
t$observed
t$expected
t$p.value

#Alpha=0.05
#H0= Independiente
#H1=No Independiente 
# Como el p-valor es menor que alpha (0.05),rechazamos la hipotesis H0, por lo que son dependientes



#Objetivo 2
datos <- read.csv("Notas.csv", sep=";", dec=",", header=T)
juniofinal<-datos$NOTA.FINAL.JUNIO...0.10.
junio<-datos$EXAMEN.JUNIO..0.7.
#H0= Las varianas son iguales
#H1= Las varianzas son distintas
var.test(juniofinal, junio)
#Como el p-valor es menor (5.752e-05) que alpha (0.05), las varianzas son distintas 
#H0= Las notas son iguales
#H1= Las notas son distintas
t.test(juniofinal,  junio, var.equal = F, paired =T)
#Como el p-valor es menor (2.2e-16), las notas son distintas

#Objetivo 3
septiembre<-datos$EXAMEN.SEPTIEMBRE..0.7.
septiembrefinal<-datos$NOTA.FINAL.SEPTIEMBRE..0.10.
#H0= Las varianas son iguales
#H1= Las varianzas son distintas
var.test(septiembrefinal, septiembre)
#Como el p-valor es mayor (0.234) que alpha (0.05), las varianzas son iguales 
#H0= Las notas son iguales
#H1= Las notas son distintas
t.test(septiembrefinal,  septiembre, var.equal = T, paired =T)
#Como el p-valor es menor (2.2e-16), las notas son distintas


#Objetivo 4


tablaGrupos <- split(datos, grupo)

practica1A<- (tablaGrupos$A$Práctica.1..0.2.3.)
practica1B<- (tablaGrupos$B$Práctica.1..0.2.3.)
t.test(practica1A,practica1B)

practica2A<- (tablaGrupos$A$Práctica.2...0.2.3.)
practica2B<- (tablaGrupos$B$Práctica.2...0.2.3.)
t.test(practica2A,practica2B)

practica3A<- (tablaGrupos$A$Práctica.3...0.2.3.)
practica3B<- (tablaGrupos$B$Práctica.3...0.2.3.)
t.test(practica3A,practica3B)

practicaTA <- (tablaGrupos$A$TOTAL.prácticas..0.2.)
practicaTB<- (tablaGrupos$B$TOTAL.prácticas..0.2.)
t.test(practicaTA,practicaTB)

cuestionario1A<- (tablaGrupos$A$Cuestionario.temas.1.y.2..0.10.)
cuestionario1B<- (tablaGrupos$B$Cuestionario.temas.1.y.2..0.10.)
t.test(cuestionario1A,cuestionario1B)

cuestionario2A<- (tablaGrupos$A$Cuestionario.tipo.test.temas.3.y.4...0.10.)
cuestionario2B<- (tablaGrupos$B$Cuestionario.tipo.test.temas.3.y.4...0.10.)
t.test(cuestionario2A,cuestionario2B)

cuestionario3A<- (tablaGrupos$A$Cuestionario.tipo.test.temas.5.y.6...0.10.)
cuestionario3B<- (tablaGrupos$B$Cuestionario.tipo.test.temas.5.y.6...0.10.)
t.test(cuestionario3A,cuestionario3B)

cuestionarioTA<- (tablaGrupos$A$Total.cuestionarios..0.1.)
cuestionarioTB<- (tablaGrupos$B$Total.cuestionarios..0.1.)
t.test(cuestionarioTA,cuestionarioTB)

junioA<- (tablaGrupos$A$EXAMEN.JUNIO..0.7.)
junioB<- (tablaGrupos$B$EXAMEN.JUNIO..0.7.)
t.test(junioA,junioB)

junioFinalA<- (tablaGrupos$A$NOTA.FINAL.JUNIO...0.10.)
junioFinalB<- (tablaGrupos$B$NOTA.FINAL.JUNIO...0.10.)
t.test(junioFinalA,junioFinalB)

septiembreA<- (tablaGrupos$A$EXAMEN.SEPTIEMBRE..0.7.)
septiembreB<- (tablaGrupos$B$EXAMEN.SEPTIEMBRE..0.7.)
t.test(septiembreA,septiembreB)

septiembreFinalA<- (tablaGrupos$A$NOTA.FINAL.SEPTIEMBRE..0.10.)
septiembreFinalB<- (tablaGrupos$B$NOTA.FINAL.SEPTIEMBRE..0.10.)
t.test(septiembreFinalA,septiembreFinalB)


