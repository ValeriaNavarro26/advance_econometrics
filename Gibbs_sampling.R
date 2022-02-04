#---------------------------------------------------------------------------------
#                       Maestria en Economia PUCP
#                          Series de Tiempo
#                            Tarea Módulo 4
#---------------------------------------------------------------------------------

rm(list=ls())

#---------------------------------------------------------------------------------
# 1. Gibbs sampling
#---------------------------------------------------------------------------------

#paquetes que vamos a necesitar

library(readxl)
library(xlsx)
library(dplyr)
library(ggplot2)
install.packages("tidyr")
library(tidyr)
install.packages("moments")
library(moments)

#Fijamos carpeta 
setwd("C:/Users/VALERIA/Desktop/Ciclo 2020-2/ECONOMETRÍA AVANZADA/módulo4/Tarea/Pregunta1")

# Importamos la data
bd <- read_excel("retornos.xlsx")
            
#Estadísticas descriptivas
summary(bd$Datos) 
var(bd$Datos)    
kurtosis(bd$Datos)
skewness(bd$Datos)
sd(bd$Datos)

plot(bd$Datos, type="l", xlab="N° Observaciones", ylab="Retornos del IGBVL")
hist(x = bd$Datos, main = "Histograma de retornos del IGBVL", 
     xlab = "Retornos del IGBVL", ylab = "Frecuencia",
     col = "ivory")

#Definimos los valores iniciales y otras cosas

N<-length(bd$Datos)  #Número de observaciones
x <- bd$Datos
mu0=0
kappa=1
sigma=1000
n0=4
s0=2
S=var(x)
r=mean(x)

pasos <- 20000 #número de iteraciones
camino <-matrix(0, nrow = pasos + 1, ncol = 2) # vector guardará las simulaciones
camino[1, 1] <- 0 # valor inicial media

#Generamos el algoritmo

for (j in 2:(pasos+1)){
  #sigma
  mu<- camino[j-1,1]
  a<-(N+n0+1)/2
  b<-((N-1)*S+N*(mu-r)^2+s0+kappa*((mu-mu0)^2))/2
  camino[j-1,2]<- 1/rgamma(1,shape=a, rate=b) #actualizar sigma
  
  #mu
  sigma2 <-camino[j-1,2]
  media<-((N/(N+kappa))*r)+((kappa*mu0)/(N+kappa))
  var<- sigma2/(N+kappa)
  camino[j,1]<-rnorm(1,media,sd=sqrt(var))
}

#Reordenamos resultados
caminata <- data.frame(pasos = 1:pasos, mu = camino[1:pasos, 1], 
                       sigma2 = camino[1:pasos, 2])

caminata_g <- caminata %>%
  gather(parametro, val, mu, sigma2) %>%
  arrange(pasos)

#Algunos gráficos de los valores obtenidos 

ggplot(filter(caminata_g, pasos > 15000), aes(x = pasos, y = val)) +
  geom_path(alpha = 0.3) +
  facet_wrap(~parametro, ncol = 1, scales = "free") +
  scale_y_continuous("")


ggplot(filter(caminata_g, pasos > 5000), aes(x = val)) +
  geom_histogram(fill = "gray") +
  facet_wrap(~parametro, ncol = 1, scales = "free") 

#algunos estadísticos resumen de la posterior
caminata_g %>%
  filter(pasos > 1000) %>% # eliminamos la etapa de calentamiento
  group_by(parametro) %>%
  summarise(
    mean(val), 
    sd(val), 
    median(val)
  )



