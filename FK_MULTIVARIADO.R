# Generación de datos vía FK modelo univariado 

#Definición de variables para almacenar los datos 
m<-array(0, dim=c(2,1,201)) #Vector columna 
m[1,1,1]<-10 
m[2,1,1]<-5  
C<-array(0, dim=c(2,2,201))
C[1,1,1]<-1000
C[2,2,1]<-1000
a<-array(0, dim=c(2,1,200))
R<-array(0, dim=c(2,2,200))
f<-integer(200)
Q<-integer(200)
y<-integer(200)
V<-1
W<-matrix(c(1, 0, 0, 0.25),nrow = 2, ncol = 2)
G<-matrix(c(1, 1, 0, 1), nrow = 2, ncol = 2)
F<-c(1, 0)
Observaciones<-(1:200)

#Inicio de la recursión 
for(j in 2:200){
  a[,,j]=G%*%m[,,j-1]
  R[,,j]=t(G)%*%C[,,j-1]%*%G+W
  f[j]=t(F)%*%a[,,j]
  Q[j]=t(F)%*%R[,,j]%*%F+V
  y[j]=rnorm(1, mean = f[j], sd = sqrt(Q[j]))
  m[,,j]=a[,,j]+R[,,j]%*%F*Q[j]^(-1)*(y[j]-f[j])
  C[,,j]=R[,,j]-Q[j]^(-1)*R[,,j]%*%F%*%t(F)%*%t(R[,,j])
}
Estados_mu<-m[1,1,2:200]  
plot(Observaciones, y, type = 'l')
lines(Observaciones, m[1,1,2:201], col = 'green')

t=1:200
Observaciones<-y
Media<- m[1,1,2:201]

data<- data.frame(t,Observaciones,Media)
data1 <- data %>%
  gather(Leyenda, val,Observaciones,Media ) %>%
  arrange(t)

library(hrbrthemes)
library(ggplot2)



ggplot(data=data1, aes(x=t, y=val, group=Leyenda)) +
  geom_line( aes(color=Leyenda), size=0.7, alpha=1, linetype=1) +
  labs(x="Observaciones", y="datos",
       title="State-space models",
       subtitle="Kalman filter for multivariate model") +
  scale_color_brewer(palette="Set1")+
theme_minimal()
