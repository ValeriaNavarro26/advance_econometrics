# Generación de datos vía FK modelo univariado 

#Definición de variables para almacenar los datos 
m<-integer(201)
C<-integer(201)
C[1]<-1000
a<-integer(200)
R<-integer(200)
f<-integer(200)
Q<-integer(200)
y<-integer(200)
V<-0.25
W<-25
phi<-0.97
theta<-0.5
Observaciones<-(1:200)

#Inicio de la recursión 
for(j in 2:200){
  a[j]=phi*m[j-1]
  R[j]=phi^(2)*C[j-1]+W
  f[j]=theta*a[j]
  Q[j]=theta^(2)*R[j]+V
  y[j]=rnorm(1, mean = f[j], sd = sqrt(Q[j]))
  m[j]=a[j]+theta*(R[j]/Q[j])*(y[j]-f[j])
  C[j]=R[j]-theta^(2)*(R[j]^(2)/Q[j])
}
Estados<-m[2:201]
Datos<-y
plot(Observaciones, Estados, type= 'l', col = 'red')
lines(Observaciones, f, col = 'blue')
lines(Observaciones, Datos, col = 'green')
legend(5, -10, legend=c("Estados", "Media", "Observaciones"), col=c("red", "blue", "green"), lty=1, cex=0.6)
