

#---------------------------------------------------------------------------------
#                                  MODELO GARCH(1,1)
#---------------------------------------------------------------------------------

#Fijamos carpeta 
setwd("C:/Users/VALERIA/Desktop/Ciclo 2020-2/ECONOMETRÍA AVANZADA/módulo4/Tarea/Pregunta 3")
## !!! INCREASE THE NUMBER OF MCMC ITERATIONS !!!
library(readxl)
library(xlsx)
library(bayesGARCH)

# Importamos la data
bd <- read_excel("Retornos.xlsx")
ipsa<-bd$Ipsa[1:2258]
csi300<-bd$CSI300[1:2213]
ftse100<-bd$FTSE100[1:2293]
nasdaq100<-bd$Nasdaq100[1:2285]
tc<-bd$tc[1:2369]

#GARCH (1,1)
#===========

#===============================================================================
#Nasdaq100
#===============================================================================

nasdaq100<-nasdaq100[2:2285]-mean(nasdaq100[2:2285])
y <- nasdaq100
## RUN THE SAMPLER (2 chains)
MCMC <- bayesGARCH(y, control = list(n.chain = 2, l.chain = 10000))
## FORM THE POSTERIOR SAMPLE
smpl <- formSmpl(MCMC, l.bi = 50)
## POSTERIOR STATISTICS
summary(smpl)
smpl <- as.matrix(smpl)
pairs(smpl)
#Gráficos
smpl<-data.frame(smpl)
#kappa
ggplot(data = smpl,
       mapping = aes(x = alpha0)) +
  geom_histogram(bins = 100,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Parámetro kappa',
       x = 'Parámetro kappa',
       y = 'conteos')
#alpha
ggplot(data = smpl,
       mapping = aes(x = alpha1)) +
  geom_histogram(bins = 100,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Parámetro alpha',
       x = 'Parámetro alpha',
       y = 'conteos')
#beta
ggplot(data = smpl,
       mapping = aes(x = beta)) +
  geom_histogram(bins = 100,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Parámetro beta',
       x = 'Parámetro beta',
       y = 'conteos')


#===============================================================================
#CSI300
#===============================================================================

csi300<-csi300[2:2213]-mean(csi300[2:2213])
y <- csi300
## RUN THE SAMPLER (2 chains)
MCMC <- bayesGARCH(y, control = list(n.chain = 2, l.chain = 10000))
## FORM THE POSTERIOR SAMPLE
smpl <- formSmpl(MCMC, l.bi = 50)
## POSTERIOR STATISTICS
summary(smpl)
smpl <- as.matrix(smpl)
pairs(smpl)
#Gráficos
smpl<-data.frame(smpl)
#kappa
ggplot(data = smpl,
       mapping = aes(x = alpha0)) +
  geom_histogram(bins = 100,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Parámetro kappa',
       x = 'Parámetro kappa',
       y = 'conteos')
#alpha
ggplot(data = smpl,
       mapping = aes(x = alpha1)) +
  geom_histogram(bins = 100,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Parámetro alpha',
       x = 'Parámetro alpha',
       y = 'conteos')
#beta
ggplot(data = smpl,
       mapping = aes(x = beta)) +
  geom_histogram(bins = 100,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Parámetro beta',
       x = 'Parámetro beta',
       y = 'conteos')



#===============================================================================
#FTSE100
#===============================================================================

ftse100<-ftse100[2:2293]-mean(ftse100[2:2293])
y <- ftse100
## RUN THE SAMPLER (2 chains)
MCMC <- bayesGARCH(y, control = list(n.chain = 2, l.chain = 10000))
## FORM THE POSTERIOR SAMPLE
smpl <- formSmpl(MCMC, l.bi = 50)
## POSTERIOR STATISTICS
summary(smpl)
smpl <- as.matrix(smpl)
pairs(smpl)
#Gráficos
smpl<-data.frame(smpl)
#kappa
ggplot(data = smpl,
       mapping = aes(x = alpha0)) +
  geom_histogram(bins = 100,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Parámetro kappa',
       x = 'Parámetro kappa',
       y = 'conteos')
#alpha
ggplot(data = smpl,
       mapping = aes(x = alpha1)) +
  geom_histogram(bins = 100,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Parámetro alpha',
       x = 'Parámetro alpha',
       y = 'conteos')
#beta
ggplot(data = smpl,
       mapping = aes(x = beta)) +
  geom_histogram(bins = 100,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Parámetro beta',
       x = 'Parámetro beta',
       y = 'conteos')


#===============================================================================
#IPSA
#===============================================================================

ipsa<-ipsa[2:2258]-mean(ipsa[2:2258])
y <- ipsa
## RUN THE SAMPLER (2 chains)
MCMC <- bayesGARCH(y, control = list(n.chain = 2, l.chain = 10000))
## FORM THE POSTERIOR SAMPLE
smpl <- formSmpl(MCMC, l.bi = 50)
## POSTERIOR STATISTICS
summary(smpl)
smpl <- as.matrix(smpl)
pairs(smpl)
#Gráficos
smpl<-data.frame(smpl)
#kappa
ggplot(data = smpl,
       mapping = aes(x = alpha0)) +
  geom_histogram(bins = 100,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Parámetro kappa',
       x = 'Parámetro kappa',
       y = 'conteos')
#alpha
ggplot(data = smpl,
       mapping = aes(x = alpha1)) +
  geom_histogram(bins = 100,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Parámetro alpha',
       x = 'Parámetro alpha',
       y = 'conteos')
#beta
ggplot(data = smpl,
       mapping = aes(x = beta)) +
  geom_histogram(bins = 100,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Parámetro beta',
       x = 'Parámetro beta',
       y = 'conteos')




#===============================================================================
#TC
#===============================================================================

tc<-tc[2:2369]-mean(tc[2:2369])
y <- tc
## RUN THE SAMPLER (2 chains)
MCMC <- bayesGARCH(y, control = list(n.chain = 2, l.chain = 10000))
## FORM THE POSTERIOR SAMPLE
smpl <- formSmpl(MCMC, l.bi = 50)
## POSTERIOR STATISTICS
summary(smpl)
smpl <- as.matrix(smpl)
pairs(smpl)
#Gráficos
smpl<-data.frame(smpl)
#kappa
ggplot(data = smpl,
       mapping = aes(x = alpha0)) +
  geom_histogram(bins = 100,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Parámetro kappa',
       x = 'Parámetro kappa',
       y = 'conteos')
#alpha
ggplot(data = smpl,
       mapping = aes(x = alpha1)) +
  geom_histogram(bins = 100,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Parámetro alpha',
       x = 'Parámetro alpha',
       y = 'conteos')
#beta
ggplot(data = smpl,
       mapping = aes(x = beta)) +
  geom_histogram(bins = 100,
                 position = 'identity',
                 alpha = 0.8) +
  labs(title = 'Parámetro beta',
       x = 'Parámetro beta',
       y = 'conteos')
