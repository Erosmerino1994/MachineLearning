#Establecemos la semilla
set.seed(1234) 

#Instalamos los paquetes necesarios para manipular los datos y los gráficos
install.packages('tidyverse')
library(tidyverse)

# --------------------------------------------------------------
# Distribución normal
# --------------------------------------------------------------

# Función de densidad normal de probabilidad
# --------------------------------------------------------------

n<-seq(-4,4,0.1)  # 81 puntos de -4 a 4 en intervalos de 0,1
datos <- data.frame(n=n, 
                    densidad=dnorm(n, mean=0, sd=1),
                    distribucion=pnorm(n, mean=0, sd=1)) 

#Dibujamos la gráfica
ggplot(datos, aes(x=n, y=densidad )) + 
  geom_line(colour="blue")  + 
  ggtitle ("Función normal de densidad \ndnorm( media=0, desviacion=1)") + 
  theme_minimal()


# Función de distribución normal acumulada
# --------------------------------------------------------------

#Dibujamos la gráfica
ggplot(datos, aes(x=n, y=distribucion )) + 
  geom_line(colour="blue") +
  ggtitle ("Función normal acumulada \npnorm( media=0, desviacion=1)") + 
  theme_minimal()

# Función normal de cuartiles
# --------------------------------------------------------------
q<-seq(0.01,0.99,0.01)  # 99 puntos de 1% a 99% en intervalos de 1%
datos <- data.frame(Q=q, cuartiles=qnorm(q, mean=0, sd=1))  

#Dibujamos la gráfica
ggplot(datos, aes(x=q, y=cuartiles )) + 
  geom_line(colour="blue")  + 
  ggtitle ("Distribución normal de cuartiles \npnorm( media=0, desviacion=1)") + 
  theme_minimal()


# Función normal aleatoria
# --------------------------------------------------------------
datos <- data.frame(Z=n, 
                    x=rnorm(n, mean=0, sd=1),
                    y=rnorm(n, mean=0, sd=1)) 

#Dibujamos la gráfica
ggplot(datos, aes(x=x, y=y)) + 
  geom_point(colour="blue") +
  ggtitle("Función normal aleatoria \npnorm( media=0, desviacion=1)")+
  theme_minimal()


# --------------------------------------------------------------
# Distribución exponencial
# --------------------------------------------------------------

# Función de densidad exponencial 
# --------------------------------------------------------------

x_exp <- seq(0,10, by= 0.25) #41 puntos de 0 a 10 con intervalos de 0.25
y_dexp <- dexp(x_dexp, rate=1) #Algoritmo que calcula la distribución de densidad
datos_dexp <- data.frame(x_exp, y_dexp)

#Dibujamos la gráfica
ggplot(datos_dexp, aes(x=x_exp, y=y_dexp )) + 
  geom_line(colour="blue")  + 
  ggtitle ("Función exponencial de densidad") + 
  theme_minimal()

# Función de distribución exponencial acumulada
# --------------------------------------------------------------

z_pexp <- pexp(x_exp, rate=1) #Algoritmo que calcula la distribución acumulada
datos_pexp <- data.frame(x_exp,z_pexp)

#Dibujamos la gráfica
ggplot(datos_pexp, aes(x=x_exp, y=z_pexp )) + 
  geom_line(colour="blue")  + 
  ggtitle ("Función exponencial acumulada") + 
  theme_minimal()

# Función exponencial de cuartiles
# --------------------------------------------------------------

c_exp <- seq(0.001,0.999,0.001)  #999 puntos de 0.001 a 0.999 con intervalos de 0.001
t_qexp <- qexp(c_exp, rate=1) #Algoritmo que calcula la distribución de cuartiles
datos_cexp <- data.frame(c_exp, t_qexp)

#Dibujamos la gráfica
ggplot(datos_cexp, aes(x=c_exp, y=t_qexp)) + 
  geom_line(colour="blue")  + 
  ggtitle ("Función exponencial cuartiles") + 
  theme_minimal()

# Función exponencial aleatoria
# --------------------------------------------------------------

datos_aexp <- data.frame(Z=x_exp, 
                         x=rexp(x_exp, rate=1),
                         y=rexp(x_exp, rate=1)) #Algoritmo que calcula la distribución aleatoria y la introduce en el dataframe

#Dibujamos la gráfica
ggplot(datos_aexp, aes(x=x, y=y)) + 
  geom_point(colour="blue") +
  ggtitle("Función exponencial aleatoria")+
  theme_minimal()

# --------------------------------------------------------------
# Distribución binomial
# --------------------------------------------------------------

# Función de densidad binomial 
# --------------------------------------------------------------

x_bin <- seq(0,200, by=1) #201 puntos de 0 a 200 con intervalos de 1
y_dbin <- dbinom(x_bin, size=200, prob=0.6) #Algoritmo que calcula la distribución de densidad
datos_dbin <- data.frame(x_bin, y_dbin)

#Dibujamos la gráfica
ggplot(datos_dbin, aes(x=x_bin, y=y_dbin)) + 
  geom_line(colour="blue")+
  ggtitle("Función binomial de densidad")+
  theme_minimal()

# Función de distribución binomial acumulada
# --------------------------------------------------------------

z_pbin <- pbinom(x_bin, size=200, prob=0.6) #Algoritmo que calcula la distribución acumulada
datos_pbin <- data.frame(x_bin,z_pbin)

#Dibujamos la gráfica
ggplot(datos_pbin, aes(x=x_bin, y=z_pbin )) + 
  geom_line(colour="blue")  + 
  ggtitle ("Función binomial acumulada") + 
  theme_minimal()

# Función binomial de cuartiles
# --------------------------------------------------------------

c_bin <- seq(0.001,0.999,0.001) #999 puntos de 0.001 a 0.999 con intervalos de 0.001
t_qbin <- qbinom(c_exp, size=200, prob=0.6) #Algoritmo que calcula la distribución de cuartiles
datos_qbin <- data.frame(c_bin, t_qbin )

#Dibujamos la gráfica
ggplot(datos_qbin, aes(x=c_bin, y=t_qbin)) + 
  geom_line(colour="blue")  + 
  ggtitle ("Función binomial de cuartiles") + 
  theme_minimal()

# Función binomial aleatoria
# --------------------------------------------------------------

datos_abin <- data.frame(Z=x_bin, 
                         x=rbinom(x_bin, size=200, prob=0.6),
                         y=rbinom(x_bin, size=200, prob=0.6)) #Algoritmo que calcula la distribución aleatoria y la introduce en el dataframe

#Dibujamos la gráfica
ggplot(datos_abin, aes(x=x, y=y)) + 
  geom_point(colour="blue") +
  ggtitle("Función binomial aleatoria")+
  theme_minimal()

# --------------------------------------------------------------
# Distribución de Poisson
# --------------------------------------------------------------

# Función de distribución de Poisson de densidad 
# --------------------------------------------------------------

x_pois <- seq(0,20, by=1) #21 puntos de 0 a 20 con intervalos de 1
y_dpois <- dpois(x_pois, lambda = 8) #Algoritmo que calcula la distribución de densidad
datos_dpois <- data.frame(x_pois, y_dpois)

#Dibujamos la gráfica
ggplot(datos_dpois, aes(x=x_pois, y=y_dpois)) + 
  geom_line(colour="blue")+
  ggtitle("Función de distribución de Poisson de densidad")+
  theme_minimal()

# Función de distribución de Poisson acumulada
# --------------------------------------------------------------

z_ppois <- ppois(x_pois, lambda = 8) #Algoritmo que calcula la distribución acumulada
datos_ppois <- data.frame(x_pois,z_ppois) 

#Dibujamos la gráfica
ggplot(datos_ppois, aes(x=x_pois, y=z_ppois )) + 
  geom_line(colour="blue")  + 
  ggtitle ("Función de distribución de Poisson acumulada") + 
  theme_minimal()

# Función de distribución de Poisson de cuartiles
# --------------------------------------------------------------

c_pois <- seq(0.001,0.999,0.001) #999 puntos de 0.001 a 0.999 con intervalos de 0.001
t_qpois <- qpois(c_pois, lambda = 8) #Algoritmo que calcula la distribución de cuartiles
datos_qpois <- data.frame(c_pois, t_qpois)

#Dibujamos la gráfica
ggplot(datos_qpois, aes(x=c_pois, y=t_qpois)) + 
  geom_line(colour="blue")  + 
  ggtitle ("Función de distribución de Poisson de cuartiles") + 
  theme_minimal()

# Función de distribución de Poisson aleatoria
# --------------------------------------------------------------

datos_apois <- data.frame(Z=x_pois, 
                          x=rpois(x_pois, lambda = 8),
                          y=rpois(x_pois, lambda = 8)) #Algoritmo que calcula la distribución aleatoria y la introduce en el dataframe

#Dibujamos la gráfica
ggplot(datos_apois, aes(x=x, y=y))  + 
  geom_point(colour="blue") +
  ggtitle("Función de distribución de Poisson aleatoria")+
  theme_minimal()


# --------------------------------------------------------------
# Distribución χ2
# --------------------------------------------------------------

# Función de distribución χ2 de densidad 
# --------------------------------------------------------------

x_chisq <- seq(3,72, by=3) #24 puntos de 3 a 72 con intervalos de 3
y_dchisq <- dchisq(x_chisq, df=20) #Algoritmo que calcula la distribución de densidad
datos_dchisq <- data.frame(x_chisq, y_dchisq)

#Dibujamos la gráfica
ggplot(datos_dchisq, aes(x=x_chisq, y=y_dchisq)) + 
  geom_line(colour="blue")+
  ggtitle("Función de distribución χ2 de densidad")+
  theme_minimal()

# Función de distribución χ2 acumulada
# --------------------------------------------------------------

z_pchisq <- pchisq(x_chisq, df=20) #Algoritmo que calcula la distribución acumulada
datos_pchisq <- data.frame(x_chisq,z_pchisq) 

#Dibujamos la gráfica
ggplot(datos_pchisq, aes(x=x_chisq, y=z_pchisq )) + 
  geom_line(colour="blue")  + 
  ggtitle ("Función de distribución χ2 acumulada") + 
  theme_minimal()

# Función de distribución χ2 de cuartiles
# --------------------------------------------------------------

c_chisq <- seq(0.001,0.999,0.001) #999 puntos de 0.001 a 0.999 con intervalos de 0.001
t_qchisq <- qchisq(c_chisq, df=20) #Algoritmo que calcula la distribución de cuartiles
datos_qchisq <- data.frame(c_chisq, t_qchisq)

#Dibujamos la gráfica
ggplot(datos_qchisq, aes(x=c_chisq, y=t_qchisq)) + 
  geom_line(colour="blue")  + 
  ggtitle ("Función de distribución χ2 de cuartiles") + 
  theme_minimal()

# Función de distribución χ2 aleatoria
# --------------------------------------------------------------

datos_achisq <- data.frame(Z=x_chisq, 
                           x=rchisq(x_chisq, df=20),
                           y=rchisq(x_chisq, df=20)) #Algoritmo que calcula la distribución de cuartiles y la introduce en el dataframe

#Dibujamos la gráfica
ggplot(datos_achisq, aes(x=x, y=y)) + 
  geom_point(colour="blue") +
  ggtitle("Función de distribución χ2 aleatoria")+
  theme_minimal()

