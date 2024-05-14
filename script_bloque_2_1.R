# Inferencia Bayesiana en Ecología con R (IBER)
# Bloque 2.1: Modelos de Ocupación (site-occupancy models).

# Queremos modelizar la ocupación (probabilidad de presencia) de una especie a 
# partir de datos de cámaras trampa. Nuestro área de estudio es una zona montañosa 
# dividida en una malla de 50x50 celdas. Sabemos que nuestra especie utiliza 
# mayormente el hábitat de matorral de los fondos de valle de nuestro área de 
# estudio. Disponemos de un total de 100 cámaras de fototrampeo que hemos dispuesto 
# aleatoriamente en nuestro área de estudio que han estado activas durante 7 noches 
# utilizando una lata de sardinas como atrayente. De las 100 cámaras, 47 de ellas 
# son de la marca A y 53 de la marca B.

# Cargamos los paquetes que vamos autilizar
library(terra)
library(nimble)
library(MCMCvis)
library(tidyverse)

# Leemos nuestros datos en formato CVS
datos <- read.csv("https://raw.githubusercontent.com/jabiologo/web/master/tutorials/tallerSECEM_files/occuDatos.csv")

# Le pedimos a R que nos muestre nuestros datos.
datos
# Cargamos las capas raster correspondientes a nuestras covariables predictoras,
# en este caso elevación y porcentaje de vegetación arbustiva.
elev <- rast("https://github.com/jabiologo/web/raw/master/tutorials/tallerSECEM_files/elev.tif")
arbu <- rast("https://github.com/jabiologo/web/raw/master/tutorials/tallerSECEM_files/arbu.tif")
names(elev) <- "elev"
names(arbu) <- "arbu"

# Vamos a graficar estas capas para hacernos una idea de cómo son. Además,
# colocaremos encima las localizaciones de nuestros dos modelos de cámaras.
par(mfrow = c(1,2))
plot(elev, main = "Elevación")
points(xyFromCell(elev, datos$id[datos$marca == "A"]), col = "darkred", pch = 19)
points(xyFromCell(elev, datos$id[datos$marca == "B"]), col = "darkblue", pch = 19)
plot(arbu, main = "Porcentaje arbusto")

# En este paso estandarizaremos nuestras variables para que su media sea 0 y su
# desviación estándar 1. Esta es una práctica habitual en modelización que ayuda
# a ajustar los modelos de forma más efectiva. La fórmula es:
# valor escalado = (valor de la cov - valor medio de la cov)/ SD de la cov
datos$elev <- scale(elev[datos$id])[1:100]
datos$arbu <- scale(arbu[datos$id])[1:100]

# Inspeccionamos las primeras filas de nuestro juego de datos.
head(datos)

# NIMBLE!

# Definimos nuestros datos (lo que va a la izquierda de las fórmulas)
my.data <- list(hdet = datos[,3:9])

# Definimos nuestras constantes (iteradores y lo que va a la derecha de las fórmulas)
my.constants <- list(elev = datos$elev,
                     arbu = datos$arbu,
                     marca = as.numeric(datos$marca =="A"),
                     dia = datos[,10:16],
                     nsitios = 100,
                     nocasiones = 7)

# Escribimos el modelo
occu.mod <- nimbleCode({
  # Priors
  b0 ~ dnorm(mean = 0, sd = 1) 
  b1 ~ dnorm(mean = 0, sd = 1) 
  b2 ~ dnorm(mean = 0, sd = 1) 
  a0 ~ dnorm(mean = 0, sd = 1) 
  a1 ~ dnorm(mean = 0, sd = 1)
  a2 ~ dnorm(mean = 0, sd = 1)
  a3 ~ dnorm(mean = 0, sd = 1)
  
  # Likelihood
  for (i in 1:nsitios) {
    Y[i] ~ dbern(psi[i]) # Proceso ecológico
    logit(psi[i]) <- b0 + b1*elev[i] + b2*arbu[i]
    for (j in 1:nocasiones) {
      hdet[i,j] ~ dbern(Y[i] * p[i,j]) # Proceso observacional
      logit(p[i,j]) <- a0 + a1*marca[i] + a2*arbu[i] + a3*dia[i,j]
    }
  }
  
})

# Definimos nuestros valores iniciales
initial.values <- list(b0 = rnorm(1,0,1),
                       b1 = rnorm(1, 0, 1),
                       b2 = rnorm(1, 0, 1),
                       a0 = rnorm(1,0,1),
                       a1 = rnorm(1,0,1),
                       a2 = rnorm(1,0,1),
                       a3 = rnorm(1,0,1),
                       Y = as.numeric(rowSums(my.data$hdet) > 0))

# Definimos los parámetros que queremos quedarnos
parameters.to.save <- c("b0", "b1", "b2", "a0", "a1", "a2", "a3")

# Ajustes del MCMC
n.iter <- 3000
n.burnin <- 200
n.chains <- 3
n.thin <- 1

# Corremos el modelo
mcmc.output <- nimbleMCMC(code = occu.mod,     
                          data = my.data,  
                          constants = my.constants,
                          inits = initial.values,
                          monitors = parameters.to.save,
                          thin = n.thin,
                          niter = n.iter, 
                          nburnin = n.burnin, 
                          nchains = n.chains)

# Inspeccionamos los traceplots
MCMCtrace(mcmc.output, pdf = F)

# Resumen del modelo
MCMCsummary(mcmc.output, round = 2)

# Una vez nos hemos asegurado que las cadenas convergen, podemos unirlas
mcmc.bind <- rbind(mcmc.output$chain1, mcmc.output$chain2, mcmc.output$chain3)
nrow(mcmc.bind)

# Predicciones
mcmc.bind <- rbind(mcmc.output$chain1, mcmc.output$chain2, mcmc.output$chain3)
# Se muestra sólo la predicción de la detectabilidad en función de los días
# que lleve instalada la cámara.
# Para ello utilizaremos la media del porcentaje de arbustos (0) y la cámara 
# marca B (0)
pred.dia <- seq(from = 1, to = 7, by = 1)

pred.p <- matrix(NA, nrow = nrow(mcmc.bind), ncol = length(pred.dia))
for(i in 1:nrow(pred.p)){
  pred.p[i,] <- plogis(mcmc.bind[i,"a0"] + 
                         mcmc.bind[i,"a1"] * 0 +
                         mcmc.bind[i,"a2"] * pred.dia + 
                         mcmc.bind[i,"a3"] * 0 )
}

mean.p <- apply(pred.p, 2, mean) # extraemos la media
sd.p <- apply(pred.p, 2, sd) # extraemos la sd
ci <- apply(pred.p, 2, quantile, c(0.1, 0.9)) # 80% CI

pred.p.results <- tibble(dia = pred.dia, 
                         mean = mean.p,
                         sd = sd.p,
                         cinf = ci[1,],
                         csup = ci[2,])

# Graficamos los resultados
plot(pred.p.results$dia, pred.p.results$mean, type = "l", lwd = 3, col = "darkred",
     xlab = "Días desde que se instaló la cámara", ylab = "Probabilidad de detección")
lines(pred.p.results$dia, pred.p.results$cinf, lty = 2, col = "darkblue")
lines(pred.p.results$dia, pred.p.results$csup, lty = 2, col = "darkblue")

# Predicción sobre el mapa

# Primero debemos estandarizar nuestras variables de la misma forma que lo hicimos
# anteriormente
elevS <- (elev - 958.29) / 187.6693 
arbuS <- (arbu - 49.78) / 7.041493

# Vamos a hacer una predicción sólo sobre la media (no propagaremos la incertidumbre!)
# Para ello utilizaremos las medias de las distribuciones a posteriori obtenidas
prediccion <- exp(-0.41 - 1.01*elevS + 0.29*arbuS)/(1+exp(-0.41 - 1.01*elevS + 0.29*arbuS))
plot(prediccion)