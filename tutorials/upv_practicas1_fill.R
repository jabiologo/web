#########################################################################
# Introducción a los Modelos Lineales: fundamentos teóricos y prácticos #
# Universidad del País Vasco, octubre-noviembre 2025                    #
#########################################################################

############################################
# MÓDULO 1: INTRODUCCIÓN A LA MODELIZACIÓN #
############################################

# 1.1) LAS SIMULACIONES COMO UN LABORATORIO
# El lenguaje R nos ofrece multitud de distribuciones de probabilidad. 
help(Distributions)

# Todas ellas vienen asociadas con una serie de funciones con distintos fines:
# dnorm(x, mean, sd): "probabilidad" en el punto x
# pnorm(q, mean, sd): probabilidad acumulada en el punto q
# rnorm(n, mean, sd): generación de números n aleatorios
# A nosotros la que más nos interesa ahora es rnorm(n, mean, sd). Esta función
# permite simular n números aleatorios extraídos de una distribución normal con
# media "mean" y desviación estándar "sd"
rnorm(n=5, mean=0, sd=1)
rnorm(n=5, mean=30, sd=5)

# Ejercicio 1) Simular números aleatorios de variables con las que he trabajado
# utilizando las distribuciones Normal/Gaussiana, de Poisson, Binomial, y
# Uniforme. Explorar los números generados, así como distintos valores para los
# parámetros.
rnorm(n=1000, mean=0, sd=1)
rpois(n=100, lambda=4.5)
rbinom(n=100, size=1, prob=0.5)
rbinom(n=100, size=1, prob=1) # Tooos los valores son 1
rpois(n=100, lambda=-4) # Lambda debe ser >0 # Importante para el futuro
rbinom(n=100, size=1, prob=1.3) # 0 < p < 1  # Importante para el futuro

# Ejercicio 2) Visualizar los histogramas.
hist(rnorm(n=1000, mean=0, sd=1))
hist(rpois(n=1000, lambda=5))
hist(rnorm(n=10000, mean=10, sd=2))
hist(rnorm(n=50, mean=10, sd=2))
hist(rnorm(n=12, mean=10, sd=2))

# Ejercicio 3) Estudiar el efecto de diferentes tamaños muestrales.

# Ejercicio 4) Con los números aleatorios generados a partir de una distribución
# Normal/Gaussiana, estimar la meadia mean() y la desviación estándar sd().
# Estudiar el efecto del tamaño muestral.
mi_muestra <- rnorm(n=10000, mean=10, sd=2)
mean(mi_muestra)
sd(mi_muestra)

mi_muestra2 <- rnorm(n=10, mean=10, sd=2)
mean(mi_muestra2)
sd(mi_muestra2)

# Ejercicio 5) Intentar ajustar un modelo lineal que recupere la media y la
# desviación estándar de una muestra de números aleatorios obtenidos de una
# distribución Normal/Gaussiana. Entender el efecto de diferentes tamaños
# muestrales
df <- data.frame(mi_muestra <- rnorm(n=10000, mean=10, sd=2))
summary(lm(mi_muestra ~ 1, data = df))
summary(glm(mi_muestra ~ 1, data = df))
sigma(glm(mi_muestra ~ 1, data = df))

#########################
# 1.2) EL MODELO LINEAL #
#########################

# Ejercicio 0) Simular una distribución normal con una y dos medias diferentes.
mu <- rep(2, 1000)
hist(rnorm(n=1000, mean = mu, sd = 1), breaks = 20)

mu <- rep(c(-2,2), each = 500)
hist(rnorm(n=1000, mean = mu, sd = 1), breaks = 20)

# Ejercicio 1) Vamos a simular 500 observaciones de una distribución uniforme 
# entre 40 y 70 que interpretaremos como nuestra variable predictora, "latitud".
latitud <- runif(500, 40, 70)
hist(latitud, breaks=10)
mean(latitud)

# Ahora utilizaremos esta variable para intorducirla en el predictor lineal y
# generar así otros 500 valores, que no serán otra cosa que las medias de las
# distribuciones normales que generarán la variable respuesta. Antes, tengo que
# dar valores a los parámetros beta0 y beta1
b0 <- -4
b1 <- 0.8
mu <- b0 + b1*latitud

# Finalmente, simularemos nuestra variable respuesta como 500 valores aleatorios
# obtenidos de una distribución normal, solo que esta vez, en lugar de darle una
# única media, le daremos 500 diferentes, las creadas con nuestro predictor
# lineal. También daremos un valor a la desviación estándar.
sigma_normal <- 3
peso <- rnorm(500, mean = mu, sd = sigma_normal)

# Guardamos los datos en un data.frame
lobo <- data.frame(peso = peso, latitud = latitud)

# Ejercicio 2) Ajustar un modelo lineal con los datos simulados e interpretar
# los parámetros. ¿Qué significan los estimates? ¿Cuántos parámetros estamos
# estimando?

m1 <- glm(peso ~ latitud, family = "gaussian", data = lobo)
summary(m1)
sigma(m1)

plot(x=latitud, y=peso)
abline(m1, lwd = 2, col = "darkred")

peso_predicho <- -3.58010 + 0.79362*latitud
peso50 <- -3.58010 + 0.79362*50
-3.58010 + 0.79362*60 # peso a latitud 60
-3.58010 + 0.79362*40 # peso a latitud 40

res_m1 <- residuals(m1)
hist(res_m1)
mean(res_m1)

# Ejercicio 3) Propuesto. Simular nuestra propia variable predictora, un 
# predictor lineal, y ajustar un modelo lineal. Probar con diferentes tamaños
# muestrales (10, 100, 1000).
library(tidyverse)
lobo_subsample <- sample_n(lobo, 10)

m1_subsample <- glm(peso ~ latitud, family = "gaussian", data = lobo_subsample)
summary(m1_subsample)
sigma(m1_subsample)

# Efecto de una sigma mayor
sigma_normal2 <- 10
peso2 <- rnorm(500, mean = mu, sd = sigma_normal2)
lobo2 <- data.frame(peso = peso2, latitud = latitud)

m2 <- glm(peso ~ latitud, family = "gaussian", data = lobo2)
summary(m2)
sigma(m2)


# Ejercicio 4) Sobre el p-valor...
p_valores<- NA

for (i in 1:1000){
  n <- 30
  b0 <- 0; b1 <- 0 # bajo H0 verdadera: no hay efecto del predictor!
  x <- rnorm(n)
  mu <- b0 + b1*x
  y <- rnorm(n, mean = mu, sd = 1)
  mod <- lm(y ~ x)
  p_valores[i] <- summary(mod)$coefficients[2, 4]
  print(i)
}
mean(p_valores < 0.05)


# Ejercicio 5) Sobre el tamaño del efecto...
n <- 600
# peso ~ perimetro_cabeza + estatura
perimetro_cabeza_cm <- rnorm(n, 55, 2)
estatura_cm <- rnorm(n, 170, 10)


b0 <- 5     # Intercepto
b1 <- 0.6   # Efecto de perímetro de la cabeza
b2 <- 0.5   # Efecto de la estatura
sigma <- 3

mu <- b0 + b1*perimetro_cabeza_cm + b2*estatura_cm
peso <- rnorm(n, mu, sigma)

estatura_m  <- estatura_cm / 100
dat <- data.frame(peso, perimetro_cabeza_cm, estatura_cm, estatura_m)

m_m  <- lm(peso ~ scale(perimetro_cabeza_cm) + scale(estatura_m), data = dat)
m_cm <- lm(peso ~ perimetro_cabeza_cm + estatura_cm, data = dat)

summary(m_m)$coef
summary(m_cm)$coef

# Ejercicio 6) Plots diagnóstico.
plot(m1)

# Ejercicio 7) Bondad de ajuste.

summary(m1)$deviance        # Deviance del modelo
summary(m1)$null.deviance   # Deviance del modelo nulo (solo intercepto)

1 - (m1$deviance / m1$null.deviance) # Pseudo R2

# Ejercicio 8) Modelo con heterogeneidad no modelada, no linealidad y outlier.
X1 <- runif(500, 40, 70)
X2 <- runif(500, 40, 70)

b0 <- -4
b1 <- 0.5
b2 <- -0.7
b3 <- -0.5
mu <- b0 + b1*X1 + b2*X2 + b3*X1^2
mu[500] <- -800
sigma_normal <- 3
respuesta <- rnorm(500, mean = mu, sd = sigma_normal)

datos <- data.frame(respuesta = respuesta, X1 = X1, X2 = X2)
datos2 <- datos[-500,]

m2 <- glm(respuesta ~ X1 + I(X1^2) + X2, family = "gaussian", data = datos2)
summary(m2)
sigma(m2)

plot(x=X1, y=respuesta)
abline(m2, lwd = 2, col = "darkred")

res_m2 <- residuals(m2)
hist(res_m2, breaks = 100)

plot(m2)


summary(m2)$deviance        
summary(m2)$null.deviance   

1 - (m2$deviance / m2$null.deviance)
