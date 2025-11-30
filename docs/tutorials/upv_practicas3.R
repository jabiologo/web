#########################################################################
# Introducción a los Modelos Lineales: fundamentos teóricos y prácticos #
# Universidad del País Vasco, octubre-noviembre 2025                    #
#########################################################################

################################################
# MÓDULO 3: MODELOS MIXTOS Y OTRAS EXTENSIONES #
################################################

# 3.1) Modelos mixtos (factores aleatorios)
# install.packages("glmmTMB")
library(glmmTMB)
garrapatas <- read.csv("https://raw.githubusercontent.com/jabiologo/web/master/tutorials/garrapatas_cotos.csv")
garrapatas$coto <- as.factor(garrapatas$coto)
summary(garrapatas$coto)

m1 <- glmmTMB(garrapatas ~ temp_escalada, family = "poisson", data = garrapatas)
summary(m1)
m2 <- glmmTMB(garrapatas ~ temp_escalada + coto -1, family = "poisson", data = garrapatas)
summary(m2)
m3 <- glmmTMB(garrapatas ~ temp_escalada + (1|coto) -1, family = "poisson", data = garrapatas)
summary(m3)

# Betas por coto del modelo de efectos fijos
b_fijos <- fixef(m2)$cond[2:27]     # todos los coeficientes
b_fijos_coto <- b_fijos[grepl("^coto", names(b_fijos))]

# Interceptos por coto del modelo mixto
b_random <- ranef(m3)$cond$coto[,1]

# Comparamos los valores estimados por cada modelo:
plot(b_fijos, b_random,
     xlab = "Estimas efectos fijos",
     ylab = "Estimas efectos aleatorios",
     main = "Comparación de estimas por coto",
     pch = 19, col = "steelblue")
abline(0, 1, lty = 2, col = "red")
cbind(b_fijos,b_random)

# AIC/BIC de los modelos (penalización: AIC=2*k; BIC=k*log(N))
AIC(m2);AIC(m3)
BIC(m2);BIC(m3)

# Bondad de ajuste, R2 de Nakagawa
library(performance)
r2_nakagawa(m3)

# Inestabilidad de coeficientes efectos fijos con poca muestra
set.seed(1)
# Tomamos una muestra aleatoria de 120 ciervos
gar_red <- garrapatas[sample(1:1300, 100),]
summary(gar_red$coto) # Vemos que hay cotos con muy pocos ciervos (2)

m1 <- glmmTMB(garrapatas ~ temp_escalada, family = "poisson", data = gar_red)
summary(m1)
m2 <- glmmTMB(garrapatas ~ temp_escalada + coto -1, family = "poisson", data = gar_red)
summary(m2)
m3 <- glmmTMB(garrapatas ~ temp_escalada + (1|coto) -1, family = "poisson", data = gar_red)
summary(m3)

# Betas por coto del modelo de efectos fijos
b_fijos <- fixef(m2)$cond[2:27]     # todos los coeficientes
b_fijos_coto <- b_fijos[grepl("^coto", names(b_fijos))]

# Interceptos por coto del modelo mixto
b_random <- ranef(m3)$cond$coto[,1]

# Comparamos los valores estimados por cada modelo:
plot(b_fijos, b_random,
     xlab = "Estimas efectos fijos",
     ylab = "Estimas efectos aleatorios",
     main = "Comparación de estimas por coto",
     pch = 19, col = "steelblue")
abline(0, 1, lty = 2, col = "red")
cbind(b_fijos,b_random)

# 3.2) Modelos mixtos y medidas repetidas
peso <- read.csv("https://raw.githubusercontent.com/jabiologo/web/master/tutorials/peso.csv")

# 3.3) Modelos mixtos y medidas repetidas
farmaco <- read.csv("https://raw.githubusercontent.com/jabiologo/web/master/tutorials/farmaco.csv")

# 3.4) Modelos mixtos, medidas repetidas y caso/control
longi <- read.csv("https://raw.githubusercontent.com/jabiologo/web/master/tutorials/logitudinal.csv")
