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
m1 <- glmmTMB(garrapatas ~ temp_escalada, family = "poisson", data = garrapatas)
summary(m1)

m2 <- glmmTMB(garrapatas ~ temp_escalada + coto -1, family = "poisson", data = garrapatas)
summary(m2)

m3 <- glmmTMB(garrapatas ~ temp_escalada + (1|coto) -1, family = "poisson", data = garrapatas)
summary(m3)

# Betas por coto del modelo de efectos fijos
b_fijos <- fixef(m2)$cond        # todos los coeficientes
b_fijos_coto <- b_fijos[grepl("^coto", names(b_fijos))]

# Interceptos por coto del modelo mixto
b0 <- fixef(m3)$cond["(Intercept)"]
u_coto <- ranef(m3)$cond$coto[,"(Intercept)"]
b_mix <- b0 + u_coto

# Bondad de ajuste, R2 de Nakagawa
library(performance)
r2_nakagawa(m3)
