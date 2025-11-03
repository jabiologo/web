#########################################################################
# Introducción a los Modelos Lineales: fundamentos teóricos y prácticos #
# Universidad del País Vasco, octubre-noviembre 2025                    #
#########################################################################

###########################################
# MÓDULO 2: EL MODELO LINEAL GENERALIZADO #
###########################################

# 2.1.1) Factores categóricos
# Simulamos la concentración de sal en lotes embutido de dos marcas (A y B)
lote <- rep(c("A","B"), each =200)
lote_dummy <- rep(c(0,1), each =200)
b0 <- 1.2
b1 <- 0.6

mu <- b0 + b1*lote_dummy
sal <- rnorm(n=400, mean=mu, sd = 0.3)
datos_embutido <- data.frame(sal = sal, lote = lote)
hist(datos_embutido$sal, breaks = 100)
boxplot(sal ~ lote)

m_sal <- glm(sal ~ lote, family = "gaussian", data = datos_embutido)
summary(m_sal)

# 2.1.2) Caso práctico, fungidicdas triazoles
# Guardamos el archivo en un obejto que denominaremos "datos"
datos <- read.csv("https://raw.githubusercontent.com/jabiologo/web/master/tutorials/estadisticaBasica_files/datosPracticas.csv")

# Podemos utilizar la función str() para ver la estructura interna del data.frame
str(datos) 


# Paquete para visualizar los efectos de los predictores
library(effects)

################################################################################
# 2.2) Funciones vínculo: más allá de la distribución normal.
temp <- runif(n=500, min=7, max=35)
temp_s <- scale(temp)
b0 <- 1
b1 <- -2.2

lambda <- exp(b0 + b1*temp_s)
ciervos <- rpois(n=500, lambda = lambda)
# ??

datos_ciervos <- data.frame(ciervos = ciervos, temp_s = temp_s)
  
m_ciervos <- glm(ciervos ~ temp_s, family = poisson(link = "log"), data = datos_ciervos)
summary(m_ciervos)  
  
# Prueba con presencia/ausencia y función vínculo logit
prob_logit <- b0 + b1*temp_s
prob <- exp(prob_logit)/(1+exp(prob_logit)) # Inverso de función logit
ciervos_pres <- rbinom(n=500, size = 1, prob = prob)

datos_ciervos$presencia <- ciervos_pres

m_ciervos_pres <- glm(presencia ~ temp_s, family = binomial(link = "logit"), data = datos_ciervos)
summary(m_ciervos)  
plot(datos_ciervos$temp_s, datos_ciervos$presencia, pch=19, cex=0.3)


