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
# Visualización de datos
summary(datos_embutido$sal)
hist(datos_embutido$sal, breaks = 100)
boxplot(sal ~ lote)

m_sal <- glm(sal ~ lote, family = "gaussian", data = datos_embutido)
summary(m_sal)

# Podemos comprobar la convergencia de los modelos lineales con otros test
# clásicos como los ANOVA

anova_sal <- aov(sal ~ lote, data = datos_embutido)
summary(anova_sal)

# Comparamos coeficientes ajustados
coef(m_sal) # Modelo lineal
coef(anova_sal) # ANOVA

# Comparamos las tablas de análisis de la varianza
anova(m_sal)
anova(anova_sal)

###############################################
# 2.1.2) Caso práctico, fungidicdas triazoles #
###############################################
# Los fungicidas triazoles son compuestos químicos que se aplican habitualmente
# en semillas de cultivos para prevenir el crecimiento de hongos patógenos de 
# plantas. Sin embargo, cuando las semillas son consumidas por la fauna 
# silvestre, estos compuestos pueden producir efectos crónicos perjudiciales en 
# su sauld y desarrollo. Queremos estudiar el efecto de los fungicidas triazoles
# sobre la condición corporal (peso) en perdices rojas (Alectoris rufa). Para 
# ello se han capturado un total de 300 perdices en tres hábitats diferentes 
# (semiurbano, agrícola y monte matorralizado) a las que se les ha sexado y 
# extraido muestras de heces para obtener la concentración de fungicidas 
# triazoles (ngramos de compuesto/gramo de heces). 

# Guardamos el archivo en un obejto que denominaremos "datos"
datos <- read.csv("https://raw.githubusercontent.com/jabiologo/web/master/tutorials/estadisticaBasica_files/datosPracticas.csv")

# Podemos utilizar la función str() para ver la estructura interna del data.frame
str(datos) 

# Explora las diferentes variables tomadas. ¿De qué tipo son? ¿Cuál es mi
# variable respuesta? ¿Cuáles son mis predictores? Construye modelos que vayan
# desde lo más sencillo hasta lo más complejo. Explora los plots diagnóstico de
# las asunciones del modelo y comprueba la bondad de ajuste usando 

# Sexo: factor categórico de dos niveles
sum(datos$sex == "Hembra")
sum(datos$sex == "Macho")
boxplot(peso ~ sex, data = datos)

# Hábitat: factor categórico de tres niveles
sum(datos$hab == "Agricola")
sum(datos$hab == "Urbano")
sum(datos$hab == "Matorral")
boxplot(peso ~ hab, data = datos)

# Triazoles: covariable cuantitativa continua
summary(datos$tri)
hist(datos$tri, breaks = 100)

# Peso: covariable cuantitativa continua
summary(datos$peso)
hist(datos$peso, breaks = 100)

m1 <- glm(peso ~ sex, family = "gaussian", data = datos)
summary(m1) # Resumen del modelo
plot(m1) # Gráfico diagnóstico
1 - (m1$deviance / m1$null.deviance) 
# <0.2 mal; 0.2 < m1 < 0.4 regular; 0.4 < m1 < 0.6 bien; > 0.6 muy bien
# Mi modelo no es capaz de predecir correctamente mis datos (o por lo menos
# todo lo bien que me gustaría). Quiere decir que el ajuste (bondad de ajuste)
# no es muy buena.

m2 <- glm(peso ~ sex + hab, family = "gaussian", data = datos)
summary(m2) # Resumen del modelo
1 - (m2$deviance / m2$null.deviance) # Pseudo RSquare

m3 <- glm(peso ~ sex + hab + tri, family = "gaussian", data = datos)  
summary(m3) # Resumen del modelo
plot(m3)
1 - (m3$deviance / m3$null.deviance) # Pseudo RSquare
522.22294 + (28.29805*1) + (-10.71749*0) + (-16.21739*1) + (-2.84291*6)

# Bondad de ajuste: 1 - (m1$deviance / m1$null.deviance)

# Paquete para visualizar los efectos de los predictores
library(effects)

# Predicciones: comprueba que resolviendo la fórmula del modelo "a mano" se
# obtiene el mismo resultado que con la función predict().
nuevos_datos <- data.frame(sex = c("Hembra", "Macho", "Macho"),
                           hab = c("Agricola", "Urbano", "Matorral"),
                           tri = c(20, 6, 12))
predict(m1, nuevos_datos, type="response") # Sólo estamos teniendo en cuenta el sexo
predict(m2, nuevos_datos, type="response") # Tenemos en cuenta el sexo y el habitat
predict(m3, nuevos_datos, type="response") # Tenemos en cuenta todos los predictores

sigma(m3)

m4 <- glm(peso ~ tri, family = "gaussian", data = datos)
1 - (m4$deviance / m4$null.deviance) 

# Relación entre bondad de ajuste y complejidad del modelo (nº de parámetros
# estimados).

library(MuMIn) # Selección de modelos mediante AIC (o AICc)
AICc(m3)


##############################################################
# 2.2) Funciones vínculo: más allá de la distribución normal #
##############################################################

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

####################################################
# 2.3) Correlación/colinealidad en los predictores #
####################################################
set.seed(123)
library(car) # Necesario para la función VIF

# Simulación: escenario A (sin correlación entre predictores)
# Predictores poco correlacionados
x1 <- rnorm(100, 0, 1)
x2 <- rnorm(100, 0, 1)  # independiente de x1
cor(x1, x2)
plot(x1,x2)

# Predictor lineal
beta0 <- 1
beta1 <- 1
mu <- beta0 + beta1 * x1
y  <- rnorm(100, mean = mu, sd = 2)

# Ajuste del modelo con dos covariables no correlacionadas
m_low <- glm(y ~ x1 + x2)
summary(m_low)
1 - (m_low$deviance / m_low$null.deviance)
vif(m_low)

# Simulación: escenario B (predictores correlacionados entre sí)
# Predictores muy correlacionados
x1 <- rnorm(100, 0, 1)
rho <- 0.9  # nivel de colinealidad
x2 <- rho * x1 + sqrt(1 - rho^2) * rnorm(100, 0, 1)
cor(x1, x2)
plot(x1,x2)

# Predictor lineal
beta0 <- 1
beta1 <- 1
mu <- beta0 + beta1 * x1
y  <- rnorm(n, mean = mu, sd = 2)
m_high <- glm(y ~ x1 + x2)
summary(m_high)
1 - (m_high$deviance / m_high$null.deviance)
vif(m_high)


###############################################
# 2.4) Modelización de relaciones no lineales #
###############################################
library(effects) # Graficar los efectos de las variables
library(mgcv) # Ajustar GAM
x1 <- rnorm(100, 0, 1)
beta0 <- 1
beta1 <- 1
beta2 <- -1
beta3 <- 1
mu <- beta0 + beta1 * x1 + beta2 * I(x1^2) + beta3 * I(x1^3)
y  <- rnorm(n, mean = mu, sd = 2)
plot(x1, y)

m1 <- glm()
m2 <- glm()
m3 <- glm()
summary(m1)
summary(m2)
summary(m3)
AIC(m1); AIC(m2); AIC(m3)

plot(allEffects(m3))

m_gam <- gam(y ~ s(x1), method = "REML")
summary(m_gam)
plot(m_gam, shade = TRUE)

#######################################################
# 2.4) Caso de estudio, invernada de aves en portugal #
#######################################################
library(terra) # Manejo de rasters

# Cargo covariables en formato ráster y los datos
covar <- terra::unwrap(readRDS(url("https://raw.githubusercontent.com/jabiologo/web/master/tutorials/caso2_covar.rds")))
datos <- readRDS(url("https://raw.githubusercontent.com/jabiologo/web/master/tutorials/caso2_datos.rds"))

summary()
boxplot()
plot()

# Estudio de la correlación entre covariables
library(corrplot)
corrplot(cor(datos[,3:8]), method ="number")

# Ajuste de modelo completo
m_full <- glm(, family=binomial(link = "logit"), data = datos, na.action = na.fail)
summary(m_full)
vif(m_full)

# Evaluación del modelo
library(DHARMa) # Diagnostics for Hierarchical And Regression Models (el camino correcto)
library(pROC) # Receiver Operating Characteristic
plot(m_full)
sim_res <- simulateResiduals(m_sel)
plot(sim_res)

# TPR (True Positive Rate) = sensibilidad
# FPR (False Positive Rate) = 1 – especificidad
1 - (m_full$deviance / m_full$null.deviance)
roc(datos$pres, fitted(m_full))$auc

# Selección de modelos mediante AIC
library(MuMIn) # Multi Model INference, selección de modelos
modelos <- dredge(m_full)

##########################
# Relaciones no lineales #
##########################
m_full <- glm(, family=binomial(link = "logit"), data = datos, na.action = na.fail)
summary(m_full)
