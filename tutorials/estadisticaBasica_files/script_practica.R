# Guardamos el archivo en un obejto que denominaremos "datos"
datos <- read.csv("https://raw.githubusercontent.com/jabiologo/web/master/tutorials/estadisticaBasica_files/datosPracticas.csv")

# Podemos utilizar la función str() para ver la estructura interna del data.frame
str(datos)

# Primeros elementos de la base de datos
head(datos)

# Histograma de nuestra variable dependiente (peso)
hist(datos$peso)

# Comprobamos la normalidad de nuestra variable
shapiro.test(datos$peso)  

# Cómo trabaja la función shapiro.test()?
help("shapiro.test")

# Diferencia entre medias
# ¿Quién pesa más, los machos o las hembras? Filtramos los datos usando tidyverse
datosHe <- datos %>% filter(sex == "Hembra")
datosMa <- datos %>% filter(sex == "Macho")

# Recordamos: 
# H0 (hipótesis nula): no hay diferencias significativas entre gurpos
# H1 (hipótesis alternativa): hay diferencias significativas entre gurpos

# Paramétrico: T-Student
t.test(datosHe$peso, y = datosMa$peso) 

# No paramétrico: U de Mann-Whitney
wilcox.test(datosHe$peso, y = datosMa$peso)

# Diferencia entre medias
# ¿Dónde pesan más? Ambientes urbanos, agrícolas o matorral

# Paramétrico: ANOVA
anova <- aov(peso ~ hab, data = datos)
summary(anova)

# Tukey PostHoc
TukeyHSD(anova, conf.level=.95)

# Plots (gráficos)
library(effects)
plot(allEffects(anova))
boxplot(peso ~ hab, data = datos)

# No paramétrico: Kruskal-Wallis
kruskal.test(peso ~ hab, data = datos)

# ¿Cómo afecta la concentración de fungidida al peso?
# Correlación entre dos variables continuas

# Paramétrico: Correlación de Pearson
cor.test(datos$tri, datos$peso, method = "pearson")

# No paramétrico: Correlación de Spearman
cor.test(datos$tri, datos$peso, method = "spearman")

# Regresión
regresion <- lm(peso ~ tri, data = datos)

# Gráfico de dispersión (scatter-plot)
plot(datos$tri, datos$peso)

################################################################################
# <3 Common statistical tests are linear models (or: how to teach stats) <3
# https://lindeloev.github.io/tests-as-linear/ <3

# Test paramétricos como modelos lineales

# ¿Quién pesa más, los machos o las hembras?
m1 <- lm(peso ~ sex, data = datos)
summary(m1)

# ¿Dónde pesan más? Ambientes urbanos, agrícolas o matorral
m2 <- lm(peso ~ hab, data = datos)
summary(m2)

# ¿Cómo afecta la concentración de fungidida al peso?
m3 <- lm(peso ~ tri, data = datos)
summary(m3)

# ¿Cómo afectan la concentración de fungicida, el sexo y el habitat al peso?
m4 <- lm(peso ~ tri + sex + hab, data = datos)
summary(m4)

plot(allEffects(m4))

################################################################################

# ¿Qué efecto tendría un tamaño de muestra menor?
datosRed <- datos %>% sample_n(12)

m5 <- lm(peso ~ tri + sex + hab, data = datosRed)
summary(m5)
plot(allEffects(m5))


# Q-Q Plot
plot(quantile(rnorm(1000),probs = seq(0, 1, 0.01)), 
     quantile(sort(runif(1000)),probs = seq(0, 1, 0.01)))

