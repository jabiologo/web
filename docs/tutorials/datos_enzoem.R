# ANOVA

lotes <- c((rep(0,42)),(rep(1,37)))
mu <- 1.015+(0.17*lotes)

densidad <- rnorm(length(mu), mean = mu, sd = 0.05)
hist(densidad)
shapiro.test(densidad)
shapiro.test(log(densidad))
lote <- c((rep("A",42)),(rep("B",37)))
guinnes <- data.frame(lote = lote, densidad = round(densidad,4))
head(guinnes)
boxplot(densidad ~ lote, data = guinnes)
t.test(densidad ~ lote, data = guinnes)

# ANOVA
# Simular datos con tamaño de muestra desbalanceado
set.seed(123)

ufc_pollo <- rpois(20, lambda = 1e6)    # grupo más contaminado
ufc_cerdo <- rpois(15, lambda = 5e5)    # nivel intermedio
ufc_bovino <- rpois(10, lambda = 1e5)   # grupo menos contaminado

# Unir los datos en un solo vector
ufc <- c(ufc_pollo, ufc_cerdo, ufc_bovino)
grupo <- factor(c(rep("Pollo", 20), rep("Cerdo", 15), rep("Bovino", 10)))

# Visualizar UFC sin transformación
boxplot(ufc ~ grupo, main = "UFC/g sin transformación", ylab = "UFC/g")

# ANOVA sin transformación
anova_raw <- aov(ufc ~ grupo)
summary(anova_raw)

# Transformar log10(UFC + 1)
log_ufc <- log10(ufc + 1)

# Visualizar UFC transformado
boxplot(log_ufc ~ grupo, main = "log10(UFC/g)", ylab = "log10(UFC/g)")

# ANOVA con transformación logarítmica
anova_log <- aov(log_ufc ~ grupo)
summary(anova_log)

# Verificar supuestos del ANOVA transformado
par(mfrow = c(1, 2))
plot(anova_log, which = 1)  # residuos vs valores ajustados (homogeneidad)
plot(anova_log, which = 2)  # Q-Q plot (normalidad)

# Pruebas formales (opcional)
shapiro.test(residuals(anova_log))     # normalidad de los residuos
bartlett.test(log_ufc ~ grupo)         # homogeneidad de varianzas

# Pruebas formales (opcional)
shapiro.test(residuals(anova_raw))     # normalidad de los residuos
bartlett.test(log_ufc ~ grupo)         # homogeneidad de varianzas

######################## U
# Crear los datos
infestacion <- c(1, 2, 1, 2, 3, 2, 1, 1, 2, 3,  # juveniles
                 2, 1, 1, 3, 2, 3, 2, 2, 3, 3,
                 2, 2, 3, 1, 2, 3, 2, 2, 3, 3)  # adultos
grupo <- c(rep("juvenil", 14), rep("adulto", 16))

garrapatas <- data.frame(infestacion = infestacion, edad = grupo)

# Analizar con Mann-Whitney U
wilcox.test(infestacion ~ grupo)
boxplot(infestacion ~ grupo)


######################## K-W
# Crear los datos
calidad <- c(2, 3, 2, 3, 2, 1, 4, 2, 3, 2,      # Tratamiento A
             3, 5, 3, 4, 4, 2, 4, 5, 4, 5,      # Tratamiento B
             1, 2, 2, 1, 2, 3, 2, 2, 1, 2)      # Tratamiento C
tratamiento <- c(rep("A", 10), rep("B", 10), rep("C", 10))

conservante <- data.frame(calidad = calidad, conservante = tratamiento)

# Analizar con Kruskal-Wallis
kruskal.test(calidad ~ tratamiento)
boxplot(calidad ~ tratamiento)
# dunnTest()

read.csv("~/IREC/docencia/24_25/ENZOEM/triazol.csv")
