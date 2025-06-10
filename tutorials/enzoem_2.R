###############################################################
# Curso de Estadística ENZOEM 2025                            #
# José Antonio Blanco-Aguiar y Javier Fernández-López         #
# Módulo 2 - Test estadísticos en ciencias experimentales     #
# https://jabiologo.github.io/web/tutorials/enzoem_2.html     #
###############################################################

# Lo primero que debemos hacer es seleccionar nuestro directorio de trabajo
setwd("mi/directorio/de/trabajo")

##############################################################
# Práctica 1: simulación de distribuciones, visualización,   # 
# estadística descriptiva y Q-Q plots                        #
##############################################################

# Podemos simular 500 observaciones sacados de una distribución normal con
# media en 100 y desviación estandar 20
mi_normal <- 

# ¿Podrías graficar el resultado en un histograma?

# ¿Podrías obtener la media?

# ¿Podrías obtener la mediana?

# ¿Podrías obtener la moda?

# ¿Podrías obtener la varianza?

# ¿Podrías obtener la desviación estándar?

# Simula otras 500 observaciones que sigan una distribución de Poisson con 
# parámetro lambda igual a 4
mi_poisson <- 

# ¿Qué ocurre si intento un valor de lambda de -4?

# Simula otros 500 observaciones que sigan una distribución binomial con
# parámetro p igual a 0.6
mi_binomial <- 
  
# ¿Qué ocurre si intento un valor de p de 1.2?
  
# Vamos a utilizar ahora la base de datos denominada "warren_data.csv"
# Esta base de datos contiene conteos de madrigueras de conejos en los taludes
# de las autovías de Castilla-La Mancha.
warren <-   
  
# ¿Podrías comentar el tipo de cada una de las variables de la base de datos?
# Puedes utilizar head() o str() o ir inspeccionando variable a variable...
  
# Podrías darme los siguientes estadísticos descriptivos de todas las variables?
# Media, mediana, mínimo, máximo y desviación estándar.

# Vamos a trabajar ahora un poco con los cuantiles para entender esta operación
# ¿Podrías calcular los valores de "corte" si dividimos la variable "slope" en
# cuartiles? ¿Y en deciles? Puedes utilizar la función quantile()


# Un Q-Q plot, o un quantile-quantile plot sirve para comparar visualmente la 
# distribución de una variable con una distribución teórica. En resumen, compara
# los cuantiles de tu juego de datos con los cuantiles de una distribución
# teórica. Si tu variable sigue esa distribución, los cuantiles deberían coincidir
  
normal_ordenada <- sort(mi_normal)  
n <- length(normal_ordenada)

p <- (1:n - 0.5) / n  # posiciones de los cuantiles empíricos

cuantiles_teoricos <- qnorm(p)  # para comparar contra Normal(0,1)

plot(cuantiles_teoricos, normal_ordenada,
     main = "Q-Q Plot a mano",
     xlab = "Cuantiles teóricos N(0,1)",
     ylab = "Cuantiles de los datos",
     pch = 19, col = "blue")
abline(lm(normal_ordenada ~ cuantiles_teoricos), col = "red", lwd = 2)  # línea de ajuste

# También podemos usar la función qqplot()

# Prueba un Q-Q plot con tus variables generadas con otras distribuciones

# Prueba un Q-Q plot con un menor número de datos en mi_normal

###########################################################################
# Práctica 2: Contraste de Hipótesis. Test de normalidad de Shapiro-Wilk  #                      #
###########################################################################

# Ejecuta un test de Shapiro-Wilk sobre tu variable simulada mi_normal
# Puedes usar la función shapiro.test()  

# Ejecuta un test de Shapiro-Wilk sobre tu variable simulada mi_poisson

# Ejecuta un test de Shapiro-Wilk sobre las variables slope, roads, soil, 
# ag_forestry y warrens

# Prueba un test de Shapiro-Wilk con un menor numero de datos de mi_normal,
# ¿puedes implementarlo en un bucle?

#######################################################################
# Práctica 3: Visualización de valores anómalos: boxplot y Cleveland  #
#######################################################################

# La función boxplot nos ayuda a visualizar valores que se apartan de los valores
# más comunes en nuestra distribución. Para ello usamos aquellos valores que sean
# más extremos que los "bigotes". 
Q1 <- quantile(mi_normal, 0.25)
Q3 <- quantile(mi_normal, 0.75)
IQR <- Q3 - Q1
lim_inf <- Q1 - 1.5 * IQR
lim_sup <- Q3 + 1.5 * IQR

boxplot(mi_normal)


# El gráfico de Cleveland se basa en representar las diferencias absolutas con 
# respecto a una tendencia (como la media, mediana, etc)

# Calcular desviaciones absolutas con respecto a la mediana
desviaciones <- abs(mi_normal - median(mi_normal))

# Gráfico de Cleveland
plot(desviaciones, type = "h", lwd = 2, col = "steelblue",
     main = "Gráfico de Cleveland (outliers univariantes)",
     ylab = "Desviación absoluta respecto a la mediana",
     xlab = "Índice del dato")
abline(h = 3 * mad(mi_normal), col = "red", lty = 2)  # umbral de referencia

# Valores que superan 2 o 3 veces la MAD (mediana de desviaciones absolutas) 
# son sospechosos de ser outliers.

# Explora si existen valorea anómalos en las variables slope, roads, soil, 
# ag_forestry y warrens

#####################################################################
# Práctica 4: Estandarización, logaritmo, y programación funcional  #
#####################################################################

# Estandariza las variables mi_normal y mi_poisson. ¿Cuál es la media de las
# nuevas variables? ¿Cuál es su desviación estándar?

# Toma logaritmos a las variables slope, roads, soil, ag_forestry y warrens.
# ¿Hemos conseguido "normalizar" alguna?

# Serías capaz de crear una función para estandarizar variables?








