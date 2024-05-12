# Inferencia Bayesiana en Ecología con R (IBER)
# Bloque 1.1: Modelos lineales generalizados con R: distribuciones de 
# probabilidad, programación, simulaciones y modelos básicos.

# R nos ofrece la posibilidad de simular números aleatorios obtenidos a partir
# de una distribución de probabilidad dada.

# 20 números aleatorios obtenidos a partir de una distribución normal de media=0
# y sd=1
rnorm(n=20, mean = 0, sd = 1)

# Podemos graficar un histograma de frecuencias de estos números, para comprobar
# cómo se distribuyen realmente los números simulados
hist(rnorm(n=20, mean = 0, sd = 1))

# Cuanto mayor sea la muestra de números aleatorios, más se parecerá el histograma
# a su distribución real

hist(rnorm(n=100000, mean = 0, sd = 1))
hist(rnorm(n=10, mean = 0, sd = 1))

# También podemos generar números aleatorios sacados de una distribución variando
# los parámetros de esa distribución.

# Queremos 1000 números aleatorios de los cuales 500 provengan de una distribución
# normal de media=-2 y sd=1 y otros 500 provengan de una distribución normal de
# media=2 y sd=1

mu <- rep(c(-2,2), each = 500)
hist(rnorm(n=1000, mean = mu, sd = 1), breaks = 20)

# En programación, una herramienta útil cuando queremos hacer tareas repetitivas
# son los bucles o loops. Los bucles son secuencia de instrucciones de código que 
# se ejecuta repetidas veces, hasta que una condición deja de cumplirse. Uno de 
# los bucles más utilizados es el denominado "for"

# Por ejemplo, podemos pedirle a R que nos imprima en pantalla los 5 primeros 
# números de una serie de 100 muestras a partir de una distribución normal 
# de media=0 y sd=1

numeros <- rnorm(n = 100, mean = 0, sd = 1)
for (i in 1:5){
  print(paste("El número",i, "es el", numeros[i]))
}

# Podemos consultar todas las distribuciones disponibles en R base en utilizando
# help("distribution"). Vamos a simular 1000 números obtenidos a partir de una
# distribución de Poisson con parámetro lambda=4

hist(rpois(n=1000, lambda = 4))
#hist(rpois(n=1000, lambda = -4))

# Como hemos visto, podemos hacer variar el parámetro de una distribución en
# función de una variable aleatoria (predictor). Vamos a simular 1000 números
# a partir de una distribución de Poisson cuyo parámetro lambda va a variar
# en función a una variable aleatoria (x1 ~ Uniforme entre -2 y 10), con 
# coeficientes b0=1 (intercepto) y b1=0.5 (pendiente).

x1 <- round(runif(1000,-2,10),1)
b0 <- 1
b1 <- 0.5
lam <- exp(b0 + b1 * x1) 
conteos <- rpois(1000, lam)

# Podemos ver la relación de nuestra variable predictora con los números aleatorios
# generados

plot(x1, conteos)

# Ahora podemos aplicar la maquinaria de los modelos lineales generalizados para
# ver si somos capaces de recuperar los parámetros simulados (b0 y b1)

datos <- data.frame(conteos = conteos, x1 = x1)
head(datos)
modelo <- glm(conteos ~ x1, family = poisson(link = "log"), data = datos)
summary(modelo)$coefficients

# Las simulaciones son interesantes porque permiten recrear condiciones controladas
# (laboratorio) que son útiles a la hora de realizar inferencias a partir de nuestro
# modelo. En palabras de Michael Schaub y Marc Kéry:
# 1) En las simulaciones la "verdad" es conocida, por tanto podemos estudiar si 
# el modelo produce estimas no sesgadas de los parámetros, investigar la violación 
# de asunciones, etc.
# 2) Útiles para hacer análisis de potencia (power análisis)
# 3) Se puede verificar la identificabilidad de nuestros parámetros.
# 4) Es una prueba de que has entendido el modelo.

datos100 <- datos[sample(1:1000,100),]
datos50 <- datos[sample(1:1000,50),]
datos10 <- datos[sample(1:1000,10),]
datos5 <- datos[sample(1:1000,5),]

m100 <- glm(conteos ~ x1, family = poisson(link = "log"), data = datos100)
m50 <- glm(conteos ~ x1, family = poisson(link = "log"), data = datos50)
m10 <- glm(conteos ~ x1, family = poisson(link = "log"), data = datos10)
m5 <- glm(conteos ~ x1, family = poisson(link = "log"), data = datos5)

summary(modelo)$coefficients
summary(m100)$coefficients
summary(m50)$coefficients
summary(m10)$coefficients
summary(m5)$coefficients

# Las simulaciones son ideales para entender ciertos fenómenos que siempre hemos
# escuchado en torno a la modelización:
# 1) ¿Qué pasa si mis datos no siguen la distribución que estoy utilizando?
# 2) ¿Qué ocurre si introduzco variables predictoras correlacionadas entre sí?
# 3) ¿Qué efecto puede tener la heterogeneidad no modelada?
# 4) ¿Cómo afectan a mis estimas los errores en mi base de datos?

