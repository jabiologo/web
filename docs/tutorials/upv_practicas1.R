#########################################################################
# Introducción a los Modelos Lineales: fundamentos teóricos y prácticos #
# Universidad del País Vasco, octubre-noviembre 2025                    #
#########################################################################

# MÓDULO 1: INTRODUCCIÓN A LA MODELIZACIÓN

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

# Ejercicio 2) Visualizar los histogramas.

# Ejercicio 3) Estudiar el efecto de diferentes tamaños muestrales.

# Ejercicio 4) Con los números aleatorios generados a partir de una distribución
# Normal/Gaussiana, estimar la meadia mean() y la desviación estándar sd().
# Estudiar el efecto del tamaño muestral.

# Ejercicio 5) Intentar ajustar un modelo lineal que recupere la media y la
# desviación estándar de una muestra de números aleatorios obtenidos de una
# distribución Normal/Gaussiana. Entender el efecto de diferentes tamaños
# muestrales

# 1.2) Introducción al modelo lineal
