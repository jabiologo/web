###################################################################################
# Máster Universitario en Investigación Básica y Aplicada en Recursos Cinegéticos #
# 15-10-2024                                                                      #
# Estadística básica en ciencias experimentales: Introducción a R                 #
###################################################################################


# La almohadilla se utiliza para incluir un comentario. Todo lo que se encuentre
# después de una almohadilla no se ejecutará en la consola.
# Unir los números 3, 7, 12 y 4 en un único vector
c(3, 7, 12, 4)

# Obtener la media de los números 3, 7, 12 y 4
mean(c(3, 7, 12, 4))

# Descargamos e instalamos el paquete
install.packages("lme4")
# Cargamos el paquete en nuestra sesión
library(lme4)

# Almacenamos en un objeto llamado "numeros" el resultado de concatenar 3, 7, 12 y 4
numeros <- c(3, 7, 12, 4)

# Ahora podemos "llamar" a "numeros" para ver qué tiene dentro
numeros

# También podemos utilizar a "numeros" dentro de otra función, por ejemplo mean()
mean(numeros)

# Por último podemos guardar dentro de otro objeto el resultado de la linea anterior
m <- mean(numeros)
m

# Para averiguar la clase de un objeto usamos la función class()
class(numeros)

# Vamos a crear un objeto con los caracteres "a", "b" y "c"
letras <- c("a", "b", "c")

# Exploramos la clase de letras
class(letras)

# Vamos a intentar hacer la media de letras
mean(letras)

# Exploramos mi directorio de trabajo actual
getwd()

# Cambiamos el directorio de trabajo
setwd("/home/javifl/github/web/tutorials/estadisticaBasica_files")

# Guardamos el archivo en un obejto que denominaremos "datos"
datos <- read.csv("https://raw.githubusercontent.com/jabiologo/web/master/tutorials/estadisticaBasica_files/datosPracticas.csv")
class(datos)

# Podemos utilizar la función head() para explorar los primeros elementos de datos
head(datos)

# Podemos utilizar la función str() para ver la estructura interna del data.frame
str(datos)

# O incluso podemos pedirle un resumen con summary()
summary(datos)

# Visualizamos los nombres de las columnas de datos
colnames(datos)

# Visualizamos los 15 primeros elementos de la columna peso
datos$peso[1:15]

# Visualizamos el dato que está en la fila 3 y columna 4
datos[3,4]

# Visualizamos todos los datos de la fila 7
datos[7,]

# Instalamos (en caso necesario) y cargamos las librerías de tidyverse
# install.packages(tidiverse)
library(tidyverse)

# Tomamos el objeto datos, y lo filtramos dejando tan solo los machos
datosMacho <- datos %>% filter(sex == "Macho")

# Visualizamos los primeros elementos del nuevo objeto
head(datosMacho)

# También podemos obtener una muestra aleatoria de un número de filas
muestra11 <- datos %>% sample_n(11)

# Número de filas de muestra11
nrow(muestra11)
muestra11

# Vamos a intentar obtener la media del objeto letras
mean(letras)

# No entendemos por qué obtenemos este error... vamos a consultar la ayuda para
# entender qué necesita la función mean()
help(mean)

# Como vemos en el apartado Arguments, a la función mean() sólo se le pueden
# proporcionar objetos que sean de la clase numeric, logical, vectors, date,
# date-time y time_interval.
# Dado que la clase de letras es character, ahora entendemos por qué no podemos
# utilizar mean con letras.

# Guardamos el archivo en un obejto que denominaremos "datos"
datos <- read.csv("https://raw.githubusercontent.com/jabiologo/web/master/tutorials/estadisticaBasica_files/datosPracticas.csv")

# Podemos utilizar la función str() para ver la estructura interna del data.frame
str(datos)


