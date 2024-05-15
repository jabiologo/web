# Si hemos cargado los paquetes necesarios anteriormente, no es necesario volver
# a cargarlos. Los paquetes que utilizaremos terra, unmarked y AICcmodavg
# Leemos nuestros datos en formato CVS.
datos <- read.csv("https://github.com/jabiologo/web/raw/master/tutorials/tallerSECEM_files/nmixDatos.csv")
# Si hemos descargado previamente los datos, podemos indicar la ruta (la carpeta
# en nuestro ordenador donde se localizan los archivos) y cargarlos desde ahí
# datos <- read.csv("mi/ruta/nmixDatos.csv").

# Visualizamos las primeras filas de nuestros datos.
head(datos)

# Cargamos las capas raster correspondientes a nuestras covariables predictoras,
# en este caso tipo de cobertura de vegetación y temperatura media de cada sitio.
cober <- rast("https://github.com/jabiologo/web/raw/master/tutorials/tallerSECEM_files/cober.tif")
tempe <- rast("https://github.com/jabiologo/web/raw/master/tutorials/tallerSECEM_files/tempe.tif")
# Si hemos descargado previamente las capas, podemos indicar la ruta (la carpeta
# en nuestro ordenador donde se localizan los archivos) y cargarlos desde ahí.
# cober <- rast("mi/ruta/cober.tif")
# tempe <- rast("mi/ruta/tempe.tif")
names(cober) <- "cober"
names(tempe) <- "tempe"

# Vamos a graficar estas capas para hacernos una idea de cómo son. Colocaremos 
# encima las localizaciones de nuestros conteos repetidos. Nótese que la 
# variable cobertura del suelo es categórica y cada númer corresponde a un tipo
# de cobertura:
# 1 = urbano
# 2 = agrícola
# 3 = transición
# 4 = bosque
par(mfrow = c(1,2))
plot(tempe, main = "Temperatura")
points(xyFromCell(tempe, datos$id), pch = 19, cex = 0.5)
plot(cober, main = "Cobertura del suelo")

# Estandarizamos la variable temperatura y convertimos a factor la variable de
# cobertura del suelo, ya que se trata de una variable categórica.
datos$tempe <- scale(tempe[datos$id])[1:100]
datos$cober <- as.factor(cober[datos$id][,1])

# Volvemos a visualizar las primeras filas de nuestro juego de datos.
head(datos)