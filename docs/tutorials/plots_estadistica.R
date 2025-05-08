# Datos de ejemplo
set.seed(123)
x <- 1:20
y <- 2 * x + rnorm(20, mean = 0, sd = 5)  # y con algo de ruido

# Modelo lineal
modelo <- lm(y ~ x)

# Predicciones
y_pred <- predict(modelo)

# Gráfico
plot(x, y, main = "Concepto de residuo", pch = 19, col = "blue",
     xlab = "Variable predictora o independiente (x)", ylab = "Variable respuesta o dependiente (y)")
abline(modelo, col = "red", lwd = 2)  # Línea de regresión

# Añadir residuos como líneas verticales
segments(x0 = x, y0 = y, x1 = x, y1 = y_pred, col = "darkgreen", lwd = 2.5)

# Opcional: leyenda
legend("topleft", legend = c("Datos observados", "Recta de regresión", "Residuos"),
       col = c("blue", "red", "darkgreen"), pch = c(19, NA, NA), 
       lty = c(NA, 1, 1), lwd = c(NA, 2, 1.5))



library(ggplot2)
library(dplyr)

# Datos de ejemplo
set.seed(123)
x <- 1:20
y <- 2 * x + rnorm(20, mean = 0, sd = 5)
datos <- data.frame(x = x, y = y)

# Ajustar modelo
modelo <- lm(y ~ x, data = datos)

# Agregar predicciones y residuos
datos <- datos %>%
  mutate(pred = predict(modelo),
         residuo = y - pred)

# Crear gráfico con leyenda
ggplot(datos, aes(x = x, y = y)) +
  # Línea de regresión
  geom_smooth(aes(color = "Recta de regresión (predicción)"), method = "lm", se = FALSE, size = 1.2) +
  
  # Puntos observados
  geom_point(aes(color = "Datos observados"), size = 3) +
  
  # Residuos
  geom_segment(aes(xend = x, y = pred, yend = y, color = "Residuos"),
               linetype = "dashed", linewidth = 1) +
  
  # Escala de colores manual para controlar los colores y el orden de la leyenda
  scale_color_manual(
    name = " ",
    values = c("Datos observados" = "#2980B9",
               "Recta de regresión (predicción)" = "#E74C3C",
               "Residuos" = "#27AE60"
               )
  ) +
  
  # Tema y etiquetas
  theme_minimal(base_size = 14) +
  labs(
    title = "Visualización de residuos en un modelo lineal",
    subtitle = "Líneas verdes punteadas indican residuos (diferencias entre valor observado y predicho)",
    x = "Variable independiente (x)",
    y = "Variable dependiente (y)"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

library(ggplot2)

# Datos de ejemplo: dos grupos con diferente media
set.seed(123)
grupo <- rep(c("A", "B"), each = 20)
valor <- c(rnorm(20, mean = 5, sd = 1), rnorm(20, mean = 7, sd = 1))
datos <- data.frame(grupo = grupo, valor = valor)

# Crear gráfico
ggplot(datos, aes(x = grupo, y = valor, fill = grupo)) +
  # Boxplot
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  
  # Puntos individuales
  geom_jitter(aes(color = grupo), width = 0.15, size = 2, alpha = 0.7) +
  
  # Media de cada grupo (como línea gruesa horizontal)
  #stat_summary(fun = mean, geom = "crossbar", width = 0.5, fatten = 2, color = "black") +
  
  # Estilo del gráfico
  scale_fill_manual(values = c("A" = "#3498DB", "B" = "#E74C3C")) +
  scale_color_manual(values = c("A" = "#2980B9", "B" = "#C0392B")) +
  
  theme_minimal(base_size = 14) +
  labs(
    title = "Comparación de medias entre dos grupos",
    #subtitle = "Boxplot con observaciones individuales y medias destacadas",
    x = "Variable predictora o independiente (x)",
    y = "Variable respuesta o dependiente (y)"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

