# Paquetes
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# --- Preparación de datos y niveles de referencia ---
datos <- datos %>% 
  mutate(
    sex = factor(sex, levels = c("Hembra", "Macho")),
    hab = factor(hab, levels = c("Agricola", "Matorral", "Urbano"))
  )

# Centrar tri para que el intercepto sea interpretable
datos <- datos %>% mutate(tri_c = scale(tri, center = TRUE, scale = FALSE)[,1])

# --- Modelos (por si quieres usarlos luego) ---
m1 <- glm(peso ~ sex, data = datos)
m2 <- glm(peso ~ sex + hab, data = datos)
m3 <- glm(peso ~ sex + hab + tri, data = datos)  # misma pendiente para todos los grupos

# ===============================
# 1) Boxplot SOLO con sex
#    (β0 = media de Hembra; β1 = diferencia Macho - Hembra)
# ===============================
p1 <- ggplot(datos, aes(x = sex, y = peso, fill = sex)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  labs(
    title = "Paso 1: Solo sexo",
    subtitle = expression(paste(beta[0], " = media Hembra;  ", beta[1], " = (Macho - Hembra)")),
    x = "Sexo", y = "Peso"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

p1

# ===============================
# 2) Boxplot con sex + hab
#    6 cajas: (Hembra/Macho) x (Agrícola/Matorral/Urbano)
#    β0: Hembra-Agrícola
#    β1: Macho (vs Hembra) en Agrícola
#    β2: Matorral (vs Agrícola) en Hembra
#    β3: Urbano  (vs Agrícola) en Hembra
#    Combinaciones se interpretan por suma de efectos
# ===============================
datos <- datos %>%
  mutate(grupo = interaction(sex, hab, sep = " · "))

p2 <- ggplot(datos, aes(x = grupo, y = peso, fill = hab)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2.5, fill = "white") +
  labs(
    title = "Paso 2: Sexo + Hábitat (6 cajas)",
    subtitle = expression(paste(beta[0], ": Hembra-Agrícola;  ",
                                beta[1], ": Macho vs Hembra (en Agrícola);  ",
                                beta[2], ": Matorral vs Agrícola (en Hembra);  ",
                                beta[3], ": Urbano vs Agrícola (en Hembra)")),
    x = "Grupo (Sexo · Hábitat)", y = "Peso", fill = "Hábitat"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

p2

# ===============================
# 3) Efecto de tri (centrado) + sex + hab
#    6 rectas con MISMA pendiente (modelo sin interacciones con tri_c)
#    Idea: calcular predicciones del modelo m3 por grupo (sex×hab) en
#    una secuencia de tri_c y dibujar líneas por grupo.
# ===============================

# Secuencia de tri centrado para dibujar rectas
grid_tri <- tibble(tri = seq(min(datos$tri, na.rm = TRUE),
                               max(datos$tri, na.rm = TRUE),
                               length.out = 100))

# Todas las combinaciones de grupos (sex × hab)
grupos <- expand.grid(
  sex = levels(datos$sex),
  hab = levels(datos$hab),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

# Data de predicción
newdat <- grupos %>%
  as_tibble() %>%
  crossing(grid_tri) %>%
  mutate(grupo = interaction(sex, hab, sep = " · "))

# Predicciones con el modelo SIN interacciones con tri_c (misma pendiente)
newdat$pred <- predict(m3, newdata = newdat)

# Scatter + rectas por grupo
p3 <- ggplot() +
  geom_point(data = datos,
             aes(x = tri, y = peso, color = hab, shape = sex),
             alpha = 0.5, size = 2) +
  geom_line(data = newdat,
            aes(x = tri, y = pred, color = hab, linetype = sex),
            linewidth = 1) +
  labs(
    title = "Paso 3: Misma pendiente para las 6 rectas (modelo: peso ~ sex + hab + tri_c)",
    subtitle = "Interceptos distintos por grupo; pendiente común de tri_c",
    x = "tri", y = "Peso", color = "Hábitat", shape = "Sexo", linetype = "Sexo"
  ) +
  theme_minimal()

p3
