# MALTHUS
# Parámetros
N0 <- 10
r  <- 0.3
# Tiempos
t <- seq(0, 50, by = 0.2)
# Solución exacta
N <- N0 * exp(r * t)
plot(t, N, type="l", lwd=2,
     xlab="Tiempo", ylab="N",
     main="Modelo de Malthus (solución exacta)")

# VERHULST
# Parámetros
N0 <- 10
r  <- 0.1
K  <- 100
# Tiempos
t <- seq(0, 100, length.out = 200)
# Solución exacta
N <- K / (1 + ((K - N0) / N0) * exp(-r * t))
plot(t, N, type="l", lwd=2,
     xlab="Tiempo", ylab="N",
     main="Modelo logístico (solución exacta)")
abline(h = K, lty=2)

# LOTKA-VOLTERRA
library(deSolve)
lotka <- function(t, state, parms) {
  with(as.list(c(state, parms)), {
    dP <- a * P - b * P * D      # presa / plaga
    dD <- -c * D + d * P * D     # depredador
    list(c(dP, dD))
  })
}
# Parámetros
parms <- c(a = 1, b = 0.05, c = 0.5, d = 0.02)
# Condiciones iniciales
state <- c(P = 40, D = 9)
# Tiempo
times <- seq(0, 100, by = 0.1)
# Resolver
out_lv <- ode(y = state, times = times, func = lotka, parms = parms)
out <- as.data.frame(out_lv)
# Plot conjunto
plot(out$time, out$P, type = "l", lwd = 2, col = "blue",
     xlab = "Tiempo", ylab = "Abundancia",
     main = "Modelo Lotka–Volterra: Presa y Depredador")
lines(out$time, out$D, lwd = 2, col = "red", lty = 2)
legend("topright",
       legend = c("Presa / Plaga (P)", "Depredador (D)"),
       col = c("blue", "red"),
       lty = c(1, 2),
       lwd = 2, bty = "n")




library(deSolve)

# Creamos una función para simular la dinámica del modelo Lotka-Volterra
lotka <- function(t, state, parms) {
  with(as.list(c(state, parms)), {
    dP <- a * P - b * P * D      # presa / plaga
    dD <- -c * D + d * P * D     # depredador
    list(c(dP, dD))
  })
}

# Parámetros
parms <- c(
  a = 0.5,   # crecimiento plaga
  b = 0.01,  # tasa de depredación
  c = 0.5,   # mortalidad depredador
  d = 0.05   # conversión presa->depredador
)
# Condiciones iniciales
iniciales <- c(P = 15, D = 25)
# Tiempo
times <- seq(0, 20, by = 0.1)
# Resolver
out_lv <- ode(y = iniciales, times = times, func = lotka, parms = parms)
out <- as.data.frame(out_lv)
# Plot conjunto
plot(out$time, out$D, type = "l", lwd = 2, col = "blue",
     xlab = "Tiempo", ylab = "Abundancia",
     main = "Modelo Lotka–Volterra: Presa y Depredador", ylim = c(0,120))
lines(out$time, out$P, lwd = 2, col = "red", lty = 2)
legend("topleft",
       legend = c( "Depredador (D)", "Presa / Plaga (P)"),
       col = c("blue", "red"),
       lty = c(1, 2),
       lwd = 2, bty = "n")
