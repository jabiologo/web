# MALTHUS
# Parámetros
N0 <- 10
r  <- 0.3
# Tiempos
t <- seq(0, 50, length.out = 200)
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
