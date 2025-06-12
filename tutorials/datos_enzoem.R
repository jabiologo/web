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
