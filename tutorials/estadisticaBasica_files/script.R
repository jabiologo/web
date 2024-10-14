library(fastDummies)
set.seed(7)
#ngramos de triazoles/gramos de heces
cont <- rnorm(n = 300, mean = 25, sd = 10)
sex <- rbinom(n = 300, size = 1, prob = 0.5)
habF <- rep(0, 300)
for(i in 1:300){
  if(runif(1, 0, 1) < 0.5){
    habF[i] <- habF[i] + 1
    if(runif(1, 0, 1) < 0.5){
      habF[i] <- habF[i] + 1
    }
  }
}

habD <- dummy_cols(habF)[,2:4]
colnames(habD) <- c("urb", "agr", "mat")


mpeso <- 510 + 30*sex + 15*habD$agr + 5*habD$mat - 3*cont
peso <- round(rnorm(n = 300, mean = mpeso , sd = 15),2)
hist(peso)  
shapiro.test(peso)  

library(effects)
library(tidyverse)

df <- data.frame(peso = peso, sex = as.factor(sex), hab = as.factor(habF), tri = cont)
m0 <- glm(peso ~ sex + tri + hab, data = df)
summary(m0)
dfSmall <- df %>% sample_n(40)

hist(df$peso)  
shapiro.test(df$peso)  
hist(dfSmall$peso)
shapiro.test(dfSmall$peso)  

################################################################################
# T de Student 
dfHe <- df %>% filter(sex == 0)
dfMa <- df %>% filter(sex == 1)

dfHeS <- dfSmall %>% filter(sex == 0)
dfMaS <- dfSmall %>% filter(sex == 1)

t.test(dfHe$peso, y = dfMa$peso) 
t.test(dfHeS$peso, y = dfMaS$peso)

# U de Mann-Whitney 
wilcox.test(dfHe$peso, y = dfMa$peso)
wilcox.test(dfHeS$peso, y = dfMaS$peso)

# Modelo lineal
m1 <- lm(peso ~ sex, data = df)
summary(m1)
plot(allEffects(m1))
# Boxplot
boxplot(peso ~ sex, data = df)

# Modelo lineal
m1 <- lm(peso ~ sex, data = dfSmall)
summary(m1)
plot(allEffects(m1))
# Boxplot
boxplot(peso ~ sex, data = dfSmall)

################################################################################
# Anova
anova <- aov(peso ~ hab, data = df)
summary(anova)
# Tukey PostHoc
TukeyHSD(anova, conf.level=.95)
# Plots
plot(allEffects(anova))
boxplot(peso ~ hab, data = df)
# Kruskal-Wallis
kruskal.test(peso ~ hab, data = df)
# Modelo lineal
m2 <- lm(peso ~ hab, data = df)
summary(m2)
plot(allEffects(m2))

################################################################################

cor.test(df$tri, df$peso, method = "pearson")
cor.test(df$tri, df$peso, method = "spearman")

m3 <- lm(peso ~ tri, data = df)
summary(m3)
plot(allEffects(m3))

