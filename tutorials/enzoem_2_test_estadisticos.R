###############################################################
# Curso de Estadística ENZOEM 2025                            #
# José Antonio Blanco-Aguiar y Javier Fernández-López         #
# Módulo 2 - Test estadísticos en ciencias experimentales     #
# https://jabiologo.github.io/web/tutorials/enzoem_2.html     #
###############################################################

##############################################
# Práctica 5: Principales test estadísticos  #
##############################################

################
# Chi-cuadrado #
################

# ¿La especie de ungulado está asociada a la presencia de enfermedad hemorrágica epizoótica (EHE)?

tabla_ehe <- as.table(rbind(c(30, 10, 13), c(20, 35, 16)))
rownames(tabla_ehe) <- c("afectados", "no_afectados")
colnames(tabla_ehe) <- c("ciervo", "gamo", "corzo")

tabla_ehe
chisq.test(tabla_ehe)

# ¿La marca de carne picada está asociada con la aparición de contaminación por listeria?

tabla_listeria <- as.table(rbind(c(11, 32, 27), c(1404, 5341, 2745)))
rownames(tabla_listeria) <- c("contaminada", "no_contaminada")
colnames(tabla_listeria) <- c("mercadona", "aldi", "coviran")

tabla_listeria
chisq.test(tabla_listeria)


