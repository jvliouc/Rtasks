library(readxl)
library(dplyr)
perfil <- read_excel("Rtasks/Tarea 5 R/Tarea Análisis Conjunto Datos-1.xlsx", 
                     sheet = "Perfiles")
consumidor <- read_excel("Rtasks/Tarea 5 R/Tarea Análisis Conjunto Datos-1.xlsx", 
                         sheet = "Datos")
consumidor <- consumidor %>% #quitamos las filas que dicen NA
  slice(1:(n() - 2))

n_con<-373 #numero de consumidores
#primero crear las variables dummies necesarias ya que son todas categorias menos precio
dummies <- model.matrix(~ Marca + GB + Minutos + Precio, data = perfil)[,-1]

# probando si funciona
#fit123 <- lm(consumidor[[2]] ~ dummies)
#crear la matriz que guarde los datos con 12 filas: 11 coeficientes(dummies) y el r2

utilidades_parciales<-matrix(NA, nrow = ncol(dummies)+2, ncol = n_con)
rownames(utilidades_parciales) <- c("intercepto",colnames(dummies), "R2")
colnames(utilidades_parciales) <- c(colnames(consumidor[,-1]))

#claro, 15GB, 350min PERFIL BASE GENERADO POR R
# por lo tanto se deja la constante
for (k in 1:n_con) {
  fit <- lm(consumidor[[k+1]] ~ dummies)
  utilidades_parciales[1:11, k] <- fit$coefficients
  utilidades_parciales[12,k]<- summary(fit)$r.squared
}

hist(utilidades_parciales[12,],
     main = "Histograma de R² de los consumidores", 
     xlab = "Valores de R²", 
     ylab = "Frecuencia", 
     col = "blue", 
     border = "black", 
     xlim = c(0.2, 1),
     breaks = 50)
boxplot(utilidades_parciales[12, ], 
        main = "Boxplot de R2", 
        ylab = "Valores de R2", 
        col = "lightblue")
usuarios_alto_r2 <- sum(utilidades_parciales[12, ] >= 0.5)

usuarios_alto_r2/n_con*100