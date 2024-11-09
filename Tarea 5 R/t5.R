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
dummies <- model.matrix(~ Marca + GB + Minutos + Precio, data = perfil)[, -1]

# probando si funciona
#fit123 <- lm(consumidor[[2]] ~ dummies)
#crear la matriz que guarde los datos con 11 filas: 10 coeficientes(dummies) y el r2
utilidades_parciales<-matrix(NA, nrow = 11, ncol = n_con)
rownames(utilidades_parciales) <- c(colnames(dummies), "R2")
colnames(utilidades_parciales) <- c(colnames(consumidor[,-1]))

for (k in 1:n_con) {
  fit <- lm(consumidor[[k+1]] ~ dummies)
  utilidades_parciales[1:10, k] <- fit$coefficients[-1]
  utilidades_parciales[11,k]<- summary(fit)$r.squared
}

hist(utilidades_parciales[11,],
     main = "Histograma de R² de los consumidores", 
     xlab = "Valores de R²", 
     ylab = "Frecuencia", 
     col = "blue", 
     border = "black", 
     xlim = c(0.2, 1),
     breaks = 50)