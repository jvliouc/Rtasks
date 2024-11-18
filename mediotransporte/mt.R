library(readxl)
library(mlogit)
data1 <- read_excel("Rtasks/mediotransporte/Ejemplo Transporte.xlsx", sheet="Hoja2")
data2 <- read_excel("Rtasks/mediotransporte/Ejemplo Transporte.xlsx", sheet="Hoja3")
data_mlogit <- mlogit.data(data1, choice = "Elección", shape = "long", 
                           alt.var = "Alternativa", id.var = "Individuo")

#modelo
fit<- mlogit(Elección ~ Tiempo | 0, data = data_mlogit)
summary(fit)
#prediccion
fitted(fit,type="probabilities")