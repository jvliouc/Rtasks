#------------------------------------------------------------------------------#

#Pregunta 1
#primero importamos los datos desde enviroment->importar dataset
library(ggplot2)
library(cluster)
library(dplyr)
library(factoextra)
library(psych)
head(datos)
summary(datos)
#Para esta primera parte vamos a graficar el histograma para cada variable
ggplot(datos, aes(x = Recencia)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histograma de Recencia",
       x = "R",
       y = "frecuencia") +
       scale_x_continuous(breaks = seq(1, 12, by = 1), labels = seq(1, 12, by = 1)) +
  theme_minimal()
ggplot(datos, aes(x = Frecuencia)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Histograma de Frecuencia",
       x = "F",
       y = "frecuencia") +
       scale_x_continuous(breaks = seq(1, 12, by = 1), labels = seq(1, 12, by = 1)) + 
  theme_minimal()
ggplot(datos, aes(x = Monto)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black") +
  labs(title = "Histograma de Monto",
       x = "M",
       y = "frecuencia") +
       scale_x_continuous(breaks = seq(0, 120, by = 10), labels = seq(0, 120, by = 10)) +
  theme_minimal()
#------------------------------------------------------------------------------#

#Pregunta 2

# primero, vamos a filtrar las columnas RFM y luego las vamos a estandarizar
#mediante subset y as.data.frame creamos una nueva base de datos con RFM

RFM=subset(datos,select = c(Recencia,Frecuencia,Monto))
#luego otra base con las variables estandarizadas
RFM_s=as.data.frame(scale(RFM,center = TRUE, scale = TRUE))
#y con el siguiente summary visualizamos que esten estandarizadas
summary(RFM_s)
# posteriormente usamos kmeans y guardamos las distancias en el vector distancia y
#ajustamos por raiz cuadrada
dist=numeric(length=10)
for (i in 1:10){
  fit=kmeans(RFM_s,centers = i,nstart=10)
  dist[i]=sqrt(fit$tot.withinss)}
#luego lo ponemos en un dataframe para graficar con ggplot
r_k=data.frame(k=1:10,distancias=dist)
ggplot(r_k, aes(x = k, y = distancias)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 10, by = 1), labels = seq(1, 10, by = 1)) +
  geom_point() +
  labs(title = "Distancia interior",
       x = "número de segmentos",
       y = "distancia interior")
#------------------------------------------------------------------------------#
#Pregunta 3
# a partir del grafico, tenemos que el k óptimo es 4, entonces guardamos los datos
#en un vector
k_opt=kmeans(RFM_s, centers = 4, nstart=10)
centros=k_opt$centers
tamaño=k_opt$size
# y especialmente los segmentos a donde pertenece cada individuo
f_c=k_opt$cluster
#finalmente agregar la columna f_c que contiene los segmentos de cada persona
datos$cluster <- f_c
datos_segmentado=datos

#vamos a separar por segmento y a partir de eso las variables demográficas
s1=subset(datos,cluster==1)
s2=subset(datos,cluster==2)
s3=subset(datos,cluster==3)
s4=subset(datos,cluster==4)
summary(s1)
summary(s2)
summary(s3)
summary(s4)
#------------------------------------------------------------------------------#
#ANEXOS QUE NOS SIRVEN PARA ANALIZAR MEJOR LOS DATOS
#Grafico 3D

library(plotly)
plot_ly(datos_segmentado, x = ~Recencia, y = ~Frecuencia, z = ~Monto, color = ~as.factor(cluster), colors = c("red", "green", "blue", "purple"),
        type = "scatter3d", mode = "markers",
        marker = list(size = 5)) %>%
  layout(title = "Gráfico de Dispersión 3D Segmentado",
         scene = list(
           xaxis = list(title = "Recencia"),
           yaxis = list(title = "Frecuencia"),
           zaxis = list(title = "Monto")
         ))
#------------------------------------------------------------------------------#

resultadosSEX <- datos %>%
  group_by(cluster) %>%
  count(Sexo)
print(resultadosSEX,n=24)
ggplot2::ggplot(resultadosSEX, aes(x = Sexo, y = n, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribución de segmentos por Sexo",
       x = "Sexo",
       y = "frecuencia") +
  theme_minimal()
resultadosGSE <- datos %>%
  group_by(cluster) %>%
  count(GSE)
print(resultadosGSE,n=100)

ggplot2::ggplot(resultadosGSE, aes(x = GSE, y = n, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribución de segmentos por GSE",
       x = "GSE",
       y = "frecuencia") +
  theme_minimal()