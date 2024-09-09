library(readxl)
library(ggplot2)
#Aca lo primero que hicimos fue exportar las tablas de excel a base de datos que podamos trabajar
p_atributos <- read_excel("Tarea 2 R/tablas.xlsx", 
                     sheet = "Promedio atributos")
c_fisicas <- read_excel("Tarea 2 R/tablas.xlsx", 
                     sheet = "Caracteristicas físicas")
loadings <- read_excel("Tarea 2 R/tablas.xlsx", 
                     sheet = "Loadings")
puntajes_m <- read_excel("Tarea 2 R/tablas.xlsx", 
                     sheet = "Puntajes de cada marca")
preferencias <- read_excel("Tarea 2 R/tablas.xlsx", 
                     sheet = "Preferencias")
#------------------------------------------------------------------------------#
#Luego para la parte a) creamos los siguientes gráficos 
#grafico entre factor 1 y 2
ggplot(puntajes_m, aes(x = Factor1, y = Factor2, label = Modelo)) +
  geom_point() +  # Añadir puntos
  geom_text(vjust = -0.5, size = 2.5) +  # Añadir etiquetas de marcas
  geom_hline(yintercept = 0, linetype = "dotted") +  # Línea punteada en y=0
  geom_vline(xintercept = 0, linetype = "dotted") +  # Línea punteada en x=0
  xlab("Factor 1") +  # Etiqueta del eje X
  ylab("Factor 2") +  # Etiqueta del eje Y
  ggtitle("Mapa de Posicionamiento") +  # Título
  theme_minimal()  # Estilo minimalista
#------------------------------------------------------------------------------#
#grafico entre factor 2 y 3
ggplot(puntajes_m, aes(x = Factor2, y = Factor3, label = Modelo)) +
  geom_point() +  # Añadir puntos
  geom_text(vjust = -0.5, size = 2.5) +  # Añadir etiquetas de marcas
  geom_hline(yintercept = 0, linetype = "dotted") +  # Línea punteada en y=0
  geom_vline(xintercept = 0, linetype = "dotted") +  # Línea punteada en x=0
  xlab("Factor 2") +  # Etiqueta del eje X
  ylab("Factor 3") +  # Etiqueta del eje Y
  ggtitle("Mapa de Posicionamiento") +  # Título
  theme_minimal()  # Estilo minimalista
#------------------------------------------------------------------------------#
#grafico entre factor 1 y 3
ggplot(puntajes_m, aes(x = Factor1, y = Factor3, label = Modelo)) +
  geom_point() +  # Añadir puntos
  geom_text(vjust = -0.5, size = 2.5) +  # Añadir etiquetas de marcas
  geom_hline(yintercept = 0, linetype = "dotted") +  # Línea punteada en y=0
  geom_vline(xintercept = 0, linetype = "dotted") +  # Línea punteada en x=0
  xlab("Factor 1") +  # Etiqueta del eje X
  ylab("Factor 3") +  # Etiqueta del eje Y
  ggtitle("Mapa de Posicionamiento") +  # Título
  theme_minimal()  # Estilo minimalista
#------------------------------------------------------------------------------#
#grafico entre factor 3D con colores
ggplot(puntajes_m, aes(x = Factor1, y = Factor2, color = Factor3)) +
  geom_point(size = 4) +  # Añadir puntos con tamaño específico
  geom_text(aes(label = Modelo), vjust = -0.5, size = 2.5) +  # Añadir etiquetas de marcas
  scale_color_gradient(low = "orange", high = "blue")+
  geom_hline(yintercept = 0, linetype = "dotted") +  # Línea punteada en y=0
  geom_vline(xintercept = 0, linetype = "dotted") +  # Línea punteada en x=0
  xlab("Factor 1") +  # Etiqueta del eje X
  ylab("Factor 2") +  # Etiqueta del eje Y
  ggtitle("Mapa de Posicionamiento con 3 Factores") +  # Título
  theme_minimal() 


#-----------------------------------------------------------------#
#----------------Solucion con 2 factores--------------------------#
#-----------------------------------------------------------------#
#c)
#realizar los modelos con los factores
#prueba
pref_scaled<-as.data.frame(scale(preferencias[2:4]))
pref_fact_test<-cbind(pref_scaled,puntajes_m)
#
regresion_s11 <- lm(Segmento1~Factor1+Factor2,pref_fact_test)
regresion_s21 <- lm(Segmento2~Factor1+Factor2,pref_fact_test)
regresion_s31 <- lm(Segmento3~Factor1+Factor2,pref_fact_test)
f11<-c(coef(regresion_s11),R2=summary(regresion_s11)$r.squared)
f21<-c(coef(regresion_s21),R2=summary(regresion_s21)$r.squared)
f31<-c(coef(regresion_s31),R2=summary(regresion_s31)$r.squared)
vectores1 <- data.frame(Segmento1=f11[2:4],Segmento2=f21[2:4],Segmento3=f31[2:4])
v1<-as.data.frame(t(vectores1))
v1$Factor1 <- v1$Factor1*v1$R2
v1$Factor2 <- v1$Factor2*v1$R2
segmentos1 <- as.data.frame((v1[1:2]))
segmentos1$Modelo <- c("Segmento 1", "Segmento 2", "Segmento 3")
#------------------------------------------------------------------------------#
mapa_sin_vectores121<-ggplot(puntajes_m, aes(x = Factor1, y = Factor2, label = Modelo)) +
  geom_point() +  # Añadir puntos
  geom_text(vjust = -0.5, size = 2.5) +  # Añadir etiquetas de marcas
  geom_hline(yintercept = 0, linetype = "dotted") +  # Línea punteada en y=0
  geom_vline(xintercept = 0, linetype = "dotted") +  # Línea punteada en x=0
  xlab("Factor 1") +  # Etiqueta del eje X
  ylab("Factor 2") +  # Etiqueta del eje Y
  ggtitle("Mapa de Posicionamiento") +  # Título
  scale_x_continuous(limits = c(-1.5, 1.5),breaks = seq(-1.5, 1.5, by = 0.2)) +  # Ajustar límites del eje x
  scale_y_continuous(limits = c(-1.5, 1.5),breaks = seq(-1.5, 1.5, by = 0.2)) +  # Ajustar límites del eje y
  theme_minimal()  # Estilo minimalista
mapa_segmentos121<-mapa_sin_vectores121+
  geom_segment(data = segmentos1, aes(x = 0, y = 0, xend = Factor1, yend = Factor2),
               arrow = arrow(type = "closed", length = unit(0.1, "inches")),
               color = c("red","blue","orange"), size = 0.1) +
  geom_text(data = segmentos1, aes(x = Factor1, y = Factor2, label = Modelo),
            color =c("red","blue","orange"), vjust = -0, hjust = -0)

mapa_segmentos121



#_________________________________________________________________#

#----------------Solucion con 3 factores--------------------------#
#__________No ocupamos esta parte dado que se eligieron 2 factores#
#c)
#realizar los modelos con los factores para cada segmento
pref_fact<-merge(preferencias,puntajes_m)
regresion_s1 <- lm(Segmento1~Factor1+Factor2+Factor3,pref_fact)
regresion_s2 <- lm(Segmento2~Factor1+Factor2+Factor3,pref_fact)
regresion_s3 <- lm(Segmento3~Factor1+Factor2+Factor3,pref_fact)
f1<-c(coef(regresion_s1),R2=summary(regresion_s1)$r.squared)
f2<-c(coef(regresion_s2),R2=summary(regresion_s2)$r.squared)
f3<-c(coef(regresion_s3),R2=summary(regresion_s3)$r.squared)
vectores <- data.frame(Segmento1=f1[2:5],Segmento2=f2[2:5],Segmento3=f3[2:5])
df_escalado <- vectores[-nrow(vectores), ]
df_escalado <- scale(df_escalado)
ultima_fila <- vectores[nrow(vectores),]
df_final <- rbind(df_escalado, ultima_fila)
v<-as.data.frame(t(df_final))
v$Factor1 <- v$Factor1*v$R2
v$Factor2 <- v$Factor2*v$R2
v$Factor3 <- v$Factor3*v$R2
segmentos<--as.data.frame(t(df_escalado))
segmentos$Modelo <- c("Segmento 1", "Segmento 2", "Segmento 3")
#------------------------------------------------------------------------------#
mapa_sin_vectores12<-ggplot(puntajes_m, aes(x = Factor1, y = Factor2, label = Modelo)) +
  geom_point() +  # Añadir puntos
  geom_text(vjust = -0.5, size = 2.5) +  # Añadir etiquetas de marcas
  geom_hline(yintercept = 0, linetype = "dotted") +  # Línea punteada en y=0
  geom_vline(xintercept = 0, linetype = "dotted") +  # Línea punteada en x=0
  xlab("Factor 1") +  # Etiqueta del eje X
  ylab("Factor 2") +  # Etiqueta del eje Y
  ggtitle("Mapa de Posicionamiento") +  # Título
  scale_x_continuous(limits = c(-1.5, 1.5),breaks = seq(-1.5, 1.5, by = 0.2)) +  # Ajustar límites del eje x
  scale_y_continuous(limits = c(-1.5, 1.5),breaks = seq(-1.5, 1.5, by = 0.2)) +  # Ajustar límites del eje y
  theme_minimal()  # Estilo minimalista
mapa_segmentos12<-mapa_sin_vectores12+
  geom_segment(data = segmentos, aes(x = 0, y = 0, xend = Factor1, yend = Factor2),
               arrow = arrow(type = "closed", length = unit(0.1, "inches")),
               color = "red", size = 0.1) +
  geom_text(data = segmentos, aes(x = Factor1, y = Factor2, label = Modelo),
            color = "red", vjust = -0, hjust = -0)

mapa_segmentos12
#------------------------------------------------------------------------------#
mapa_sin_vectores23<-ggplot(puntajes_m, aes(x = Factor2, y = Factor3, label = Modelo)) +
  geom_point() +  # Añadir puntos
  geom_text(vjust = -0.5, size = 2.5) +  # Añadir etiquetas de marcas
  geom_hline(yintercept = 0, linetype = "dotted") +  # Línea punteada en y=0
  geom_vline(xintercept = 0, linetype = "dotted") +  # Línea punteada en x=0
  xlab("Factor 2") +  # Etiqueta del eje X
  ylab("Factor 3") +  # Etiqueta del eje Y
  ggtitle("Mapa de Posicionamiento") +  # Título
  scale_x_continuous(limits = c(-1.5, 1.5),breaks = seq(-1.5, 1.5, by = 0.2)) +  # Ajustar límites del eje x
  scale_y_continuous(limits = c(-1.5, 1.5),breaks = seq(-1.5, 1.5, by = 0.2)) +  # Ajustar límites del eje y
  theme_minimal()  # Estilo minimalista
mapa_segmentos23<-mapa_sin_vectores23+
  geom_segment(data = segmentos, aes(x = 0, y = 0, xend = Factor2, yend = Factor3),
               arrow = arrow(type = "closed", length = unit(0.1, "inches")),
               color = "red", size = 0.1) +
  geom_text(data = segmentos, aes(x = Factor2, y = Factor3, label = Modelo),
            color = "red", vjust = -0, hjust = -0)

mapa_segmentos23
#------------------------------------------------------------------------------#
mapa_sin_vectores13<-ggplot(puntajes_m, aes(x = Factor1, y = Factor3, label = Modelo)) +
  geom_point() +  # Añadir puntos
  geom_text(vjust = -0.5, size = 2.5) +  # Añadir etiquetas de marcas
  geom_hline(yintercept = 0, linetype = "dotted") +  # Línea punteada en y=0
  geom_vline(xintercept = 0, linetype = "dotted") +  # Línea punteada en x=0
  xlab("Factor 1") +  # Etiqueta del eje X
  ylab("Factor 3") +  # Etiqueta del eje Y
  ggtitle("Mapa de Posicionamiento") +  # Título
  scale_x_continuous(limits = c(-1.5, 1.5),breaks = seq(-1.5, 1.5, by = 0.2)) +  # Ajustar límites del eje x
  scale_y_continuous(limits = c(-1.5, 1.5),breaks = seq(-1.5, 1.5, by = 0.2)) +  # Ajustar límites del eje y
  theme_minimal()  # Estilo minimalista
mapa_segmentos13<-mapa_sin_vectores13+
  geom_segment(data = segmentos, aes(x = 0, y = 0, xend = Factor1, yend = Factor3),
               arrow = arrow(type = "closed", length = unit(0.1, "inches")),
               color = "red", size = 0.1) +
  geom_text(data = segmentos, aes(x = Factor1, y = Factor3, label = Modelo),
            color = "red", vjust = -0, hjust = -0)

mapa_segmentos13