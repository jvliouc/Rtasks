library(readxl)
library(ggplot2)
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

var_f1 <- sum(loadings$Factor1)
var_f2 <- sum(loadings$Factor2)
var_f3 <- sum(loadings$Factor3)
print(c(var_f1,var_f2,var_f3))

ggplot(puntajes_m, aes(x = Factor1, y = Factor2, label = Modelo)) +
  geom_point() +  # Añadir puntos
  geom_text(vjust = -0.5, size = 2.5) +  # Añadir etiquetas de marcas
  geom_hline(yintercept = 0, linetype = "dotted") +  # Línea punteada en y=0
  geom_vline(xintercept = 0, linetype = "dotted") +  # Línea punteada en x=0
  xlab("Factor 1") +  # Etiqueta del eje X
  ylab("Factor 2") +  # Etiqueta del eje Y
  ggtitle("Mapa de Posicionamiento") +  # Título
  theme_minimal()  # Estilo minimalista

ggplot(puntajes_m, aes(x = Factor2, y = Factor3, label = Modelo)) +
  geom_point() +  # Añadir puntos
  geom_text(vjust = -0.5, size = 2.5) +  # Añadir etiquetas de marcas
  geom_hline(yintercept = 0, linetype = "dotted") +  # Línea punteada en y=0
  geom_vline(xintercept = 0, linetype = "dotted") +  # Línea punteada en x=0
  xlab("Factor 2") +  # Etiqueta del eje X
  ylab("Factor 3") +  # Etiqueta del eje Y
  ggtitle("Mapa de Posicionamiento") +  # Título
  theme_minimal()  # Estilo minimalista

ggplot(puntajes_m, aes(x = Factor1, y = Factor3, label = Modelo)) +
  geom_point() +  # Añadir puntos
  geom_text(vjust = -0.5, size = 2.5) +  # Añadir etiquetas de marcas
  geom_hline(yintercept = 0, linetype = "dotted") +  # Línea punteada en y=0
  geom_vline(xintercept = 0, linetype = "dotted") +  # Línea punteada en x=0
  xlab("Factor 1") +  # Etiqueta del eje X
  ylab("Factor 3") +  # Etiqueta del eje Y
  ggtitle("Mapa de Posicionamiento") +  # Título
  theme_minimal()  # Estilo minimalista

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

#------------------------------------------------------------------------------#
#c)
#estandarizar preferencias
pref_st<-as.data.frame(scale(preferencias[2:5]))
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
