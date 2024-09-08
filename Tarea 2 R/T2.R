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
  geom_text(aes(label = Modelo), vjust = -0.5) +  # Añadir etiquetas de marcas
  scale_color_gradient(low = "orange", high = "blue")+
  geom_hline(yintercept = 0, linetype = "dotted") +  # Línea punteada en y=0
  geom_vline(xintercept = 0, linetype = "dotted") +  # Línea punteada en x=0
  xlab("Factor 1") +  # Etiqueta del eje X
  ylab("Factor 2") +  # Etiqueta del eje Y
  ggtitle("Mapa de Posicionamiento con 3 Factores") +  # Título
  theme_minimal() 

