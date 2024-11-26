library(readxl)
library(dplyr)  
library(ggplot2)


df <- read_excel("Rtasks/gimnasio/NYHCSurvey_070201_10-26-2016_0.xlsx", 
                                             sheet = "NYHCSurvey")

#
precios=sequence(11, 50,by=10)

#demanda

df<-df%>%
  rowwise() %>%
  mutate(maximo = max(c_across(3:8), na.rm = TRUE)) %>%
  ungroup()

demandae=rep(0,11)
for (i in 1:11){
  p=precios[i]
  demandae[i]=sum(df$maximo>=p)
  
}
  
  
dato=data.frame(demanda=demandae, precio=precios)
ggplot(dato, aes(x = precio, y = demanda)) +
  geom_point(color = "blue") +          # Puntos del gr??fico
  labs(title = "Demanda vs Precio", 
       x = "Precio", 
       y = "Demanda") +
  theme_minimal()
#b
#lineal
modlin=lm(demanda~precio, dato) 
summary(modlin)$r.s
modexp=lm(log(demanda)~precio, dato) 
summary(modexp)$r.s
modlog <- nls(demanda~c*exp(a+b*precio)/(1+exp(a+b*precio)),start=list(c=2500,a=0.01, b=-0.001),dato)

predicciones <- predict(modlog, newdata = dato)
residuos <- dato$demanda - predicciones
variacion_total <- sum((dato$demanda - mean(dato$demanda))^2)
residuos_cuadrados <- sum(residuos^2)
r_squared <- 1 - residuos_cuadrados / variacion_total
r_squared


dato<-dato%>%
  mutate(pred=predicciones)
# Graficar los datos usando ggplot
ggplot(data = dato, aes(x = precio)) +
  # Agregar los puntos de demanda real
  geom_point(aes(y = demanda), color = "blue", size = 3) +
  # Agregar los puntos de predicci??n
  geom_point(aes(y = pred), color = "red", size = 3) +
  # Agregar l??neas para las predicciones
  geom_line(aes(y = pred), color = "red", lwd = 1.5) +
  # Personalizar el gr??fico
  labs(title = "Demanda Real vs Predicci??n",
       x = "Precio",
       y = "Demanda") +
  theme_minimal()
#c#c supondiendosupondiendo queque elel mejormejor modelomodelo eses log

dato<-dato%>%
  mutate(ingreso_estimado=precio*demanda)%>%
  mutate(ingreso_predicho=precio*pred)

ggplot(data = dato, aes(x = precio)) +
  geom_line(aes(y = ingreso_estimado, color = "Ingreso obs"), lwd = 1.5) +
  geom_line(aes(y = ingreso_predicho, color = "Ingreso predicho"), lwd = 1.5) +
  scale_color_manual(values = c("Ingreso obs" = "red", "Ingreso predicho" = "blue")) +
  scale_x_continuous(breaks = seq(50, 150, by = 10)) + 
  labs(title = "Comparacion ingresos observados vs predichos por el modelo logistico",
       x = "Precio",
       y = "Ingreso",
       color = "Tipo de Ingreso") +
  theme_minimal()


dato<-dato%>%
  mutate(elasticidad=summary(modlog)$coeff[[3]]*(1-(pred/summary(modlog)$coeff[[1]]))*precio)
  
  
max(dato$ingreso_predicho)
## aa=1. 2 