library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ROCR)
library(pscl)
library(officer)
library(flextable)
data <- read_delim("Rtasks/Tarea 4 R/Datos Parque de Diversiones.csv", delim = ";", locale = locale(encoding = "Latin1"))
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

####PREGUNTA1
data$Region <- as.factor(data$Region)
colnames(data)[3]<-"Gasto_Historico"
colnames(data)[4]<-"Gasto_Mensual"
#------------------------------------------------------------------------------#
#modelo con todas las variables
modelo<-glm(Compra ~ Edad + Visitas + Gasto_Historico + Gasto_Mensual + 
              Sat_Sitio + Sat_Calidad + Sat_Precio + Sat_General + 
              Region + Cupon, data, family = "binomial")
summary(modelo)
R2_modelo <- 1- modelo$deviance/modelo$null.deviance
R2_modelo
BIC(modelo)
AIC(modelo)
#------------------------------------------------------------------------------#
modelo_sin_edad<-glm(Compra ~ Visitas+ Gasto_Historico  + Gasto_Mensual + 
                     Sat_Sitio + Sat_Calidad + Sat_Precio + Sat_General + 
                     Region + Cupon, data, family = "binomial") #sacando edad
summary(modelo_sin_edad)
R2_modelo_sin_edad <- 1- modelo_sin_edad$deviance/modelo_sin_edad$null.deviance
R2_modelo_sin_edad
BIC(modelo_sin_edad)
AIC(modelo_sin_edad)
#------------------------------------------------------------------------------#
modelo_sin_gh<-glm(Compra ~ Visitas + Gasto_Mensual + 
                     Sat_Sitio + Sat_Calidad + Sat_Precio + Sat_General + 
                     Region + Cupon, data, family = "binomial") #sacando gasto_historico
summary(modelo_sin_gh)
R2_modelo_sin_gh<- 1- modelo_sin_gh$deviance/modelo_sin_gh$null.deviance
R2_modelo_sin_gh
BIC(modelo_sin_gh)
AIC(modelo_sin_gh)
#------------------------------------------------------------------------------#
modelo_sin_sat_calidad<-glm(Compra ~ Visitas + Gasto_Mensual + 
                     Sat_Sitio + Sat_Precio + Sat_General + 
                     Region + Cupon, data, family = "binomial") #sacando sat_calidad
summary(modelo_sin_sat_calidad)
R2_modelo_sin_sat_calidad <- 1- modelo_sin_sat_calidad$deviance/modelo_sin_sat_calidad$null.deviance
R2_modelo_sin_sat_calidad
BIC(modelo_sin_sat_calidad)
AIC(modelo_sin_sat_calidad)
#------------------------------------------------------------------------------#
modelo_sin_sat_general<-glm(Compra ~ Visitas + Gasto_Mensual + 
                              Sat_Sitio + Sat_Precio+ 
                              Region + Cupon, data, family = "binomial") #sacando sat_general
summary(modelo_sin_sat_general)
R2_modelo_sin_sat_general <- 1- modelo_sin_sat_general$deviance/modelo_sin_sat_general$null.deviance
R2_modelo_sin_sat_general
BIC(modelo_sin_sat_general)
AIC(modelo_sin_sat_general)
#------------------------------------------------------------------------------#
modelo_sin_sat_sitio<-glm(Compra ~ Visitas + Gasto_Mensual + 
                              Sat_Precio+ 
                              Region + Cupon, data, family = "binomial") #sacando sat_sitio
summary(modelo_sin_sat_sitio)
R2_modelo_sin_sat_sitio <- 1- modelo_sin_sat_sitio$deviance/modelo_sin_sat_sitio$null.deviance
R2_modelo_sin_sat_sitio
BIC(modelo_sin_sat_sitio)
AIC(modelo_sin_sat_sitio)
#------------------------------------------------------------------------------#
#solo visitas, gasti_mensual, regionMidwest son siognificativas
#como regionMidwest es significativa, entonces la variable region es sognificativa
#cupon es significante pero al 10%


#Al ir sacando las variables de 1, hasta que todas seas significativas 
#la regresion final es: 
modelo2<-glm(Compra ~ Visitas + Gasto_Mensual + Sat_Precio + 
               Region + Cupon, data, family = "binomial")
summary(modelo2)
table<- round(summary(modelo2)$coefficients,4)
table<-as.data.frame(table)
table$Regresor <- rownames(table)
table <- table[, c("Regresor", "Estimate", "Std. Error", "z value", "Pr(>|z|)")]
tabla <- flextable(table)

# Crear documento de Word
doc <- read_docx()

# Agregar la tabla
doc <- body_add_flextable(doc, tabla)

# Guardar el documento
print(doc, target = "tablamodelo2.docx")



#hay que sacar AIC y BIC, BIC castiga más que AIC si ln(n) es mayor que k (n>7)
#para el caso usaremos el BIC porque castiga más por las variables utilizadas 


R2_modelo <- 1- modelo$deviance/modelo$null.deviance
R2_modelo
BIC(modelo)
AIC(modelo)


R2_modelo2 <- 1- modelo$deviance/modelo$null.deviance
R2_modelo2
BIC(modelo2)
AIC(modelo2)

pR2(modelo)[5] #maxima verosimilitud



#PREGUNTA 2
#si el B es 0.4 entonces e**0.4=2 , entonces la chance aumenta x2 dado un aumento en visita 
#la exponencial del B te da el aumento de chance x la variable
#la campaña NO debe ser dirigida a los que si o si van a comprar o a los que no compraran (probabilidad muy baja)
#viendo las regiones Midwest NO compra, asi no que vale la pena hacerle una campaña 


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#PREGUNTA3
#a la variable region no le ponemos sacar el promedio, asi que lo haremos sin ella 
modelo3<-glm(Compra ~ Visitas + Gasto_Mensual + Sat_Precio + 
               Cupon, data, family = "binomial")
summary(modelo3)

visitas <- 1:20
precio_medio <- mean(data$Sat_Precio)
gasto_medio <- mean(data$Gasto_Mensual)
cupon_medio <- mean(data$Cupon)
nuevo_data <- data.frame(Visitas = visitas, Sat_Precio = precio_medio, Gasto_Mensual =gasto_medio, Cupon=cupon_medio)


nuevo_data$prediccion <- predict(modelo3,nuevo_data,type="response")
ggplot(nuevo_data, aes(x = Visitas, y = prediccion)) +
  geom_line(color = "blue", size = 1) +  # Línea azul y más gruesa
  geom_point(color = "red", size = 2) +  # Puntos rojos en cada predicción
  labs(x = "Número de visitas", 
       y = "Probabilidad de compra",
       title = "Probabilidad de compra según el número de visitas") +
  theme_minimal() 

#grafico: no es lineal 

####PREGUNTA 4: 
#A menor punto de corte voy a tener mas accuracy 
data$prob =predict(modelo2, type="response")
data$pred25=as.numeric(data$prob>0.25)
t1=table(data$pred25, data$Compra)
prop.table(t1)



data$pred50=as.numeric(data$prob>0.50)
t2=table(data$pred50, data$Compra)
prop.table(t2)


#como la tabla tiene probabilídades para ver acurracy solo sumamos la diagonal 
#¿que es peor, falso negativo o falso positivo?



##PREGUNTA 5
pred=prediction(as.numeric(data$prob),data$Compra)
perf=performance(pred,"auc")
auc=round(perf@y.values[[1]],digits=3)
perf=performance(pred,"tpr","fpr")

df=data.frame(x=perf@x.values[[1]],y=perf@y.values[[1]])
ggplot(df)+aes(x=x,y=y)+geom_line(size=2,color="steelblue")+geom_line(aes(x,x),lty=2)+
  labs(x="Tasa Falso Positivo",y="Tasa Verdadero Positivo")+lims(x=c(0,1),y=c(0,1))




#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

