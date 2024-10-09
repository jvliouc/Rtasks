library(readxl)
library(BTYDplus)
library(dplyr)
datos <- read_excel("Rtasks/Ejercicio arroz/Datos Arroz.xlsx")
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#pregunta 1 para las compras de 1TRIMESTRE(todas las marcas) calcular los 
#parametros del modelo BN
a_df<-data.frame(x=datos$aruba,T.cal=1)
m_df<-data.frame(x=datos$miraflores,T.cal=1)
t_df<-data.frame(x=datos$tucapel,T.cal=1)
#estimar los parametros lambda_i*4= a un año
#------------------------------------------------------------------------------#
params_a<-nbd.EstimateParameters(a_df)
#1
fiv_a=(1+1/params_a[2])
pen_a=1-(params_a[2]/(4+params_a[2]))^params_a[1]
compras_a=4*params_a[1]/params_a[2]
#2
lambda_a=mean(datos$aruba)
penetracion_a=1-dpois(0,lambda_a*4)
#------------------------------------------------------------------------------#
params_m<-nbd.EstimateParameters(m_df)
#1
fiv_m=(1+1/params_m[2])
pen_m=1-(params_m[2]/(4+params_m[2]))^params_m[1]
compras_m=4*params_m[1]/params_m[2]
#2
lambda_m=mean(datos$miraflores)
penetracion_m=1-dpois(0,lambda_m*4)
#------------------------------------------------------------------------------#
params_t<-nbd.EstimateParameters(t_df)
#1
fiv_t=(1+1/params_t[2])
pen_t=1-(params_t[2]/(4+params_t[2]))^params_t[1]
compras_t=4*params_t[1]/params_t[2]
#2
lambda_t=mean(datos$tucapel)
penetracion_t=1-dpois(0,lambda_t*4)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#pregunta 2 para tucapel
#poisson
lambda_t=mean(t_df$x)
pois_penetracion_t_1trimestre=1-dpois(0,lambda_t)

#bn: tiene una critica: supone que mi tasa de compra permanece constante para siempre
# es decir tiene intensidad de compra constante
bn_pen_t_1trimestre=1-(params_t[2]/(1+params_t[2]))^params_t[1]

#obs 1-proporcion de 0
t_df<- t_df %>%
  mutate(escero=ifelse(x>0,0,1))
ceros=sum(t_df$escero)
prop_ceros=ceros/1938
pen_obs_t=1-prop_ceros