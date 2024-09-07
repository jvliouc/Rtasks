#estandarizamos los datos
library(psych)
library(ggplot2)
library(readr)
consultorio <- read_csv("Ejercicio en clases R/Datos Consultorio.csv")
View(consultorio)
seleccionados=subset(consultorio,select = -c(Sexo,GSE,Global,Cronico,Edad,Id))
datos_s=scale(seleccionados,center = TRUE, scale = TRUE)
r=cor(datos_s)
resultado <- cortest.bartlett(r, n = dim(datos_s)[1])
#pvalue casi 0 se rechaza la hip0 en favor de la significancia global
fit=principal(datos_s, nfactors=14,rotate="none")
var=fit$values
load=fit$loadings
com=fit$communality
factores=fit$scores
var_tab=as.data.frame(var)

ggplot(var_tab, aes(x = c(1:14), y = var_tab$var)) +
  scale_x_continuous(breaks = seq(1, 14, by = 1), labels = seq(1, 14, by = 1)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_point() + geom_line() + ggtitle("Varianza asociada a cada factor") +
  xlab("Factor") +
  ylab("Varianza ") +
  theme_minimal()


nd=consultorio
sc=as.data.frame(fit$scores)
datos_nuevo=cbind(nd,sc)

modelo1 <- lm(Global ~ PC1+PC2+PC3, data = datos_nuevo)
summary(modelo1)
modelo2 <- lm(Global ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14, data = datos_nuevo)
#------------------------Anexo------------------------------------------------#