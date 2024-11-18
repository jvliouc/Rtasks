library(readxl)
library(ggplot2)
sus <- read_excel("Rtasks/radio satelital/radio.xlsx", 
                    sheet = "suscripcion")
coef<- read_excel("Rtasks/radio satelital/radio.xlsx", 
                  sheet = "coeficientes")
#publicidad y precio determinan la velocidad de adopcion BASS tipo II
#en cambio el BASS tipo I se preocupa de la tasa potencial, ambos modelos son complementarios
#modelo NORTON BASS, modelo de dos generaciones
# en t0 se lanza primera generacion
# en t2 se lanza segunda generacion
# los mercados potenciales son M1 y M1+M2, es decir el mercado 2 solo entra
# en la segunda genracion debido a que es un modelo mejorado (mayor satisfaccion)
#ahora hay 3 curvas/procesos evolucionando simultaneamente cuyos procesos de difusion son iguales
#solo que llegan a niveles distintos y en otros tiempos
#N1 es el numero de personas que adoptan la primera generacion
#N2 quienes compren la segunda generacion el tiempo esta desplazado, se repite la forma de la curva
# y quienes compraron en primera se estan pasando a la segunda generacion
#
#------------------------------------------------------------------------------#

#cual es el mercado para este producto?

# Tomando en cuenta 
#------------------------------------------------------------------------------#
#mi estrategia de precios va a ser de precios muy bajo, para captar la mayor cantidad de mercado,
# automovilistas
M=77.8 #millones
#------------------------------------------------------------------------------#

p=0.00605
q=0.660




#------------------------------------------------------------------------------#
n=40
M=30
N=rep(0,n)
for(i in 2:n){N[i]=N[i-1]+(p+q*(N[i-1]/M))*(M-N[i-1])}
df=data.frame(N,Año=2000:2039)

ggplot(df) +
  geom_line(aes(Año, N), size = 2, color = "steelblue") +
  xlab("Año") +
  ylab("Número de Adoptantes (Millones)") +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) +  # Formato sin decimales
  theme_minimal() +
  ggtitle("Difusión del Producto usando el Modelo de Bass")
#------------------------------------------------------------------------------#


