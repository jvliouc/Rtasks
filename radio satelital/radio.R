library(readxl)
sus <- read_excel("Rtasks/radio satelital/radio.xlsx", 
                    sheet = "suscripcion")
coef<- read_excel("Rtasks/radio satelital/radio.xlsx", 
                  sheet = "coeficientes")
#publicidad y precio determinan la velocidad de adopcion BASS tipo II
#en cambio el BASS tipo I se preocupa de la tasa potencial, ambos modelos son complementarios
#modelo NORTON BASS, modelo de dos generaciones
#