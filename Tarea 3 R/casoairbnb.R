library(topicmodels)
library(tm)
library(wordcloud)
library(ggplot2)
library(syuzhet)
library(readr)
#------------------------------------------------------------------------------#
#libreria extra que me pidio R y itra oara agregar color a los mapas
library(RColorBrewer)
library(dplyr)
library(SnowballC)
#------------------------------------------------------------------------------#
#leer datos
reviews  <- read_csv("Rtasks/Tarea 3 R/Miami_reviews.csv")
listings <- read_csv("Rtasks/Tarea 3 R/Miami_listings.csv")
#------------------------------------------------------------------------------#
#copiando el codigo del cleansing del enunciado
docs<-iconv(reviews$comments,to="UTF-8")# corrige caracteres especiales [,2]
docs<-VCorpus(VectorSource(docs))# define un corpus o cuerpo en R para poder transformarlo
docs<-tm_map(docs,content_transformer(tolower))# inicio comandos cleansing
docs<-tm_map(docs,stripWhitespace)
docs<-tm_map(docs,removePunctuation)
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs,removeWords, stopwords("english"))#elimina conectores en ingles
docs<-tm_map(docs, stemDocument, language = "en") # fin comandos cleansing
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#                             Pregunta 1
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#Calcule los indicadores TF, IDF y TFIDF en forma agregada (para todos los documentos)
#mediante los siguientes comandos.
docs2=DocumentTermMatrix(docs) # cuenta la frecuencia de cada palabra del corpus anterior
m=as.matrix(docs2) #MATRIZ DE FRECUENCIAS
TF=apply(m,2,sum)/dim(m)[1] 
TF_df=as.data.frame(TF)
#..............................................................................#
#pruebas
sumtf=colSums(TF_df)

TF_norm<-TF_df%>%
  mutate(TF=TF/sumtf)
#..............................................................................#

df=apply(m>0,2,mean)
IDF=1+log10(1/df)
#------------------------------------------------------------------------------#
#grafico TF
palabras1=names(TF)
par(mar = c(1, 1, 1, 1))
set.seed(10)
wordcloud(words = palabras1,
          freq = TF*100, 
          min.freq = 1,      # Frecuencia mínima para mostrar una palabra
          max.words = 50, # Máximo número de palabras en la nube
          random.order = FALSE,  # Las palabras más frecuentes aparecen en el centro
          rot.per = 0.35,     # Proporción de palabras que se rotan a 90 grados
          scale = c(4, 0.5),
          colors = brewer.pal(8, "Paired"))
title(main = "TF", col.main = "Black", font.main = 4)
#------------------------------------------------------------------------------#
#grafico TFIDF
TFIDF=TF*IDF
palabras2=names(TFIDF)
par(mar = c(1, 1, 1, 1))
set.seed(10)
wordcloud(words = palabras2,
          freq = TFIDF*100,
          min.freq = 1,      # Frecuencia mínima para mostrar una palabra
          max.words = 50, # Máximo número de palabras en la nube
          random.order = FALSE,  # Las palabras más frecuentes aparecen en el centro
          rot.per = 0.35,     # Proporción de palabras que se rotan a 90 grados
          scale = c(4, 0.5),
          colors = brewer.pal(8, "Paired"))
title(main = "TFIDF", col.main = "Black", font.main = 4)
#------------------------------------------------------------------------------#
data=data.frame(terms=names(TF),TF_norm,IDF,TFIDF)
data_ord=data[order(data$TFIDF,decreasing=TRUE),][1:10,]
data_ord$terms=factor(data_ord$terms,levels=data_ord$terms[order(data_ord$TFIDF,decreasing=TRUE)])

ggplot(data_ord, aes(x = terms, y = TF)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Para que las barras sean horizontales
  labs(x = "Palabras", y = "TF", title = "Análisis TF primeras 10 palabras: Estandarizado") +
  theme_minimal()
ggplot(data_ord, aes(x = terms, y = IDF)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +  # Para que las barras sean horizontales
  labs(x = "Palabras", y = "IDF", title = "Análisis IDF primeras 10 palabras") +
  theme_minimal()
ggplot(data_ord, aes(x = terms, y = TFIDF)) +
  geom_bar(stat = "identity", fill = "seagreen") +
  coord_flip() +  # Para que las barras sean horizontales
  labs(x = "Palabras", y = "TFIDF", title = "Análisis TFIDF primeras 10 palabras") +
  theme_minimal()
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#                             Pregunta 2
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
n=length(docs)
docs1=rep(0,n)
for (i in 1:n){docs1[i]=docs[[i]]$content}
sentiment=get_sentiment(char_v=docs1, method="syuzhet")
sentiment_df <- data.frame(sentiment = sentiment)

# Crear un histograma con ggplot2
ggplot(sentiment_df, aes(x = sentiment)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Distribución de los Puntajes de Sentimiento",
       x = "Puntaje de Sentimiento",
       y = "Frecuencia") +
  scale_x_continuous(
    breaks = seq(0, 20, by = 1),
    labels = function(x) paste0("[", x, ",", x + 1, "[")
  ) +
  theme_minimal()
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#                             Pregunta 3
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

listings<-listings%>%
  mutate(Sentiment=sentiment)%>%
  mutate(log_occupancy1=log(occupancy+1))%>%
  mutate(log_price=log(price))

modelo_con_sentiment<-lm(log_occupancy1~log_price+Sentiment+rating,listings)
summary(modelo_con_sentiment)
modelo_sin_sentiment<-lm(log_occupancy1~log_price+rating,listings)
summary(modelo_sin_sentiment)



#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#                             Pregunta 4
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
docs2=DocumentTermMatrix(docs) #calcula la matrix de términos
docs3=removeSparseTerms(docs2, .99) #elimina palabras con baja frecuencia
sumr=rowSums(as.matrix(docs3)) #elimina documentos con cero palabras
docs3=docs3[sumr>0,]
fit=LDA(docs3,k=12,method="GIBBS",control=list(keep=10000,thin=1,iter=5000,burnin=2500)) # estima el modelo LDA 
terms=posterior(fit)$terms #probabilidades de términos
topics=posterior(fit)$topics # probabilidades de tópicos

