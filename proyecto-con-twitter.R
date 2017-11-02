#install.packages(c("devtools", "rjson", "bit64", "httr"))
#library(devtools)
#install_github("geoffjentry/twitteR")
#install.packages(c('wordcloud','tm'),dependencies = T)

#librerías
library(devtools)
library(wordcloud)
library(tm)
library(twitteR)
library(magrittr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(dplyr)
library(igraph)
library(stringr)

# Accesos de Twitter
# Regístrense en https://apps.twitter.com/
# Den de alta una aplicación (app). Obtengan las llaves, tokens y secretos...
#Consumer Key (API Key)
consumer_key <- 'jGGAneo7mjdFXMJs18mxvYPmY'
#Consumer Secret (API Secret)
consumer_secret <- 'L3JlRMvkFSxacGm7T6YSp24CeWzYSEJEqfcvp1V2FJcr9EYaKf'
#Access Token
access_token <- '102567729-d7cJz4otCK3ycTSGoYgxrGhxpZb9Waa7Hvp0OTir'
#Access Token Secret
access_secret <- '4wklxXYZydAsjHhiN1uTe7CI4wADihgtvMXpfnVuJil4i'

# Solicitar acceso OAuth
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Pedir 1,500 tweets con esos datos
twitterBigData <- searchTwitter('bigdata', n=1500)
# También se puede usar un límite geográfico: geocode='40.7361,-73.9901,5mi'

# Revisemos
head(twitterBigData)
length(twitterBigData)
str(twitterBigData)
str(twitterBigData[1])

#Accediendo a la riqueza de datos

# Tomemos el texto
as.data.frame(twitterBigData)
tweets <- c(unlist(lapply(twitterBigData,function(t) {t$text})))
head(tweets)



# Limpiemos
#Sin espacios
tweets.ok <- str_replace_all(tweets," "," ")
# Sin URLs
tweets.ok <- str_replace_all(tweets.ok, "http://t.co/[a-zA-Z0-9]{10}","")
# Sin Header de RT
tweets.ok <- str_replace(tweets.ok,"RT @[a-z,A-Z]*: ","")
# Sin hashtags
#tweets.ok <- str_replace_all(tweets.ok,"#[a-z,A-Z]*","")
# Sin nombres de usuario
tweets.ok <- str_replace_all(tweets.ok,"@[a-z,A-Z]*","")  
tweets.ok

# Quitemos links
link.regex <- " ?(f|ht)tp(s?)://(.*)[.][a-z/]+" #"http[://].* |https[://].* "
grep(link.regex,tweets.ok)
tweets.ok[grep(link.regex,tweets.ok)]
gsub(link.regex,'',tweets.ok)
tweets.ok[999]
tweets.ok2 <- gsub(link.regex," ",tweets.ok)
tweets.ok2

#Nube de palabras
palabras <- tweets2 %>% 
  strsplit(' ') %>%
  unlist %>%
  tolower
wordcloud(palabras,min.freq = 20)

#Análisis de Hashtags
soloHashtags <- palabras[grep('^#.*',palabras)] %>%
  gsub(pattern='#',replacement='') %>%
  data.frame
names(soloHashtags) <- 'hshtg'
head(soloHashtags)
soloHashtags %>%
  table
wordcloud(soloHashtags$hshtg,min.freq = 10)


# Minería de Texto
stopwords.ingles <- c(stopwords('english'))
head(stopwords.ingles)
#Carga de Corpus
corpus <- Corpus(VectorSource(tweets.ok2)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords,stopwords.ingles)
corpus

#matriz término-documento
tdm <- TermDocumentMatrix(corpus)
tdm

#Palabras más populares
findFreqTerms(tdm, lowfreq=60)

#Asociaciones
findAssocs(tdm, 'datascience', 0.2) 

#Clusterplot
tdm2 <- removeSparseTerms(tdm, sparse=0.9)
tdm2
tdm2.df <- tdm2 %>% inspect %>% as.data.frame
tdm2.df
nrow(tdm2.df)
ncol(tdm2.df)

tdm2.df.sc <- scale(tdm2.df)
d <- dist(tdm2.df.sc, method = "euclidean") # matriz de distancias
fit <- hclust(d, method="ward.D2")
plot(fit) # dendograma
grupos <- cutree(fit, k=4) # cortar árbol en k clusters
# bordes rojos en dendograma
rect.hclust(fit, k=4, border="red")


