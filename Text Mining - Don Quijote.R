##################################################
#  Text Mining Obra - Don Quijote de la Mancha   #
##################################################

# limpiamos el workspace
rm(list = ls())
# Limpiamos la consola
cat("\014")
# fijamos a UTF-8
options(encoding = "utf-8")

#########################
# 1. Librerias necesarias
#########################

# Vamos a cargar las librerias necesarias
library(pdftools)
library(dplyr)
library(stopwords)
library(tidytext)
library(stringi)
library(stringr)
library(ggplot2)
library(scales)
library(tidyr)
library(widyr)
library(ggraph)
library(igraph)
library(quanteda)
library(topicmodels)
library(cvTools)

########################
# 2. Carga de datos
########################

# Nos ubicamos en nuestro directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


# Lectura de archivos de texto
texto01 <- pdftools::pdf_text("DONQUIJOTE_PARTE1.pdf")
texto02 <- pdftools::pdf_text("DONQUIJOTE_PARTE2.pdf")

texto01[1]

length(texto01)
length(texto02)

texto <- c(texto01,texto02)

length(texto)

# Vamos a hacer un poco de limpieza de texto
# ------------------------------------------
texto <- gsub("\\r", " ", texto)
texto <- gsub("\\n", "", texto)
texto <- gsub("\\d\\K\\.(?=\\d)", "", texto, perl = TRUE)#  Los puntos de separador de mil, lo sustituimos por un espacio


# Juntamos todas las páginas del libros
texto<-paste(texto, collapse = '')
length(texto)

texto[1]

# Vamos a estructurar el texto
texto <- gsub("http://www.educa.jcyl.es","",texto)

vector = c()
for(i in 1:length(texto)){
  temp<-(strsplit(texto[[i]], "\\.")[[1]])
  print(temp)
  vector <- c(vector, temp)
}


# Lo convertimos a un dataframe
frases_texto<-as.data.frame(vector)

#####################################
# 3. Limpieza de texto y tokenizaci?n
#####################################

# Ahora, hacemos un poco de limpieza manual
# -----------------------------------------
colnames(frases_texto)[1]<-"frase"

# Quitamos los espacios de encabezado y pie de página
frases_texto$frase<-trimws(frases_texto$frase, "l") # para la izquierda trimws(frase,'r')
# Convertimos a caracter
frases_texto$frase <-as.character(frases_texto$frase)

# Vamos a hacer un poco de limpieza de texto
# ------------------------------------------
frases_texto$frase<-gsub("El Ingenioso Hidalgo Don Quijote de la Mancha","",frases_texto$frase)
frases_texto$frase<-gsub("PRIMERA PARTE","",frases_texto$frase)
frases_texto$frase<-gsub("Miguel de Cervantes Saavedra","",frases_texto$frase)
frases_texto$frase<-gsub("Portal Educativo EducaCYL","",frases_texto$frase)


#####################################
# 4. Analisis exploratorio de texto
#####################################
# Nos creamos un lexicon de stopwords en espa?ol 
lexiconSW<-stopwords("es")
lexiconSW <- append(lexiconSW,c("capítulo","d"," "))

lexiconSW<-as.data.frame(lexiconSW) # convertimos a dataframe

names(lexiconSW)<-"word"
lexiconSW$word<-as.character(lexiconSW$word)


# 1.1. Algunos análisis básicos
# -----------------------------
df <- tibble::rowid_to_column(frases_texto, "ID") #  Generamos un ID para cada frase

review_words <- df %>%
  distinct(frase, .keep_all = TRUE) %>% # eliminar filas duplicadas basadas en frase
  unnest_tokens(word, frase, drop = FALSE) %>%
  distinct(ID, word, .keep_all = TRUE) %>%
  anti_join(lexiconSW) %>% # devuelve todas las filas de x donde no hay valores coincidentes en y, manteniendo solo columnas de x.
  filter(str_detect(word, "[^\\d]")) %>% # selececciona words con algun comentario
  group_by(word) %>%
  dplyr::mutate(word_total = n()) %>%
  ungroup() # agrega nuevas variables y conserva las existentes


# Contamos las palabras resultantes
# ---------------------------------
word_counts <- review_words %>%
  dplyr::count(word, sort = TRUE)

word_counts %>%
  head(40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "blue") +
  scale_y_continuous(labels = comma_format()) +
  coord_flip() +
  labs(title = paste0("Palabras mas utilizadas"),
       subtitle = "Stopwords retiradas",
       x = "Palabra",
       y = "Numero de veces usada")


# Generamos nuestro WordCloud
# ---------------------------
library(wordcloud)
library(RColorBrewer)

df_grouped_V <- review_words %>% group_by(word) %>% count(word) %>%  
  group_by(word) %>% mutate(frecuencia = n/dim(review_words)[1])
 

# Generamos el wordcloud
wordcloud(words = df_grouped_V$word, freq = df_grouped_V$frecuencia,
          max.words = 400, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))


# 1.2. Bigramas
# -------------
# A veces nos interesa entender la relaci?n entre palabras en una opini?n. 
review_bigrams <- df %>%
  unnest_tokens(bigram, frase, token = "ngrams", n = 2) # separamos token 2 - grams
bigrams_separated <- review_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") # separamos word por bigrama
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% lexiconSW$word) %>%
  filter(!word2 %in% lexiconSW$word) # eliminamos  stop words por bigrama
bigram_counts <- bigrams_filtered %>% 
  dplyr::count(word1, word2, sort = TRUE) # contamos la cantidad de words por bigrama
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ") # count bigrams cleaning
bigrams_united %>%
  dplyr::count(bigram, sort = TRUE)


# Podemos visualizarlo tambien
review_subject <- df %>% 
  unnest_tokens(word, frase) %>% 
  anti_join(lexiconSW)
my_stopwords <- data_frame(word = c(as.character(1:10)))
review_subject <- review_subject %>% 
  anti_join(my_stopwords)
title_word_pairs <- review_subject %>% 
  pairwise_count(word, ID, sort = TRUE, upper = FALSE)

# Nos generamos el listado de bigramas
listadoBigramas<-title_word_pairs[which(title_word_pairs$n>100),]
set.seed(1234)
title_word_pairs %>%
  filter(n >= 100) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle('Bigramas')






# Gracias!!







