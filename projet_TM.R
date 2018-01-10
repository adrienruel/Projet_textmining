require(XML)
require(tm)
require(SnowballC)
data <- xmlParse("D:/aruel/Téléchargements/train_2017.xml")

#création des vecteurs pour stocker les phrases et les polarités
xml_data <- xmlToList(data)
phrases <- vector()
category <- vector()
target <- vector()
polarity <- vector()
index <- vector()
textes <- vector()
x <- 1
n <- length(xml_data)

#certaines phrases n'ont pas d'opinion, nous les estimerons à neutre
for(i in 1:n){
  
  m <- length(xml_data[i]$Review$sentences)
  for(j in 1:m){
    index[x] <- i
    
    phrases[x] <- xml_data[i]$Review$sentences[j]$sentence$text
    if(length(xml_data[i]$Review$sentences[j]$sentence$Opinions$Opinion)  == 0){
      
      category[x] <- "NULL"
      target[x] <- "NULL"
      polarity[x] <- "neutral"
      
      
    }else{
      category[x] <- xml_data[i]$Review$sentences[j]$sentence$Opinions$Opinion[2]
      target[x] <- xml_data[i]$Review$sentences[j]$sentence$Opinions$Opinion[1]
      polarity[x] <- xml_data[i]$Review$sentences[j]$sentence$Opinions$Opinion[3]
      x <- x+1
    }
  }
  
  
}

#recupération d'une liste de mots outils
mots_outils <- read.table("C:/Users/aruel/Desktop/mot_outils.txt")
mots_outils <- as.vector(mots_outils)
#on nettoie cette liste, puisqu'il y a des éléments de ponctuation
mots_outils <- lapply(mots_outils,as.character)
mots_outils <- lapply(mots_outils, removePunctuation)


df <- cbind(phrases,category,target,polarity,index)
df <- as.data.frame(df)
#on met les phrases en miniscules
df$phrases <- lapply(df$phrases, tolower)
#on enlève la ponctuation
df$phrases <- lapply(df$phrases, removePunctuation)
#on enlève les nombres
df$phrases <- lapply(df$phrases, removeNumbers)
#on enlève les mots-outils de notre liste et de la liste fournie par R
df$phrases <- as.character(df$phrases)
#df$phrases <- removeWords(df$phrases, stopwords("english"))
#df$phrases <- removeWords(df$phrases, mots_outils$V1)
#on utilise l'algorithme de stemming de porter 
for(i in 1:length(df$phrases)){
  df$phrases[i] <- stemDocument(df$phrases[i], language = "english")
}
#On enlève les polarités neutres
df <- df[which(df$polarity != "neutral"),]
df$polarity <- as.character(df$polarity)
df$polarity  <- as.factor(df$polarity)





#on créé un corpus pour les phrases positives et un pour les phrases négatives
Corp_tout <- Corpus(VectorSource(df$phrases))
Corp_positive <- Corpus(VectorSource(df$phrases[which(df$polarity == "positive")]))
Corp_negative <- Corpus(VectorSource(df$phrases[which(df$polarity == "negative")]))



#fonction pour calculer la fréquences de chaque mot dans un corpus
wordfreq <- function(data){
  myTdm <- as.matrix(TermDocumentMatrix(data))
  FreqMat <- data.frame(ST = rownames(myTdm), 
                        Freq = rowSums(myTdm), 
                        row.names = NULL)
  
  return(FreqMat)
}

#on calcule ensuite les fréquences des mots pour les corpus
freq_mot_tout <- wordfreq(Corp_tout)
freq_mot_positive <- wordfreq(Corp_positive)
freq_mot_negative <- wordfreq(Corp_negative)

freq_mot_positive <- freq_mot_positive[order(freq_mot_positive$Freq, decreasing = T),]
freq_mot_negative <- freq_mot_negative[order(freq_mot_negative$Freq, decreasing = T),]
freq_mot_tout <- freq_mot_tout[order(freq_mot_tout$Freq, decreasing = T),]

barplot(freq_mot_positive$Freq[1:10], names.arg = freq_mot_positive$ST[1:10])

barplot(freq_mot_negative$Freq[1:10], names.arg = freq_mot_negative$ST[1:10])

barplot(freq_mot_tout$Freq[1:10], names.arg = freq_mot_tout$ST[1:10])

wordcloud::wordcloud(freq_mot_tout$ST, max.words = 100, freq = freq_mot_tout$Freq)
wordcloud::wordcloud(freq_mot_positive$ST, max.words = 100, freq = freq_mot_positive$Freq)
  
wordcloud::wordcloud(freq_mot_negative$ST, max.words = 100, freq = freq_mot_negative$Freq)

#on ramène cette fréquence à la taille du corpus
occurence_positive = freq_mot_positive$Freq/length(Corp_positive)
occurence_negative = freq_mot_negative$Freq/length(Corp_negative)

freq_mot_positive <- cbind(freq_mot_positive, occurence_positive)
freq_mot_negative <- cbind(freq_mot_negative, occurence_negative)

#on va voir, pour les mots qui apparaissent dans les 2 corpus
#qu'elles sont les mots qui apparaisse beaucoup plus dans un corpus que dans l'autre
difference <- vector()
mot_communs <- intersect(freq_mot_negative$ST, freq_mot_positive$ST)
for(i in 1:length(mot_communs)){
  
  x <- freq_mot_negative$occurence_negative[mot_communs[i] == freq_mot_negative$ST]
  y <- freq_mot_positive$occurence_positive[mot_communs[i] == freq_mot_positive$ST]
  
  difference[i] <- x/y
  names(difference)[i] <- mot_communs[i]
  
}

#boxplot(difference[order(difference)])

#on met le seuil à 3, ça veut dire que l'on garde les mots qui apparaissent 3 fois plus dans un corpus que dans l'autre
x <- names(difference[which(difference < 3 & difference > 0.3)])
df$phrases <- removeWords(df$phrases, x)

#on mélange les phrases pour modifier l'échantillon test et train à chaque itération
df <- df[sample(nrow(df)),]
require(RTextTools)
#il y a une erreur dans la function create_matrix
#il y a une majuscule à Acronym qu'il faut enlever ligne 42 à l'aide de la commande suivante
trace("create_matrix",edit=T)

#création de la matrice documents/termes pour l'échantillon train (700 phrases)
dtMatrix <- create_matrix(df$phrases[1:1300])

#container : structure de données pour utiliser les fonctions d'après
#on lui donne la matrice, la variable à estimer et des paramètres complémentaires

container <- create_container(dtMatrix, df$polarity, trainSize = 1:1300, virgin = FALSE)
# on entraine le modèle
#le paramètre kernel correspond au type de noyau
# linear, radial pour gaussien, sigmoid et polynomial 
model <- train_model(container,"SVM",kernel = "linear", cost = 1)
#échantillon test
predictionData <- df$phrases[1301:1630]
#on créé la matrice pour l'échantillon test, mais en gardant les colonnes de la première matrice
predMatrix <- create_matrix(predictionData, originalMatrix=dtMatrix)

predSize <-  length(predictionData)
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)

#on calcule les valeurs
results <- classify_model(predictionContainer, model)
#on met un seuil pour le terme de neutralité
#ici on regarde la valeur que prend la probabilité si aucun spécial n'est dans la phrase
#si on obtient cette probabilité, on dit que la phrase est neutre
nuls <- which(rowSums(as.matrix(predMatrix)) == 0)

res <- ifelse(results$SVM_PROB == results$SVM_PROB[nuls[1]] , "neutral", results$SVM_LABEL)
res <- ifelse(res == "1", "negative", ifelse(res == "2", "positive", "neutral"))
#matrice de confusion
tab <- table(results$SVM_LABEL, df$polarity[1301:1630])

results$SVM_PROB[which(results$SVM_LABEL != df$polarity[1301:1630])]
tab <- table(res, df$polarity[1301:1630])
tab

#taux de réussite
sum(diag(tab[-2,]))/sum(tab[-2,])








#on réitère les mêmes étapes, mais la dtm comprend maintenant des ngrams
library(RWeka) 
texts <- df$phrases

#on définit les ngrams possible, ici allant de 1 à 3
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))
dtm <- DocumentTermMatrix(Corpus(VectorSource(texts)),
                          control=list(
                            weighting = weightTf,
                            tokenize = TrigramTokenizer))

#View(as.matrix(dtm))
container <- create_container(dtm, df$polarity, virgin=F,trainSize = 1:700)
models <- train_models(container, "SVM", kernel = "linear", cost = 1)

predictionData <- df$phrases[701:1630]
predMatrix <- create_matrix(predictionData, originalMatrix=dtm)
predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)

results <- classify_models(predictionContainer, models)

t <- table(results$SVM_LABEL, df$polarity[701:1630] )

sum(diag(t))/sum(t)