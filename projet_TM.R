require(XML)
require(tm)
require(SnowballC)
data <- xmlParse("D:/aruel/Téléchargements/train_2017.xml")

xml_data <- xmlToList(data)
phrases <- vector()
category <- vector()
target <- vector()
polarity <- vector()
index <- vector()
textes <- vector()
polarite <- vector()
x <- 1
n <- length(xml_data)

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


mots_outils <- read.table("C:/Users/aruel/Desktop/mot_outils.txt")
mots_outils <- as.vector(mots_outils)
mots_outils <- lapply(mots_outils,as.character)
mots_outils <- lapply(mots_outils, removePunctuation)


df <- cbind(phrases,category,target,polarity,index)
df <- as.data.frame(df)
df$phrases <- lapply(df$phrases, tolower)
df$phrases <- lapply(df$phrases, removePunctuation)
df$phrases <- lapply(df$phrases, removeNumbers)
df$phrases <- as.character(df$phrases)
df$phrases <- removeWords(df$phrases, stopwords("english"))
df$phrases <- removeWords(df$phrases, mots_outils)
for(i in 1:length(df$phrases)){
  df$phrases[i] <- stemDocument(df$phrases[i], language = "english")
}

df <- df[which(df$polarity != "neutral"),]
df$polarity <- as.character(df$polarity)
df$polarity  <- as.factor(df$polarity)






Corp_tout <- Corpus(VectorSource(df$phrases))
Corp_positive <- Corpus(VectorSource(df$phrases[which(df$polarity == "positive")]))
Corp_negative <- Corpus(VectorSource(df$phrases[which(df$polarity == "negative")]))


Corp_tout <- tm_map(Corp_tout, removeWords, stopwords("english"))
Corp_positive <-  tm_map(Corp_positive, removeWords, stopwords("english"))
Corp_negative <-  tm_map(Corp_negative, removeWords, stopwords("english"))



wordfreq <- function(data){
  myTdm <- as.matrix(TermDocumentMatrix(data))
  FreqMat <- data.frame(ST = rownames(myTdm), 
                        Freq = rowSums(myTdm), 
                        row.names = NULL)
  
  return(FreqMat)
}


freq_mot_tout <- wordfreq(Corp_tout)
freq_mot_positive <- wordfreq(Corp_positive)
freq_mot_negative <- wordfreq(Corp_negative)

occurence_positive = freq_mot_positive$Freq/length(Corp_positive)
occurence_negative = freq_mot_negative$Freq/length(Corp_negative)

freq_mot_positive <- cbind(freq_mot_positive, occurence_positive)
freq_mot_negative <- cbind(freq_mot_negative, occurence_negative)

difference <- vector()
mot_communs <- intersect(freq_mot_negative$ST, freq_mot_positive$ST)
for(i in 1:length(mot_communs)){
  
  x <- freq_mot_negative$occurence_negative[mot_communs[i] == freq_mot_negative$ST]
  y <- freq_mot_positive$occurence_positive[mot_communs[i] == freq_mot_positive$ST]
  
  difference[i] <- x/y
  names(difference)[i] <- mot_communs[i]
  
}

boxplot(difference[order(difference)])

x <- names(difference[which(difference < 5 & difference > 0.2)])
df$phrases <- removeWords(df$phrases, x)

#pos <- which(df$polarity == "positive")
#neg <- which(df$polarity == "negative")


#x1 <- sample(pos, size = 200)
#x2 <- sample(neg, size = 200)
require(RTextTools)
#trace("create_matrix",edit=T)
dtMatrix <- create_matrix(df$phrases)



container <- create_container(dtMatrix, df$polarity, trainSize = 1:700, virgin = FALSE)

# train a SVM Model
model <- train_models(container,"SVM",kernel = "linear", cost = 1)

predictionData <- df$phrases[701:1630]
predMatrix <- create_matrix(predictionData, originalMatrix=dtMatrix)

predSize = length(predictionData)
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)

results <- classify_models(predictionContainer, model)
#res <- ifelse(results$SVM_PROB < 0.80, 1, results$SVM_LABEL)

tab <- table(results$SVM_LABEL, df$polarity[701:1630])

#results$SVM_PROB[which(results$SVM_LABEL != df$polarity[701:1630])]
#tab <- table(res, df$polarity[701:1630])
tab


sum(diag(tab))/sum(tab)




library(RWeka) # this library is needed for NGramTokenizer

texts <- df$phrases
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))
dtm <- DocumentTermMatrix(Corpus(VectorSource(texts)),
                          control=list(
                            weighting = weightTf,
                            tokenize = TrigramTokenizer))

View(as.matrix(dtm))
container <- create_container(dtm, df$polarity, virgin=F,trainSize = 1:700)
models <- train_models(container, "SVM", kernel = "linear", cost = 1)

predictionData <- df$phrases[701:1630]
predMatrix <- create_matrix(predictionData, originalMatrix=dtm)
predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)

results <- classify_models(predictionContainer, models)

t <- table(results$SVM_LABEL, df$polarity[701:1630] )

sum(diag(t))/sum(t)