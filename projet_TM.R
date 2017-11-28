require(XML)
data <- xmlParse("D:/aruel/Téléchargements/train_2017.xml")

xml_data <- xmlToList(data)
phrases <- vector()
category <- vector()
target <- vector()
polarity <- vector()
index <- vector()
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




df <- cbind(phrases,category,target,polarity,index)
df <- as.data.frame(df)
df$phrases <- lapply(df$phrases, tolower)
df$phrases <- lapply(df$phrases, removePunctuation)


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


