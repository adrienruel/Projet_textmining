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
      polarity[x] <- "NULL"
      
      
    }else{
    category[x] <- xml_data[i]$Review$sentences[j]$sentence$Opinions$Opinion[2]
    target[x] <- xml_data[i]$Review$sentences[j]$sentence$Opinions$Opinion[1]
    polarity[x] <- xml_data[i]$Review$sentences[j]$sentence$Opinions$Opinion[3]
    x <- x+1
    }
  }
  
  
}




df <- cbind(phrases,category,target,polarity,index)

