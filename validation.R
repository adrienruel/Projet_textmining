#cette fonction necessite d'avoir utiliser le fichier projet_TM.R auparavant


#prend en entrer un vecteur de phrases de tests et les valeurs du seuil



validation <- function(phrases_tests, x1 = 3){
  #on nettoie les phrases test
  phrases_tests <- lapply(phrases_tests, tolower)
  #on enlève la ponctuation
  phrases_tests <- lapply(phrases_tests, removePunctuation)
  #on enlève les nombres
  phrases_tests <- lapply(phrases_tests, removeNumbers)
  #on enlève les mots-outils de notre liste et de la liste fournie par R
  phrases_tests <- as.character(phrases_tests)
  for(i in 1:length(phrases_tests)){
    phrases_tests[i] <- stemDocument(phrases_tests[i], language = "english")
  }
  
  
  
  #on met le seuil à 3, ça veut dire que l'on garde les mots qui apparaissent 3 fois plus dans un corpus que dans l'autre
  x <- names(difference[which(difference < x1 & difference > 1/x1)])
  phrases_tests <- removeWords(phrases_tests, x)
  
  
  
  dtMatrix <- create_matrix(df$phrases)
  
  container <- create_container(dtMatrix, df$polarity, trainSize = 1:1630, virgin = FALSE)
  # on entraine le modèle
  #le paramètre kernel correspond au type de noyau
  # linear, radial pour gaussien, sigmoid et polynomial 
  model <- train_model(container,"SVM",kernel = "linear", cost = 1)
  #échantillon test
  predictionData <- phrases_tests
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
  #on peut mettre en commentaire la ligne au dessus pour enlever les neutres
  
  res <- ifelse(res == "1", "negative", ifelse(res == "2", "positive", "neutral"))
  
  return(res)
  #renvoie un vecteur d'estimation
}