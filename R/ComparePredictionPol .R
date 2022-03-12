source("R/Functions:Libraries.R")
source("R/ImportData.R")
ile=2
lata <- list(X1year, X2year, X3year, X4year, X5year)
set.seed(123) 

for (year in lata) {
  
  Xyear <- dataYear(year)
  
  porownanieMetod1r2 <- data.frame("Metoda"=0, "Precyzja"=0, "Czulosc"=0, "Dokladnosc"=0, "F1"=0, "AUC"=0)
  
  ## Analiza dyskryminacyjna ----
  
  # Train the model
  precis <- c()
  recal <- c()
  accura <- c()
  auc <- c() 
  
  print("lda")
  for(i in 1:ile)
  {
    training_sample <- sample(c(TRUE, FALSE), nrow(Xyear), replace = T, prob = c(0.7,0.3))
    train <- Xyear[training_sample, ]
    test <- Xyear[!training_sample, ]
    
    train.control <- trainControl(method = "cv", number = 10, classProbs = TRUE, 
                                  savePredictions = T,
                                  summaryFunction = twoClassSummary)
    
    lda1 <- caret::train(class ~., data = train, method = "lda",
                         trControl = train.control, metric="ROC")
    
    pred1 <- predict(lda1, test)
    
    pred1V <- ifelse(as.vector(pred1)=="a0.Wyplacalny", 0, 1)
    testV <- ifelse(as.vector(test$class)=="a0.Wyplacalny", 0, 1)
    auc_ROCR <- performance(prediction(pred1V, testV), measure = "auc")
    
    precis[i] <- recallBankrut(table(test$class, pred1))[1]
    recal[i] <- recallBankrut(table(test$class, pred1))[2]
    accura[i] <- recallBankrut(table(test$class, pred1))[3]
    auc[i] <- auc_ROCR@y.values[[1]]
    
    print(paste((i*100)/ile, "%"))
  }
  
  
  porownanieMetod1r2[1,1] <- "Analiza dyskryminacyjna"
  porownanieMetod1r2[1,2] <- mean(precis, na.rm = TRUE)
  porownanieMetod1r2[1,3] <- mean(recal, na.rm = TRUE)
  porownanieMetod1r2[1,4] <- mean(accura, na.rm = TRUE)
  porownanieMetod1r2[1,5] <- 2 * (porownanieMetod1r2[1,2]*porownanieMetod1r2[1,3]/(porownanieMetod1r2[1,2]+porownanieMetod1r2[1,3]))
  porownanieMetod1r2[1,6] <- mean(auc, na.rm = TRUE) 
  
  ## drzewa decyzyjne ----
  
  precis <- c()
  recal <- c()
  accura <- c()
  auc <- c()
  
  print("rpart")
  
  for(i in 1:ile)
  {
    training_sample <- sample(c(TRUE, FALSE), nrow(Xyear), replace = T, prob = c(0.7,0.3))
    train <- Xyear[training_sample, ]
    test <- Xyear[!training_sample, ]
    tree.rose<-train(class ~., data = train, method = "rpart", metric="ROC",
                     trControl = trainControl(method = "cv", number = 10, classProbs = TRUE, 
                                              savePredictions = T,
                                              summaryFunction = twoClassSummary))
    #fancyRpartPlot(tree.rose$finalModel)
    #varImp(tree.rose)
    #tree.rose <- rpart(class ~ ., data = train)
    
    pred.tree.rose <- predict(tree.rose, newdata = test)
    
    porownanie <- data.frame("wartosc"=test$class, "tree"=pred.tree.rose)
    #porownanie$tree <- ifelse(porownanie$tree==0,"a0.Wyplacalny", "a1.Bankrut")
    
    pred1V <- ifelse(as.vector(porownanie$tree)=="a0.Wyplacalny", 0, 1)
    testV <- ifelse(as.vector(porownanie$wartosc)=="a0.Wyplacalny", 0, 1)
    auc_ROCR  <-  performance(prediction(pred1V, testV), measure = "auc")
    
    precis[i] <- recallBankrut(table(porownanie$wartosc, porownanie$tree))[1]
    recal[i] <- recallBankrut(table(porownanie$wartosc, porownanie$tree))[2]
    accura[i] <- recallBankrut(table(porownanie$wartosc, porownanie$tree))[3]
    auc[i] <- auc_ROCR@y.values[[1]]
    print(paste((i*100)/ile, "%"))
  }
  
  
  porownanieMetod1r2[2,1] <- "Drzewo decyzyjne"
  porownanieMetod1r2[2,2] <- mean(precis, na.rm = TRUE)
  porownanieMetod1r2[2,3] <- mean(recal, na.rm = TRUE)
  porownanieMetod1r2[2,4] <- mean(accura, na.rm = TRUE)
  porownanieMetod1r2[2,5] <- 2 * (porownanieMetod1r2[2,2]*porownanieMetod1r2[2,3]/(porownanieMetod1r2[2,2]+porownanieMetod1r2[2,3]))
  porownanieMetod1r2[2,6] <- mean(auc, na.rm = TRUE)
  
  ## randomForest ----
  
  precis <- c()
  recal <- c()
  accura <- c()
  auc <- c()
  
  print("rf")
  
  for(i in 1:ile)
  {
    training_sample <- sample(c(TRUE, FALSE), nrow(Xyear), replace = T, prob = c(0.7,0.3))
    train <- Xyear[training_sample, ]
    test <- Xyear[!training_sample, ]
    
    rf <- train(class ~., data = train, method = "rf",
                trControl = trainControl(method = "cv",
                                         number = 10,
                                         classProbs = TRUE, 
                                         savePredictions = T,
                                         summaryFunction = twoClassSummary), metric="ROC")
    varImp(rf)
    pred_rf = predict(rf, newdata=test)
    wynik_rf <- data.frame("class"=test$class, "prognoza"=pred_rf)
    
    pred1V <- ifelse(as.vector(wynik_rf$prognoza)=="a0.Wyplacalny", 0, 1)
    testV <- ifelse(as.vector(wynik_rf$class)=="a0.Wyplacalny", 0, 1)
    auc_ROCR  <-  performance(prediction(pred1V, testV), measure = "auc")
    
    precis[i] <- recallBankrut(table(wynik_rf$class, wynik_rf$prognoza))[1]
    recal[i] <- recallBankrut(table(wynik_rf$class, wynik_rf$prognoza))[2]
    accura[i] <- recallBankrut(table(wynik_rf$class, wynik_rf$prognoza))[3]
    auc[i] <- auc_ROCR@y.values[[1]]
    print(paste((i*100)/ile, "%"))
  }
  
  porownanieMetod1r2[3,1] <- "Random forest"
  porownanieMetod1r2[3,2] <- mean(precis, na.rm = TRUE)
  porownanieMetod1r2[3,3] <- mean(recal, na.rm = TRUE)
  porownanieMetod1r2[3,4] <- mean(accura, na.rm = TRUE)
  porownanieMetod1r2[3,5] <- 2 * (porownanieMetod1r2[3,2]*porownanieMetod1r2[3,3]/(porownanieMetod1r2[3,2]+porownanieMetod1r2[3,3]))
  porownanieMetod1r2[3,6] <- mean(auc, na.rm = TRUE)
  
  ## NAIVE BAYES ----
  precis <- c()
  recal <- c()
  accura <- c()
  auc <- c()
  
  print("nb")
  
  for(i in 1:ile)
  {
    training_sample <- sample(c(TRUE, FALSE), nrow(Xyear), replace = T, prob = c(0.7,0.3))
    train <- Xyear[training_sample, ]
    test <- Xyear[!training_sample, ]
    nb = train(class ~ .,
               data=train, 'nb', 
               metric = 'ROC',
               trControl = trainControl(method = "cv", number = 10, classProbs = TRUE, 
                                        savePredictions = T,
                                        summaryFunction = twoClassSummary))
    
    varImp(nb)
    pred_nb = predict(nb, newdata=test)
    wynik_nb <- data.frame(class = test$class, pred = pred_nb)
    
    pred1V <- ifelse(as.vector(wynik_nb$pred)=="a0.Wyplacalny", 0, 1)
    testV <- ifelse(as.vector(wynik_nb$class)=="a0.Wyplacalny", 0, 1)
    auc_ROCR  <-  performance(prediction(pred1V, testV), measure = "auc")
    
    precis[i] <- recallBankrut(table(wynik_nb$class, wynik_nb$pred))[1]
    recal[i] <- recallBankrut(table(wynik_nb$class, wynik_nb$pred))[2]
    accura[i] <- recallBankrut(table(wynik_nb$class, wynik_nb$pred))[3]
    auc[i] <- auc_ROCR@y.values[[1]]
    print(paste((i*100)/ile, "%"))
  }
  
  
  
  porownanieMetod1r2[4,1] <- "Naive Bayes"
  porownanieMetod1r2[4,2] <- mean(precis, na.rm = TRUE)
  porownanieMetod1r2[4,3] <- mean(recal, na.rm = TRUE)
  porownanieMetod1r2[4,4] <- mean(accura, na.rm = TRUE)
  porownanieMetod1r2[4,5] <- 2 * (porownanieMetod1r2[4,2]*porownanieMetod1r2[4,3]/(porownanieMetod1r2[4,2]+porownanieMetod1r2[4,3]))
  porownanieMetod1r2[4,6] <- mean(auc, na.rm = TRUE)
  
  ## sieci mlp ----
  precis <- c()
  recal <- c()
  accura <- c()
  auc <- c()
  
  print("mlp")
  
  for(i in 1:ile)
  {
    training_sample <- sample(c(TRUE, FALSE), nrow(Xyear), replace = T, prob = c(0.7,0.3))
    train <- Xyear[training_sample, ]
    test <- Xyear[!training_sample, ]
    
    siec <- train(class ~., data = train, method = "nnet",
                trControl = trainControl(method = "cv",
                                         number = 10,
                                         classProbs = TRUE, 
                                         savePredictions = T,
                                         summaryFunction = twoClassSummary), metric="ROC", 
                )
    
    pred1 <- predict(siec, test)
    
    pred1V <- ifelse(as.vector(pred1)=="a0.Wyplacalny", 0, 1)
    
    
    testV <- ifelse(as.vector(test$class)=="a0.Wyplacalny", 0, 1)
    
    auc_ROCR <- performance(prediction(pred1V, testV), measure = "auc")
    
    precis[i] <- recallBankrut(table(test$class, pred1))[1]
    recal[i] <- recallBankrut(table(test$class, pred1))[2]
    accura[i] <- recallBankrut(table(test$class, pred1))[3]
    auc[i] <- auc_ROCR@y.values[[1]]
    
    print(paste((i*100)/ile, "%"))
  }
  
  
  porownanieMetod1r2[5,1] <- "Sieci neuronowe"
  porownanieMetod1r2[5,2] <- mean(precis, na.rm = TRUE)
  porownanieMetod1r2[5,3] <- mean(recal, na.rm = TRUE)
  porownanieMetod1r2[5,4] <- mean(accura, na.rm = TRUE)
  porownanieMetod1r2[5,5] <- 2 * (porownanieMetod1r2[5,2]*porownanieMetod1r2[5,3]/(porownanieMetod1r2[5,2]+porownanieMetod1r2[5,3]))
  porownanieMetod1r2[5,6] <- mean(auc, na.rm = TRUE)
  
  
  ## GBM ----
  precis <- c()
  recal <- c()
  accura <- c()
  auc <- c()
  ctrl <- trainControl(method = "cv",
                       number = 10,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE)
  
  print("gbm")
  
  for(i in 1:ile)
  {
    training_sample <- sample(c(TRUE, FALSE), nrow(Xyear), replace = T, prob = c(0.7,0.3))
    train <- Xyear[training_sample, ]
    test <- Xyear[!training_sample, ]
    orig_fit <- train(class ~ .,
                      data = train,
                      method = "gbm",
                      verbose = FALSE,
                      trControl = trainControl(method = "cv",
                                               number = 10,
                                               classProbs = TRUE, 
                                               savePredictions = T,
                                               summaryFunction = twoClassSummary),
                      metric="ROC")
    
    pred_gbm = predict(orig_fit, test)
    wynik_gbm <- data.frame(class = test$class, pred = pred_gbm)
    
    pred1V <- ifelse(as.vector(wynik_gbm$pred)=="a0.Wyplacalny", 0, 1)
    testV <- ifelse(as.vector(wynik_gbm$class)=="a0.Wyplacalny", 0, 1)
    auc_ROCR  <-  performance(prediction(pred1V, testV), measure = "auc")
    
    precis[i] <- recallBankrut(table(wynik_gbm$class, wynik_gbm$pred))[1]
    recal[i] <- recallBankrut(table(wynik_gbm$class, wynik_gbm$pred))[2]
    accura[i] <- recallBankrut(table(wynik_gbm$class, wynik_gbm$pred))[3]
    auc[i] <- auc_ROCR@y.values[[1]]
    print(paste((i*100)/ile, "%"))
  }
  
  porownanieMetod1r2[6,1] <- "GBM"
  porownanieMetod1r2[6,2] <- mean(precis, na.rm = TRUE)
  porownanieMetod1r2[6,3] <- mean(recal, na.rm = TRUE)
  porownanieMetod1r2[6,4] <- mean(accura, na.rm = TRUE)
  porownanieMetod1r2[6,5] <- 2 * (porownanieMetod1r2[6,2]*porownanieMetod1r2[6,3]/(porownanieMetod1r2[6,2]+porownanieMetod1r2[6,3]))
  porownanieMetod1r2[6,6] <- mean(auc, na.rm = TRUE)
  
  if(mean(year[1,]==X1year[1,], na.rm = TRUE)==1){
    porownanieMetodrok1 <- porownanieMetod1r2
  }
  if(mean(year[1,]==X2year[1,], na.rm = TRUE)==1){
    porownanieMetodrok2 <- porownanieMetod1r2
  }
  if(mean(year[1,]==X3year[1,], na.rm = TRUE)==1){
    porownanieMetodrok3 <- porownanieMetod1r2
  }
  if(mean(year[1,]==X4year[1,], na.rm = TRUE)==1){
    porownanieMetodrok4 <- porownanieMetod1r2
  }
  if(mean(year[1,]==X5year[1,], na.rm = TRUE)==1){
    porownanieMetodrok5 <- porownanieMetod1r2
  }
}


porownanieMetodWykres <- rbind(porownanieMetodrok1, porownanieMetodrok2, porownanieMetodrok3, porownanieMetodrok4, porownanieMetodrok5)
porownanieMetodWykres$rok <- c(rep(1, 6),rep(2, 6),rep(3, 6), rep(4, 6), rep(5, 6))


ggplot(porownanieMetodWykres, aes(x=rok, y=Dokladnosc, col=Metoda))+geom_line() + ggtitle("Porównanie dokładności predykcji")

ggplot(porownanieMetodWykres, aes(x=rok, y=Precyzja, col=Metoda))+geom_line() + ggtitle("Porównanie precyzji predykcji")

ggplot(porownanieMetodWykres, aes(x=rok, y=Czulosc, col=Metoda))+geom_line() + ggtitle("Porównanie czułości predykcji")

ggplot(porownanieMetodWykres, aes(x=rok, y=F1, col=Metoda))+geom_line() + ggtitle("Porównanie miary F1")

ggplot(porownanieMetodWykres, aes(x=rok, y=AUC, col=Metoda))+geom_line() + ggtitle("Porównanie miary AUC")

