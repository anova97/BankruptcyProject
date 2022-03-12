ile=1
lata <- list(dataSk1, dataSk2, dataSk3, dataSk4)

for (year in lata) {
  
  Xyear <- dataYearSk(year)
  
  porownanieMetod1Sk <- data.frame("Metoda"=0, "Precyzja"=0, "Czulosc"=0, "Dokladnosc"=0, "F1"=0, "AUC"=0)
  
  set.seed(123) 
  train.control <- trainControl(method = "cv", number = 10, classProbs = TRUE,
                                summaryFunction = twoClassSummary)
  ## Analiza dyskryminacyjna ----
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
    auc_ROCR  <-  performance(prediction(pred1V, testV), measure = "auc")
    
    precis[i] <- recallBankrut(table(test$class, pred1))[1]
    recal[i] <- recallBankrut(table(test$class, pred1))[2]
    accura[i] <- recallBankrut(table(test$class, pred1))[3]
    auc[i] <- auc_ROCR@y.values[[1]]
    print(paste((i*100)/ile, "%"))
  }
  
  
  porownanieMetod1Sk[1,1] <- "Analiza dyskryminacyjna"
  porownanieMetod1Sk[1,2] <- mean(precis, na.rm = TRUE)
  porownanieMetod1Sk[1,3] <- mean(recal, na.rm = TRUE)
  porownanieMetod1Sk[1,4] <- mean(accura, na.rm = TRUE)
  porownanieMetod1Sk[1,5] <- 2 * (porownanieMetod1Sk[1,2]*porownanieMetod1Sk[1,3]/(porownanieMetod1Sk[1,2]+porownanieMetod1Sk[1,3]))
  porownanieMetod1Sk[1,6] <- mean(auc, na.rm = TRUE) 
  
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
    tree <- train(class ~., data = train, method = "rpart",
                trControl = trainControl(method = "cv",
                                         number = 10,
                                         classProbs = TRUE,
                                         summaryFunction = twoClassSummary), metric="ROC")
    
    pred.tree <- predict(tree, newdata = test)
    
    porownanie <- data.frame("wartosc"=test$class, "tree"=pred.tree)
    
    pred1V <- ifelse(as.vector(porownanie$tree)=="a0.Wyplacalny", 0, 1)
    testV <- ifelse(as.vector(porownanie$wartosc)=="a0.Wyplacalny", 0, 1)
    auc_ROCR  <-  performance(prediction(pred1V, testV), measure = "auc")
    
    precis[i] <- recallBankrut(table(porownanie$wartosc, porownanie$tree))[1]
    recal[i] <- recallBankrut(table(porownanie$wartosc, porownanie$tree))[2]
    accura[i] <- recallBankrut(table(porownanie$wartosc, porownanie$tree))[3]
    auc[i] <- auc_ROCR@y.values[[1]]
    print(paste((i*100)/ile, "%"))
  }
  
  
  porownanieMetod1Sk[2,1] <- "Drzewo decyzyjne"
  porownanieMetod1Sk[2,2] <- mean(precis, na.rm = TRUE)
  porownanieMetod1Sk[2,3] <- mean(recal, na.rm = TRUE)
  porownanieMetod1Sk[2,4] <- mean(accura, na.rm = TRUE)
  porownanieMetod1Sk[2,5] <- 2 * (porownanieMetod1Sk[2,2]*porownanieMetod1Sk[2,3]/(porownanieMetod1Sk[2,2]+porownanieMetod1Sk[2,3]))
  porownanieMetod1Sk[2,6] <- mean(auc, na.rm = TRUE)
  
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
                                         summaryFunction = twoClassSummary), metric="ROC")
    
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
  
  porownanieMetod1Sk[3,1] <- "Random forest"
  porownanieMetod1Sk[3,2] <- mean(precis, na.rm = TRUE)
  porownanieMetod1Sk[3,3] <- mean(recal, na.rm = TRUE)
  porownanieMetod1Sk[3,4] <- mean(accura, na.rm = TRUE)
  porownanieMetod1Sk[3,5] <- 2 * (porownanieMetod1Sk[3,2]*porownanieMetod1Sk[3,3]/(porownanieMetod1Sk[3,2]+porownanieMetod1Sk[3,3]))
  porownanieMetod1Sk[3,6] <- mean(auc, na.rm = TRUE)
  
  ## NAIVE BAYES ----
  precis <- c()
  recal <- c()
  accura <- c()
  auc <- c()
  
  print("nb")
  
  for(i in 1:ile)
  {
    training_sample  <-  sample(c(TRUE, FALSE), nrow(Xyear), replace = T, prob = c(0.7,0.3))
    train <- Xyear[training_sample, ]
    test <- Xyear[!training_sample, ]
    nb = train(class ~ .,
               data=train, 'nb', 
               metric = 'ROC',
               trControl = trainControl(method = "cv",
                                        number = 10, 
                                        classProbs = TRUE))
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
  
  
  
  porownanieMetod1Sk[4,1] <- "Naive Bayes"
  porownanieMetod1Sk[4,2] <- mean(precis, na.rm = TRUE)
  porownanieMetod1Sk[4,3] <- mean(recal, na.rm = TRUE)
  porownanieMetod1Sk[4,4] <- mean(accura, na.rm = TRUE)
  porownanieMetod1Sk[4,5] <- 2 * (porownanieMetod1Sk[4,2]*porownanieMetod1Sk[4,3]/(porownanieMetod1Sk[4,2]+porownanieMetod1Sk[4,3]))
  porownanieMetod1Sk[4,6] <- mean(auc, na.rm = TRUE)
  
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
                                         summaryFunction = twoClassSummary), metric="ROC")
    
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
  
  
  porownanieMetod1Sk[5,1] <- "Sieci MLP"
  porownanieMetod1Sk[5,2] <- mean(precis, na.rm = TRUE)
  porownanieMetod1Sk[5,3] <- mean(recal, na.rm = TRUE)
  porownanieMetod1Sk[5,4] <- mean(accura, na.rm = TRUE)
  porownanieMetod1Sk[5,5] <- 2 * (porownanieMetod1Sk[5,2]*porownanieMetod1Sk[5,3]/(porownanieMetod1Sk[5,2]+porownanieMetod1Sk[5,3]))
  porownanieMetod1Sk[5,6] <- mean(auc, na.rm = TRUE)
  
  
  ## GBM ----
  precis <- c()
  recal <- c()
  accura <- c()
  auc <- c()
  ctrl  <-  trainControl(method = "cv",
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
                      trControl = ctrl, metric="ROC")
    
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
  
  porownanieMetod1Sk[6,1] <- "GBM"
  porownanieMetod1Sk[6,2] <- mean(precis, na.rm = TRUE)
  porownanieMetod1Sk[6,3] <- mean(recal, na.rm = TRUE)
  porownanieMetod1Sk[6,4] <- mean(accura, na.rm = TRUE)
  porownanieMetod1Sk[6,5] <- 2 * (porownanieMetod1Sk[6,2]*porownanieMetod1Sk[6,3]/(porownanieMetod1Sk[6,2]+porownanieMetod1Sk[6,3]))
  porownanieMetod1Sk[6,6] <- mean(auc, na.rm = TRUE)
  
  if(mean(year[1,]==dataSk1[1,], na.rm = TRUE)==1){
    porownanieMetodrokSk1 <- porownanieMetod1r2
  }
  if(mean(year[1,]==dataSk2[1,], na.rm = TRUE)==1){
    porownanieMetodrokSk2 <- porownanieMetod1r2
  }
  if(mean(year[1,]==dataSk3[1,], na.rm = TRUE)==1){
    porownanieMetodrokSk3 <- porownanieMetod1r2
  }
  if(mean(year[1,]==dataSk4[1,], na.rm = TRUE)==1){
    porownanieMetodrokSk4 <- porownanieMetod1r2
  }
}


porownanieMetodWykresSk <- rbind(porownanieMetodrokSk1, porownanieMetodrokSk2, porownanieMetodrokSk3, porownanieMetodrokSk4)
porownanieMetodWykresSk$rok <- c(rep(1, 6),rep(2, 6),rep(3, 6), rep(4, 6))


ggplot(porownanieMetodWykresSk, aes(x=rok, y=Dokladnosc, col=Metoda))+geom_line() + ggtitle("Porównanie dokładności predykcji")

ggplot(porownanieMetodWykresSk, aes(x=rok, y=Precyzja, col=Metoda))+geom_line() + ggtitle("Porównanie precyzji predykcji")

ggplot(porownanieMetodWykresSk, aes(x=rok, y=Czulosc, col=Metoda))+geom_line() + ggtitle("Porównanie czułości predykcji")

ggplot(porownanieMetodWykresSk, aes(x=rok, y=F1, col=Metoda))+geom_line() + ggtitle("Porównanie miary F1")

ggplot(porownanieMetodWykresSk, aes(x=rok, y=AUC, col=Metoda))+geom_line() + ggtitle("Porównanie miary F1")
