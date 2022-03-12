library(readr)
library(psych)
library(naniar)
library(corrplot)
library(e1071)
library(polycor)
library(psych)
library(lares)
library(ggcorrplot)
library(car)
library(tidyverse)
library(MASS)
library(klaR)
library(caret)
library(MASS)
library(FactoMineR)
library(factoextra)
library(missMDA)
library(DMwR2)
library(PerformanceAnalytics)
library(ROCR)
library(rattle)
library(nnet)
library(gridExtra)
library(ggpubr)


## Funkcje ----

## 1
usunNAWiersz <- function(data)
{
  data$sumaNA <- 0
  for(i in 1:nrow(data)){
    data$sumaNA[i] <- sum(is.na(data[i,]))
  }
  
  data <- data[which(data$sumaNA<5),]
  return(data[,-(ncol(data))])
}

## 2
usunNAKol <- function(data)
{
  data[nrow(data)+1,] <- 0
  for(i in 1:ncol(data)){
    data[nrow(data),i] <- sum(is.na(data[,i]))
  }
  
  tyle <- mean(as.numeric(data[nrow(data),]))
  
  data <- data[,which(data[nrow(data),]<tyle)]
  return(data)
}

## 3
zastapSredniaGrup <- function(data)
{
  data <- as.data.frame(data)
  for (i in 1:ncol(data)) {
    ktore <- which(is.na(data[,i]))
    
    if(length(ktore)>0){
      for(j in 1:length(ktore)){
        if(data[ktore[j],ncol(data)]==0){
          data[ktore[j],i]=mean(data[which(data$class==0),i], na.rm = T)
        }
        else{
          data[ktore[j],i]=mean(data[which(data$class==1),i], na.rm = T)
        }
      }
    }
    
  }
  return(data)
}

## 4
outliery <- function(x)
{
  for(i in 1:ncol(x)-1){
    outliersIndex <- which(abs(x[,i])-3>0)
    if(sum(outliersIndex)!=0)
    {
      x <- x[-outliersIndex,]
    }
  }
  x <- as.data.frame(x)
  return(x)
}

outlieryKwantyl <- function(x)
{
  x <- as.data.frame(x)
  for(i in 1:(ncol(x)-1))
  {
    coefAsy <- moment(as.double(x[,i]), order=3, center=TRUE, na.rm = TRUE)/((sd(x[,i], na.rm = TRUE))^3)
    
    if(coefAsy>1){
      outliersIndex <- which((x[,i])>quantile(x[,i], .99, na.rm = TRUE))
      if(length(outliersIndex)>0){
        x <- x[-outliersIndex,] 
      }
    }
    
    if(coefAsy<(-1)){
      outliersIndex <- which((x[,i])<quantile(x[,i], .01, na.rm = TRUE))
      if(length(outliersIndex)>0){
        x <- x[-outliersIndex,] 
      }
    }
    if(coefAsy>-1&&coefAsy<1){
      mniejsze <- which((x[,i])<quantile(x[,i], .005, na.rm = TRUE))
      wieksze <- which((x[,i])>quantile(x[,i], .995, na.rm = TRUE))
      outliersIndex <- c(mniejsze, wieksze)
      if(length(outliersIndex)>0){
        x <- x[-outliersIndex,] 
      }
    }
    
  }
  x <- as.data.frame(x)
  return(x)
}


##5
wz <- function(x)
{
  sd(x, na.rm = T)/mean(x, na.rm = T)
}

dataYear <- function(x)
{
  x$sumaBrak <- 0
  for(i in 1:nrow(x))
  {
    x$sumaBrak[i] <- sum(is.na(x[i,]))
  }
  x <- usunNAKol(x)
  x <- usunNAWiersz(x)
  
  class <- x$class
  x <- x[,-(ncol(x)-1)]
  x$class <- class
  
  dane_sc <- c()
  dane_sc <- scale(x)
  dane_sc <- as.data.frame(dane_sc)
  dane_sc$class <- as.factor(x$class)
  
  knnOutput <- knnImputation(x, scale = TRUE)
  knnOutput  <-  knnImputation(dane_sc[,1:(ncol(dane_sc)-2)]) 
  knnOutput$sumaBrak <- dane_sc$sumaBrak
  knnOutput$class <- dane_sc$class
  X1year_n <-  outlieryKwantyl(knnOutput)
  
  wsp.zm <- sapply(X1year_n[,1:(ncol(X1year_n)-1)],wz)
  wsp.zm <- round(wsp.zm, 2)
  
  X1year_n2 <- X1year_n[,abs(wsp.zm)>0.2]
  X1year_n2$class <- ifelse(X1year_n2$class==0,"a0.Wyplacalny", "a1.Bankrut")
  X1year_n2$class <- as.factor(X1year_n2$class)
  
  korelacja <- cor(X1year_n2[,1:(ncol(X1year_n2)-1)])
  
  highCorr  <-  findCorrelation(korelacja, cutoff = .9)
  X1year_n2 <- X1year_n2[, -highCorr]
  
  return(X1year_n2)
}

kwantyl1 <- function(x)
{
  return(quantile(x, 0.25, na.rm = TRUE))
}
kwantyl3 <- function(x)
{
  return(quantile(x, 0.75, na.rm = TRUE))
}

maks <- function(x)
{
  return(max(x, na.rm = TRUE))
}

statystykiWydruk <- function(x, ktore)
{
  dx <- describe(x[1:ncol(x)-1])
  statystykix <- data.frame("Minimum"=dx$min, "Kwantyl1" = apply(x[,1:(ncol(x)-1)], 2, kwantyl1), "Mediana"=dx$median, 
                            "Średnia"=dx$mean, "Kwantyl3"=apply(x[,1:(ncol(x)-1)], 2, kwantyl3), "Maksimum"=apply(x[,1:(ncol(x)-1)], 2, maks), 
                            "Odchyleniest."=dx$sd,"Rozstęp"=dx$range, "Kurtoza"=dx$kurtosis,"Skosnosc"=dx$skew)
  statystykix <- round(statystykix,2)
  colnames(statystykix) <- c("Minimum", "Kwantyl 1", "Mediana", "Srednia", "Kwantyl 3", "Maksimum", "Odchylenie st.", 
                             "Rozstep", "Kurtoza", "Skosnosc")
  statystykix <- format(statystykix,decimal.mark=",")
  pdf(paste("wydruki/statystyki", ktore, "pdf", sep = "."), height=18, width=13)
  grid.table(statystykix)
  dev.off()
}

przedKor <- function(x)
{
  x$sumaBrak <- 0
  for(i in 1:nrow(x))
  {
    x$sumaBrak[i] <- sum(is.na(x[i,]))
  }
  x <- usunNAKol(x)
  x <- usunNAWiersz(x)
  
  class <- x$class
  x <- x[,-(ncol(x)-1)]
  x$class <- class
  
  dane_sc <- c()
  dane_sc <- scale(x)
  dane_sc <- as.data.frame(dane_sc)
  dane_sc$class <- as.factor(x$class)
  
  knnOutput <- knnImputation(x, scale = TRUE)
  knnOutput  <-  knnImputation(dane_sc[,1:(ncol(dane_sc)-2)]) 
  knnOutput$sumaBrak <- dane_sc$sumaBrak
  knnOutput$class <- dane_sc$class
  X1year_n <-  outlieryKwantyl(knnOutput)
  
  #wsp.zm <- sapply(X1year_n[,1:(ncol(X1year_n)-1)],wz)
  #wsp.zm <- round(wsp.zm, 2)
  
  #X1year_n2 <- X1year_n[,abs(wsp.zm)>0.2]
  X1year_n$class <- ifelse(X1year_n$class==0,"a0.Wyplacalny", "a1.Bankrut")
  X1year_n$class <- as.factor(X1year_n$class)
  
  
  return(X1year_n)
}

zmiennosc <- function(x)
{
  x$sumaBrak <- 0
  for(i in 1:nrow(x))
  {
    x$sumaBrak[i] <- sum(is.na(x[i,]))
  }
  x <- usunNAKol(x)
  x <- usunNAWiersz(x)
  
  class <- x$class
  x <- x[,-(ncol(x)-1)]
  x$class <- class
  
  dane_sc <- c()
  dane_sc <- scale(x)
  dane_sc <- as.data.frame(dane_sc)
  dane_sc$class <- as.factor(x$class)
  
  knnOutput <- knnImputation(x, scale = TRUE)
  knnOutput  <-  knnImputation(dane_sc[,1:(ncol(dane_sc)-2)]) 
  knnOutput$sumaBrak <- dane_sc$sumaBrak
  knnOutput$class <- dane_sc$class
  X1year_n <-  outlieryKwantyl(knnOutput)
  
  wsp.zm <- sapply(X1year_n[,1:(ncol(X1year_n)-1)],wz)
  wsp.zm <- round(wsp.zm, 2)
  return(wsp.zm)
}

recallBankrut<-function(x, what)
{
  if(ncol(x)==1)
  {
    return(c(1,1))
  }
  else
  {
    precis=x[2,2]/(x[2,2]+x[1,2])
    recal=x[2,2]/(x[2,2]+x[2,1])
    accura=(x[1,1]+x[2,2])/(sum(x[1:2, 1:2]))
    if(!missing(what))
    {
      
      if(what=="p")
      {
        return(precis)
      }
      if(what=="r")
      {
        return(recal)
      } 
    }
    else
    {
      return(c(precis, recal, accura))
    }
  }
  
}

usunNAWierszSk<-function(data){
  data$sumaNA<-0
  for(i in 1:nrow(data))
  {
    data$sumaNA[i]<-sum(is.na(data[i,]))
  }
  
  
  naIndex<-which((data$sumaNA>=5)&data$class==0)
  if(length(naIndex)>0)
  {
    data<-data[-naIndex,-(ncol(data))]
  }
  return(data)
}

usunNAKolSk<-function(data){
  data[nrow(data)+1,]<-0
  for(i in 1:ncol(data))
  {
    data[nrow(data),i]<-sum(is.na(data[,i]))
  }
  
  tyle<-mean(as.numeric(data[nrow(data),]))
  
  
  data<-data[,which(data[nrow(data),]<tyle)]
  return(data[-nrow(data),])
}

outlieryKwantylSk<-function(x){
  x<-as.data.frame(x)
  for(i in 1:(ncol(x)-1))
  {
    coefAsy<-moment(as.double(x[,i]), order=3, center=TRUE, na.rm = TRUE)/((sd(x[,i], na.rm = TRUE))^3)
    
    
    if(coefAsy>1)
    {
      outliersIndex<-which((x[,i])>quantile(x[,i], .99, na.rm = TRUE)&x$class!=1)
      if(length(outliersIndex)>0)
      {
        x<-x[-outliersIndex,] 
      }
    }
    
    if(coefAsy<(-1))
    {
      outliersIndex<-which((x[,i])<quantile(x[,i], .01, na.rm = TRUE)&x$class!=1)
      if(length(outliersIndex)>0)
      {
        x<-x[-outliersIndex,] 
      }
    }
    if(coefAsy>-1&&coefAsy<1)
    {
      wieksze<-which((x[,i])>quantile(x[,i], .995, na.rm = TRUE)&x$class!=1) 
      mniejsze<-which((x[,i])<quantile(x[,i], .005, na.rm = TRUE)&x$class!=1)
      
      outliersIndex<-c(wieksze, mniejsze)
      
      if(length(outliersIndex)>0)
      {
        x<-x[-outliersIndex,] 
      }
    }
    
  }
  x<-as.data.frame(x)
  return(x)
}

dataYearSk<-function(x){
  x$sumaBrak<-0
  for(i in 1:nrow(x))
  {
    x$sumaBrak[i]<-sum(is.na(x[i,]))
  }
  x<-usunNAKolSk(x)
  x<-usunNAWierszSk(x)
  
  class<-x$class
  x<-x[,-(ncol(x)-1)]
  x$class<-class
  
  dane_sc<-c()
  dane_sc<-scale(x)
  dane_sc<-as.data.frame(dane_sc)
  dane_sc$class<-as.factor(x$class)
  
  
  knnOutput <- knnImputation(dane_sc[,1:(ncol(dane_sc)-2)]) 
  knnOutput$sumaBrak<-dane_sc$sumaBrak
  knnOutput$class<-dane_sc$class
  X1year_n<- outlieryKwantylSk(knnOutput)
  
  X1year_n$class<-ifelse(X1year_n$class==0,"a0.Wyplacalny", "a1.Bankrut")
  X1year_n$class<-as.factor(X1year_n$class)
  
  korelacja<-cor(X1year_n[,1:(ncol(X1year_n)-1)])
  
  highCorr <- findCorrelation(korelacja, cutoff = .9)
  X1year_n <- X1year_n[, -highCorr]
  
  return(X1year_n)
}


