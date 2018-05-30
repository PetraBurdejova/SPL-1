#This is a test script

#import required libraries
if(!require("psych")) install.packages("psych"); library("psych")
if(!require("factoextra")) install.packages("factoextra"); library("factoextra")
if(!require("readxl")) install.packages("readxl"); library("readxl")
if(!require("GPArotation")) install.packages("GPArotation"); library("GPArotation")


#import data
clean <- function(){
  x <- read_excel("Big5.xlsx")
  x$country <- as.factor(x$country)
  x$race <- as.factor(x$race)
  x$gender <- as.factor(x$gender)
  x$hand <- as.factor(x$hand)
  x$source <- as.factor(x$source)
  
  #Replace unrealistic age valus
  x[x$age > 100,]$age <- NaN
  x$ageCat <- findInterval(x$age,c(10,20,30,40,50,60,70,80,90))
  return(x)
}

data <- clean()
#transform some columns from numeric to factor

#Principal Componant Analysis
#This is the method which performs the PCA. I chose 5 factors since this corresonds to the Big 5.
pcaFunc1 <- function(data){
  pca <- principal(data[,8:57],nfactors = 5,rotate = "varimax")
  fa.diagram(pca)
  #fa.graph(pca)
  fiveFactors <- data.frame(pca$scores)
  colnames(fiveFactors) <-  c("Intro/Extra","Neuro","Agree","Conscient","Openess")
  fiveFactors <- cbind(data[,1:7],data[,58],fiveFactors)
  return(fiveFactors)
}

pcaFunc2 <- function(data){
  pca2 <- prcomp(data[,8:57],scale. = FALSE)
  fviz_eig(pca2,ncp = 50)
  pcaDF <- cbind(data[,1:7],data[,58], (data.frame(pca2$x)[,0:5]))
  return(pcaDF)
}

pcaFunc3 <- function(data){
  pca3 <- princomp(data[,8:57])
  fviz_eig(pca3)
  pcaDF2 <- data.frame(pca3$scores[,0:5])
  pcaDF2 <- cbind(data[,1:7],data[,58],pcaDF2)
  return(pcaDF2)
}

facFunc <- function(data){
  temp <- fa(data[,8:57],nfactors = 5)
  tempDF <- data.frame(temp$scores)
  colnames(tempDF) <- c("Intro/Extra","Neuro","Agree","Conscient","Openess")
  return(cbind(data[,1:7],data[,58],tempDF))
}

#This space is for testing of data preparation
# pca1 <- pcaFunc1(data)
# pca2 <- pcaFunc2(data)
# pca3 <- pcaFunc3(data)
#fac1 <- fa(data[,8:57],nfactors = 5)
# scaled.pca <- scale(pca2[,9:13])
# pca3b<- princomp(data[,8:57])
# pca1b<- principal(data[,8:57],nfactors = 5,rotate = "varimax")
# factor.congruence(list(pca1b,fac1))
# vss(data[,8:57])
# fa.plot(fac1)
# fa.diagram(fac1)
# fa.graph(fac1)