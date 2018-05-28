#This is a test script

#import required libraries
if(!require("psych")) install.packages("psych"); library("psych")
if(!require("factoextra")) install.packages("factoextra"); library("factoextra")
if(!require("readxl")) install.packages("readxl"); library("readxl")


#import data
data <- read_excel("Big5.xlsx")
clean <- function(x){
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

data <- clean(data)
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
  pca2 <- prcomp(data[,8:57],scale. = TRUE)
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
  return(fa(data[,8:57],nfactors = 5))
}
