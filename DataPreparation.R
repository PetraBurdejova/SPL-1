#import required libraries
if(!require("psych")) install.packages("psych"); library("psych")
if(!require("factoextra")) install.packages("factoextra"); library("factoextra")
if(!require("readxl")) install.packages("readxl"); library("readxl")
if(!require("GPArotation")) install.packages("GPArotation"); library("GPArotation")
if(!require("nFactors")) install.packages("nFactors"); library(nFactors)

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


#Functions to get the "real" values according to the evaluation key
#The results have been rescaled by dividing them through 10 and shifting the mean to 0
getResults <- function(dataSet){
  start <- which(colnames(dataSet)=="E1")
  extraversion <- dataSet[,start:(start+9)]
  extraversion[,2] <- 6-extraversion[,2]
  extraversion[,4] <- 6-extraversion[,4]
  extraversion[,6] <- 6-extraversion[,6]
  extraversion[,8] <- 6-extraversion[,8]
  extraversion[,10] <- 6-extraversion[,10]
  neuroticism <- dataSet[,(start+10):(start+19)]
  change <- c(1,3,5,6,7,8,9,10)
  for( c in change){
    neuroticism[,c] <- 6-neuroticism[,c]
  }
  agreeableness <- dataSet[,(start+20):(start+29)]
  change <- c(1,3,5,7)
  for( c in change){
    agreeableness[,c] <- 6- agreeableness[,c]
  }
  conscientiousness <- dataSet[,(start+30):(start+39)]
  change <- c(2,4,6,8)
  for(c in change){
    conscientiousness[,c] <- 6- conscientiousness[,c]
  }
  openess <- dataSet[,(start+40):(start+49)]
  change <- c(2,4,6)
  for(c in change){
    openess[,c] <- 6- openess[,c]
  }
  dataSet$Extraversion <- rowSums(extraversion)
  dataSet$Neuroticism <- rowSums(neuroticism)
  dataSet$Agreeableness <- rowSums(agreeableness)
  dataSet$Conscientiousness <- rowSums(conscientiousness)
  dataSet$Openess <- rowSums(openess)
  return(dataSet)
}


getDataSetWithBig5 <- function(data,grit){
  tempSet <- getResults(data)
  if(grit){
    tempSet <- tempSet[,99:103]
  }
  else {
    tempSet <- cbind(tempSet[,1:7],tempSet[58:63])
  }
  tempSet$Extraversion <- tempSet$Extraversion/10
  tempSet$Extraversion <- tempSet$Extraversion-mean(tempSet$Extraversion)
  tempSet$Neuroticism <- tempSet$Neuroticism/10
  tempSet$Neuroticism <- tempSet$Neuroticism-mean(tempSet$Neuroticism)
  tempSet$Openess <- tempSet$Openess/10
  tempSet$Openess <- tempSet$Openess-mean(tempSet$Openess)
  tempSet$Conscientiousness <- tempSet$Conscientiousness/10
  tempSet$Conscientiousness <- tempSet$Conscientiousness-mean(tempSet$Conscientiousness)
  tempSet$Agreeableness <- tempSet$Agreeableness/10
  tempSet$Agreeableness <- tempSet$Agreeableness-mean(tempSet$Agreeableness)
  return(tempSet)
}

