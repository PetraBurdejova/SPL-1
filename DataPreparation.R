# import required libraries
if (!require("psych")) install.packages("psych")
library("psych")
if (!require("factoextra")) install.packages("factoextra")
library("factoextra")
if (!require("readxl")) install.packages("readxl")
library("readxl")
if (!require("GPArotation")) install.packages("GPArotation")
library("GPArotation")
if (!require("nFactors")) install.packages("nFactors")
library(nFactors)
if (!require("plyr")) install.packages("plyr")
library(plyr)
if (!require("formatR")) install.packages("formatR")
library(formatR)

# import data
clean = function() {
    x         = read_excel("Big5.xlsx")
    x$country = as.factor(x$country)
    x$race    = as.factor(x$race)
    x$gender  = as.factor(x$gender)
    x$hand    = as.factor(x$hand)
    x$source  = as.factor(x$source)
    
    # Replace unrealistic age valus
    x[x$age > 100, ]$age = 0
    x$ageCat = findInterval(x$age, c(10, 20, 30, 40, 50, 60, 70, 80, 90))
    return(x)
}

data = clean()


# Functions to get the 'real' values according to the evaluation key.
getResults = function(dataSet) {
    start               = which(colnames(dataSet) == "E1")
    extraversion        = dataSet[, start:(start + 9)]
    extraversion[, 2]   = 6 - extraversion[, 2]
    extraversion[, 4]   = 6 - extraversion[, 4]
    extraversion[, 6]   = 6 - extraversion[, 6]
    extraversion[, 8]   = 6 - extraversion[, 8]
    extraversion[, 10]  = 6 - extraversion[, 10]
    neuroticism         = dataSet[, (start + 10):(start + 19)]
    change              = c(1, 3, 5, 6, 7, 8, 9, 10)
    for (c in change) {
        neuroticism[, c] = 6 - neuroticism[, c]
    }
    agreeableness = dataSet[, (start + 20):(start + 29)]
    change        = c(1, 3, 5, 7)
    for (c in change) {
        agreeableness[, c] = 6 - agreeableness[, c]
    }
    conscientiousness = dataSet[, (start + 30):(start + 39)]
    change            = c(2, 4, 6, 8)
    for (c in change) {
        conscientiousness[, c] = 6 - conscientiousness[, c]
    }
    openess = dataSet[, (start + 40):(start + 49)]
    change  = c(2, 4, 6)
    for (c in change) {
        openess[, c] = 6 - openess[, c]
    }
    dataSet$Intro       = rowSums(extraversion)
    dataSet$Neuro       = rowSums(neuroticism)
    dataSet$Agree       = rowSums(agreeableness)
    dataSet$Conscient   = rowSums(conscientiousness)
    dataSet$Openess     = rowSums(openess)
    return(dataSet)
}


getDataSetWithBig5 = function(data, grit, scale) {
    tempSet = getResults(data)
    names   = c("Intro","Neuro","Agree","Conscient","Openess")
    if (grit) {
        tempSet = tempSet[, names]
        if(scale){
          tempSet[,1:5] = data.frame(scale(tempSet[,1:5]))
        }
    } else {
        tempSet = cbind(tempSet[, 1:7], tempSet[,names])
        if(scale){
          tempSet[,8:12] = data.frame(scale(tempSet[,8:12]))
        }
    }
    return(tempSet)
}

