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

reorderColumns = function(dataSet,gritSort,questionnaireNames = NULL,gritNames = NULL){
    if(missing(questionnaireNames)){
        letters            = c("E","N","A","C","O")
        questionnaireNames = c()
        for(l in letters){
          for(x in 1:10){
            questionnaireNames = c(questionnaireNames, paste(c(l,x),collapse = ""))
          }
        }
    }
    notQuestionnaire = setdiff(colnames(dataSet),questionnaireNames)
    resultDF         = dataSet[,c(notQuestionnaire,questionnaireNames)]
    
    if(gritSort){
        if(missing(gritNames)){
            letter    = "GS"
            gritNames = c()
            for(x in 1:12){
                gritNames = c(gritNames, paste(c(letter,x),collapse = ""))
            }
        }
        notGrit  = setdiff(colnames(resultDF),gritNames)
        resultDF = resultDF[,c(notGrit,gritNames)]
    }
    return(resultDF)
}


clean = function(sourceFile,gritSort) {
    tryCatch({
        x         = read_excel(sourceFile)
        x$country = as.factor(x$country)
        x$race    = as.factor(x$race)
        x$gender  = as.factor(x$gender)
        x$hand    = as.factor(x$hand)
        if(is.null(x$source)){
            x$source  = 6
        }    else{
            x$source  = as.factor(x$source)
        }
        # Replace unrealistic age valus
        x[x$age > 1911,]$age = abs((2012 - x[x$age > 1911,]$age)) 
        agePredictor = lm(age~., data = x[x$age <= 100,])
        tempAge      = predict(agePredictor, newdata = x[x$age > 100,])
        tempAge      = as.integer(tempAge)
        if(anyNA(tempAge)){
            x[x$age > 100, ]$age = as.integer(mean(x[x$age <= 100,]$age))
        } else{
            for(y in 1:length(tempAge)){
              if(tempAge[y] < 10){
                tempAge[y] = 10
              }
            }
            x[x$age > 100,]$age = tempAge
        }
        # x[x$age > 100, ]$age = 0
        # x[x$age > 100, ]$age = median(x[x$age <= 100,]$age)
        # tempAges             = sample(x[x$age <= 100,]$age, nrow(x[x$age > 100,]),replace = T)
        # x[x$age > 100, ]$age = tempAges
        x$ageCat             = findInterval(x$age, c(10, 20, 30, 40, 50, 60, 70, 80, 90))
        x                    = reorderColumns(x,gritSort)
        return(x)
    }, error = function(e) e)
  
    tryCatch({
        x         = read.delim(sourceFile)
        x$country = as.factor(x$country)
        x$race    = as.factor(x$race)
        x$gender  = as.factor(x$gender)
        x$hand    = as.factor(x$hand)
        if(is.null(x$source)){
            x$source  = 5
        }    else{
            x$source  = as.factor(x$source)
        }
        # Replace unrealistic age valus
        x[x$age > 1911,]$age = abs(2012 - x[x$age > 1911,]$age) 
        agePredictor = lm(age~., data = x[x$age <= 100,])
        tempAge      = predict(agePredictor, newdata = x[x$age > 100,])
        tempAge      = as.integer(tempAge)
        if(anyNA(tempAge)){
            x[x$age > 100, ]$age = as.integer(mean(x[x$age <= 100,]$age))
        } else{
            for(y in 1:length(tempAge)){
                if(tempAge[y] < 10){
                    tempAge[y] = 10
                }
            }
            x[x$age > 100,]$age = tempAge
        }
        # x[x$age > 100, ]$age = 0
        # x[x$age > 100, ]$age = median(x[x$age <= 100,]$age)
        # tempAges             = sample(x[x$age <= 100,]$age, nrow(x[x$age > 100,]),replace = T)
        # x[x$age > 100, ]$age = tempAges
        x$ageCat = findInterval(x$age, c(10, 20, 30, 40, 50, 60, 70, 80, 90))
        if(!is.null(x$familysize)){
            x[x$familysize > 10,]$familysize = median(x$familysize)
        }
    },error = function(e) e)
    if(!is.null(x$married)){
        x$married = as.factor(x$married)
    }
    if(!is.null(x$voted)){
        x$voted = as.factor(x$voted)
    }
    if(!is.null(x$religion)){
        x$religion = as.factor(x$religion)
    }
    if(!is.null(x$race)){
        x$race = as.factor(x$race)
    }
    if(!is.null(x$orientation)){
        x$orientation = as.factor(x$orientation)
    }
    x = reorderColumns(x,gritSort)
    return(x)
}

data = clean("Big5.xlsx",F)


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
        tempSet = cbind(tempSet[, c("country", "gender", "engnat", "age", "hand","race", "ageCat", "source")], tempSet[,names])
        if(scale){
          tempSet[,names] = data.frame(scale(tempSet[,names]))
        }
    }
    return(tempSet)
}

getGritDF = function(fileName = "Grit.csv"){
    grit          = clean(fileName,T)
    factorsGrit   = fa(grit[, which(colnames(grit) == "E1"):(which(colnames(grit) == "E1")+49)], nfactors = 5, rotate = "varimax", fm = "ml")
    gritValue     = fa(grit[, which(colnames(grit) == "GS1"):(which(colnames(grit) == "GS1")+11)], nfactors = 1, rotate = "varimax", fm = "ml")
    gritQuestions = grit[, which(colnames(grit) == "GS1"):(which(colnames(grit) == "GS1")+11)]
    temp          = rep(6, nrow(gritQuestions))
    for (i in c(1, 4, 6, 9, 10, 12)) {
        gritQuestions[, i] = temp - gritQuestions[, i]
    }
    temp2                     = getDataSetWithBig5(grit,TRUE,FALSE)
    gritScores                = factorsGrit$scores
    colnames(gritScores)      = c("Intro/Extra", "Neuro", "Agree", "Conscient", "Openess")
    gritScores                = data.frame(gritScores)
    gritScores$Neuro          = -1 * (gritScores$Neuro)
    gritScores2               = getDataSetWithBig5(grit, TRUE,FALSE)
    temp                      = gritValue$scores
    colnames(temp)            = c("Grit")
    gritFactors               = cbind(grit[, c("country", "education", "urban", "gender", "engnat", "age", "hand", "religion", "orientation","race","voted","married", "familysize", "ageCat", "source")], gritScores2, temp)
    gritFactors$realGrit      = rowSums(gritQuestions)
    return(gritFactors)
}

getCombinedData = function(scaled,big5FileName = "Big5.xlsx", gritFileName = "Grit.csv"){
    fiveFactors = getDataSetWithBig5(clean(big5FileName,FALSE),FALSE,scaled)
    gritFactors = getGritDF(gritFileName)
    tempDF      = rbind(fiveFactors,gritFactors[,colnames(fiveFactors)])
    return(tempDF)
}
