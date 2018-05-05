#This is a test script

#import required libraries
library(readxl)
if(!require("psych")) install.packages("psych"); library("psych")

#import data
data <- read_excel("Big5.xlsx")

#transform some columns from numeric to factor
data$country <- as.factor(data$country)
data$race <- as.factor(data$race)
data$gender <- as.factor(data$gender)
data$hand <- as.factor(data$hand)
data$source <- as.factor(data$source)

#Replace unrealistic age valus
data[data$age > 100,]$age <- NaN

#Principal Componant Analysis
#This is the method which performs the PCA. I chose 5 factors since this corresonds to the Big 5.
pca <- principal(data[8:57],nfactors = 5,rotate = "varimax")

fa.diagram(pca)
#This graphs show which variables are represented in which principal component.
#As expected the questions each component is represents the questions that deal with one personality traint.
#E.g. component 1 represents questions E1-E10 which deal with Extraversion. 
fa.graph(pca)
fiveFactors <- data.frame(pca$scores)
colnames(fiveFactors) <-  c("Intro/Extra","Neuro","Agree","Conscient","Openess")
fiveFactors <- cbind(data[,1:7],fiveFactors)