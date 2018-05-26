#This is a test script

#import required libraries
if(!require("psych")) install.packages("psych"); library("psych")
if(!require("factoextra")) install.packages("factoextra"); library("factoextra")
if(!require("readxl")) install.packages("readxl"); library("readxl")
library(readxl)
library(factoextra)

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
data$ageCat <- findInterval(data$age,c(10,20,30,40,50,60,70,80,90))
#Principal Componant Analysis
#This is the method which performs the PCA. I chose 5 factors since this corresonds to the Big 5.
pca <- principal(data[,8:57],nfactors = 5,rotate = "varimax")
pca2 <- prcomp(data[,8:57],scale. = TRUE)
pca3 <- princomp(data[,8:57])
factorA <- fa(data[,8:57],nfactors = 5)
fviz_eig(pca2,ncp = 50)
fviz_eig(pca3)
fa.diagram(pca)
#This graphs show which variables are represented in which principal component.
#As expected the questions each component is represents the questions that deal with one personality traint.
#E.g. component 1 represents questions E1-E10 which deal with Extraversion. 
fa.graph(pca)
fiveFactors <- data.frame(pca$scores)
pcaDF <- cbind(data[,1:7],data[,58], (data.frame(pca2$x)[,0:5]))
pcaDF2 <- data.frame(pca3$scores[,0:5])
pcaDF2 <- cbind(data[,1:7],data[,58],pcaDF2)
colnames(fiveFactors) <-  c("Intro/Extra","Neuro","Agree","Conscient","Openess")
fiveFactors <- cbind(data[,1:7],data[,58],fiveFactors)

# avgAge1 <- apply(fiveFactors[fiveFactors$ageCat == 1,9:13],2,mean,na.rm=T)
# avgAge2 <- apply(fiveFactors[fiveFactors$ageCat == 2,9:13],2,mean,na.rm=T)
# avgAge3 <- apply(fiveFactors[fiveFactors$ageCat == 3,9:13],2,mean,na.rm=T)
# avgAge4 <- apply(fiveFactors[fiveFactors$ageCat == 4,9:13],2,mean,na.rm=T)
# avgAge5 <- apply(fiveFactors[fiveFactors$ageCat == 5,9:13],2,mean,na.rm=T)
# avgAge6 <- apply(fiveFactors[fiveFactors$ageCat == 6,9:13],2,mean,na.rm=T)
# avgAge7 <- apply(fiveFactors[fiveFactors$ageCat == 7,9:13],2,mean,na.rm=T)
# avgAge8 <- apply(fiveFactors[fiveFactors$ageCat == 8,9:13],2,mean,na.rm=T)
# avgAge9 <- apply(fiveFactors[fiveFactors$ageCat == 9,9:13],2,mean,na.rm=T)
# 
# ages <- data.frame(rbind(avgAge1,avgAge2,avgAge3,avgAge4,avgAge5,avgAge6,avgAge7,avgAge8,avgAge9))
# 
# plot(c(1:9),ages$Intro.Extra,type = "l",col="blue",ylim = c(-1.8,1.8),xlab = "Age Category", ylab = "Values of Traits", main = "Average values of the five traits in different age groups")
# lines(ages$Neuro,col="red")
# lines(ages$Agree,col="green")
# lines(ages$Openess,col="orange")     
# lines(ages$Conscient,col="black")
#legend("topleft", inset=.05, title="Trait", c("Extra","Neuro","Agree","Openess","Conscient"), fill=c("blue","red","green","orange","black"), horiz=TRUE,)
