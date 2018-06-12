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


#Analyse how many factors to extract. Of course we want 5 since those are the personality traits measured.
#This seems to be supported with this quite simple test.
vss(data[,8:57],fm="ml")
fa.parallel(data[,8:57],se.bars = T,fm = "ml")

ev <- eigen(cor(data[,8:57])) # get eigenvalues
ap <- parallel(subject=nrow(data[,8:57]),var=ncol(data[,8:57]),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)
par(col = "black")

#Principal Componant Analysis
#This is the method which performs the PCA. I chose 5 factors since this corresonds to the Big 5.
psychPCA <- function(data){
  pca <- principal(data[,8:57],nfactors = 5,rotate = "varimax")
  fa.diagram(pca)
  #fa.graph(pca)
  fiveFactors <- data.frame(pca$scores)
  colnames(fiveFactors) <-  c("Intro/Extra","Neuro","Agree","Conscient","Openess")
  fiveFactors <- cbind(data[,1:7],data[,58],fiveFactors)
  return(fiveFactors)
}

prcompPCA <- function(data){
  pca2 <- prcomp(data[,8:57],scale. = FALSE)
  fviz_eig(pca2,ncp = 50)
  pcaDF <- cbind(data[,1:7],data[,58], (data.frame(pca2$x)[,0:5]))
  return(pcaDF)
}

princompPCA <- function(data){
  pca3 <- princomp(data[,8:57])
  fviz_eig(pca3)
  pcaDF2 <- data.frame(pca3$scores[,0:5])
  pcaDF2 <- cbind(data[,1:7],data[,58],pcaDF2)
  return(pcaDF2)
}

getFactors <- function(data){
  temp <- fa(data[,8:57],nfactors = 5,rotate = "varimax",fm="ml")
  tempDF <- data.frame(temp$scores)
  colnames(tempDF) <- c("Intro/Extra","Neuro","Agree","Openess","Conscient")
  return(cbind(data[,1:7],data[,58],tempDF))
}

# This space is for testing of data preparation
pca1 <- principal(data[,8:57],nfactors = 5,rotate = "varimax")
# pca2 <- pcaFunc2(data)
# pca3 <- pcaFunc3(data)

#Comparing two different functions for factor extraction.
#The first one "fa" is from the "psych" package and by default uses the minres solution and oblimin rotation
#The factanal function uses maximum likelihood and varimax rotation.
factors <- fa(data[,8:57],nfactors = 5)
factors1 <- fa(data[,8:57],nfactors = 5,rotate = "varimax",fm="ml")
factors1b <- fa(data[,8:57],nfactors = 5,rotate = "varimax")
factors1c <- fa(data[,8:57],nfactors = 5,fm="ml")
factors2 <- factanal(data[,8:57], 5)
fScores <- data.frame(factors$scores)
fScores1 <- data.frame(factors1$scores)
fScores1b <- data.frame(factors1b$scores)
fScores1c <- data.frame(factors1c$scores)
coef <- solve(factors2$correlation) %*% factors2$loadings

#Here we compare the results of scaling the data vs. no scaling.
fScores2 <- data.frame(scale(data[,8:57],FALSE,FALSE) %*% coef)
fScores2Scaled <- data.frame(scale(data[,8:57],TRUE,TRUE) %*% coef)

factors1Evaluation <- fa.stats(data[,8:57],factors1$loadings)
factorsEvaluation <- fa.stats(data[,8:57],factors$loadings)

#This shows the average difference in values comparing fa and factanal, both scaled.
#The first comparison is for the default settings of fa vs. factanal. 
#The second comparisson is fa using varimax and ml vs. factanal.
#The average difference in the first comparisson is quite significant with roughly 0.121.
#As expected the results of the second comparisson are almost 0.
temp <- fScores-fScores2Scaled
sum(abs(temp))/19719/5

temp <- fScores1-fScores2Scaled
sum(abs(temp))/19719/5

temp <- fScores1b-fScores2Scaled
sum(abs(temp))/19719/5

temp <- fScores1c-fScores2Scaled
sum(abs(temp))/19719/5
# cov(data[,8:17])
# cov(data[,18:27])
# cov(data[,28:37])
# cov(data[,38:47])
# cov(data[,48:57])
# scaled.pca <- scale(pca2[,9:13])
# pca3b<- princomp(data[,8:57])
# pca1b<- principal(data[,8:57],nfactors = 5,rotate = "varimax")
data.frame(factor.congruence(list(pca1,factors)))[6:10,0:5]
data.frame(factor.congruence(list(pca1,factors2)))[6:10,0:5]
# fa.plot(fac1)
# create_faGraph <- function(){
#   temp <- fa(data[,8:17],nfactors = 1)
#   fa.diagram(temp)
#   temp <- fa(data[,18:27],nfactors = 1)
#   fa.diagram(temp)
#   temp <- fa(data[,28:37],nfactors = 1)
#   fa.diagram(temp)
#   temp <- fa(data[,38:47],nfactors = 1)
#   fa.diagram(temp)
#   temp <- fa(data[,48:57],nfactors = 1)
#   fa.diagram(temp)
# }
# create_faGraph()
#fa.diagram(factors1)
#fa.graph(fac1)
