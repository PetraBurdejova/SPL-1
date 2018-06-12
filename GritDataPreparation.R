#import required libraries
source("DataPreparation.R")
grit <- read.delim("data.csv")


factorsGrit <- fa(grit[,43:92],nfactors = 5,rotate = "varimax",fm="ml")
gritValue <- fa(grit[,3:14],nfactors = 1,rotate = "varimax",fm="ml")
summary(gritValue$scores)
summary(gritValue1$scores)
sd(gritValue$scores)

gritQuestions <- grit[,3:14]
temp <-rep(6,nrow(gritQuestions))
for (i in c(1,4,6,9,10,12)){
  gritQuestions[,i] <- temp - gritQuestions[,i]
}
temp <- rowSums(gritQuestions)/12

gritEvaluation <- fa.stats(grit[,43:92],factorsGrit$loadings)
gritEvaluation2 <- fa.stats(grit[,3:14],gritValue$loadings)

data.frame(factor.congruence(list(factorsGrit,factors1)))[6:10,0:5]


gritScores <- factorsGrit$scores
colnames(gritScores)<- c("Intro/Extra","Neuro","Agree","Openess","Conscient")
temp <- gritValue$scores
colnames(temp) <- c("Grit")
gritFactors <- cbind(grit[,1],grit[,31:42],gritScores,temp)
colnames(gritFactors)[1] <- "Country"
gritFactors$realGrit <- rowSums(gritQuestions)/12
gritFactors$realGrit <- gritFactors$realGrit - mean(gritFactors$realGrit)

density1 <- density(fScores1[,1])
density2 <- density(gritScores[,1])
plot(density1,main = "Comparisson of Introversion/Extraversion Big5/Grit",col = "red",xlab = "Introversion/Extraversion")
lines(density2,col="blue")
legend(x = "topright", y = NULL, legend=c("Big5", "Grit"),
       col=c("red", "blue"),pch = 15)

density1 <- density(fScores1[,2])
density2 <- density(gritScores[,2])
plot(density1,main = "Comparisson of Neuroticism Big5/Grit",col = "red",xlab = "Neuroticism")
lines(density2,col="blue")
legend(x = "topright", y = NULL, legend=c("Big5", "Grit"),
       col=c("red", "blue"),pch = 15)

density1 <- density(fScores1[,3])
density2 <- density(gritScores[,3])
plot(density1,main = "Comparisson of Agreeableness Big5/Grit",col = "red",xlab = "Agreeableness")
lines(density2,col="blue")
legend(x = "topright", y = NULL, legend=c("Big5", "Grit"),
       col=c("red", "blue"),pch = 15)

density1 <- density(fScores1[,4])
density2 <- density(gritScores[,4])
plot(density1,main = "Comparisson of Openness to Experience Big5/Grit",col = "red",xlab = "Openness to Experience")
lines(density2,col="blue")
legend(x = "topright", y = NULL, legend=c("Big5", "Grit"),
       col=c("red", "blue"),pch = 15)

density1 <- density(fScores1[,5])
density2 <- density(gritScores[,5])
plot(density1,main = "Comparisson of Conscient. Big5/Grit",col = "red",xlab = "Conscient.")
lines(density2,col="blue")
legend(x = "topright", y = NULL, legend=c("Big5", "Grit"),
       col=c("red", "blue"),pch = 15)

gritDensity <- density(gritFactors[,19])
realGritDensity <- density(gritFactors[,20])
plot(gritDensity,main = "The Density Distribution of Grit",col = "red", xlab = "Grit",ylim = c(0,0.5))
lines(realGritDensity,col="blue")
legend(x = "topright", y = NULL, legend=c("Factor Grit", "Real Grit"),
       col=c("red", "blue"),pch = 15)
