# import required libraries
source("DataPreparation.R")
source("DataPreparationAnalysis.R")
grit = read.delim("data.csv")
if (!require("glmnet")) install.packages("glmnet")
library("glmnet")

factorsGrit = fa(grit[, 43:92], nfactors = 5, rotate = "varimax", fm = "ml")
gritValue   = fa(grit[, 3:14], nfactors = 1, rotate = "varimax", fm = "ml")
summary(gritValue$scores)
sd(gritValue$scores)

gritQuestions = grit[, 3:14]
temp          = rep(6, nrow(gritQuestions))
for (i in c(1, 4, 6, 9, 10, 12)) {
    gritQuestions[, i] = temp - gritQuestions[, i]
}
temp            = rowSums(gritQuestions)/12
temp2 = getDataSetWithBig5(grit,T,F)

gritEvaluation  = fa.stats(grit[, 43:92], factorsGrit$loadings)
gritEvaluation2 = fa.stats(grit[, 3:14], gritValue$loadings)

data.frame(factor.congruence(factorsGrit, factors1))

gritScores                = factorsGrit$scores
fScores1                  = factors1$scores
colnames(gritScores)      = c("Intro/Extra", "Neuro", "Agree", "Conscient", "Openess")
gritScores                = data.frame(gritScores)
gritScores$Neuro          = -1 * (gritScores$Neuro)
gritScores2               = getDataSetWithBig5(grit, TRUE,F)
temp                      = gritValue$scores
colnames(temp)            = c("Grit")
gritFactors               = cbind(grit[, 1], grit[, 31:42], gritScores2, temp)
colnames(gritFactors)[1]  = "Country"
# gritFactors$realGrit      = rowSums(gritQuestions)/12
# gritFactors$realGrit      = gritFactors$realGrit - mean(gritFactors$realGrit)
# gritFactors$rescaled      = gritFactors$Grit * (sd(gritFactors$realGrit)/sd(gritFactors$Grit))
gritFactors$realGrit      = rowSums(gritQuestions)
#gritFactors$realGrit      = scale(gritFactors$realGrit)
gritFactors$rescaled      = gritFactors$Grit * (sd(gritFactors$realGrit)/sd(gritFactors$Grit))

density1 = density(fScores1[, 1])
density2 = density(gritScores[, 1])
plot(density1, main = "Comparison of Introversion/Extraversion Big5/Grit", col = "red", xlab = "Introversion/Extraversion")
lines(density2, col = "blue")
legend(x = "topright", y = NULL, legend = c("Big5", "Grit"), col = c("red", "blue"), pch = 15)

density1 = density(fScores1[, 2])
density2 = density(gritScores[, 2])
plot(density1, main = "Comparison of Neuroticism Big5/Grit", col = "red", xlab = "Neuroticism")
lines(density2, col = "blue")
legend(x = "topright", y = NULL, legend = c("Big5", "Grit"), col = c("red", "blue"), pch = 15)

density1 = density(fScores1[, 3])
density2 = density(gritScores[, 3])
plot(density1, main = "Comparison of Agreeableness Big5/Grit", col = "red", xlab = "Agreeableness")
lines(density2, col = "blue")
legend(x = "topright", y = NULL, legend = c("Big5", "Grit"), col = c("red", "blue"), pch = 15)

density1 = density(fScores1[, 4])
density2 = density(gritScores[, 4])
plot(density1, main = "Comparison of Openness to Experience Big5/Grit", col = "red", xlab = "Openness to Experience")
lines(density2, col = "blue")
legend(x = "topright", y = NULL, legend = c("Big5", "Grit"), col = c("red", "blue"), pch = 15)

density1 = density(fScores1[, 5])
density2 = density(gritScores[, 5])
plot(density1, main = "Comparison of Conscient. Big5/Grit", col = "red", xlab = "Conscient.")
lines(density2, col = "blue")
legend(x = "topright", y = NULL, legend = c("Big5", "Grit"), col = c("red", "blue"), pch = 15)

gritDensity     = density(gritFactors[, 19])
realGritDensity = density(gritFactors[, 20])
scaledDenisty   = density(gritFactors$rescaled)
plot(gritDensity, main = "The Density Distribution of Grit", col = "red", xlab = "Grit", ylim = c(0, 0.5))
lines(realGritDensity, col = "blue")
lines(scaledDenisty, col = "orange")
legend(x = "topright", y = NULL, legend = c("Factor Grit", "Real Grit", "Rescaled Grit"), col = c("red", "blue", "orange"), 
    pch = 15)


malesGrit   = gritFactors[gritFactors$gender == 1, ]
femalesGrit = gritFactors[gritFactors$gender == 2, ]
fiveFactors = getFactors(data)
males       = fiveFactors[fiveFactors$gender == 1, ]
females     = fiveFactors[fiveFactors$gender == 2, ]
t.test(males[, 9], malesGrit[, 14])
t.test(females[, 9], femalesGrit[, 14])

weightsNonGrit  = factors1$weights
weightsGrit     = factorsGrit$weights



name = colnames(gritFactors)
name = name[-c(19,21,22,23)]
gritPredictor             = lm(realGrit ~ Intro + Neuro + Agree + Openess + Conscient +age +gender + education, data = gritFactors)
#gritPredictor             = glmnet(as.matrix(gritFactors[,c("Intro","Agree","Neuro","Conscient","Openess")]), gritFactors$realGrit,alpha = 1,lambda = 0)
summary(gritPredictor)
gritFactors$predictedGRit = predict(gritPredictor, newdata = gritFactors)
#gritFactors$predictedGRit = scale(gritFactors$predictedGRit)
temp                      = mean((gritFactors$realGrit - gritFactors$predictedGRit)^2)


predictedGritDensity      = density(gritFactors$predictedGRit)
plot(realGritDensity, main = "The Density Distribution of Grit", col = "red", xlab = "Grit", ylim = c(0, 0.5))
lines(predictedGritDensity, col="blue")
  
res = gritPredictor$residuals
plot(sort(res),type = "l")
density1 = density(res)
h = hist(res,breaks = 40, col = "red")
xfit<-seq(min(res),max(res),length=40)
yfit<-dnorm(xfit,mean=mean(res),sd=sd(res))
yfit <- yfit*diff(h$mids[1:2])*length(res)
lines(xfit, yfit, col="blue", lwd=2) 

qqnorm(res)

quantile(gritFactors$realGrit,0.05)
quantile(gritFactors$realGrit,0.95)
quantile(gritFactors$predictedGRit, 0.05)
quantile(gritFactors$predictedGRit, 0.95)

density1 = density(gritFactors$realGrit)
density2 = density(gritFactors$predictedGRit)
plot(density1, main = "Comparison of real vs. predicted Grit", col = "red", xlab = "Grit", ylim = c(0,0.07))
lines(density2, col = "blue")
legend(x = "topright", y = NULL, legend = c("Real", "Predicted"), col = c("red", "blue"), pch = 15)


regressors = c("Intro", "Neuro", "Agree", "Openess", "Conscient", "age", "gender", "education")
target = c("realGrit")

addingRegressors = function(regressors, target, dataSet){
  RSS            = double()
  for(x in regressors){
    tempString    = c(tempString,x)
    gritPredictor = lm(as.formula(paste(target, paste(tempString, collapse=" + "), sep=" ~ ")), data = dataSet)
    tempRSS       = mean(gritPredictor$residuals * gritPredictor$residuals)
    RSS           = c(RSS, tempRSS)
  }
  names(RSS) = regressors
  return(RSS)
}


temp = addingRegressors(regressors,target, gritFactors)
plot(temp, xaxt = "n", type = "l", ylab = "Residual Sum of Squares", xlab = "Regressor", main = "RSS changes when adding new Regressors")
axis(1, at = 1:8, labels = regressors, las = 2)




