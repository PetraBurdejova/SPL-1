# import required libraries
source("DataPreparation.R")
source("DataPreparationAnalysis.R")
if (!require("glmnet")) install.packages("glmnet")
library("glmnet")

gritFactors = getGritDF()

malesGrit   = gritFactors[gritFactors$gender == 1, ]
femalesGrit = gritFactors[gritFactors$gender == 2, ]
fiveFactors = getFactors(data)
males       = fiveFactors[fiveFactors$gender == 1, ]
females     = fiveFactors[fiveFactors$gender == 2, ]
t.test(males[, 9], malesGrit[, 14])
t.test(females[, 9], femalesGrit[, 14])
t.test(malesGrit$realGrit,femalesGrit$realGrit)


name = colnames(gritFactors)
name = name[-c(19,21,22,23)]
gritPredictor             = lm(realGrit ~ Intro + Neuro + Agree + Openess + Conscient +age 
                               +gender + education + voted + married + urban, data = gritFactors)
summary(gritPredictor)
gritFactors$predictedGRit = predict(gritPredictor, newdata = gritFactors)

predictedGritDensity      = density(gritFactors$predictedGRit)
realGritDensity           = density(gritFactors$realGrit)
plot(realGritDensity, main = "The Density Distribution of Grit", col = "red", xlab = "Grit", ylim = c(0, 0.08))
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


regressors = c("Intro", "Neuro", "Agree", "Openess", "Conscient", "age", "gender", "education", "voted", "married", "urban")
target = c("realGrit")

addingRegressors = function(regressors, target, dataSet){
  RSS            = double()
  tempString     = c()
  for(x in regressors){
    tempString    = c(tempString,x)
    gritPredictor = lm(as.formula(paste(target, paste(tempString, collapse=" + "), sep=" ~ ")), data = dataSet)
    tempRSS       = mean(gritPredictor$residuals * gritPredictor$residuals)
    RSS           = c(RSS, tempRSS)
  }
  names(RSS) = regressors
  return(RSS)
}

attach(mtcars)
par(mfrow=c(2,1))
temp = addingRegressors(regressors,target, gritFactors)
temp
plot(temp, xaxt = "n", type = "l", ylab = "Residual Sum of Squares", xlab = "Regressor", main = "RSS changes when adding new Regressors")
axis(1, at = 1:11, labels = regressors, las = 2)

temp = addingRegressors(rev(regressors),target, gritFactors)
temp
plot(temp, xaxt = "n", type = "l", ylab = "Residual Sum of Squares", xlab = "Regressor", main = "RSS changes when adding new Regressors")
axis(1, at = 1:length(regressors), labels = rev(regressors), las = 2)
par(mfrow=c(1,1))


