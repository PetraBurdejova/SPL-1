# import required libraries
source("DataPreparation.R")
source("DataPreparationAnalysis.R")
if (!require("glmnet")) install.packages("glmnet")
library("glmnet")

gritFactors = getGritDF()

malesGrit   = gritFactors[gritFactors$gender == 1, ]
femalesGrit = gritFactors[gritFactors$gender == 2, ]
t.test(malesGrit$realGrit,femalesGrit$realGrit)


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



#Comparing top 5% and bottom 5% of people w.r.t. grit
quantile(gritFactors$realGrit,0.05)
quantile(gritFactors$realGrit,0.95)


top5Grit    = gritFactors[gritFactors$realGrit>=quantile(gritFactors$realGrit,0.95),]
bottom5Grit = gritFactors[gritFactors$realGrit<=quantile(gritFactors$realGrit,0.05),]
summary(top5Grit)
summary(bottom5Grit)
t.test(top5Grit$education,bottom5Grit$education)
top5Grit$education    = as.factor(top5Grit$education)
bottom5Grit$education = as.factor(bottom5Grit$education)
levels(top5Grit$education) =  c("N/A", "Less than High School", "High School", "University", "Graduate")
levels(bottom5Grit$education) =  c("N/A", "Less than High School", "High School", "University", "Graduate")

barplot(rbind(prop.table(table(bottom5Grit$education)),prop.table(table(top5Grit$education))),
        beside = TRUE, col= c("red","blue"),ylim = c(0,0.6), 
        main = "Comparison of the top 5% vs. bottom 5% w.r.t. education levels",
        xlab = "Level of education", ylab = "Density")
legend(x = "topright", y = NULL, legend = c("Bottom 5%", "Top 5%"), col = c("red", "blue"), pch = 15)
