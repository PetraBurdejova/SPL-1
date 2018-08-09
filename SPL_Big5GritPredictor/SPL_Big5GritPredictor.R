# import required libraries
source("SPL_Big5GritDataPreparation/SPL_Big5GritDataPreparation.R")


gritFactors = getGritDF()


#Building an estimator for grit and evaluating the results.

regressors    = c("Intro", "Neuro", "Agree", "Openess", "Conscient", "age", "gender", "education", "voted", "familysize", "married", "urban")
target        = c("realGrit")
gritPredictor = lm(as.formula(paste(target, paste(regressors, collapse=" + "), sep=" ~ "))
                               , data = gritFactors)
summary(gritPredictor)
gritFactors$predictedGRit = predict(gritPredictor, newdata = gritFactors)


res = gritPredictor$residuals
plot(sort(res),type = "l")
density1 = density(res)
h        = hist(res,breaks = 40, col = "red")
xfit     = seq(min(res),max(res),length=40)
yfit     = dnorm(xfit,mean=mean(res),sd=sd(res))
yfit     = yfit*diff(h$mids[1:2])*length(res)
lines(xfit, yfit, col="blue", lwd=2) 

qqnorm(res)

density1 = density(gritFactors$realGrit)
density2 = density(gritFactors$predictedGRit)
plot(density1, main = "Comparison of real vs. predicted Grit", col = "red", xlab = "Grit", ylim = c(0,0.07))
lines(density2, col = "blue")
legend(x = "topright", y = NULL, legend = c("Real", "Predicted"), col = c("red", "blue"), pch = 15)



#Looking at the effect of adding the different regressors one by one.

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
axis(1, at = 1:length(regressors), labels = regressors, las = 2)

temp = addingRegressors(rev(regressors),target, gritFactors)
temp
plot(temp, xaxt = "n", type = "l", ylab = "Residual Sum of Squares", xlab = "Regressor", main = "RSS changes when adding new Regressors")
axis(1, at = 1:length(regressors), labels = rev(regressors), las = 2)
par(mfrow=c(1,1))