source("DataPreparation.R")
if (!require("compare")) install.packages("compare")
library("compare")

realValues = getDataSetWithBig5(data, FALSE)

# Analyse how many factors to extract. Of course we want 5 since those are the personality traits measured. This
# seems to be supported with this quite simple test.
vss(data[, 8:57], fm = "ml")
fa.parallel(data[, 8:57], se.bars = T, fm = "ml")
temp = princomp(data[, 8:57])
screeplot(temp, npcs = 15, main = "Screeplot for PCA")
temp =  cov(data[,8:57])


ev = eigen(cor(data[, 8:57]))  # get eigenvalues
ap = parallel(subject = nrow(data[, 8:57]), var = ncol(data[, 8:57]), rep = 100, cent = 0.05)
nS = nScree(x = ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS)
par(col = "black")

# Principal Componant Analysis This is the method which performs the PCA. I chose 5 factors since this corresonds to
# the Big 5.
psychPCA = function(data) {
    pca                   = principal(data[, 8:57], nfactors = 5, rotate = "varimax")
    fiveFactors           = data.frame(pca$scores)
    colnames(fiveFactors) = c("Intro/Extra", "Neuro", "Agree", "Conscient", "Openess")
    fiveFactors           = cbind(data[, 1:7], data[, 58], fiveFactors)
    return(fiveFactors)
}

prcompPCA = function(data) {
    pca2 = prcomp(data[, 8:57], scale. = FALSE)
    fviz_eig(pca2, ncp = 20)
    tempDF = (data.frame(pca2$x)[, 0:5])
    colnames(tempDF) = c("Intro", "Neuro", "Agree", "Conscient", "Openess")
    pcaDF = cbind(data[, 1:7], data[, 58], tempDF)
    return(pcaDF)
}

princompPCA = function(data) {
    pca3 = princomp(data[, 8:57])
    fviz_eig(pca3)
    pcaDF2 = data.frame(pca3$scores[, 0:5])
    colnames(pcaDF2) = c("Intro", "Neuro", "Agree", "Conscient", "Openess")
    pcaDF2 = cbind(data[, 1:7], data[, 58], pcaDF2)
    pcaDF2$Neuro = -1 * pcaDF2$Neuro
    return(pcaDF2)
}

getFactors = function(data) {
    temp              = fa(data[, 8:57], nfactors = 5, rotate = "varimax", fm = "ml")
    tempDF            = data.frame(temp$scores)
    colnames(tempDF)  = c("Intro", "Neuro", "Agree", "Conscient", "Openess")
    tempDF$Neuro      = -1 * tempDF$Neuro
    return(cbind(data[, 1:7], data[, 58], tempDF))
}

# This space is for testing of data preparation
pca1 = principal(data[, 8:57], nfactors = 5, rotate = "varimax", scores = F)
pca1b = principal(data[, 8:17], nfactors = 1, rotate = "varimax")
pca2 = princomp(data[, 8:57])
# pca2 = pcaFunc2(data) pca3 = pcaFunc3(data)

# Comparing two different functions for factor extraction. The first one 'fa' is from the 'psych' package and by
# default uses the minres solution and oblimin rotation The factanal function uses maximum likelihood and varimax
# rotation. factors = fa(data[,8:57],nfactors = 5)
factors1 = fa(data[, 8:57], nfactors = 5, rotate = "varimax", fm = "ml")
factors1b = fa(data[, 8:17], nfactors = 1, rotate = "varimax", fm = "ml")
# factors1b = fa(data[,8:57],nfactors = 5,rotate = 'varimax') factors1c = fa(data[,8:57],nfactors = 5,fm='ml')
# factors2 = factanal(data[,8:57], 5) fScores = data.frame(factors$scores) fScores1 = data.frame(factors1$scores)
# fScores1b = data.frame(factors1b$scores) fScores1c = data.frame(factors1c$scores) coef =
# solve(factors2$correlation) %*% factors2$loadings

# Here we compare the results of scaling the data vs. no scaling. fScores2 =
# data.frame(scale(data[,8:57],FALSE,FALSE) %*% coef) fScores2Scaled = data.frame(scale(data[,8:57],TRUE,TRUE) %*%
# coef)

factors1Evaluation  = fa.stats(data[, 8:57], factors1$loadings)
pca1Evaluation      = fa.stats(data[, 8:57], pca2$loadings)


# This shows the average difference in values comparing fa and factanal, both scaled. The first comparison is for the
# default settings of fa vs. factanal.  The second comparison is fa using varimax and ml vs. factanal. The average
# difference in the first comparison is quite significant with roughly 0.121. As expected the results of the second
# comparison are almost 0.  temp = fScores-fScores2Scaled sum(abs(temp))/19719/5 temp = fScores1-fScores2Scaled
# sum(abs(temp))/19719/5 temp = fScores1b-fScores2Scaled sum(abs(temp))/19719/5 temp = fScores1c-fScores2Scaled
# sum(abs(temp))/19719/5 cov(data[,8:17]) cov(data[,18:27]) cov(data[,28:37]) cov(data[,38:47]) cov(data[,48:57])
# scaled.pca = scale(pca2[,9:13]) pca3b= princomp(data[,8:57]) pca1b= principal(data[,8:57],nfactors = 5,rotate =
# 'varimax')
data.frame(factor.congruence(pca2, factors1))
factorLoadings  = data.frame(factors1$loadings[1:50,1:5])
pcaLoadings     = data.frame(pca1$loadings[1:50,1:5])
temp            = abs(pcaLoadings - factorLoadings) 
sum(rowSums(temp))/250
# data.frame(factor.congruence(list(pca1,factors2)))[6:10,0:5] fa.plot(fac1) create_faGraph = function(){ temp =
# fa(data[,8:17],nfactors = 1) fa.diagram(temp) temp = fa(data[,18:27],nfactors = 1) fa.diagram(temp) temp =
# fa(data[,28:37],nfactors = 1) fa.diagram(temp) temp = fa(data[,38:47],nfactors = 1) fa.diagram(temp) temp =
# fa(data[,48:57],nfactors = 1) fa.diagram(temp) } create_faGraph()
fa.diagram(factors1)
fa.graph(factors1)

compareDesities = function(d) {
    realValues  = getDataSetWithBig5(d, F)
    oldValues   = getFactors(d)
    pcaValues   = prcompPCA(d)
    par(lwd = 2)
    for( x in  c("Intro", "Neuro", "Agree", "Conscient", "Openess")){
      real          = density(realValues[,c(x)])
      estimatedFA   = density(oldValues[,c(x)])
      estimatedPCA  = density(pcaValues[,c(x)])
      plot(estimatedFA, main = paste("The density distribution of",x), col = "blue", xlab = x)
      lines(real, col = "red")
      lines(estimatedPCA, col = "green")
      legend(x = "topright", y = NULL, legend = c("True", "FA", "PCA"), col = c("red", "blue", "green"), pch = 15)
    }
    
    par(lwd = 1)
}
  
realValues  = getDataSetWithBig5(data, F)
oldValues   = getFactors(data)
pcaValues   = princompPCA(data)

avgDiffFA   = abs(realValues[, 9:13] - oldValues[, 9:13])
n           = nrow(avgDiffFA) * 5
avgDiffFA   = rowSums(avgDiffFA)
avgDiffFA   = sum(avgDiffFA)/n

avgDiffPCA  = abs(realValues[, 9:13] - pcaValues[, 9:13])
avgDiffPCA  = rowSums(avgDiffPCA)
avgDiffPCA  = sum(avgDiffPCA)/n

summary(realValues[, 9:13])
summary(oldValues[, 9:13])
summary(pcaValues[, 9:13])
compareDesities(data)
