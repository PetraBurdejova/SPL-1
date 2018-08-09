source("Big5GritDataPreparation/Big5GritDataPreparation.R")

realValues       = getCombinedData(TRUE)
traitNames       = c("Intro", "Neuro", "Agree", "Conscient", "Openess")
start            = which(colnames(data) == "E1")
finish           = start + 49
notQuestionnaire = setdiff(colnames(data), colnames(data)[start:finish])


# Analyse how many factors to extract. Of course we want 5 since those are the personality traits
# measured. This seems to be supported with this quite simple test.
vss(data[, start:finish], fm = "ml")
fa.parallel(data[, start:finish], se.bars = T, fm = "ml")
temp = princomp(data[, start:finish])
screeplot(temp, npcs = 15, main = "Screeplot for PCA")
temp = cov(data[, start:finish])


ev = eigen(cor(data[, start:finish]))  # get eigenvalues
ap = parallel(subject = nrow(data[, start:finish]), var = ncol(data[, start:finish]), rep = 100, cent = 0.05)
nS = nScree(x = ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS)
par(col = "black")

# Principal Componant Analysis: This is the method which performs the PCA. I chose 5 pcas since
# this corresonds to the Big 5.
psychPCA = function(data) {
    pca                   = principal(data[, start:finish], nfactors = 5, rotate = "none")
    fiveFactors           = data.frame(pca$scores)
    colnames(fiveFactors) = c("Intro", "Neuro", "Agree", "Conscient", "Openess")
    fiveFactors           = cbind(data[, notQuestionnaire], fiveFactors)
    fiveFactors$Neuro     = -1 * fiveFactors$Neuro
    return(fiveFactors)
}

prcompPCA = function(data) {
    pca = prcomp(data[, start:finish], scale. = F)
    fviz_eig(pca, ncp = 20)
    tempDF           = (data.frame(pca$x)[, 0:5])
    colnames(tempDF) = c("Intro", "Neuro", "Agree", "Conscient", "Openess")
    pcaDF            = cbind(data[, notQuestionnaire], tempDF)
    return(pcaDF)
}

princompPCA = function(data) {
    pca = princomp(data[, start:finish])
    fviz_eig(pca)
    pcaDF           = data.frame(pca$scores[, 0:5])
    colnames(pcaDF) = c("Intro", "Neuro", "Agree", "Conscient", "Openess")
    pcaDF           = cbind(data[, notQuestionnaire], pcaDF)
    pcaDF$Neuro     = -1 * pcaDF$Neuro
    return(pcaDF)
}

getFactors = function(data) {
    temp             = fa(data[, start:finish], nfactors = 5, rotate = "varimax", fm = "ml")
    tempDF           = data.frame(temp$scores)
    colnames(tempDF) = c("Intro", "Neuro", "Agree", "Conscient", "Openess")
    tempDF$Neuro     = -1 * tempDF$Neuro
    return(cbind(data[, notQuestionnaire], tempDF))
}

# This space is for testing of data preparation
pca = princomp(data[, start:finish])
pca2 = prcomp(data[, start:finish])
fviz_screeplot(pca, addlabels = T)
fviz_screeplot(pca2, addlabels = T)

for (x in 1:5) {
    plot(fviz_contrib(pca, choice = "var", axes = x, top = 10))
}


factors = fa(data[, start:finish], nfactors = 5, rotate = "varimax", fm = "ml")


factorsEvaluation = fa.stats(data[, start:finish], factors$loadings)
pcaEvaluation = fa.stats(data[, start:finish], pca$loadings)

# Compare two pca methods
mean(pca$center - pca2$center)
mean(pca$sdev - pca2$sdev)


compareDesities = function(d) {
    realValues = getDataSetWithBig5(d, F, T)
    oldValues = getFactors(d)
    pcaValues = princompPCA(d)
    par(lwd = 2)
    for (x in traitNames) {
        real = density(realValues[, c(x)])
        estimatedFA = density(oldValues[, c(x)])
        estimatedPCA = density(pcaValues[, c(x)])
        plot(estimatedFA, main = paste("The density distribution of", x), col = "blue", xlab = x, 
            xlim = c(-6, 6))
        lines(real, col = "red")
        lines(estimatedPCA, col = "green")
        legend(x = "topright", y = NULL, legend = c("True", "FA", "PCA"), col = c("red", "blue", "green"), 
            pch = 15)
    }
    
    par(lwd = 1)
}


compareDifferences = function(d) {
    realValues = getDataSetWithBig5(d, F, T)
    oldValues  = getFactors(d)
    pcaValues  = psychPCA(d)
    
    avgDiffFA = abs(realValues[, traitNames] - oldValues[, traitNames])
    n = nrow(avgDiffFA) * 5
    avgDiffFA = rowSums(avgDiffFA)
    avgDiffFA = sum(avgDiffFA)/n
    
    avgDiffPCA = abs(realValues[, traitNames] - pcaValues[, traitNames])
    avgDiffPCA = rowSums(avgDiffPCA)
    avgDiffPCA = sum(avgDiffPCA)/n
    
    summary(realValues[, traitNames])
    summary(oldValues[, traitNames])
    summary(pcaValues[, traitNames])
    print(avgDiffFA)
    print(avgDiffPCA)
}

compareDesities(data)
compareDifferences(data)



# Check how much overlap there is between the original data set and the grit data set.  Since
# there is very little we can assume that they are distinct data set. Therefore we can combine
# them during the analysis, in order to have a larger data set.

fiveFactors = getDataSetWithBig5(data, FALSE, F)
gritFactors = getGritDF()

nrow(merge(x = fiveFactors, y = gritFactors, by = c(c("country", "gender", "engnat", "age", "hand", 
    "race", "ageCat"), traitNames), all = FALSE))
nrow(merge(x = fiveFactors, y = gritFactors, by = traitNames, all = F))
