source("DataPreparation.R")
fiveFactors <- facFunc(data)

males = fiveFactors[fiveFactors$gender==1,]
females =fiveFactors[fiveFactors$gender==2,]
males[,9]
mean(males[,9])-mean(females[,9])
