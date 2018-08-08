source("DataPreparation.R")
if (!require("corrplot")) install.packages("corrplot")
library(corrplot)


big5 = getCombinedData(data,F)
traits = c("Intro", "Neuro", "Openess", "Conscient", "Agree")

big5Cor = cor(big5[,traits])
big5Cor
corrplot.mixed(big5Cor,lower.col = "black", upper ="square")


big5$gender = as.numeric(levels(big5$gender))[big5$gender]
corWithAgeGender = cor(big5[,c(traits,c("age","gender"))])
corrplot.mixed(corWithAgeGender,lower.col = "black", upper ="square")
corrplot.mixed(corWithAgeGender,lower.col = "black", upper ="square",order="AOE")






temp = cor(clusters[,c("age","Intro", "Neuro", "Agree", "Conscient", "Openess","Cluster")])
corrplot.mixed(temp,lower = "number", upper = "pie", lower.col  = "black")