source("SPL_Big5GritDataPreparation/SPL_Big5GritDataPreparation.R")
if (!require("corrplot")) install.packages("corrplot")
library(corrplot)


big5 = getCombinedData(FALSE)
traits = c("Intro", "Neuro", "Openess", "Conscient", "Agree")

big5Cor = cor(big5[, traits])
big5Cor
corrplot.mixed(big5Cor, lower.col = "black", upper = "square")


big5$gender = as.numeric(levels(big5$gender))[big5$gender]
corWithAgeGender = cor(big5[, c(traits, c("age", "gender"))])
corrplot.mixed(corWithAgeGender, lower.col = "black", upper = "square")
corrplot.mixed(corWithAgeGender, lower.col = "black", upper = "square", order = "AOE")

