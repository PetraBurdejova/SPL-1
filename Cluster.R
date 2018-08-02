source("DataPreparation.R")
if (!require("corrplot")) install.packages("corrplot")
library("corrplot")

fiveFactors = getDataSetWithBig5(data, F, F)

#Clustering with 2 to 5 clusters
for(x in 2:5){
  temp <- kmeans(fiveFactors[,c("Intro", "Neuro", "Agree", "Conscient", "Openess")],x)
  clusters         = fiveFactors
  clusters$Cluster = temp$cluster
  print(x)
  for(y in 1:x){
    print(apply(clusters[clusters$Cluster == y,c("age","Intro", "Neuro", "Agree", "Conscient", "Openess")],2,mean))
  }
  
}



temp = cor(clusters[,c("age","Intro", "Neuro", "Agree", "Conscient", "Openess","Cluster")])
corrplot.mixed(temp,lower = "number", upper = "pie", lower.col  = "black")
