source("DataPreparation.R")
if (!require("corrplot")) install.packages("corrplot")
library("corrplot")
if (!require("cluster")) install.packages("cluster")
library("cluster")

traits = c("Intro", "Neuro", "Openess", "Conscient", "Agree")
big5 = getCombinedData(data,T)[,c("age",traits)]

#Clustering with 2 to 5 clusters
for(x in 2:5){
  temp <- kmeans(big5,x,nstart = 25)
  plot(fviz_cluster(temp, data = big5, geom = "point",
               stand = FALSE, ellipse.type = "norm"))
  clusters         = big5
  clusters$Cluster = temp$cluster
  print(x)
  for(y in 1:x){
    print(apply(clusters[clusters$Cluster == y,c("age","Intro", "Neuro", "Agree", "Conscient", "Openess")],2,mean))
  }
  
}


# Number of clusters 

# Elbow method
# k.max = 10
# wss <- sapply(2:k.max, 
#               function(k){kmeans(big5[,traits], k, nstart = 15 ,iter.max = 150 )$tot.withinss})
# 
# plot(2:k.max, wss,
#      type="b", pch = 19, frame = FALSE, 
#      xlab="Number of clusters K",
#      ylab="Total within-clusters sum of squares")

fviz_nbclust(big5[sample(nrow(big5),2000),], kmeans, method = "wss")
fviz_nbclust(big5[sample(nrow(big5),2000),], pam, method = "wss")

# Average Silhoutte
# sil <- rep(0, k.max)
# for(i in 2:k.max){
#   kCluster <- kmeans(big5, centers = i, nstart = 25)
#   ss <- silhouette(kCluster$cluster, dist(big5))
#   sil[i] <- mean(ss[, 3])
# }
# plot(1:k.max, sil, type = "b", pch = 19, 
#      frame = FALSE, xlab = "Number of clusters k",xlim = c(0,k.max))

fviz_nbclust(big5, kmeans, method = "silhouette")
fviz_nbclust(big5[sample(nrow(big5),2000),], pam, method = "silhouette")

#GAP_Stat
fviz_nbclust(big5[sample(nrow(big5),1000),], kmeans, method = "gap_stat")
fviz_nbclust(big5[sample(nrow(big5),1000),], pam, method = "gap_stat")



# K-means vs. PAM
kCluster <- kmeans(big5,2,nstart = 25)
fviz_cluster(kCluster, data = big5, geom = "point",
             stand = FALSE, ellipse.type = "norm")

pamCluster = pam(big5, 2)
fviz_cluster(pamCluster, stand = FALSE, geom = "point",
             elliplse.type = "norm")