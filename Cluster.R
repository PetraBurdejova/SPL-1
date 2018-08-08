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
  fviz_cluster(temp, data = big5, geom = "point",
               stand = FALSE, ellipse.type = "norm")
  clusters         = big5
  clusters$Cluster = temp$cluster
  print(x)
  for(y in 1:x){
    print(apply(clusters[clusters$Cluster == y,c("age","Intro", "Neuro", "Agree", "Conscient", "Openess")],2,mean))
  }
  
}


# K-means vs. PAM
kCluster <- kmeans(big5[,traits],4,nstart = 25)
fviz_cluster(kCluster, data = big5[,traits], geom = "point",
             stand = FALSE, ellipse.type = "norm")
pamCluster = pam(big5[,traits], 2)
pamCluster$cluster
fviz_cluster(pamCluster, stand = FALSE, geom = "point",
             frame.type = "norm")



# Number of clusters 

# Elbow method
k.max = 10
wss <- sapply(2:k.max, 
              function(k){kmeans(big5[,traits], k, nstart = 15 ,iter.max = 150 )$tot.withinss})

plot(2:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Average Silhoutte

sil <- rep(0, k.max)
for(i in 2:k.max){
  kCluster <- kmeans(big5, centers = i, nstart = 25)
  ss <- silhouette(kCluster$cluster, dist(big5))
  sil[i] <- mean(ss[, 3])
}

# Plot the  average silhouette width
plot(1:k.max, sil, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k")

