source("SPL_Big5GritDataPreparation/SPL_Big5GritDataPreparation.R")
fiveFactors = getCombinedData(FALSE)


males   = fiveFactors[fiveFactors$gender==1,]
females = fiveFactors[fiveFactors$gender==2,]

#Count occurances of country levels in males
temp <- count(males, "country")

mean(males[,9])-mean(females[,9])
#Added t-test to compare the means of males and females to see wether it is significant or not.
#The difference in group size is quite large. One might consider to not use the data of all femals.
t.test(males[,9],females[,9])
t.test(males[,10],females[,10])
t.test(males[,11],females[,11])
t.test(males[,12],females[,12])
t.test(males[,13],females[,13])

#An example of how to compare two density distributions. In this case the distribution of Introversion/Extraversion
#in males and females.
m <- density(males[,9])
f <- density(females[,9])
plot(m,main = "The density distribution of Introversion/Extraversion",col = "red",xlab = "Introversion/Extraversion")
lines(f,col="blue")
legend(x = "topright", y = NULL, legend=c("Male", "Female"),
       col=c("red", "blue"),pch = 15)



#Neuro calcs
plot(density(females[,10]), main = "The Density Distribution of Neuroticism",col = "blue",xlab = "Neuro")
lines(density(males[,10]),col="red")
legend(x = "topright", y = NULL, legend=c("Male", "Female"),
       col=c("red", "blue"),pch = 15)


#Agree calcs
plot(density(males[,11]),main = "The Density Distribution of Agreeableness",col = "red",xlab = "Agree")
lines(density(females[,11]),col="blue")
legend(x = "topright", y = NULL, legend=c("Male", "Female"),
       col=c("red", "blue"),pch = 15)


#Openeness calcs
plot(density(males[,12]),main = "The Density Distribution of Openness",col = "red",xlab = "Openeness")
lines(density(females[,12]),col="blue")
legend(x = "topright", y = NULL, legend=c("Male", "Female"),
       col=c("red", "blue"),pch = 15)


#Conscient calcs
plot(density(males[,13]),main = "The Density Distribution of Conscient",col = "red",xlab = "Conscient")
lines(density(females[,13]),col="blue")
legend(x = "topright", y = NULL, legend=c("Male", "Female"),
       col=c("red", "blue"),pch = 15)




#Country tests for later purpose
t.test(fiveFactors[fiveFactors$country=="US",9],fiveFactors[fiveFactors$country=="GB",9])
t.test(fiveFactors[fiveFactors$country=="US",9],fiveFactors[fiveFactors$country=="IN",9])
t.test(fiveFactors[fiveFactors$country=="US",9],fiveFactors[fiveFactors$country=="DE",9])
t.test(fiveFactors[fiveFactors$country=="US",9],fiveFactors[fiveFactors$country=="AU",9])


#Finding outliers in personality traits Intro/Extra, Neuro, Agree, Conscient and Openess

OL <- boxplot(fiveFactors[9:13], main="Boxplot of all personality traits")

#Summary of all Factors 

sumup <- fiveFactors[9:13]




 

  
  
  