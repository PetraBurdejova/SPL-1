source("DataPreparation.R")
fiveFactors <- getDataSetWithBig5(data,FALSE)

#Count occurances of country levels in males
temp <- count(males, "country")

males = fiveFactors[fiveFactors$gender==1,]
females =fiveFactors[fiveFactors$gender==2,]
mean(males[,9])-mean(females[,9])
#Added t-test to compare the means of males and females to see wether it is significant or not.
#The difference in group size is quite large. One might consider to not use the data of all femals.
t.test(males[,9],females[,9])

#An example of how to compare two density distributions. In this case the distribution of Introversion/Extraversion
#in males and females.
m <- density(males[,9])
f <- density(females[,9])
plot(m,main = "The density distribution of Introversion/Extraversion",col = "red",xlab = "Introversion/Extraversion")
lines(f,col="blue")
legend(x = "topright", y = NULL, legend=c("Male", "Female"),
       col=c("red", "blue"),pch = 15)

