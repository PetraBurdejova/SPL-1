source("DataPreparation.R")
fiveFactors <- getFactors(data)

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

males =fiveFactors[fiveFactors$gender==1,]
females =fiveFactors[fiveFactors$gender==2,]
mean(males[,10])-mean(females[,10])
#Added t-test to compare the means of males and females to see wether it is significant or not.
#The difference in group size is quite large. One might consider to not use the data of all femals.
t.test(males[,10],females[,10])

#An example of how to compare two density distributions. In this case the distribution of Neuro
#in males and females.
m <- density(males[,10])
f <- density(females[,10])
plot(m,main ="the density distribution of Neuro",col="red",xlab = "Neuro")
lines(f,col="green")
legend(x = "topright", y = NULL, legend=c("Male", "Female"),
       col=c("red", "green"),pch = 15)
males =fiveFactors[fiveFactors$gender==1,]
females =fiveFactors[fiveFactors$gender==2,]
mean(males[,11])-mean(females[,11])
#Added t-test to compare the means of males and females to see wether it is significant or not.
#The difference in group size is quite large. One might consider to not use the data of all femals.
t.test(males[,11],females[,11])

#An example of how to compare two density distributions. In this case the distribution of Agree
#in males and females.
m <- density(males[,11])
f <- density(females[,11])
plot(m,main ="the density distribution of Agree",col="red",xlab = "Agree")
lines(f,col="green")
legend(x = "topright", y = NULL, legend=c("Male", "Female"),
       col=c("red", "green"),pch = 15)

males =fiveFactors[fiveFactors$gender==1,]
females =fiveFactors[fiveFactors$gender==2,]
mean(males[,12])-mean(females[,12])
#Added t-test to compare the means of males and females to see wether it is significant or not.
#The difference in group size is quite large. One might consider to not use the data of all femals.
t.test(males[,12],females[,12])

#An example of how to compare two density distributions. In this case the distribution of Openess
#in males and females.
m <- density(males[,12])
f <- density(females[,12])
plot(m,main ="the density distribution of Openess",col="red",xlab = "Openess")
lines(f,col="yellow")
legend(x = "topright", y = NULL, legend=c("Male", "Female"),
       col=c("red", "yellow"),pch = 15)

males =fiveFactors[fiveFactors$gender==1,]
females =fiveFactors[fiveFactors$gender==2,]
mean(males[,12])-mean(females[,13])
#Added t-test to compare the means of males and females to see wether it is significant or not.
#The difference in group size is quite large. One might consider to not use the data of all femals.
t.test(males[,13],females[,13])

#An example of how to compare two density distributions. In this case the distribution of Conscient
#in males and females.
m <- density(males[,13])
f <- density(females[,13])
plot(m,main ="the density distribution of Conscient",col="red",xlab = "Conscient")
lines(f,col="gray")
legend(x = "topright", y = NULL, legend=c("Male", "Female"),
       col=c("red", "gray"),pch = 15

       
       
       

males =fiveFactors[fiveFactors$gender==1,]
females =fiveFactors[fiveFactors$gender==2,]
mean(males[,12])-mean(females[,12])

#Added t-test to compare the means of males and females to see wether it is significant or not.
#The difference in group size is quite large. One might consider to not use the data of all femals.
t.test(males[males$ageCat=="US",2],males[males$ageCat=="GB",2])


       

males = fiveFactors[fiveFactors$gender==1,]
females =fiveFactors[fiveFactors$gender==2,]
mean(males[,12])-mean(females[,12])
#Added t-test to compare the means of males and females to see wether it is significant or not.
#The difference in group size is quite large. One might consider to not use the data of all femals.
t.test(males[,12],females[,12])

#An example of how to compare two density distributions. In this case the distribution of Conscient
#in males and females.
m <- density(males[,12])
f <- density(females[,12])
plot(m,main = "The density distribution of Conscient",col = "red",xlab = "Conscient")
lines(f,col="blue")
legend(x = "topright", y = NULL, legend=c("Male", "Female"),
       col=c("red", "blue"),pch = 15)



       
       
       


