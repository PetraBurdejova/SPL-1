#Tests for defining the Average values of all personality traits between males and females over the age

source("DataPreparation.R")

fiveFactors = getDataSetWithBig5(data, FALSE,T)

#All People
avgAge1 <- apply(fiveFactors[fiveFactors$ageCat == 1,9:13],2,mean,na.rm=T)
avgAge2 <- apply(fiveFactors[fiveFactors$ageCat == 2,9:13],2,mean,na.rm=T)
avgAge3 <- apply(fiveFactors[fiveFactors$ageCat == 3,9:13],2,mean,na.rm=T)
avgAge4 <- apply(fiveFactors[fiveFactors$ageCat == 4,9:13],2,mean,na.rm=T)
avgAge5 <- apply(fiveFactors[fiveFactors$ageCat == 5,9:13],2,mean,na.rm=T)
avgAge6 <- apply(fiveFactors[fiveFactors$ageCat == 6,9:13],2,mean,na.rm=T)
avgAge7 <- apply(fiveFactors[fiveFactors$ageCat == 7,9:13],2,mean,na.rm=T)
avgAge8 <- apply(fiveFactors[fiveFactors$ageCat == 8,9:13],2,mean,na.rm=T)
avgAge9 <- apply(fiveFactors[fiveFactors$ageCat == 9,9:13],2,mean,na.rm=T)

ages <- data.frame(rbind(avgAge1,avgAge2,avgAge3,avgAge4,avgAge5,avgAge6,avgAge7))


#Males
mavgAge1 <- apply(fiveFactors[fiveFactors$ageCat == 1 & fiveFactors$gender==1,9:13],2,mean,na.rm=T)
mavgAge2 <- apply(fiveFactors[fiveFactors$ageCat == 2 & fiveFactors$gender==1,9:13],2,mean,na.rm=T)
mavgAge3 <- apply(fiveFactors[fiveFactors$ageCat == 3 & fiveFactors$gender==1,9:13],2,mean,na.rm=T)
mavgAge4 <- apply(fiveFactors[fiveFactors$ageCat == 4 & fiveFactors$gender==1,9:13],2,mean,na.rm=T)
mavgAge5 <- apply(fiveFactors[fiveFactors$ageCat == 5 & fiveFactors$gender==1,9:13],2,mean,na.rm=T)
mavgAge6 <- apply(fiveFactors[fiveFactors$ageCat == 6 & fiveFactors$gender==1,9:13],2,mean,na.rm=T)
mavgAge7 <- apply(fiveFactors[fiveFactors$ageCat == 7 & fiveFactors$gender==1,9:13],2,mean,na.rm=T)
mavgAge8 <- apply(fiveFactors[fiveFactors$ageCat == 9 & fiveFactors$gender==1,9:13],2,mean,na.rm=T)
mavgAge9 <- apply(fiveFactors[fiveFactors$ageCat == 9 & fiveFactors$gender==1,9:13],2,mean,na.rm=T)

mages <- data.frame(rbind(mavgAge1,mavgAge2,mavgAge3,mavgAge4,mavgAge5,mavgAge6,mavgAge7))

# left away -> ,mavgAge8,mavgAge9

#females
favgAge1 <- apply(fiveFactors[fiveFactors$ageCat == 1 & fiveFactors$gender==2,9:13],2,mean,na.rm=T)
favgAge2 <- apply(fiveFactors[fiveFactors$ageCat == 2 & fiveFactors$gender==2,9:13],2,mean,na.rm=T)
favgAge3 <- apply(fiveFactors[fiveFactors$ageCat == 3 & fiveFactors$gender==2,9:13],2,mean,na.rm=T)
favgAge4 <- apply(fiveFactors[fiveFactors$ageCat == 4 & fiveFactors$gender==2,9:13],2,mean,na.rm=T)
favgAge5 <- apply(fiveFactors[fiveFactors$ageCat == 5 & fiveFactors$gender==2,9:13],2,mean,na.rm=T)
favgAge6 <- apply(fiveFactors[fiveFactors$ageCat == 6 & fiveFactors$gender==2,9:13],2,mean,na.rm=T)
favgAge7 <- apply(fiveFactors[fiveFactors$ageCat == 7 & fiveFactors$gender==2,9:13],2,mean,na.rm=T)
favgAge8 <- apply(fiveFactors[fiveFactors$ageCat == 8 & fiveFactors$gender==2,9:13],2,mean,na.rm=T)
favgAge9 <- apply(fiveFactors[fiveFactors$ageCat == 9 & fiveFactors$gender==2,9:13],2,mean,na.rm=T)

fages <- data.frame(rbind(favgAge1,favgAge2,favgAge3,favgAge4,favgAge5,favgAge6,favgAge7))

# left away -> ,favgAge8,favgAge9

#Extraversion

plot(c(1:7),mages$Intro,type = "l",col="red",ylim = c(-0.5,1.0)
     ,xlab = "Age Category", ylab = "Values of Traits", 
     main = "Average values of Extraversion between males and females over the age", cex.main=0.9)
lines(fages$Intro,col="blue")
lines(ages$Intro,col="grey")
legend("topleft", inset=.05, c("Male","Female","All"), fill=c("red","blue","grey"), horiz=TRUE)

#openness


plot(c(1:7),mages$Openess,type = "l",col="red",ylim = c(-1.8,1.0)
     ,xlab = "Age Category", ylab = "Values of Traits", 
     main = "Average values of Openness between males and females over the age", cex.main=0.9)
lines(fages$Openess,col="blue")
lines(ages$Openess,col="grey")
legend("topleft", inset=.05, c("Male","Female","All"), fill=c("red","blue","grey"), horiz=TRUE)


#Conscientiousness


plot(c(1:7),mages$Conscient,type = "l",col="red",ylim = c(-1.8,1.0)
     ,xlab = "Age Category", ylab = "Values of Traits", 
     main = "Avg values of Conscientiousness between males and females over the age", cex.main=0.9)
lines(fages$Conscient,col="blue")
lines(ages$Conscient,col="grey")
legend("topleft", inset=.05, c("Male","Female","All"), fill=c("red","blue","grey"), horiz=TRUE)



#Agreeableness


plot(c(1:7),mages$Agree,type = "l",col="red",ylim = c(-1.8,1.0)
     ,xlab = "Age Category", ylab = "Values of Traits", 
     main = "Avg values of Agreeableness between males and females over the age", cex.main=0.9)
lines(fages$Agree,col="blue")
lines(ages$Agree,col="grey")
legend("topleft", inset=.05, c("Male","Female","All"), fill=c("red","blue","grey"), horiz=TRUE)



#Neuroticism


plot(c(1:7),mages$Neuro,type = "l",col="red",ylim = c(-3.0,2.0)
     ,xlab = "Age Category", ylab = "Values of Traits", 
     main = "Avg values of Neuroticism between males and females over the age", cex.main=0.9)
lines(fages$Neuro,col="blue")
lines(ages$Neuro,col="grey")
legend("topleft", inset=.05, c("Male","Female","All"), fill=c("red","blue","grey"), horiz=TRUE)

