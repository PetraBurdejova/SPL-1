# import required libraries
source("Big5GritDataPreparation/Big5GritDataPreparation.R")


gritFactors = getGritDF()

malesGrit   = gritFactors[gritFactors$gender == 1, ]
femalesGrit = gritFactors[gritFactors$gender == 2, ]
t.test(malesGrit$realGrit,femalesGrit$realGrit)


#Comparing top 5% and bottom 5% of people w.r.t. grit
quantile(gritFactors$realGrit,0.05)
quantile(gritFactors$realGrit,0.95)


top5Grit    = gritFactors[gritFactors$realGrit>=quantile(gritFactors$realGrit,0.95),]
bottom5Grit = gritFactors[gritFactors$realGrit<=quantile(gritFactors$realGrit,0.05),]
middle      = gritFactors[gritFactors$realGrit<quantile(gritFactors$realGrit,0.95) & 
                          gritFactors$realGrit>quantile(gritFactors$realGrit,0.05),]
summary(top5Grit)
summary(bottom5Grit)
t.test(top5Grit$education,bottom5Grit$education)

#Top 5% vs. Bottom 5%: Education
top5Grit$education            = as.factor(top5Grit$education)
bottom5Grit$education         = as.factor(bottom5Grit$education)
levels(top5Grit$education)    =  c("N/A", "Less than High School", "High School", "University", "Graduate")
levels(bottom5Grit$education) =  c("N/A", "Less than High School", "High School", "University", "Graduate")

barplot(rbind(prop.table(table(bottom5Grit$education)),prop.table(table(top5Grit$education))),
        beside = TRUE, col= c("red","blue"),ylim = c(0,0.6), 
        main = "Comparison of the top 5% vs. bottom 5% w.r.t. education levels",
        xlab = "Level of education", ylab = "Density")
legend(x = "topright", y = NULL, legend = c("Bottom 5%", "Top 5%"), col = c("red", "blue"), pch = 15)



#Top 5% vs. Bottom 5%: Voted

levels(top5Grit$voted)    =  c( "N/A", "Yes", "No")
levels(bottom5Grit$voted) =  c("N/A","Yes", "No")
levels(middle$voted)      =  c("N/A","Yes", "No")
barplot(rbind(prop.table(table(bottom5Grit$voted)),prop.table(table(middle$voted)),prop.table(table(top5Grit$voted))),
        beside = TRUE, col= c("red","blue","green"),ylim = c(0,0.8), 
        main = "Comparison of the top 5% vs. bottom 5% w.r.t. education levels",
        xlab = "Voted", ylab = "Density")
legend(x = "topleft", y = NULL, legend = c("Bottom 5%", "Middle 90%", "Top 5%"), col = c("red","green", "blue"), pch = 15)




#Grit over age

plot(gritFactors[gritFactors$age > 0,]$age,gritFactors[gritFactors$age > 0,]$realGrit,xlab = "Age",ylab = "Grit")
par(mfrow=c(2,1))
ageGrit = c()
for(x in min(gritFactors[gritFactors$age > 0,]$age):60){
  ageGrit = c(ageGrit,mean(gritFactors[gritFactors$age == x,]$realGrit))
}
for(x in 1:length(ageGrit)){
  if(is.nan(ageGrit[x])){
    ageGrit[x] = (ageGrit[x-1]+ageGrit[x+1])/2
  }
}
plot(min(gritFactors[gritFactors$age > 0,]$age):60, ageGrit, type = "l",ylab = "Grit",xlab = "Age", 
     main = "Grit over Age")

ageCatGrit = c()
for(x in (min(gritFactors$ageCat)+1):max(gritFactors$ageCat)){
  ageCatGrit = c(ageCatGrit,mean(gritFactors[gritFactors$ageCat == x,]$realGrit))
}
plot(ageCatGrit, type = "l", ylab = "Grit", xlab = "Age-Category", main = "Grit over Age-Categories")
par(mfrow=c(1,1))

mean(top5Grit$age)
mean(bottom5Grit$age)
mean(gritFactors$age)

