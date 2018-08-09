source("SPL_Big5GritDataPreparation/SPL_Big5GritDataPreparation.R")

DATA = getCombinedData(FALSE)

#Do the analysis of variance

aov1f = aov(Agree~age,data=DATA)         #an example of analysis of variance for Agree ~age
summary(aov1f) #show the summary table

#creating ANOVA for Personality traits against independent variable 
#for Age, Hand, Race, Gender (add. information out of ind. variable a better fit?):

anova_multi <- function(DATA,set1, set2, set3, set4){
  
  i_dataset = length(DATA[i:max(length(DATA))])
  aov_test <- matrix(nrow=5,ncol=4)
  rownames(aov_test) <- names(DATA[9:13])
  colnames(aov_test) <- c('Test Stat - Gender',"Test Stat - Age","Test Stat - Hand","Test Stat - Race")
  for(i in 9: i_dataset){
    aov_test[i-8,1] <- anova(lm(DATA[,i] ~ set1))$"Pr(>F)"[1]
    aov_test[i-8,2] <- anova(lm(DATA[,i] ~ set2))$"Pr(>F)"[1]
    aov_test[i-8,3] <- anova(lm(DATA[,i] ~ set3))$"Pr(>F)"[1]
    aov_test[i-8,4] <- anova(lm(DATA[,i] ~ set4))$"Pr(>F)"[1]
  }    
  return(aov_test)
}

#DATA run:
v = anova_multi(DATA,DATA[,2],DATA[,3],DATA[,4],DATA[,5])


#add a varaible
aov2f = aov(Openess~age+gender,data=DATA)         #do the analysis of variance
summary(aov2f)                                    #show the summary table
#print(model.tables(aov2f,"means"),digits=3)      
#report the means and the number of subjects/cell - still in thinking..
#attach(DATA)
#interaction.plot(Neuro,gender,age)  #another way to graph the means 
#detach(DATA)


