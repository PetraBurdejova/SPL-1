source("DataPreparation.R")

DATA = getDataSetWithBig5(data, F, F)


#Do the analysis of variance

aov1f = aov(Agree~age,data=DATA)         #an example of analysis of variance for Agree ~age
summary(aov1f)#show the summary table

#define

  v1 = DATA$Intro
  v2 = DATA$Neuro
  v3 = DATA$Agree
  v4 = DATA$Conscient
  v5 = DATA$Openess
  
  uv1 = DATA$country
  uv2 = DATA$gender
  uv3 = DATA$age
  uv4 = DATA$hand
  uv5 = DATA$race
  uv6 = DATA$ageCat   

  
#creating ANOVA for Personality traits against independent variable 
#for Age, Hand, Race, Gender (add. information out of ind. variable a better fit?):  
z <- matrix(nrow=5,ncol=4)
rownames(z) <- names(DATA[9:13])
colnames(z) <- c("Test Stat - Age","Test Stat - Hand","Test Stat - Race",'Test Stat - Gender')

z[1,1] = anova(lm(v1 ~ uv3))$"Pr(>F)"[1] 
z[2,1] = anova(lm(v2 ~ uv3))$"Pr(>F)"[1] 
z[3,1] = anova(lm(v3 ~ uv3))$"Pr(>F)"[1] 
z[4,1] = anova(lm(v4 ~ uv3))$"Pr(>F)"[1] 
z[5,1] = anova(lm(v5 ~ uv3))$"Pr(>F)"[1] 

z[1,2] = anova(lm(v1 ~ uv4))$"Pr(>F)"[1] 
z[2,2] = anova(lm(v2 ~ uv4))$"Pr(>F)"[1] 
z[3,2] = anova(lm(v3 ~ uv4))$"Pr(>F)"[1] 
z[4,2] = anova(lm(v4 ~ uv4))$"Pr(>F)"[1] 
z[5,2] = anova(lm(v5 ~ uv4))$"Pr(>F)"[1] 

z[1,3] = anova(lm(v1 ~ uv5))$"Pr(>F)"[1] 
z[2,3] = anova(lm(v2 ~ uv5))$"Pr(>F)"[1] 
z[3,3] = anova(lm(v3 ~ uv5))$"Pr(>F)"[1] 
z[4,3] = anova(lm(v4 ~ uv5))$"Pr(>F)"[1] 
z[5,3] = anova(lm(v5 ~ uv5))$"Pr(>F)"[1] 

z[1,4] = anova(lm(v1 ~ uv2))$"Pr(>F)"[1] 
z[2,4] = anova(lm(v2 ~ uv2))$"Pr(>F)"[1] 
z[3,4] = anova(lm(v3 ~ uv2))$"Pr(>F)"[1] 
z[4,4] = anova(lm(v4 ~ uv2))$"Pr(>F)"[1] 
z[5,4] = anova(lm(v5 ~ uv2))$"Pr(>F)"[1] 

TableOfVariance1 = z

TableOfVariance1

#add a varaible
aov2f = aov(Openess~age+gender,data=DATA)         #do the analysis of variance
summary(aov2f)                                    #show the summary table
print(model.tables(aov2f,"means"),digits=3)      
#report the means and the number of subjects/cell - still in thinking..
attach(DATA)
interaction.plot(Neuro,gender,age)  #another way to graph the means 
detach(DATA)


#not important - lags as Granger no use for data set... will be deleted prior submit..
select.lags<-function(x,y,max.lag=8) {
y<-as.numeric(y)
y.lag<-embed(y,max.lag+1)[,-1,drop=FALSE]
x.lag<-embed(x,max.lag+1)[,-1,drop=FALSE]

t<-tail(seq_along(y),nrow(y.lag))

ms=lapply(1:max.lag,function(i) lm(y[t]~y.lag[,1:i]+x.lag[,1:i]))

pvals<-mapply(function(i) anova(ms[[i]],ms[[i-1]])[2,"Pr(>F)"],max.lag:2)
ind<-which(pvals<0.05)[1]
ftest<-ifelse(is.na(ind),1,max.lag-ind+1)

aic<-as.numeric(lapply(ms,AIC))
bic<-as.numeric(lapply(ms,BIC))
structure(list(ic=cbind(aic=aic,bic=bic),pvals=pvals,
               selection=list(aic=which.min(aic),bic=which.min(bic),ftest=ftest)))
}
#s<-select.lags(ages$Intro,ages$Neuro,8)
#t(s$selection)
###----

