#This is a test script

print("Hello")

#Here you can add some code

#Aron Testeintrag

#Neuer test

library(readxl)
data <- read_excel("Big5.xlsx")
#View(data)
data$country <- as.factor(data$country)
