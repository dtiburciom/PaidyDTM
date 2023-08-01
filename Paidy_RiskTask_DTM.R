#1. Installing the libraries##

install.packages("stringr")
install.packages("devtools")
install.packages("readxl")
install.packages("dplyr")
install.packages("data.table")
install.packages("tidyr")
install.packages("witexl")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("randomForest")
install.packages("plotly")
install.packages("gapminder")
install.packages("ggpubr")
install.packages("corrplot")
install.packages("foreign")

##2. Adding libraries##

library(stringr)
library(devtools)
library(dplyr)
library(readxl)
library(data.table)
library(tidyr)
library(writexl)
library(ggplot2)
library(tidyverse)
library(randomForest)
library(plotly)
library(gapminder)
library(ggpubr)
library(corrplot)
library(foreign)

devtools::install_github("tidyverse/tidyr") 
library(tidyverse) 

#Functions to extract#
left = function(text, num_char) {
  substr(text, 1, num_char)
}
mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

##Credit Paidy code##

customers_paidy_training <- as.data.frame(cs_training)
customers_paidy_test <- as.data.frame(cs_test)
summary(cs_training)
hist(customers_paidy_training$age)
table(customers_paidy_training$age)
table(customers_paidy_training$SeriousDlqin2yrs)
table(customers_paidy_training$MonthlyIncome)
hist(customers_paidy_training$MonthlyIncome)
summary(customers_paidy_training)

table(cs_training$SeriousDlqin2yrs)

customers_paidy_training_withoutNA <- na.omit(customers_paidy_training)
summary(customers_paidy_training_withoutNA)

#Delinquency graph

barplot(prop.table(table(customers_paidy_training$SeriousDlqin2yrs)),col=c("orange","blue"),legend.text=c("No","Yes"))

##Data Cleanse


## Correlation ##

M <- cor(customers_paidy_training)
corrplot(M, method = "color")


## Replace missing ones

training_withoutNA_MonthlyIncome <- customers_paidy_training %>%  filter(!is.na(MonthlyIncome))
training_withoutNA_NumberofDependants <- customers_paidy_training %>%  filter(!is.na(NumberOfDependents))
summary(training_withoutNA_MonthlyIncome)
summary(training_withoutNA_NumberofDependants)

##Mean, median, 0's##

median_Monthly_income <- median(training_withoutNA_MonthlyIncome$MonthlyIncome)
median_NumberofDependants <- median(training_withoutNA_NumberofDependants$NumberOfDependents)

customers_paidy_training$MonthlyIncome[is.na(customers_paidy_training$MonthlyIncome)] <- 0
customers_paidy_training$NumberOfDependents[is.na(customers_paidy_training$NumberOfDependents)] <- 0
summary(customers_paidy_training)

##Fixed correlation

M1 <- cor(customers_paidy_training)
corrplot(M1, method = "color")

##Data summaries

table(customers_paidy_training$NumberOfDependents)
table(customers_paidy_training$NumberOfOpenCreditLinesAndLoans)
table(customers_paidy_training$NumberRealEstateLoansOrLines)

##Past Dues

table(customers_paidy_training$`NumberOfTime30-59DaysPastDueNotWorse`)
table(customers_paidy_training$`NumberOfTime60-89DaysPastDueNotWorse`)
table(customers_paidy_training$NumberOfTimes90DaysLate)

##Logistic Regression

Customers_Dlq_1 <- glm(SeriousDlqin2yrs~`NumberOfTime30-59DaysPastDueNotWorse`,data=customers_paidy_training,
                     family="binomial")
Customers_Dlq_2 <- glm(SeriousDlqin2yrs~`NumberOfTime30-59DaysPastDueNotWorse`+`NumberOfTime60-89DaysPastDueNotWorse`,data=customers_paidy_training,
                       family="binomial")
Customers_Dlq_3 <- glm(SeriousDlqin2yrs~`NumberOfTime30-59DaysPastDueNotWorse`+`NumberOfTime60-89DaysPastDueNotWorse`+age,data=customers_paidy_training,
                       family="binomial")
#Customers_Dlq_4 <- glm(SeriousDlqin2yrs~`NumberOfTime30-59DaysPastDueNotWorse`+`NumberOfTime60-89DaysPastDueNotWorse`+age+RevolvingUtilizationOfUnsecuredLines,data=customers_paidy_training,
#                       family="binomial")
Customers_Dlq_5 <- glm(SeriousDlqin2yrs~`NumberOfTime30-59DaysPastDueNotWorse`+`NumberOfTime60-89DaysPastDueNotWorse`+age+DebtRatio,data=customers_paidy_training,
                       family="binomial")
Customers_Dlq_6 <- glm(SeriousDlqin2yrs~`NumberOfTime30-59DaysPastDueNotWorse`+`NumberOfTime60-89DaysPastDueNotWorse`+age+MonthlyIncome ,data=customers_paidy_training,
                       family="binomial")
Customers_Dlq_7 <- glm(SeriousDlqin2yrs~`NumberOfTime30-59DaysPastDueNotWorse`+`NumberOfTime60-89DaysPastDueNotWorse`+age+MonthlyIncome+NumberOfDependents ,data=customers_paidy_training,
                       family="binomial")
Customers_Dlq_8 <- glm(SeriousDlqin2yrs~`NumberOfTime30-59DaysPastDueNotWorse`+`NumberOfTime60-89DaysPastDueNotWorse`+age+MonthlyIncome+NumberOfDependents+NumberOfOpenCreditLinesAndLoans ,data=customers_paidy_training,
                       family="binomial")
Customers_Dlq_9 <- glm(SeriousDlqin2yrs~`NumberOfTime30-59DaysPastDueNotWorse`+`NumberOfTime60-89DaysPastDueNotWorse`+age+MonthlyIncome+NumberOfDependents+NumberOfOpenCreditLinesAndLoans+NumberRealEstateLoansOrLines ,data=customers_paidy_training,
                       family="binomial")
Customers_Dlq_10 <- glm(SeriousDlqin2yrs~`NumberOfTime30-59DaysPastDueNotWorse`+`NumberOfTime60-89DaysPastDueNotWorse`+age+MonthlyIncome+NumberOfDependents+NumberOfOpenCreditLinesAndLoans+NumberRealEstateLoansOrLines+DebtRatio ,data=customers_paidy_training,
                       family="binomial")
Customers_Dlq_11 <- glm(SeriousDlqin2yrs~`NumberOfTime30-59DaysPastDueNotWorse`+`NumberOfTime60-89DaysPastDueNotWorse`+age+MonthlyIncome+NumberOfDependents+NumberOfOpenCreditLinesAndLoans+NumberRealEstateLoansOrLines+DebtRatio+RevolvingUtilizationOfUnsecuredLines ,data=customers_paidy_training,
                        family="binomial")

summary(Customers_Dlq_11)

anova(Customers_Dlq_1,Customers_Dlq_2,Customers_Dlq_3,Customers_Dlq_5,Customers_Dlq_6,Customers_Dlq_7,Customers_Dlq_8,Customers_Dlq_9,Customers_Dlq_10,Customers_Dlq_11,test="Chisq")

Customers_Dlq_11_pred <- predict(Customers_Dlq_11,type="response")  
Customers_Dlq_11_pred
Customers_paidy_pred <- cbind(customers_paidy_training,Customers_Dlq_11_pred)
Customers_paidy_pred
summary(Customers_paidy_pred)

Customers_paidy_pred_1 <- subset(Customers_paidy_pred, Customers_Dlq_11_pred <= .06684)
Customers_paidy_pred_2 <- subset(Customers_paidy_pred, Customers_Dlq_11_pred <= .1 & Customers_Dlq_11_pred > .06684)
Customers_paidy_pred_3 <- subset(Customers_paidy_pred, Customers_Dlq_11_pred <= .3 & Customers_Dlq_11_pred > .1)
Customers_paidy_pred_4 <- subset(Customers_paidy_pred, Customers_Dlq_11_pred <= .5 & Customers_Dlq_11_pred > .3)
Customers_paidy_pred_4 <- subset(Customers_paidy_pred, Customers_Dlq_11_pred <= .75 & Customers_Dlq_11_pred > .5)
Customers_paidy_pred_5 <- subset(Customers_paidy_pred, Customers_Dlq_11_pred > .75)

##End## 


