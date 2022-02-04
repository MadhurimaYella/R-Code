
#Loading and understanding the data

#install.packages("tidyverse")
#library(dplyr)
#library(readr)
loanData<-readRDS("loanData.rds")

summary(loanData)
str(loanData)


#Installing necessary packages
#install.packages("gmodels")
#install.packages("crosstable")

library(gmodels)
library(crosstable)
library(dplyr)

# Calling crosstable() function 
crosstable(loanData, creditGrade, by=isLoanDefault) %>%
  as_flextable(keep_id=TRUE)

# Summary statistics
library(ggplot2)
library(gridExtra)

summary(loanData)

# Scatter Plots
#income vs employmentyears
plot(loanData$employmentYears, loanData$incomeAnnual, main="Employmentyears Vs Income", 
     xlab="employmentYears ", ylab="incomeAnnual ", pch=19)
abline(lm(loanData$incomeAnnual ~ loanData$employmentYears))

#Income vs Age
plot(loanData$incomeAnnual, loanData$ageYears, main="Income Vs Age", 
     xlab="Income ", ylab="Age ", pch=19)
abline(lm(loanData$ageYears ~ loanData$incomeAnnual))

#Age vs Experience
plot( loanData$ageYears,loanData$employmentYears, main= "EmploymentYears Vs Age", 
      xlab="Age", ylab="employmentYears ", pch=19)
abline(lm(loanData$employmentYears ~ loanData$ageYears))

#Identifying the outliers 
unique(loanData$ageYears)

#removing the row with age=144years
loanDataNoOutliers<- subset(loanData, loanData$ageYears!="144")
unique(loanDataNoOutliers$ageYears)

#outlier info
outlier<-subset(loanData, loanData$ageYears=="144")
outlier

# Sending the loanDataNoOutliers data into loanDataNoOutliersNA
loanDataNoOutliersNA<- loanDataNoOutliers
head(loanDataNoOutliersNA)


#percentage missing values in each column

mean(is.na(loanDataNoOutliersNA$isLoanDefault))*100 
mean(is.na(loanDataNoOutliersNA$loanAmount))*100 
mean(is.na(loanDataNoOutliersNA$interestRate))*100 #
mean(is.na(loanDataNoOutliersNA$creditGrade))*100 
mean(is.na(loanDataNoOutliersNA$employmentYears))*100 #
mean(is.na(loanDataNoOutliersNA$homeLiving))*100 
mean(is.na(loanDataNoOutliersNA$incomeAnnual))*100 
mean(is.na(loanDataNoOutliersNA$ageYears))*100 

# Adding a column

library(dplyr)

# Adding column based on other column:
loanDataNoOutliersNA <- loanDataNoOutliersNA %>%
  mutate(interestRateNA = case_when(
    is.na(loanDataNoOutliersNA$interestRate) ~ "1",
    !is.na(loanDataNoOutliersNA$interestRate) ~ "0"
  ))

loanDataNoOutliersNA <- loanDataNoOutliersNA %>%
  mutate(employmentYearsNA = case_when(
    is.na(loanDataNoOutliersNA$employmentYears) ~ "1",
    !is.na(loanDataNoOutliersNA$employmentYears) ~ "0"
  ))

# Replacing NA with Median Values 

loanDataNoOutliersNA<- loanDataNoOutliersNA %>% 
  mutate(employmentYears = ifelse(is.na(loanDataNoOutliersNA$employmentYears),
                                  median(loanDataNoOutliersNA$employmentYears, na.rm = T),
                                  loanDataNoOutliersNA$employmentYears))

loanDataNoOutliersNA<- loanDataNoOutliersNA %>% 
  mutate(interestRate = ifelse(is.na(loanDataNoOutliersNA$interestRate),
                               median(loanDataNoOutliersNA$interestRate, na.rm = T),
                               loanDataNoOutliersNA$interestRate))

crosstable(loanData, loanAmount, by=isLoanDefault) %>%
  as_flextable(keep_id=TRUE)


#regression

#  Setting seed
set.seed(2020)
#nrow(loanDataNoOutliersNA)
#oanTest  <- sample(loanDataNoOutliersNA,9697,replace=TRUE)
#loanTrain <- sample(loanDataNoOutliersNA,19394,replace=TRUE)
#nrow(loanTest)
#nrow(loanTrain)

#
samplesize<-round(nrow(loanDataNoOutliersNA)*.667)
loantestindex<- sample(seq_len(nrow(loanDataNoOutliersNA)), size=samplesize)

#
loanTrain <- loanDataNoOutliersNA[loantestindex,]
loanTest <- loanDataNoOutliersNA[-loantestindex,]
nrow(loanTest)
nrow(loanTrain)

str(loanTest)
str(loanTrain)


#calling logistic regression
glmBase = glm(loanTrain$isLoanDefault  ~ loanAmount+interestRate+employmentYears+homeLiving+incomeAnnual+ageYears+creditGrade, family = "binomial", data = loanTrain)
glmBase

#summary
summary(glmBase)


#
predictionsBase <- predict(object = glmBase, newdata=loanTest, type="response")

#
loanTest<-cbind(loanTest,predictionsBase)

#
summary(predictionsBase)
summary(loanTest[,"isLoanDefault"])
summary(predictionsBase - loanTest[, "isLoanDefault"])


