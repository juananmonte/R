#install packages

install.packages("ranger")
install.packages("caret")
install.packages("caTools")


#Importing the libraries

library(ranger)
library(caret)
library(data.table)
library(caTools)

credit_card <- creditcard

#DATA ANALYSIS

head(credit_card, 10)

table(credit_card$Class) # we have 284315 of 0 and 492 cases of 1

summary(credit_card$Amount) #the mean of the purchases with the credit cards is $88.35. We have a case of
                            #a really high purchase of $25,691.16

var(credit_card$Amount)

sd(credit_card$Amount)

#graph amount

credit_card %>% ggplot(aes(x=Amount)) + geom_histogram(bins = 50, fill = "blue", color= "#e9ecef", alpha = 0.9)+
  ggtitle("Amount")


credit_card$Amount=scale(credit_card$Amount)
NewData=credit_card[,-c(1)]
head(NewData)

#DATA modeling and separation for train and test sets

set.seed(123)
data_sample = sample.split(NewData$Class, SplitRatio = 0.80)
train_data = subset(NewData, data_sample == TRUE)
test_data = subset(NewData, data_sample == FALSE)
dim(train_data)
dim(test_data)

#Fitting Logistic Regression Model

Logistic_Model=glm(Class~.,test_data, family=binomial())
summary(Logistic_Model)





