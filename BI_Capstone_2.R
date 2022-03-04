install.packages("corrplot")
install.packages("randomForest")
install.packages("rpart.plot")
install.packages("MLmetrics")

library(tidyverse)
library(dslabs)
library(caret)
library(ggplot2)
library(RColorBrewer)
library(corrplot)
library(data.table)
library(e1071)
library(randomForest)
library(rpart)
library(rpart.plot) 
library(MLmetrics)
# In this case we are only going to refer to the mathematics results of students performance.

d1<- read.csv("C:/Users/belen/Documents/Lic en Economía - Desarrollo/capstone/student-mat.csv",sep=";",header=TRUE)

# Adding a column for Pass or Failed 

d1<-d1%>% mutate(pass=ifelse(d1$G3>9,1,0))


# Encoding the categorical features as factors and integers to numeric
d1$famsize<-factor(d1$famsize)
d1$address<-factor(d1$address)
d1$Pstatus<-factor(d1$Pstatus)
d1$Medu<-factor(d1$Medu)
d1$Fedu<-factor(d1$Fedu)
d1$Mjob<-factor(d1$Mjob)
d1$Fjob<-factor(d1$Fjob)
d1$schoolsup<-factor(d1$schoolsup)
d1$famsup<-factor(d1$famsup)
d1$nursery<-factor(d1$nursery)
d1$pass<-factor(d1$pass)


d1$age <- as.numeric(d1$age)
d1[c("traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences")] <- lapply(d1[c("traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences")], as.numeric)


d1$G1<- NULL
d1$G2<- NULL
d1$G3<- NULL

#Exploratory Analysis
str(d1)
mean(d1$pass)
d1%>% ggplot(aes(sex,fill=pass))+geom_bar(position="dodge")+ggtitle("Passing Number per Sex")
d1 %>% ggplot(aes(address,fill=pass))+geom_bar(position="dodge")+ggtitle("Passing Number per Area")
d1 %>% group_by(address) %>% summarise(porcentagepass=mean(pass=="1")*100)
d1 %>% ggplot(aes(famsize,fill=pass))+geom_bar(position="dodge")+ggtitle("Passing Number per Size of Family")
d1 %>% group_by(famsize) %>% summarise(porcentagepass=mean(pass=="1")*100)
d1%>% ggplot(aes(Pstatus,fill=pass))+geom_bar(position="dodge")+ggtitle("Passing Number per Parent cohabitation status")
d1 %>% group_by(Pstatus) %>% summarise(porcentagepass=mean(pass=="1")*100)
d1 %>% ggplot(aes(Medu,fill=pass))+geom_bar(position="dodge")+ggtitle("Passing Number per Mom's education")
d1 %>% group_by(Medu) %>% summarise(porcentagepass=mean(pass=="1")*100)
d1 %>% ggplot(aes(Fedu,fill=pass))+geom_bar(position="dodge")+ggtitle("Passing Number per Dad's education")
d1 %>% group_by(Fedu) %>% summarise(porcentagepass=mean(pass=="1")*100)
d1 %>% ggplot(aes(absences,fill=pass))+geom_bar(position="dodge")+ggtitle("Passing Number per Absences")
d1 %>% group_by(absences) %>% summarise(porcentagepass=mean(pass=="1")*100) 
table1<- d1 %>% group_by(absences) %>% summarise(porcentagepass=mean(pass=="1")*100) 
table1 %>% ggplot(aes(absences,porcentagepass))+geom_point()+geom_smooth(method = "lm")
lm(table1$porcentagepass~table1$absences)
summary(lm(table1$porcentagepass~table1$absences))
cor(table1$porcentagepass,table1$absences)
d1 %>% ggplot(aes(Dalc,fill=pass))+geom_bar(position="dodge")+ggtitle("Passing Number per Daily Alcohol consumption")
d1 %>% group_by(Dalc) %>% summarise(porcentagepass=mean(pass=="1")*100)
d1 %>% ggplot(aes(studytime,fill=pass))+geom_bar(position="dodge")+ggtitle("Passing Number per Studytime")
d1 %>% group_by(studytime) %>% summarise(porcentagepass=mean(pass=="1")*100)
d1 %>% ggplot(aes(famrel,fill=pass))+geom_bar(position="dodge")+ggtitle("Passing Number per Family RElation")
d1 %>% group_by(famrel) %>% summarise(porcentagepass=mean(pass=="1")*100)
d1 %>% ggplot(aes(nursery,fill=pass))+geom_bar(position="dodge")+ggtitle("Passing Number per Nursery")
d1 %>% group_by(nursery) %>% summarise(porcentagepass=mean(pass=="1")*100)
d1 %>% ggplot(aes(internet,fill=pass))+geom_bar(position="dodge")+ggtitle("Passing Number per Internet")
d1 %>% group_by(internet) %>% summarise(porcentagepass=mean(pass=="1")*100)
d1 %>% ggplot(aes(failures,fill=pass))+geom_bar(position="dodge")+ggtitle("Passing Number per Previous Failures")
d1 %>% group_by(failures) %>% summarise(porcentagepass=mean(pass=="1")*100)

ggplot(d1,aes(pass,age)) + geom_boxplot(aes(fill=factor(pass)),alpha=0.5) + ggtitle("Age distribution based on if they passed")




#Partitioning in validation and train

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = d1$pass, times = 1, p = 0.75, list = FALSE)
train <- d1[test_index,]
validation <- d1[-test_index,]
nrow(validation)
nrow(train)

#Partitioning the train into train set and test set
trainingindex<- createDataPartition(train$pass,times=1,p=0.8,list = FALSE)
training_set<- train[trainingindex,]
test_set<- train[-trainingindex,]
nrow(training_set)
nrow(test_set)


#Data Exploration

summary(d1)
str(d1)
head(d1)
ncol(d1)
nrow(d1)
d1%>% ggplot(aes(sex))+geom_bar()
d1 %>% ggplot(aes(school))+geom_bar()
d1 %>% ggplot(aes(age))+geom_histogram()
d1 %>% group_by(famsize) %>% ggplot(aes(famsize))+geom_bar()
d1 %>% group_by(Medu) %>% ggplot(aes(Medu))+geom_histogram()
d1 %>% group_by(Fedu) %>% ggplot(aes(Fedu))+geom_histogram()
d1 %>% group_by(Mjob) %>% ggplot(aes(Mjob)) + geom_bar()
d1 %>% group_by(Fjob) %>% ggplot(aes(Fjob)) + geom_bar()
d1 %>% ggplot(aes(guardian)) + geom_bar()
d1 %>% ggplot(aes(famsup))+geom_bar()
d1 %>% ggplot(aes(G3)) + geom_density()
d1%>% ggplot(aes(famrel))+geom_bar()

d1 %>% group_by(Medu) %>% summarise(avg_grade=mean(pass))
d1 %>% group_by(Fedu) %>% summarise(avg_grade=mean(pass))
d1%>% group_by(Mjob) %>% summarise(avg_grade=mean(pass))
d1%>% group_by(Fjob) %>% summarise(avg_grade=mean(pass))
d1%>% group_by(studytime) %>% summarise(avg_grade=mean(pass))
d1%>% group_by(failures) %>% summarise(avg_grade=mean(pass))
d1 %>% group_by(activities) %>% summarise(avg_grade=mean(pass))
d1 %>% group_by(pass) %>% summarise(n=n())
failed<- d1 %>% filter(pass==0)
summary(failed)
explore1<- sapply(d1)

# Binary
d1<-d1%>% mutate(pass=ifelse(d1$G3>9,1,0))
d1%>% ggplot(aes(pass))+geom_bar()
d1 %>% lm(pass~age,data=.)
d1%>% ggplot(aes(age,pass))+geom_col()




# Predictions

corrplot(cor(training_set[,unlist(lapply(training_set,is.numeric))]))

#Naive Bayes Method

# Fitting Naive Bayes to the Training set
classifier = naiveBayes(pass ~ ., data = training_set)

# Predicting the Validation set results
y_pred = predict(classifier, newdata = test_set[,-which(names(test_set)=="pass")])

# Checking the prediction accuracy
confusionmatrix<- table(test_set$pass, y_pred) # Confusion matrix
normalizedcm<- confusionmatrix/sum(confusionmatrix)
normalizedcm

error <- mean(test_set$pass != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))


#SVM Model
classifier = svm(pass ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')
# Predicting the Validation set results
y_pred = predict(classifier, newdata = test_set[,-which(names(test_set)=="pass")])

# Checking the prediction accuracy
table(test_set$pass, y_pred) # Confusion matrix

error <- mean(test_set$pass != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))


#Random Forest
# Fitting Decision Tree Classification Model to the Training set
classifier = rpart(pass ~ ., data = training_set, method = 'class')

# Tree Visualization
rpart.plot(classifier, extra=4)
# Predicting the Validation set results
y_pred = predict(classifier, newdata = test_set[,-which(names(test_set)=="pass")], type='class')

# Checking the prediction accuracy
table(test_set$pass, y_pred) # Confusion matrix
error <- mean(test_set$pass != y_pred) # Misclassification error
paste('Accuracy',round(1-error,4))
