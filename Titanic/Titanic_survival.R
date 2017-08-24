library(Amelia)
library(dplyr)
library(pscl)
library('randomForest')

training_raw <- read.csv('~/data/titanic_train.csv',header=T,na.strings=c(""))

#Data overview: variable classes & missingness:

#count the number of NA observations for each variable:
sapply(training_raw,function(x) sum(is.na(x)))
#count the number of unique observations for each variable:
sapply(training_raw, function(x) length(unique(x)))



#A visual representation of missingness with the 'Amelia' package:
missmap(training_raw, main = "Missing values vs observed")

#From our missingmap we can see cabin has too many missing values to gain any
#relevant insights, so we will drop this variable. Additionally, we'll remove
#the PassengerId/Ticket as neither variables can provide any useful information

#create our new training set with the subset function:
data = subset(training_raw,select=c(2,3,5,6,7,8,10,12))

#Dealing with the rest of the missingness/Imputation:
sapply(data, class)

#For numeric class - we'll impute by the mean:
data$Age[is.na(data$Age)] = mean(data$Age,na.rm=T)

#to check how R encodes our catgegorical variables- use contrasts function:
contrasts(data$Sex)
contrasts(data$Embarked)

#discard the only two missing observations in embarked
data = data[!is.na(data$Embarked),]
rownames(data) = NULL

#MODEL FITTING:
#Split our data into a train & test sets
dim(data) # 889 rows x 8 columns

#90/10 split:
train <- data[1:800,]
test <- data[801:889,]


#fitting with a glm model, specifying binomial b/c its a binary classifcation problem: 
glm_model <- glm(Survived ~.,family=binomial(link='logit'),data=train)

#check the result:
summary(glm_model)

#Interpreting our results:

#Coefficients:
# We have statistical significance with: Pclass/Sexmale/Age/SibSp
#sex has the lowest p-value, suggesting a strong relationship & b/c
#we have a negative coefficient suggests that all (all other vars being held equal)
#being male reduces the log odds of survival by 2.75
#unit increase in Age reduces log odds of survival by 0.037


#table of deviance using anova:
anova(glm_model, test="Chisq")
#shows how our model is doing against a null model (only an intercept)

#assessing the fit using McFadden (similar to R-squared)

pR2(glm_model)
#Mcfadden score: .33

#Assessing the predictive ability of the model:


fitted.results = predict(glm_model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results = ifelse(fitted.results > 0.5,1,0)

misClasificError = mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))

# 0.84 accuracy on the test set. Further evalutations will be to use CV & compare
#accuracy scores against other ML algorithms.









