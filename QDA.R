#Loading the Mass and caTools for libraries for qda() and sample.split() function respectively
library(MASS)
library(caTools)
#Reading the appointment1000.csv in R
appoint=read_xlsx(file.choose())
attach(appoint)
# Fitting the original dataset in the Logistic Regression model
qda.fit=qda(No.show~Gender+Age+Hypertension+Diabetes+Alcoholism+Disability+SMS_received, data=appoint)
qda.fit
# Set Seed
set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion")
# Divide the dataset based on split ratio
sample = sample.split(appoint, SplitRatio = .6)
# Segregate the train and test data based on the above split ratio
train = subset(appoint, sample == TRUE)
test = subset(appoint, sample == FALSE)
# Using Predict to get a list of three elements namely 'class', 'posterior' and 'x'
qda.class=predict(qda.fit,test)$class 
# 'class' contains the predictions about the people going as per their appointments or not
# Confusion matrix between the predicted values and the original values in the test data
table(qda.class,test$No.show)
# Calculating the mean to find out the proportion of correct predictions
mean(qda.class==test$No.show)
# Calculating the mean to find out the proportion of incorrect predictions (test error rate)
mean(qda.class!=test$No.show)
