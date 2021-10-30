# Loading the library caTools for sample.split()
library(caTools)
# Reading the appointment1000.csv in R
appoint=read_xlsx(file.choose())
attach(appoint)
# Mosaic plot for plotting Number of people who showed up for the appointment and people who recieved the SMS
mosaicplot(No.show ~ SMS_received, xlab="Attendence for the Appointment", ylab="SMS Recieved", main="Affect of Recieved SMS on the Attendence of the patients", col=c("powderblue","mistyrose"))
# Set Seed so that same sample can be reproduced in future
set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion")
# Divide the dataset based on split ratio
sample = sample.split(appoint, SplitRatio = .6)
# Segregate the train and test data based on the above split ratio
train = subset(appoint, sample == TRUE)
test = subset(appoint, sample == FALSE)
# Fitting the original dataset in the Logistic Regression model
glm.fit=glm(No.show~Gender+Age+Hypertension+Diabetes+Alcoholism+Disability+SMS_received, family=binomial, data=appoint)
summary(glm.fit)
# Show the contrasts associated with the factor No.show
contrasts(No.show)
# Predicting the probabilities for No.show from the original dataset
glm.probs=predict(glm.fit,type="response")
# Generate a vector with replicated values 'No'
glm.pred=rep("No",1000)
# Converting the values with probability more than 0.5 to values 'Yes'
glm.pred[glm.probs>0.5]="Yes"
# Confusion matrix between the predicted values and the original values
table(glm.pred,appoint$No.show)
# calculating the mean to find out the proportion of correct predictions
mean(glm.pred==appoint$No.show)
# Fitting the train dataset in the Logistic Regression model
glm_train.fit=glm(No.show~Gender+Age+Hypertension+Diabetes+Alcoholism+Disability+SMS_received, family=binomial, data=train)
# Predicting the probabilities for No.show from the train dataset
glm_train.probs=predict(glm_train.fit,test,type="response")
# Generate a vector with replicated values 'No' with size as that of the observations in the test dataset
glm_train.pred=rep("No",454)
#Converting the values with probability more than 0.5 to values 'Yes'
glm_train.pred[glm_train.probs>0.5]="Yes"
#Confusion matrix between the predicted values and the original values in the test dataset
table(glm_train.pred,test$No.show)
# calculating the mean to find out the proportion of correct predictions
mean(glm_train.pred==test$No.show)
# calculating the mean to find out the proportion of incorrect predictions (test error rate)
mean(glm_train.pred!=test$No.show)
