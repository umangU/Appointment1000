# Loading the libraries class and caTools for knn() and sample.split() respectively
library(class)
library(caTools)
#Reading the appointment1000.csv in R
appoint=read.table("D:/Documents/UNE/Trimester-4/STAT430 - StatisticalLearning/Assignment-1/appointment1000.csv", header = TRUE,sep=",")
#Conveting the original dataset to numeric dataset
app <- sapply(appoint, as.numeric)
#Set Seed so that same sample can be reproduced in future
set.seed(123, kind = "Mersenne-Twister", normal.kind = "Inversion")
#Divide the dataset based on split ratio
sample = sample.split(appoint, SplitRatio = .6)
#Segregate the train and test data based on the above split ratio
train = subset(app, sample == TRUE)
test = subset(app, sample == FALSE)
#Binds the columns of the train and test dataset as a matrix
train.X<-transform(train, M <- paste0(Gender,Age,Hypertension,Diabetes,Alcoholism,Disability,SMS_received))
test.X<-transform(test, M <- paste0(Gender,Age,Hypertension,Diabetes,Alcoholism,Disability,SMS_received))
#Extracting the original values of the dependent variable from the train dataset
train.show=train[,11]
set.seed(2)
#For Loop to get confusion matrix for different values of k
for(i in 1:20)
{
  #Training and Predicting the dataset with ith value of k
  knn.pred=knn(train.X,test.X,train.show,k=i)
  #Line break
  cat('\n')
  print(paste0("For k = ",i))
  #Printing the confusion matrix between the predicted values and the original values in the test dataset
  print(table(knn.pred,test[,11]))
  #calculating the mean to find out the proportion of incorrect predictions (test error rate)
  print(mean(knn.pred!=test[,11]))
}
