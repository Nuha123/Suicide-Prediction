# Random Forest Regression

# Importing the dataset
library(readr)
dataset <- read.csv('input.csv')
data=dataset[1] #country column
n<-nrow(data)
print(n)
#country names
d=0
p=0
count <- readline(prompt="Enter Country: ")
for(i in 1:n){
  if(data$country[i]==count)
    d=d+1
}
print(d)
stopifnot(d>0)
for(i in 1:n){
  if(data$country[i]==count){
    p=i
    break;
  }
}
w<-p+d-1
dataset=dataset[p:w,1:7]

#age
opt <-0
opt <- readline(prompt="Enter age group number:
                1.5-14 years
                2.15-24 years
                3.25-34 years
                4.35-44 years
                5.55-74 years
                6.75+ years ")
opt <- as.numeric(opt)
age<-switch(opt,"5-14 years","15-24 years","25-34 years","35-54 years","55-74 years","75+ years",stopifnot("Entred option"))
d1<-data.frame()
data=dataset[4]   #age column
n<-nrow(data)
print(n)
d=0
p=0
for(i in 1:n){
  if(data$age[i]==age){
    d=d+1
    d1<- rbind(d1,dataset[i,1:7],deparse.level = 1)
  }
}
stopifnot(d>0)
yr <- d1$year[1]
for(i in 2:d){
  if(d1$year[i]>yr)
    yr <- d1$year[i]
}
print(yr)
test<-data.frame()
train<-data.frame()
for(i in 1:d){
  if(d1$year[i]==yr)
    test<-rbind(test,d1[i,1:7],deparse.level = 1)
  else
    train<-rbind(train,d1[i,1:7],deparse.level = 1)
  
}
q<-train[,2:3]
q1<-train[,5:7]
train<-cbind(q,q1,deparse.level = 1)
q<-test[,2:3]
q1<-test[,5:7]
test<-cbind(q,q1,deparse.level = 1)
tno<-nrow(test)

#Feature scaling
ab <- as.numeric(train[,4])
ab <- scale(numeric(train[,4]))

# Fitting Random Forest Regression to the dataset
install.packages('randomForest')
library(randomForest)
regressor = randomForest(x = train[1:4],
                         y = train$suicides_no,
                         ntree = 6000)
# Predicting a new result with Random Forest Regression
y_pred = predict(regressor, test)
y_pred1 = predict(regressor, test1)