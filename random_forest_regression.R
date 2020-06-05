# SVR
# Importing the dataset
library(readr)
dataset <- read_csv('input.csv')
y<-0
data=dataset[1]   #country column
n<-nrow(data)
print(n)
a<-c()
#country names
for (i in 1:n) {
  a[i]<-data$country[i]
}
#print(a)
final<-unique(a)
print(final)

u= data.frame();
e=data.frame();
for(count in final){
#country names
d=0
p=0
for(i in 1:n){
  if(data$country[i]==count)
    d=d+1
}
print(d)
for(i in 1:n){
  if(data$country[i]==count){
    p=i
    break;
  }
}
w<-p+d-1
dataset1=dataset[p:w,1:7]
#age


#age group
b<-c()
age<-dataset[4]
print(age)
for (i in 1:n) {
  b[i]<-age1$age[i]
}
b<-unique(b)    #unique age group
print(b)

for(age in b){
d1<-data.frame()
data1=dataset1[4]   #age column
n<-nrow(data1)
print(n)
d=0
p=0
for(i in 1:n){
  if(data1$age[i]==age){
    d=d+1
    d1<- rbind(d1,dataset1[i,1:7],deparse.level = 1)
  }
}
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
# install.packages('e1071')
library(e1071)
regressor = svm(formula = suicides_no ~ .,
                data = train,
                type = 'eps-regression',
                kernel = 'radial')
# Predicting a new result
for(i in 1:tno){
  test2=test[i,1:4]
  y_pred = predict(regressor, test2)
  u<-rbind(u,y_pred,deparse.level = 1)
  print("Predicted:")
  print(y_pred)
  print("Actual:")
  e<-rbind(e,test[i,5],deparse.level = 1)
  print(test[i,5])
}
}
}
t<-nrow(e)
ss <- 0
for(i in 1:t){
  ss <- ss + sqrt(mean((u[i,1] - e[i,1])^2))
}
ss <- ss/t
ss