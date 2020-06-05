RANDOM FOREST CODE
#Random Forest Regression
# Importing the dataset
dataset = read.csv('Albenia 25-34 M.csv')
train= dataset[1:21,1:5]
test= dataset[22,1:4]
# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')    #use this if the package is not installed in your system
library(randomForest)
regressor = randomForest(x = train[1:4],
                         y = train$suicides_no,
                         ntree = 500)
# Predicting a new result with Random Forest Regression
y_pred = predict(regressor,test)
SVR CODE
# SVR
# Importing the dataset
dataset = read.csv('Albenia 15-24 M.csv')
train= dataset[1:21,1:5]
test= dataset[22,1:4]
# Fitting SVR to the dataset
# install.packages('e1071')     #use this if the package is not installed in your system
library(e1071)
regressor = svm(formula = suicides_no ~ .,
                data = train,
                type = 'eps-regression',
                kernel = 'radial')
# Predicting a new result
y_pred = predict(regressor, test)