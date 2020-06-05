dataset=read.csv('Albenia 35-54  M.csv')
#install.packages('randomForest')
#library(randomForest)
train= dataset[1:21,1:5]
test= dataset[22,-(2)]
regressor = randomForest(x = train[-(2)],
                         y = train$suicides_no,
                         ntree = 500)
# Predicting a new result with Random Forest Regression
y_pred = predict(regressor,test)
unsplit()
