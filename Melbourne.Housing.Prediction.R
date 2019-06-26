
# Clear old virables
rm(list=ls())
gc()

# Load related libraries
library(ggplot2)
library(leaps)
library(e1071)

encode_categorical <- function(train)
{
  for(i in names(train))
  {

    if(class(train[,i]) == "factor")
    {
      indicators <- model.matrix(~train[,i] - 1, data=train)
      colnames(indicators) <- paste(i,levels(train[,i]), sep = "_") 
      train <- cbind(train,indicators)
      train[,i] <- NULL
    }
    
  }
  return(train)
}

# Read data
melhouse <- read.csv(file="cleaned_melbourn_housing_data.csv",header = TRUE)

dim(melhouse)
str(melhouse)
summary(melhouse)
encode_categorical(melhouse)


melhouse$Date <- as.Date(melhouse$Date,format="%d/%m/%Y")
melhouse$Year <- as.numeric(format(melhouse$Date,"%Y"))

# Select related features for regression
simple_features <- c("Price" ,"Rooms", "Bathroom","Car","Distance","Landsize",
              "Propertycount","Lattitude","Longtitude","Type", "Year") 
# complex_features <- c("Price" ,"Rooms", "Bathroom","Car","Distance","Landsize",
#                      "Propertycount","Lattitude","Longtitude","Date",
#                      "Type","Method","Regionname")

# Check Price distribution
hist(melhouse$Price,xlab="Price",main="Histogram of Price")
hist(log(melhouse$Price),xlab="log(Price)",main="Histogram of log(Price)")

#



# Perform full subset selection
simple_data <- melhouse[,simple_features]
dim(simple_data)
simple_data <- encode_categorical(simple_data)
help(regsubsets)
regfit.full <- regsubsets(log(Price)~.,data=simple_data,nvmax=12,method = "forward")
summary(regfit.full)

names(summary(regfit.full))
summary(regfit.full)$rsq
reg.summary <- summary(regfit.full)

# Plot RSS and Adjusted RSQ
plot(reg.summary$rss ,xlab=" Number of Variables ",ylab=" RSS", type="l")
plot(reg.summary$adjr2 ,xlab =" Number of Variables ",ylab=" Adjusted RSq",type="l")
# The plot shows that 6 predictors are sufficient for predicting "Price"

plot(regfit.full, scale ="r2")
plot(regfit.full, scale ="adjr2")
coef(regfit.full, 6)
# Rooms      Bathroom           Car      Distance     Lattitude    Longtitude 
input_features <- c("Rooms","Bathroom","Distance","Lattitude","Longtitude", "Type_u", "Price")
pairs(melhouse[,input_features])



### SPLIT ORIGINAL DATA SET TO TRAIN AND TEST DATA SET
# Train data set (80%), and test data set (20%)
set.seed(1)
shuffled <- sample(nrow(melhouse))
train.rows = sample(1:nrow(melhouse), nrow(melhouse)*0.8)
train <- melhouse[train.rows,]
test <- melhouse[-train.rows,]

dim(train) # 16249    18
dim(test)  # 4063     18
train <- train[,input_features]
test <- test[,input_features]



# LINEAR REGESSION
str(melhouse[,simple_features])
cor(melhouse[,simple_features])[,"Price"]

model.lm <- lm(Price~.,data = train)
pred.lm.train <-(predict(model.lm, train))
(train.mse.lm <- mean((pred.lm.train - train$Price)^2)) # 2.36043e+11

pred.lm.test <-(predict(model.lm, test))
(test.mse.lm <- mean((pred.lm.test - test$Price)^2)) # 203726527031

# SVM
# Convert factors variable to numeric variable
#train.svm <- transform_numeric(train.svm)
# Fit svm model
(t1 <- format(Sys.time()))
model.svm <- svm(Price ~ ., data = train, 
                 cost=10, gamma=0.1, scale=TRUE, kernel="radial")
(t2 <- format(Sys.time()))

summary(model.svm) # Number of Support Vectors:  11821

pred.svm.train <-(predict(model.svm, train))
(train.mse.svm <- mean((pred.svm.train - train$Price)^2)) # 143,553,561,961

pred.svm.test <-(predict(model.svm, test))
(test.mse.svm <- mean((pred.svm.test - test$Price)^2)) # 127,272,645,079
# Compare to Linear regession
(train.mse.svm/train.mse.lm) # 0.608167
(test.mse.svm/test.mse.lm)   # 0.624723

# RIDGE REGRESSION

library(glmnet)
train.x.non <- model.matrix(Price~., data=train)[,-1] # drop dummy intercept
train.y.non <- train$Price

test.x.non <- model.matrix(Price~., data=test)[,-1] # drop dummy intercept
test.y.non <- test$Price


# Using cross-validation to tune lambda
set.seed(1)
cv.out <- cv.glmnet(train.x.non, train.y.non, alpha=0)
plot(cv.out)
(best.lambda <- cv.out$lambda.min) # 32722.93
best.lambda

# Train ridge regession model with the best lambda
model.ridge <- glmnet(train.x.non, train.y.non, alpha=0, lambda=best.lambda)
# Predict test outcome
predicted.y.non = predict(model.ridge, s=best.lambda, newx=test.x.non, exact=TRUE)
# Test MSE
(test.mse.non <- mean((predicted.y.non - test.y.non)^2)) # 203775559291
# Difference between two Test MSE
(test.mse.non/test.mse.lm) # 1.000241 



# So the one standard error rule, which is pretty popular now, is not use the model with the absolute minimum.
# But use a simpler model that comes within one standard deviation of the minimum.
# So the rationale for this, again I've said it, is that if the models are within one standard error of each other,
# let's choose the simplest one. Because it's easier to interpret.
