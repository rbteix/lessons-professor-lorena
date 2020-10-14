# Predictive evaluation using Iris dataset (R)
# The script includes:
# error metrics
# sampling methods (for generating training/test partitions)
# other performance measures for classification problems
# ROC curves for binary classification problems
# statistical tests for models comparison
# computation of bias and variance components in a simulated regression problem

############################################################################
# Error metrics


# Part of iris data, two features just for easing plots, and two classes
irisPart <- subset(iris, select = Petal.Length:Species)
irisPart <- subset(irisPart,Species != "setosa")
irisPart <- droplevels(irisPart)
?droplevels
library(class)

# 1nn with irisPart
knn1 <- knn.cv(irisPart[,1:2], irisPart$Species, k = 1)
#computing accuracy
ac = sum(knn1 == irisPart$Species) / length(irisPart$Species)
ac
# error rate
er = 1.0 - ac
er

# testing a regression
# using feature 1 to predict feature 2
# ploting
plot(irisPart[,1],irisPart[,2])
# trying a linear regression
# dividing the dataset into training and testing sets
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(irisPart), 0.8*nrow(irisPart))  # row indices for training data
trainingData <- irisPart[trainingRowIndex, ]  # model training data
testData  <- irisPart[-trainingRowIndex, ] 
linear <- lm(trainingData[,2]~trainingData[,1])
pred <- predict(linear, data = testData[,1])

# mse
mse <- mean((testData[,2]-pred)^2)
mse

# Exercice: compute normalized mean squared error
nmse <- mean((testData[,2]-pred)^2/ (testData[,2] - mean(testData[,2]))^2)
nmse

# Confusion matrix
# 1nn with iris (total)
knn2 <- knn.cv(iris[,1:2], iris$Species, k = 1)
table(iris$Species,knn2)

######################################################################################################
# Sampling for training/testing

# Holdout
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(iris), 0.8*nrow(iris))  # row indices for training data
trainingData <- iris[trainingRowIndex, ]  # model training data
summary(trainingData)
testData  <- iris[-trainingRowIndex, ] 
summary(testData)

# Exercice: change seed and observe the modifications

set.seed(120)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(iris), 0.8*nrow(iris))  # row indices for training data
trainingData <- iris[trainingRowIndex, ]  # model training data
summary(trainingData)
testData  <- iris[-trainingRowIndex, ] 
summary(testData)

set.seed(90)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(iris), 0.8*nrow(iris))  # row indices for training data
trainingData <- iris[trainingRowIndex, ]  # model training data
summary(trainingData)
testData  <- iris[-trainingRowIndex, ] 
summary(testData)

set.seed(70)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(iris), 0.8*nrow(iris))  # row indices for training data
trainingData <- iris[trainingRowIndex, ]  # model training data
summary(trainingData)
testData  <- iris[-trainingRowIndex, ] 
summary(testData)

# Cross-validation
install.packages("caret",dependencies=TRUE)
library("caret")
folds <- createFolds(iris$Species, k = 10, list = FALSE)

# summarizing first training/test fold
train1 <- iris[folds!=1,]
summary(train1)
test1 <- iris[folds == 1,]
summary(test1)

##########################################################################################################
# Other performance metrics for classification problems

# using the irisPart dataset
library(class)
knn1 <- knn.cv(irisPart[,1:2], irisPart$Species, k = 1)
conf <- table(irisPart$Species,knn1)
conf

# False positive rate (positive class = versicolor)
FPR = conf[2,1]/sum(conf[,2])
FPR

# Total accuracy
ac = sum(diag(conf))/sum(conf)
ac

# precision
prec = conf[1,1]/sum(conf[,1])
prec

# Exercice: compute recall, sensibility, specificity and F1 measures

#Sensibility
sens = conf[1,1]/ sum(conf[1,])
sens

#Specificity
spec = conf[2,2]/sum(conf[2,])
spec

#F1
F1 = (2*sens*prec)/ (sens+prec)
F1
########################################################################################################
# ROC curves
# Based in: https://www.youtube.com/watch?v=ypO1DPEKYFo

install.packages("ROCR")
library(ROCR)
install.packages("nnet")
library(nnet)

str(irisPart)
# fitting a multinomial linear model to the data

model <- multinom(Species~.,irisPart)
# predicting on the same data (in practice, you should use test data)

p <- predict(model,irisPart)

# confusion matrix using a threshold cut of 0.5
table(irisPart$Species,p)

# now taking the probabilities of the predictions
pred <- predict(model,irisPart,type="prob")

# comparing the probabilities with the classes of the first and last examples in irisPart
head(pred)
head(irisPart)
tail(pred)
tail(irisPart)

# getting the ROC curve
pred2 <- prediction(pred,irisPart$Species)
roc <- performance(pred2,"tpr","fpr")
plot(roc, colorize=T)
abline(a=0,b=1)

# computing the AUC value
auc <- performance(pred2,"auc")
auc <- unlist(slot(auc,"y.values"))
auc

# Exercice: repeat the ROC analysis, but separating the irisPart dataset into training and testing parts accordingly (you can use holdout)

set.seed(100)  
trainingRowIndex <- sample(1:nrow(irisPart), 0.8*nrow(irisPart))  
trainingData <- irisPart[trainingRowIndex, ]  
summary(trainingData)
testData  <- irisPart[-trainingRowIndex, ] 
summary(testData)

str(testData)
# fitting a multinomial linear model to the data
model <- multinom(Species~.,testData)
# predicting on the same data (in practice, you should use test data)
p <- predict(model,testData)
# confusion matrix using a threshold cut of 0.5
table(testData$Species,p)

# now taking the probabilities of the predictions
pred <- predict(model,testData,type="prob")
# comparing the probabilities with the classes of the first and last examples in irisPart
head(pred)
head(testData)
tail(pred)
tail(testData)

# getting the ROC curve
pred2 <- prediction(pred,testData$Species)
roc <- performance(pred2,"tpr","fpr")
plot(roc, colorize=T)
abline(a=0,b=1)

# computing the AUC value
auc <- performance(pred2,"auc")
auc <- unlist(slot(auc,"y.values"))
auc

#######################################################################################################
# Statistical tests
# From: http://htmlpreview.github.io/?https://github.com/b0rxa/scmamp/blob/master/inst/doc/Statistical_assessment_of_the_differences.html

if (!require("devtools")) {
  install.packages("devtools")
}

devtools::install_github("b0rxa/scmamp")
library(scmamp)

data(data_gh_2008)
View(data.gh.2008)

f <- friedmanTest(data.gh.2008)
f
if (f$p.value < 0.05)
{
    test <- nemenyiTest (data.gh.2008, alpha=0.05)   
    plotCD (data.gh.2008, alpha=0.05, cex=1.25)
    test
    View(test$diff.matrix)
    View(abs(test$diff.matrix) > test$statistic)
    
}

#######################################################################################################
# Bias-variance
# From: https://daviddalpiaz.github.io/r4sl/simulating-the-biasvariance-tradeoff.html

# genereting synthetic regression dataset y = x^2 + noise (gaussian with mean 0 and variance 0.3)
f = function(x) {
  x ^ 2
}

get_sim_data = function(f, sample_size = 100) {
  x = runif(n = sample_size, min = 0, max = 1)
  y = f(x) + rnorm(n = sample_size, mean = 0, sd = 0.3)
  data.frame(x, y)
}

# example of one dataset
sim_data = get_sim_data(f, sample_size = 100)
plot(sim_data)

# estimating bias and variance of some models in a specific point
set.seed(1)
n_sims = 1000 # number of runs
n_models = 4 # number of tested models
x0 = 0.9 # point for calculations
predictions = matrix(0, nrow = n_sims, ncol = n_models)
plot(y ~ x, data = sim_data, col = "white", xlim = c(0.7, 1), ylim = c(0, 1.5))

for (i in 1:n_sims) {
  
  sim_data = get_sim_data(f, sample_size = 100)
  
  fit_1 = lm(y ~ 1, data = sim_data)
  fit_2 = lm(y ~ poly(x, degree = 1), data = sim_data)
  fit_3 = lm(y ~ poly(x, degree = 2), data = sim_data)
  fit_4 = lm(y ~ poly(x, degree = 3), data = sim_data)
  
  lines(grid, predict(fit_1, newdata = data.frame(x = grid)), col = "red")
  #lines(grid, predict(fit_2, newdata = data.frame(x = grid)), col = "blue")
  #lines(grid, predict(fit_3, newdata = data.frame(x = grid)), col = "green")
  lines(grid, predict(fit_4, newdata = data.frame(x = grid)), col = "orange")
  
  predictions[i, ] = c(
    predict(fit_1, newdata = data.frame(x = x0)),
    predict(fit_2, newdata = data.frame(x = x0)),
    predict(fit_3, newdata = data.frame(x = x0)),
    predict(fit_4, newdata = data.frame(x = x0))
  )
}

points(x0, f(x0), col = "black", pch = "x", cex = 2)

#eps = rnorm(n = n_sims, mean = 0, sd = 0.3)
#y0 = f(x0) + eps
eps = runif(1)
y0 = rep(f(x0)+eps,n_sims)

get_bias = function(estimate, truth) {
  mean(estimate) - truth
}

get_mse = function(estimate, truth) {
  mean((estimate - truth) ^ 2)
}

bias = apply(predictions, 2, get_bias, f(x0))
variance = apply(predictions, 2, var)
mse = apply(predictions, 2, get_mse, y0)

bias^2
variance
mse

# Exercice: observe what happens for different values of x0: 0.75 and 0.99

x0 = 0.75 # point for calculations
eps = runif(1)
y0 = rep(f(x0)+eps,n_sims)

get_bias = function(estimate, truth) {
  mean(estimate) - truth
}

get_mse = function(estimate, truth) {
  mean((estimate - truth) ^ 2)
}

bias = apply(predictions, 2, get_bias, f(x0))
variance = apply(predictions, 2, var)
mse = apply(predictions, 2, get_mse, y0)

bias^2
variance
mse

x0 = 0.99 # point for calculations
eps = runif(1)
y0 = rep(f(x0)+eps,n_sims)

get_bias = function(estimate, truth) {
  mean(estimate) - truth
}

get_mse = function(estimate, truth) {
  mean((estimate - truth) ^ 2)
}

bias = apply(predictions, 2, get_bias, f(x0))
variance = apply(predictions, 2, var)
mse = apply(predictions, 2, get_mse, y0)

bias^2
variance
mse
