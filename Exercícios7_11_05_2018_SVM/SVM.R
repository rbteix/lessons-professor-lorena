# Support Vector Machines
######################################################################################################################

install.packages("e1071")
library("e1071")

data(iris)

# Part of iris data, two features just for easing plots, and two classes
irisPart <- subset(iris, select = Petal.Length:Species)
irisPart <- subset(irisPart,Species != "setosa")
irisPart <- droplevels(irisPart)

# separating training and test sets
train.indices <- sample(1:nrow(irisPart), 0.7*nrow(irisPart)) #0,7porcentagem do dataset para treino
iris.train <- irisPart[train.indices, ]
iris.test <- irisPart[-train.indices, ]

## classification mode

# Training
model <- svm(Species ~ ., data = iris.train,kernel="linear",cost = 1)
print(model)
summary(model)

# visualize (classes by color, SV by crosses):
# From e1071 package
plot(iris.train[,-3],
     col = as.integer(iris.train[,3]),
     pch = c("o","+")[1:70 %in% model$index + 1])

decisionplot(model, iris.train, class = "Species", main = "SVM (linear)")

# test 
pred <- predict(model, iris.test[,-3])
# Check accuracy (confusion matrix):
table(iris.test[,3], pred)

# compute decision values:
pred <- predict(model, iris.test[,-3],decision.values = TRUE)

# compute probabilities
model <- svm(Species ~ ., data = iris.train,kernel="linear",cost = 1,probability=TRUE)
pred <- predict(model, iris.test[,-3],prob = TRUE)

# Exercices: 
# Vary the cost to 0.5 and 1000 and observe what happens
# Vary the Kernel as  "radial", "polynomial" and "sigmoid" and see what happens

# Code from http://michael.hahsler.net/SMU/EMIS7332/R/viz_classifier.html
decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}

######################################################################3##################################################
# Studying the Kernel concept
install.packages("mlbench")
library("mlbench")

data <- mlbench.circle(1000)
plot(data)

data2 <- data.frame(data$x[,1],data$x[,2],data$classes)

# Making an artificial dataset
#x1 <- runif(100,min=-1,max=1)
#x2 <- runif(100,min=-1,max=1)
#y <- rep(1,200)
#y[which((x1^2+x2^2)<0.5)] <- 2
#data <- data.frame(x1,x2,y)

#plot(data$x1,data$x2,pch = as.integer(data$y))

# Mapping to three dimensions (transformação de duas dimensões para 3)
nx1 <- data2[,1]^2
nx2 <- sqrt(2) * data2[,1] * data2[,2]
nx3 <- data2[,2]^2
data3 <- data.frame(nx1,nx2,nx3,data$classes)

install.packages("scatterplot3d")
library(scatterplot3d)
colors <- c("1","2")
colors <- colors[as.numeric(data3[,4])]
scatterplot3d(data3[,1:3], pch = 16, color=colors,angle = 140)

#######################################################################################################################
# SVM tuning

obj <- tune.svm(Species~., data = iris.train, gamma = 2^(-1:1), cost = 2^(2:4))
obj

# with parameter tuning
model <- svm(Species ~ ., data = iris.train,cost = obj$best.parameters$cost,gamma=obj$best.parameters$gamma)
pred <- predict(model, iris.test[,-3])
# Check accuracy (confusion matrix):
table(iris.test[,3], pred)


#######################################################################################################################
## SVM for regression
# From e1071 package

# create data
x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)

m <- svm(x, y)
new <- predict(m, x)
# visualize
plot(x, y)
points(x, log(x), col = 2)
points(x, new, col = 4)
