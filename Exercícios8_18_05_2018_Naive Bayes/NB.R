# Naive Bayes
##########################################################################################################################

# Using dataset balance scale
balance <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/balance-scale/balance-scale.data"),header=FALSE)
# moving class to the last column
balance <- balance[,c(2,3,4,5,1)] # reordenou as classes

# Adapted from https://bitbucket.org/rodrigo_mello/ml4u/src/ab2300b432ac9105df1352d5890543cf136bef8f/naive-bayes/naive.r?at=master&fileviewer=file-view-default
naive <- function(dataset, query) {
  colId = ncol(dataset)
  classes = unique(dataset[,colId]) #ultima coluna, e fala o que tem dentro
  
  P = rep(0, length(classes)) # armazenado a probabilidade por classe
  for (i in 1:length(classes)) { # calcular as frequências necessárias
    P_classe_i = sum(dataset[,colId] == classes[i]) / nrow(dataset) # pega todo mundo e calcula a probabilidade a priori
    log10_P_classe_i = log10(P_classe_i) #aplica log pra virar uma soma
    sum_p = log10_P_classe_i #normalmente se usa log na base e, iniciando o a soma do 
    for (j in 1:length(query)) { 
      if (query[j] != "?") { #quando tem valor ausente, se n é ausente calcula
        P_aj_given_classe_i = sum(dataset[,j] == query[j] & dataset[,colId] == classes[i])/sum(dataset[,colId] == classes[i]) #calcular a probabilidade para cada um dos atributos, no denominador tem a soma de quantos tem com esse valor dessa classe 
        sum_p = sum_p + log10(P_aj_given_classe_i) 
        }
    }
    P[i] = 10^(sum_p)
  }
  ret = list() 
  ids = sort.list(classes) #ordena
  ret$classes = classes[ids] 
  ret$prob = P[ids] / sum(P[ids]) # probabilidade individual/ pela soma (normalização)
  ret$pred = ret$classes[which.max(ret$prob)] #qual retornou a maior probabilidade
  return (ret)
}

gauss <- function(mean, std,value){
  (1/(sqrt(2*pi*std^2)))*exp(-((value-mean)^2)/(2*std^2))
}
naiveGauss <- function(dataset, query) {
  colId = ncol(dataset)
  classes = unique(dataset[,colId])
  
  P = rep(0, length(classes))
  for (i in 1:length(classes)) {
    P_classe_i = sum(dataset[,colId] == classes[i]) / nrow(dataset)
    log10_P_classe_i = log10(P_classe_i)
    sum_p = log10_P_classe_i
    print(classes[])
    for (j in 1:length(query)) {
      if (query[j] != "?") {
        mean = mean(dataset[which(dataset[,colId] == classes[i]),j])
        std = sd(dataset[which(dataset[,colId] == classes[i]),j])
        P_aj_given_classe_i = gauss(mean,std,query[j])
        sum_p = sum_p + log10(P_aj_given_classe_i)
      }
    }
    P[i] = 10^(sum_p)
  }
  ret = list()
  ids = sort.list(classes)
  ret$classes = classes[ids]
  ret$prob = P[ids] / sum(P[ids])
  ret$pred = ret$classes[which.max(ret$prob)]
  return (ret)
}

# testing with a given query
model <- naive(balance,query=c(3,2,1,3))
model

# testing with a given query
model <- naiveGauss(balance,query=c(3,2,1,3))
model


######################################################################################################################
# Visualizing decision boundary
# Code from http://michael.hahsler.net/SMU/EMIS7332/R/viz_classifier.html
library(e1071)

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


# separating training and testing data, using only two predictive features
train.indices <- sample(1:nrow(iris), 100)
iris.train <- iris[train.indices, 3:5]
iris.test <- iris[-train.indices, 3:5]

model <- naiveBayes(Species ~ ., data=iris.train)
model

decisionplot(model, iris.train, class = "Species", main = "naive Bayes")

NB_Predictions=predict(model,iris.test)
#Confusion matrix to check accuracy
table(iris.test$Species,NB_Predictions)

# outputing the probabilities
NB_probs=predict(model,iris.test,type="raw")
NB_probs

#model <- naiveBayes(balance$V1 ~ ., data=balance[,3:5],laplace = 3)
#decisionplot(model, balance[,3:5], class = "V1", main = "naive Bayes")
