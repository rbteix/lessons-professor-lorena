# Neural Networks

###########################################################################
# Perceptron
# Adapted from: ML4U (youtube channel)
# and https://bitbucket.org/rodrigo_mello/ml4u/src/ab2300b432ac9105df1352d5890543cf136bef8f/perceptron/?at=master

# threshold activation function # cria limiar de ativação
f_threshold <- function(u) { # U somatório de todas entradas, multiplicadas pelo peso
  if (u >= 0)
    return (1)
  return (0)
}

# Training a perceptron
perceptron.train <- function(dataset, eta=0.1, epsilon=1e-2) {
  
  classId = ncol(dataset)
  X = dataset[,1:classId-1]
  Y = dataset[,classId]
  
  # initializing weights; rnif =  valores aleatórios de -0.5 a 0.5 (pesos)
  weights = runif(min=-0.5, max=0.5, n=ncol(dataset))
  
  ncycle = 0
  squaredError = 2*epsilon
  
  while (squaredError > epsilon) {
    squaredError = 0
    
    for (i in 1:nrow(X)) {
      example = c(1,as.numeric(X[i,]))
      class = Y[i]
      
      cat("example = ", as.numeric(X[i,]), "\n") #concatenação, nesse caso criando uma string
      cat("class = ", class, "\n")
      
      # computing predicted y
      u = example %*% weights  #%*% multiplicação entre matrizes
      
      cat("u = ", u, "\n")
      
      y = f_threshold(u)
      
      cat("predicted = ", y, "\n")
      
      # Error
      Error = class - y
      squaredError = squaredError + Error^2 # calcula erro da linha e armazena
  
      if(abs(Error) > 0){
         # update weights
          cat("updating weights...\n")
          delta = eta * Error * example   # gradiente da Rede      
          weights = weights + delta
      }
          
    }
    
    squaredError = squaredError / nrow(X) #média
    cat("Squared error = ", squaredError, "\n")
    ncycle = ncycle + 1
  }
  
  ret = list()
  ret$weights = weights
  ret$ncycle = ncycle
  cat("Final weights = ", weights, "\n")
  cat("Number of cycles = ", ncycle, "\n")
  
  return (ret)
}

# predicting with a perceptron
perceptron.run <- function(X, model) {
  
  cat("#example\tprediction\n")
  for (i in 1:nrow(X)) {           # X é uma matriz, logo Jõao  e Maria devem se tornar uma matriz
    example = c(1,as.numeric(X[i,]))
    
    # u
    u = example %*% model$weights
    y = f_threshold(u)
    
    cat(as.numeric(X[i,]), "\t", y, "\n")
  }
}


# testing code with AND dataset
perceptron.test <- function(eta=0.1, threshold=1e-2) {
  # creating dataset
  bit1 <- c(0,0,1,1)
  bit2 <- c(0,1,0,1)
  and <- c(0,0,0,1)
  andData <- data.frame(bit1,bit2,and)
  
  
  
  cat("dataset:\n")
  print(andData)
  
  # training
  cat("\n\n>> Training:\n")
  model = perceptron.train(andData, eta, threshold)
  
  # testing
  cat("\n\n>> Testing:\n")
  perceptron.run(andData[,1:ncol(andData)-1], model)
  
  return(model)
  
}

# Exercices: 
# a) Run multiple times to see the differences
# b) Change the eta to 0.01 and 0.9 and observe the effects
# c) Lets observe the effect of data scale making andData$bit1 <- andData$bit1 * 1000
# d) now test the percepton with the XOR problem
# you can also change the epsilon value and observe what happens

# Ploting the hyperplane
perceptron.plot <- function(model){
  
  range = seq(0,1,length=100) # creating between 0 and 1, dividing into 100 intervals
  matrix = outer(range,range, function(x1,x2){
    cbind(1,x1,x2) %*% model$weights 
  })
  ids = which(matrix>=0)
  matrix[ids] = 1
  matrix[-ids] = 0
  filled.contour(range,range,matrix)  
}

# Exercice: run the perceptron.test and perceptron.plot multiple times and see how the hyperplane changes

##################################################################################################
# Adaline

# Training an adaline
adaline.train <- function(dataset, eta=0.1, epsilon=1e-2, niter = 50) {
  
  classId = ncol(dataset)
  X = as.data.frame(dataset[,1:classId-1])
  Y = dataset[,classId]
  
  # initializing weights
  weights = runif(min=-0.5, max=0.5, n=ncol(dataset))
  
  ncycle = 0
  squaredError = 2*epsilon
  normalization = sum((Y - mean(Y))^2)
    
  while ((squaredError > epsilon)&&(ncycle < niter)) {
    squaredError = 0
    
    for (i in 1:nrow(X)) {
      example = c(1,as.numeric(X[i,]))
      label = Y[i]
      
      #cat("example = ", as.numeric(X[i,]), "\n")
      #cat("label = ", label, "\n")
      
      # computing predicted y
      u = example %*% weights
      
      #cat("u = ", u, "\n")
      
      y = u # linear activation function
      
      #cat("predicted = ", y, "\n")
      
      # Error
      Error = label - y
      squaredError = (squaredError + Error^2) 
      
      if(abs(Error) > epsilon){
        # update weights
        #cat("updating weights...\n")
        delta = eta * Error * example        
        weights = weights + delta
      }
      
    }
    
    squaredError = squaredError / (nrow(X) * normalization)
    cat("Squared error = ", squaredError, "\n")
    ncycle = ncycle + 1
  }
  
  ret = list()
  ret$weights = weights
  ret$ncycle = ncycle
  ret$error = squaredError
  cat("Final weights = ", weights, "\n")
  cat("Number of cycles = ", ncycle, "\n")
  
  return (ret)
}

# predicting with an adaline
adaline.run <- function(X, model) {
  
  cat("#example\tprediction\n")
  for (i in 1:nrow(X)) {
    example = c(1,as.numeric(X[i,]))
    
    # u
    u = example %*% model$weights
    y = u # linear activation function
    
    cat(as.numeric(X[i,]), "\t", y, "\n")
  }
}

# testing code 
adaline.test <- function(eta=0.1, threshold=1e-3,niter=100) {
  
  # generating an artificial dataset
  # from https://pt.coursera.org/learn/r-programming/lecture/u7in9/simulation-simulating-a-linear-model
  set.seed(20)
  x <- rnorm(100)
  e <- rnorm(100,0,2)
  y <- 0.5+2*x+e
  
  plot(x,y)
  data <- cbind(x,y)
  data <- as.data.frame(data)
  model <- adaline.train(data,eta,threshold,niter)
  abline(model$weights[1],model$weights[2])
}

# Exercice: see what happens for datasets with:
# a) noise with standard deviation of 1
# b) no noise 



################################################################################################
# Some packages: nnet, neuralnet, RSNNS, caret, RWeka
