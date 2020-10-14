# MLP
# Code from: https://bitbucket.org/rodrigo_mello/ml4u/src/ab2300b432ac9105df1352d5890543cf136bef8f/multilayer-perceptron/mlp.r?at=master&fileviewer=file-view-default

require(nnet) #conversão da classe
require(tseriesChaos) # converter datafreme para vetor

f <- function(net) { #soma ponderada
  return ( 1/(1+exp(-net)) ) #derivada da sigmoide
}

df_dnet <- function(net) { # para fazer o gráfico
  return ( f(net) * (1 - f(net)) )
}

fhard <- function(net) {
  r = net
  r[net > 0] = 1
  r[net <= 0] = 0
  return (r)
}

mlp.architecture <- function(input.length = 2, #entrada
                             hidden.length = 2, #camadas
                             output.length = 1, #saida
                             my.f = f,
                             my.df_dnet = df_dnet) {
  
  layers = list()
  layers$hidden = matrix(runif(min=-0.5, max=0.5, 
                               n=hidden.length*(input.length+1)), 
                         nrow=hidden.length, ncol=input.length+1)
  
  layers$output = matrix(runif(min=-0.5, max=0.5, 
                               n=output.length*(hidden.length+1)), 
                         nrow=output.length, ncol=hidden.length+1)
  
  model = list()
  model$layers = layers
  model$f = my.f
  model$df_dnet = my.df_dnet
  
  return (model)
}

mlp.forward <- function(model, x_p) {
  # x = c(1, 0)
  
  f_h_net_h_pj = rep(0, nrow(model$layers$hidden))
  df_h_dnet_h_pj = rep(0, nrow(model$layers$hidden))
  for (j in 1:nrow(model$layers$hidden)) {
    net_h_pj = c(x_p, 1) %*% model$layers$hidden[j,]
    f_h_net_h_pj[j] = model$f(net_h_pj)
    df_h_dnet_h_pj[j] = model$df_dnet(net_h_pj)
  }
  
  f_o_net_o_pk = rep(0, nrow(model$layers$output))
  df_o_dnet_o_pk = rep(0, nrow(model$layers$output))
  for (k in 1:nrow(model$layers$output)) {
    net_o_pk = c(f_h_net_h_pj, 1) %*% model$layers$output[k,]
    f_o_net_o_pk[k] = model$f(net_o_pk)
    df_o_dnet_o_pk[k] = model$df_dnet(net_o_pk)
  }
  
  fwd = list()
  fwd$f_h_net_h_pj = f_h_net_h_pj
  fwd$f_o_net_o_pk = f_o_net_o_pk
  fwd$df_h_dnet_h_pj = df_h_dnet_h_pj
  fwd$df_o_dnet_o_pk = df_o_dnet_o_pk
  
  return (fwd)
}

mlp.backpropagation <- function(X, Y, model, eta=0.1, threshold=1e-2,msg = TRUE,ncycle=2000) {
  
  sqerror = 2 * threshold
  n = 0
  while ((sqerror > threshold)&&(n<ncycle)) {
    sqerror = 0
    
    # Treinando com cada exemplo de meu conjunto X dadas as classes em Y
    for (p in 1:nrow(X)) {
      x_p = X[p,]
      y_p = Y[p,]
      
      fwd = mlp.forward(model, x_p)
      o_p = fwd$f_o_net_o_pk
      
      delta_p = y_p - o_p
      
      # Calculando erro quadrático
      sqerror = sqerror + sum(delta_p^2)
      
      # Calculando delta da camada de saída para um padrão
      delta_o_p = delta_p * fwd$df_o_dnet_o_pk #derivada *erro
      
      # Calculando delta da camada escondida para um padrão
      w.length = ncol(model$layers$output)-1 
      delta_h_p = fwd$df_h_dnet_h_pj * 
        (delta_o_p %*% model$layers$output[,1:w.length])
      
      # Atualizando a camada de saída
      model$layers$output = model$layers$output + 
        eta * (as.vector(delta_o_p) %*% t(c(as.vector(fwd$f_h_net_h_pj), 1)))
      
      # Atualizando a camada escondida
      model$layers$hidden = model$layers$hidden +
        eta * (as.vector(delta_h_p) %*% t(c(x_p, 1)))
      
    }
    
    sqerror = sqerror / nrow(X)
    if(msg){
      cat("Average squared error: ", sqerror, "\n")
    }
    n = n+1
  }
  
  return (model)
}

xor.test <- function(eta=0.1, threshold=1e-2, plot=T) {
  
  # creating dataset
  bit1 <- c(0,0,1,1)
  bit2 <- c(0,1,0,1)
  xor <- c(0,1,1,0)
  dataset <- data.frame(bit1,bit2,xor)
  
  #dataset = read.table("xor.dat", header=T)
  X = matrix(ts(dataset[,1:2]), nrow=4, ncol=2)
  Y = matrix(dataset[,3], ncol=1)
  
  cat("Inputs...\n")
  print(X)
  
  
  cat("Expected outputs...\n")
  print(Y)
  
  cat("Press ENTER to continue...\n")
  readline();
  
  model = mlp.architecture(2, 2, 1)
  trained.model = mlp.backpropagation(X, Y, model, eta, threshold,msg = TRUE)
  
  for (p in 1:nrow(X)) {
    x_p = X[p,]
    y_p = Y[p,]
    
    fwd = mlp.forward(trained.model, x_p)
    
    print(x_p)
    print(y_p)
    print(fwd$f_o_net_o_pk)
    
    #readline()
  }
  
  if (plot == TRUE) {
    xor.plot(trained.model)
  }
  
  return (trained.model)
}

xor.plot <- function(model) {
  A = seq(0, 1, length=100)
  net1 = outer(A, A, function(x, y) { 
    cbind(x, y, 1)%*%model$layers$hidden[1,] })
  hyperplane1 = fhard(net1)
  net2 = outer(A, A, function(x, y) { 
    cbind(x, y, 1)%*%model$layers$hidden[2,] })
  hyperplane2 = fhard(net2)
  
  res = hyperplane1 + hyperplane2
  filled.contour(res)
}

iris.test <- function(hidden.length = 2, eta=0.1, 
                      train.size=0.75, threshold=1e-2) {
  
  dataset = iris
  features = dataset[,1:4]
  
  for (i in 1:ncol(features)) { # normalization
    features[, i] = (features[,i] - min(features[,i])) / (max(features[,i]) - min(features[,i]))
  }
  
  classes = class.ind(dataset[,5])# one-to-n encoding of the class
  dataset = cbind(features, classes)
  
  train.size = ceiling(nrow(dataset) * train.size)
  train.id = sample(1:nrow(dataset), size=train.size)
  
  train.set = dataset[train.id,]
  test.set = dataset[-train.id,]
  
  X = matrix(ts(train.set[,1:4]), ncol=4)
  Y = matrix(ts(train.set[,5:7]), ncol=3)
  
  X.test = matrix(ts(test.set[,1:4]), ncol=4)
  Y.test = matrix(ts(test.set[,5:7]), ncol=3)
  
  model = mlp.architecture(4, hidden.length, 3)
  trained.model = mlp.backpropagation(X, Y, model, eta, threshold,msg = TRUE)
  
  res = NULL
  for (p in 1:nrow(X.test)) {
    x_p = X.test[p,]
    y_p = Y.test[p,]
    
    fwd = mlp.forward(trained.model, x_p)
    res = rbind(res, c(y_p, fwd$f_o_net_o_pk))
  }
  
  colnames(res) = c("E1", "E2", "E3", "O1", "O2", "O3")
  
  return (res)
}

# Effect of number of hidden units
hyper.test <- function(eta = 0.5,threshold=1e-2){
# From  https://www.youtube.com/watch?v=Vc405nhRafQ
  
  dataset = cbind(rnorm(mean=0,sd=1,n=100),rnorm(mean=0,sd=1,n=100),0)
  dataset = rbind(dataset,cbind(rnorm(mean=2,sd=1,n=100),rnorm(mean=2,sd=1,n=100),1))
  dataset = rbind(dataset,cbind(rnorm(mean=10,sd=1,n=100),rnorm(mean=10,sd=1,n=100),0))
  
  plot(dataset[,1:2],col=dataset[,3]+1)
  
  train.id = sample(1:nrow(dataset), size=150)
  
  train.set = dataset[train.id,]
  test.set = dataset[-train.id,]

  X = matrix(ts(train.set[,1:2]), ncol=2)
  Y = matrix(ts(train.set[,3]), ncol=1)
  
  X.test = matrix(ts(test.set[,1:2]), ncol=2)
  Y.test = matrix(ts(test.set[,3]), ncol=1)
  
  for(hyp in 1:5){
    model = mlp.architecture(2, hidden.length=hyp, 1)  
    trained.model = mlp.backpropagation(X, Y, model, eta, threshold,msg = FALSE)  
    
    wrong=0
    for (p in 1:nrow(X.test)) {
      x_p = X.test[p,]
      y_p = Y.test[p,]
      
      fwd = mlp.forward(trained.model, x_p)
      res = round(fwd$f_o_net_o_pk)
      if(res != y_p){
        wrong = wrong+1
      }
    }
    error = wrong/nrow(test.set)
    cat("neurons = ",hyp,"\t error = ",error,"\n")
  }  

}
