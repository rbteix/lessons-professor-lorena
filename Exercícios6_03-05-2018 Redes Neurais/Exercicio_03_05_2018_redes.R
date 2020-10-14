# Exercícios de Redes Neurais

# Importando dados
data.exer <- read.csv("E://Documentos//Mestrado//Machine_Learnig//Conj_dados_exercicio.csv", sep = ",")
data.exer$Id_cliente <- NULL # a) Removendo atributo de identificação
data.exe <- data.exer

#b) Convertendo atributos qualitativos em quantitativos
data.exe$Tamanho_família = as.numeric(data.exe$Tamanho_família)
data.exe$Comprou_antes = as.numeric(data.exe$Comprou_antes)
data.exe$Comprou_anunciado = as.numeric(data.exe$Comprou_anunciado)


#c) Normalizando os atributos quantitativos 
doNorm <- function(x) {(x - min(x))/(max(x)-min(x))}
data.exe.normalized <- as.data.frame(lapply(data.exe, doNorm))
str(data.exe.normalized)
data.exe1 <- data.exe.normalized

#d) Treine uma rede Perceptron com ?? = 0,3

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
} #modelo de 1 neuro só (uma camada)


# Treinando a rede, resultando em uma lista (matriz dos pesos, # de  ciclos), é o modelo com a função percepron
ml.percep <- perceptron.train(data.exe1, eta= 0.3)



norm_query <- function(query_vector, data_set){ 
  norm_person <- rep(0, length(query_vector)) #Define um vetor nulo.
  for(each_dim in 1:length(query_vector)){
    
    norm_person[each_dim] <- (query_vector[each_dim] - min(data_set[,each_dim])) /(max(data_set[, each_dim]) - min(data_set[,each_dim]))
    
  }#Finishes loop  
  
  
  return(norm_person)
} #função normalização dos novos dados

#d.1) Maria tem 55 anos, um rendimento de 9500 reais e uma família pequena. Além disso, já comprou outros produtos da empresa anteriormente. 

norm.maria <- norm_query(c(9500, 55, 1, 1), data.exe)
norm.joao <- norm_query(c(900, 23, 1, 1), data.exe)

test.matrix <- rbind(norm.maria, norm.joao) 


# testing code with AND dataset
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

perceptron.run(test.matrix, ml.percep)

#Maria teve como predição 1, o que indica que ela vai comprar
#Jõao teve como predição 0, o que indica que ele não vai comprar


#MLP

require(nnet) #conversão da classe
require(tseriesChaos) # converter datafreme para vetor

f <- function(net) { #soma ponderada
  return ( 1/(1+exp(-net)) ) #derivada da sigmoide
} #como fazer a ativação do neuron

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
                             output.length = 1, #saida (# de  neurons necessários para produzir a saída)
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

#Criar arquitetura da rede neural (# de neurons, # de camadas, # de saídas (output))

mlp_archi2 <- mlp.architecture(4, 2, 1) #chamado topologia

x.data.exe1 <- matrix(ts(data.exe1[,1:4]), ncol = 4)
y.data.exe1 <- matrix(ts(data.exe1[,5]), ncol= 1)
test.matrixts<- matrix(ts(test.matrix), ncol= ncol(test.matrix))
#treinamento
ml.back <- mlp.backpropagation(x.data.exe1, y.data.exe1, mlp_archi2)


mlp.output <- apply(test.matrixts, 1, mlp.forward, model=ml.back)

#Maria compra (dnet pk)
#João não compra

mlp.output[[1]][[2]]



#________________________________________________________________

mlp_archi5 <- mlp.architecture(4, 5, 1) #chamado topologia

#treinamento
ml.back5 <- mlp.backpropagation(x.data.exe1, y.data.exe1, mlp_archi5)


mlp.output5 <- apply(test.matrixts, 1, mlp.forward, model=ml.back5)
