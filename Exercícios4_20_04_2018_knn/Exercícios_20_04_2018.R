
#a) Importando dados e retirando coluna 1
data.exer <- read.csv("E://Documentos//Mestrado//Machine_Learnig//Conj_dados_exercicio.csv", sep = ",")
data.exer$Id_cliente <- NULL
data.exe <- data.exer
head(data.exe)
typeof(data.exe)
str(data.exe)


#b) Convertendo atributos qualitativos em quantitativos

data.exe$Tamanho_família = as.numeric(data.exe$Tamanho_família)
data.exe$Comprou_antes = as.numeric(data.exe$Comprou_antes)
data.exe$Comprou_anunciado = as.numeric(data.exe$Comprou_anunciado)


#c) Normalize os atributos quantitativos (os que forem cadeias de bits não precisam ser normalizados, só os que não estiverem entre 0 e 1).

doNorm <- function(x) {(x - min(x))/(max(x)-min(x))}
data.exe.normalized <- as.data.frame(lapply(data.exe, doNorm))
str(data.exe.normalized)
data.exe1 <- data.exe.normalized

#d) Usando esse conjunto de dados e a medida de distância Euclideana, classifique os seguintes potenciais novos clientes, usando um vizinho mais próximo:

knn <- function(dataset,query,k=1){
  
  idClass <- ncol(dataset) # # de colunas nos dados (representa a coluna resultado, a classe que deseja como resposta)
  
  Eucl_dist <- apply(dataset,1,function(row){ # 1 significa que para cada linha do dataset faça algo (chame uma função, que no caso vai devolver uma linha inteira do dataset), se for 2 é coluna
    sqrt(sum((query-as.numeric(row[1:idClass-1]))^2)) # consulta (query - essa linha do 1 até o idClass - 1 coluna, nesse caso menos a coluna species
  })                                                  #  faz o primeiro de query - o primeiro da row, o segundo de query - o segundo da row e me devolve os 2 valores em cima disse calula-se a distância Euclidiana
  ids <- sort.list(Eucl_dist,dec=F)[1:k]  # ordena essa distância (decrescente F= da menor distância para maior), ele devolve a posição da linha de forma ordenada. Vou selecionar as k(ocorrências) primeiras
  labels <- dataset[ids,idClass] # pega o id da linha desse conjunto e recorta a classe, ele devolve quais as classes que estão ocorrendo
  
  ret <- list() #cria uma lista
  ret$nearest <- ids #resposta retorna o mais próximo
  
  if(!is.numeric(dataset[,idClass])){ # resposta como character
    # classification problem
    U <- unique(labels) # devolve somente as ocorrências únicas, setiver um k=100 são 50 sim e 50 não, ele só devolve sim e não (nesse caso sao as species)
    R <- rep(0,length(U)) #vetor resposta que vai conter zeros e o comprimento do conjunto U, nesse caso são 3 possibilidades (setosa, versicolor e virginica)
    for (i in 1:length(U)){ # em cada uma das possibilidades que tenho no conj U (únicas que ocorrem),
      R[i] <- sum(U[i] == labels) # verifica quantas vezes ou pra quais exemplos essa classe ocorre e soma. A classe(labels) retorna como um vetor da última coluna dos dados. E por fim guardo a resposta R
    }
    idx <- which.max(R) # qual é o identificador máximo em R (resposta), vai me dar aquela possibilidade em que a contagem é maior
    
    ret$voted <- U # a lista retorna U
    ret$Nvotes <- R # a lista retorna R
    ret$pred <- U[idx] # essa é a classe de resposta U nessa posição  idx
  }
  else{
    ret$pred <- mean(labels)
  }
  
  return(ret) # retorna a lista
} #função knn

norm_query <- function(query_vector, data_set){ 
  norm_person <- rep(0, length(query_vector)) #Define um vetor nulo.
  for(each_dim in 1:length(query_vector)){
    
    norm_person[each_dim] <- (query_vector[each_dim] - min(data_set[,each_dim])) /(max(data_set[, each_dim]) - min(data_set[,each_dim]))
    
  }#Finishes loop  
  
  
  return(norm_person)
} #função normalização dos novos dados

#d.1) Maria tem 55 anos, um rendimento de 9500 reais e uma família pequena. Além disso, já comprou outros produtos da empresa anteriormente. 

norm.maria <- norm_query(c(9500, 55, 2,2), data.exe)

pred.maria <- knn(data.exe1, norm.maria, k= 1)
pred.maria 

#$nearest
#[1] 13

#$pred
#[1] 1
#Para k=1 O vizinho mais próximo é o da posição 13 e a predição é que ela compraria.

norm.joao <- norm_query(c(900, 23, 2,2), data.exe)

pred.joao <- knn(data.exe1, norm.joao, k= 1)
pred.joao

#$nearest
#[1] 9

#$pred
#[1] 1
#Para k=1 o vizinho mais próximo de João é o da posição 9 e a predição é que ele também compraria.


# d.2)Classifique os clientes anteriores usando três vizinhos mais próximos.

pred.maria <- knn(data.exe1, norm.maria, k= 3)
pred.maria 

#$nearest
#[1] 13 10  5

#$pred
#[1] 1
# Os vizinhos mais próximos de Maria são os das posições 13, 10, 5, todos com votos sim e a predição é que ela tbm compraria.


pred.joao <- knn(data.exe1, norm.joao, k= 3)
pred.joao

#$nearest
#[1]  9  5 10

#$pred
#[1] 1

# Os vizinhos mais próximos de João são os das posições 9, 5, 10, todos com votos sim e a predição é que ele tbm compraria.
