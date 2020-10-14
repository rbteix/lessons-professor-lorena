# Distance based predictive techniques

##################################################################################
# Distance and similarity measures
library(proxy) # proximity measures

# available measures
summary(pr_DB)

# Taking a sample of iris (for easing distance matrix visualization)
set.seed(1)
iris_sample <- iris[sample(nrow(iris),5),]
iris_sample
# Euclidean distance
dist(as.matrix(iris_sample[,1:4]),method="Minkowski",p=2)
dist(as.matrix(iris_sample[,1:4]),method="Euclidean")

Euclidean <- function(xi,xj){
  sqrt(sum((xi-xj)^2))
}
Euclidean(iris_sample[1,1:4],iris_sample[2,1:4])

# Exercice: generate a distance matrix for iris_sample using the Euclidean function

#####################################################################################################
# influence of scale
# modifying first attribute to be in a higher scale
iris_sample[,1] <- iris_sample[,1] * 1000
iris_sample

dist(as.matrix(iris_sample[,1:4]),method="Euclidean")

iris_sample[,1] <- iris_sample[,1] / 1000

# normalizing data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

iris_norm <- as.data.frame(lapply(iris_sample[1:4], normalize))
iris_norm
summary(iris_norm)

dist(as.matrix(iris_norm),method="Euclidean")

#####################################################################################################
# influence of noise
noise<-rnorm(5)  
iris_sample[,3] <- iris_sample[,3]+noise
iris_sample

iris_norm <- as.data.frame(lapply(iris_sample[1:4], normalize))
iris_norm
iris_sample[,3] <- iris_sample[,3]-noise

dist(as.matrix(iris_norm[,1:4]),method="Euclidean")

# Exercice: repeat previous lines with distances Manhattan and supremum

#####################################################################################################
# correlation

cor(iris_sample[,1:4])

#scale effect
iris_sample[,1] <- iris_sample[,1] * 1000
cor(iris_sample[,1:4])
iris_sample[,1] <- iris_sample[,1] / 1000

# influence of noise
noise<-rnorm(5)  
iris_sample[,3] <- iris_sample[,3]+noise
cor(iris_sample[,1:4])
iris_sample[,3] <- iris_sample[,3]-noise

#########################################################################################################
# showing that correlation captures form
library(ggplot2)
library(stats)

x1 <- c(1,5,2,7)
x2 <- c(14,19,15,22)

dat <- data.frame(c=0:3, x1, x2)
ggplot(dat) + geom_path(aes(x=c,y=x1),col=I("red")) + geom_path(aes(x=c,y=x2),col=I("blue"))

dat_norm <- as.data.frame(lapply(dat[2:3], normalize))
dat_norm

# comparing euclidean distance and correlation of the two vectors
Euclidean(dat_norm[,1],dat_norm[,2])
cor(x1,x2)
cor(dat_norm[,1],dat_norm[,2])

#########################################################################################################
# matching functions
x <- matrix(sample(c(FALSE, TRUE), 8, rep = TRUE), ncol = 2)
x
dist(x, method = "Jaccard")
dist(x, method = "simple matching")
dist(x, method = "Manhattan")

########################################################################################################
# Nearest neighbor
# From https://www.youtube.com/watch?v=XeW0e-S4kHo
# With some adaptations

knn <- function(dataset,query,k=1){
  
  idClass <- ncol(dataset) # # de colunas nos dados (representa a coluna resultado, a classe que deseja como resposta)
  
  Eucl_dist <- apply(dataset,1,function(row){ # 1 para cada linha do dataset faça algo (chame uma função, que no caso vai devolver uma linha inteira do dataset), se for 2 é coluna
    sqrt(sum((query-as.numeric(row[1:idClass-1]))^2)) # consulta (query - essa linha do 1 até o idClass - 1 coluna, nesse caso menos a coluna species
  })                                                  #  faz o primeiro de query - o primeiro da row, o segundo de query - o segundo da row e me devolve os 2 valores em cima disse calula-se a distância Euclidiana
  ids <- sort.list(Eucl_dist,dec=F)[1:k]  # ordena essa distância (decrescente F= da menor distância para maior), ele devolve a posição da linha de forma ordenada. Vou selecionar as k(ocorrências) primeiras
  labels <- dataset[ids,idClass] # pega o id da linha desse conjunto e recorta a classe, ele devolve quais as classes que estão ocorrendo
  
  ret <- list() #cria uma lista
  ret$nearest <- ids
  
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
}

# testing classification
knn(iris_sample,c(5.1,3.4,1.5,0.2),k=1)

# Exercice: modify number of nearest neighbors and see what happens

# testing regression
iris_reg <- iris_sample[-ncol(iris_sample)] # removing last collumn
knn(iris_reg,c(5.1,3.4,1.5),k=1)

# Exercice: modify number of nearest neighbors and see what happens

# some libraries that also have implementations: class, RWeka, caret, DMwR

#######################################################################
# DWNN using Gaussian weighting for regression
# From: https://bitbucket.org/rodrigo_mello/ml4u/src/ab2300b432ac9105df1352d5890543cf136bef8f/dwnn/dwnn.r?at=master&fileviewer=file-view-default

dwnn <- function(dataset, query, sigma=0.5) {
  
  classId = ncol(dataset)
  
  w = apply(dataset,1,function(row){
    eucl <- sqrt(sum((query-row[1:(classId-1)])^2))
    exp(-eucl^2 / (2*sigma^2))
  })
  Y = dataset[,classId]
  pred = sum(w*Y)/ sum(w)
  
  ret <- list()
  ret$weigths <- w
  ret$pred <- pred
  
  return (ret)
}

# testing in modified iris for regression
dwnn(iris_reg,c(5.1,3.4,1.5),sigma = 0.5)

# Exercice: change sigma value and observe


