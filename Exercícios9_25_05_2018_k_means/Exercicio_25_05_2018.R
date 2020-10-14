
# Importando dados
data.exer <- read.csv("E://Documentos//Mestrado//Machine_Learnig//Conj_dados_exercicio.csv", sep = ",")
data.exer$Id_cliente <- NULL
data.exe <- data.exer

#a) Codifique os atributos qualitativos como quantitativos. Lembre-se de não usar o
#atributo de classe, pois o algoritmo é não supervisionado.
data.exe$Tamanho_família = as.numeric(data.exe$Tamanho_família)
data.exe$Comprou_antes = as.numeric(data.exe$Comprou_antes)
data.exe$Comprou_anunciado = as.numeric(data.exe$Comprou_anunciado)

#b) Normalize os atributos numéricosdoNorm <- function(x) {(x - min(x))/(max(x)-min(x))}
doNorm <- function(x) {(x - min(x))/(max(x)-min(x))}
data.exe.normalized <- as.data.frame(lapply(data.exe[,1:4], doNorm))
str(data.exe.normalized)
data.exe1 <- data.exe.normalized

#c) Use o algoritmo k-médias para agrupar os dados, selecionando aleatoriamente
#dois pontos do conjunto como centros.

my.kmeans <- function(dataset, k=2, threshold=1e-3) {
  
  if (is.vector(dataset)) {
    dataset = as.data.frame(matrix(dataset, ncol=1))
  }
  
  # sampling initial centroids  
  ini = sample(1:nrow(dataset), size=k)
  centroid = as.data.frame(dataset[ini,])
  div = 2 * threshold
  
  while (div > threshold) {
    
    dists = NULL
    for (i in 1:k) { # For each centroid
      # computing euclidean distance from each point to the centroids
      euclidean = apply(dataset, 1, 
                        function(row) { # euclidean distance
                          sqrt(sum((row - centroid[i,])^2))})
      dists = cbind(dists, euclidean)
    }
    
    # Finding to which centroid each point is associated
    associated.centroids = apply(dists, 1, function(row) { which.min(row) } )
    
    # updating centroids
    div = 0
    for (i in 1:k) { # for each centroid
      ids = which(associated.centroids == i) # index of examples in centroid i
      new.position = colMeans(as.data.frame(dataset[ids,]))
      div = div + sqrt(sum((centroid[i,] - new.position)^2)) # computing within distances error
      centroid[i,] = new.position
    }
    div = div / k
    
    cat("Divergence: ", div, "\n")
  }
  
  
  ret = list()
  ret$k = k
  ret$initial = as.data.frame(dataset[ini,])
  ret$centroid = centroid
  ret$clusters = associated.centroids
  
  return (ret)
}


library(ggplot2)
data.exe1 <- as.data.frame(data.exe1)

result <- my.kmeans(data.exe1,k=2)


result1 <- cbind(data.exe1, result$clusters)
result1$Comprou_antes <- as.factor(result1$Comprou_antes)
result1$`result$clusters` <- as.factor(result1$`result$clusters`)

names(result1) <- c("Rendimento", "Idade", "Tamanho_família", "Comprou_antes", "Cluster")

ggplot(data = result1, aes(x = Rendimento, y = Idade, 
                                  size = Tamanho_família,
                                  shape = Comprou_antes,
                                  color = Cluster)) +scale_color_manual(values = c("red", "blue", "green", "yellow")) +geom_point() 


#Cluster 1, cor vermelha: Possuem famílias pequenas, idades  e rendimento variado,  há clientes que compraram antes e que não compraram antes.
#Cluster2, cor azul: Possuem famílias grandes, com idades e rendimento variado,  há clientes que compraram antes e também que não compraram.
#Neste caso não é possível identificar quem seriam os melhores clientes a indicar a propaganda para uma nova compra, talvez utilizar um algoritmo
#para calcular a quantidade ideal de clusters traria resultados mais claros.



