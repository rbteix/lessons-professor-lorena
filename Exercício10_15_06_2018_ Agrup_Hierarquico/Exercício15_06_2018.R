data.exer <- read.csv("E://Documentos//Mestrado//Machine_Learnig//Conj_dados_exercicio.csv", sep = ",")
data.exer$Id_cliente <- NULL
data.exe <- data.exer

#a) Codifique os atributos qualitativos como quantitativos. Lembre-se de não usar o
#atributo de classe, pois o algoritmo é não supervisionado.
data.exe$Tamanho_família = as.numeric(data.exe$Tamanho_família)
data.exe$Comprou_antes = as.numeric(data.exe$Comprou_antes)


#b) Normalize os atributos numéricos
doNorm <- function(x) {(x - min(x))/(max(x)-min(x))}
data.exe.normalized <- as.data.frame(lapply(data.exe[,1:4], doNorm))
str(data.exe.normalized)
data.exe1 <- data.exe.normalized


#c) Use o algoritmo de agrupamento hierárquico para agrupar os dados, variando o
#método de ligação entre single, average e complete.
#d) Fazendo um corte com dois grupos, avalie se os grupos gerados pelos diferentes
#métodos possuem alguma interpretação, fazendo uso da classificação
#conhecida usada nas outras atividades.

clusters <- hclust(dist(data.exe1))
plot(clusters,hang=-1)
clusterCut <- cutree(clusters, 2)
rect.hclust(clusters, k=2, border="red")
table(data.exe$Comprou_anunciado,clusterCut )


clusters <- hclust(dist(data.exe1),method="single")
plot(clusters,hang=-1)
clusterCut <- cutree(clusters, 2)
rect.hclust(clusters, k=2, border="red")
table(data.exe$Comprou_anunciado,clusterCut )


clusters <- hclust(dist(data.exe1),method="average")
plot(clusters,hang=-1)
clusterCut <- cutree(clusters, 2)
rect.hclust(clusters, k=2, border="red")
table(data.exe$Comprou_anunciado,clusterCut )

#1) Maria tem 55 anos, um rendimento de 9500 reais e uma família pequena. Além disso, já comprou outros produtos da empresa anteriormente.
#2) João é um jovem de 23 anos com rendimento de 900 reais e família pequena.

 
#Os grupos gerados pelos três méodos e dois cortes possuem a mesma acurácia, com uma taxa de 75% de acerto ao predizer os cliente que não comprariam e
#taxa de 66,66% de acerto dos clientes com grande potencial para compra, logo para os quais a propaganda deveria ser enviada.
#Os dois cortes tiveram como base a variável tamanho_família, logo Maria e João estaria classificados dentro do mesmo grupo, de acordo com as 
#variáveis idade, rendimento e comprou_antes, Maria seria classificada no grupo juntamente com o número 13, o indica que é uma potencial cliente
#para uma nova compra. João seria classificado no grupo onde se encontra o número 9, se mostrando também um potencial cliente para uma nova compra.