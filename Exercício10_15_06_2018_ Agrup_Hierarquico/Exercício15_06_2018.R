data.exer <- read.csv("E://Documentos//Mestrado//Machine_Learnig//Conj_dados_exercicio.csv", sep = ",")
data.exer$Id_cliente <- NULL
data.exe <- data.exer

#a) Codifique os atributos qualitativos como quantitativos. Lembre-se de n�o usar o
#atributo de classe, pois o algoritmo � n�o supervisionado.
data.exe$Tamanho_fam�lia = as.numeric(data.exe$Tamanho_fam�lia)
data.exe$Comprou_antes = as.numeric(data.exe$Comprou_antes)


#b) Normalize os atributos num�ricos
doNorm <- function(x) {(x - min(x))/(max(x)-min(x))}
data.exe.normalized <- as.data.frame(lapply(data.exe[,1:4], doNorm))
str(data.exe.normalized)
data.exe1 <- data.exe.normalized


#c) Use o algoritmo de agrupamento hier�rquico para agrupar os dados, variando o
#m�todo de liga��o entre single, average e complete.
#d) Fazendo um corte com dois grupos, avalie se os grupos gerados pelos diferentes
#m�todos possuem alguma interpreta��o, fazendo uso da classifica��o
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

#1) Maria tem 55 anos, um rendimento de 9500 reais e uma fam�lia pequena. Al�m disso, j� comprou outros produtos da empresa anteriormente.
#2) Jo�o � um jovem de 23 anos com rendimento de 900 reais e fam�lia pequena.

 
#Os grupos gerados pelos tr�s m�odos e dois cortes possuem a mesma acur�cia, com uma taxa de 75% de acerto ao predizer os cliente que n�o comprariam e
#taxa de 66,66% de acerto dos clientes com grande potencial para compra, logo para os quais a propaganda deveria ser enviada.
#Os dois cortes tiveram como base a vari�vel tamanho_fam�lia, logo Maria e Jo�o estaria classificados dentro do mesmo grupo, de acordo com as 
#vari�veis idade, rendimento e comprou_antes, Maria seria classificada no grupo juntamente com o n�mero 13, o indica que � uma potencial cliente
#para uma nova compra. Jo�o seria classificado no grupo onde se encontra o n�mero 9, se mostrando tamb�m um potencial cliente para uma nova compra.