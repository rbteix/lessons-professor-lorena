#Exerc�cio de SVM

# Importando dados

data.exer <- read.csv("E://Documentos//Mestrado//Machine_Learnig//Conj_dados_exercicio.csv", sep = ",")
data.exer$Id_cliente <- NULL
data.exe <- data.exer

#b) Convertendo atributos qualitativos em quantitativos
data.exe$Tamanho_fam�lia = as.numeric(data.exe$Tamanho_fam�lia)
data.exe$Comprou_antes = as.numeric(data.exe$Comprou_antes)
data.exe$Comprou_anunciado = as.numeric(data.exe$Comprou_anunciado)

#c) Normalizando os atributos quantitativos 
doNorm <- function(x) {(x - min(x))/(max(x)-min(x))}
data.exe.normalized <- as.data.frame(lapply(data.exe, doNorm))
str(data.exe.normalized)
data.exe1 <- data.exe.normalized



#c) Treine uma SVM, com par�metros default (se o c�digo usado n�o tiver a normaliza��o, ela deve ser feita antes)
library("e1071")
model <- svm(Comprou_anunciado ~ ., data = data.exe1, kernel="sigmoid", cost = 1)
print(model)


#d) Em seguida, usando a SVM obtida, classifique os seguintes potenciais novos clientes com essa SVM:
#d.1) Maria tem 55 anos, um rendimento de 9500 reais e uma fam�lia pequena. Al�m disso, j� comprou outros produtos da empresa anteriormente.
#d.2) Jo�o � um jovem de 23 anos com rendimento de 900 reais e fam�lia pequena. Ele j� comprou produtos da empresa.

norm_query <- function(query_vector, data_set){ 
  norm_person <- rep(0, length(query_vector)) #Define um vetor nulo.
  for(each_dim in 1:length(query_vector)){
    
    norm_person[each_dim] <- (query_vector[each_dim] - min(data_set[,each_dim])) /(max(data_set[, each_dim]) - min(data_set[,each_dim]))
    
  }#Finishes loop  
  
  
  return(norm_person)
} #fun��o normaliza��o dos novos dados

norm_query.maria <- norm_query(c(9500, 55, 2,2), data.exe)
norm_query.joao <- norm_query(c(900,23,2,2), data.exe)

test <- rbind(norm_query.maria, norm_query.joao) # matriz dos novos dados
pred <- predict(model, test)
pred

#e) Altere o kernel para linear e refa�a o exerc�cio.
model1 <- svm(Comprou_anunciado ~ ., data = data.exe1, kernel="polynomial",cost = 1)
print(model)

pred <- predict(model1, test)
pred

#Ambos os modelos afirmam que os dois s�o clientes indicados para se enviar a propaganda, pois 
#comprariam os produtos anunciados.