#Importando dados

data.exer <- read.csv("E://Documentos//Mestrado//Machine_Learnig//Conj_dados_exercicio.csv", sep = ",")
data.exer$Id_cliente <- NULL
data.exe <- data.exer

#a) Discretize os atributos rendimento e idade da seguinte maneira:
#Idade: menor que 30, entre 30 e 40, maior que 40

id <- data.exe[,2]
Cat.id <- cut(id, breaks = c(18, 30, 40, 80), labels = c("A", "B", "C"), right = F)
data.exe$Idade <- Cat.id

#a) Rendimento: baixo (<= 2000), m�dio (entre 2000 e 7000), alto (>= 7000)
# Sem labels � poss�vel visualizar os intervalos

rd <- data.exe[,1]
Cat.rd <- cut(rd, breaks = c(500, 2000, 7000, 40000), labels = c("Baixo", "M�dio", "Alto"))
data.exe$Rendimento <- Cat.rd

#Checando se o problema est� desbalanceado
barplot(prop.table(table(data.exe$Comprou_anunciado)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Distribution")

#Balanceando os dados

library(unbalanced)


levels(data.exe$Comprou_anunciado)[levels(data.exe$Comprou_anunciado)=="Sim"] <-"0"
levels(data.exe$Comprou_anunciado)[levels(data.exe$Comprou_anunciado)=="N�o"] <-"1"
output <- data.exe$Comprou_anunciado
input <- data.exe[ ,-ncol(data.exe)]


balance <- function(input,output,typ){
  data<-ubBalance(X= input, Y=output, type=typ, verbose=TRUE)
  balancedData<-cbind(data$X,data$Y)
  names(balancedData)[5] <- "Comprou_anunciado"
  balancedData
}

balanced<-balance(input,output,"ubSMOTE")

barplot(prop.table(table(balanced$Comprou_anunciado)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Distribution")


#b) Monte a �rvore de decis�o para classificar os clientes, a partir dessa base de experi�ncia (lembre-se de desconsiderar o atributo ID). Plote a �rvore e verifique que atributos foram efetivamente usados.

install.packages("rpart.plot")
library(rpart.plot)
library(rpart)

#Dados de treinamento e teste

train.data <- sample(1:nrow(balanced), 27)
train.data.exe <- balanced[train.data,]
test.data.exe <- balanced[-train.data,]


table(train.data.exe$Comprou_anunciado)
prop.table(table(train.data.exe$Comprou_anunciado))

#Modelo e gr�fico (neste caso, acertou tudo em uma itera��o)

ml <- rpart(Comprou_anunciado ~ . , data= train.data.exe, method = "class", minsplit=2)
rpart.plot(ml, type = 4, digits = 2, fallen.leaves = F)

#Predi��o (avaliar o modelo)

pred <- predict(ml, test.data.exe, type= c("vector"))

table(test.data.exe$Comprou_anunciado, pred)

# c) Em seguida, classifique os seguintes potenciais novos clientes para receber a propaganda e explique o racioc�nio empregado pela �rvore nessas classifica��es:
#  c.1) Maria tem 55 anos, um altorendimento e uma fam�lia pequena. Al�m disso, j� comprou outros produtos da empresa anteriormente.
     #Maria tem rendimento alto, logo de acordo com a 

# c.2) Jo�o � um adolescente com rendimento baixo e fam�lia pequena. Ele j� comprou produtos da empresa.
    #Jo�o tem  fam�lia pequena e um baixo rendimento, logo n�o commpraria.