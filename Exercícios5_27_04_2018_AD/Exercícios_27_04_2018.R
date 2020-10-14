#Importando dados

data.exer <- read.csv("E://Documentos//Mestrado//Machine_Learnig//Conj_dados_exercicio.csv", sep = ",")
data.exer$Id_cliente <- NULL
data.exe <- data.exer

#a) Discretize os atributos rendimento e idade da seguinte maneira:
#Idade: menor que 30, entre 30 e 40, maior que 40

id <- data.exe[,2]
Cat.id <- cut(id, breaks = c(18, 30, 40, 80), labels = c("A", "B", "C"), right = F)
data.exe$Idade <- Cat.id

#a) Rendimento: baixo (<= 2000), médio (entre 2000 e 7000), alto (>= 7000)
# Sem labels é possível visualizar os intervalos

rd <- data.exe[,1]
Cat.rd <- cut(rd, breaks = c(500, 2000, 7000, 40000), labels = c("Baixo", "Médio", "Alto"))
data.exe$Rendimento <- Cat.rd

#Checando se o problema está desbalanceado
barplot(prop.table(table(data.exe$Comprou_anunciado)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Distribution")

#Balanceando os dados

library(unbalanced)


levels(data.exe$Comprou_anunciado)[levels(data.exe$Comprou_anunciado)=="Sim"] <-"0"
levels(data.exe$Comprou_anunciado)[levels(data.exe$Comprou_anunciado)=="Não"] <-"1"
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


#b) Monte a árvore de decisão para classificar os clientes, a partir dessa base de experiência (lembre-se de desconsiderar o atributo ID). Plote a árvore e verifique que atributos foram efetivamente usados.

install.packages("rpart.plot")
library(rpart.plot)
library(rpart)

#Dados de treinamento e teste

train.data <- sample(1:nrow(balanced), 27)
train.data.exe <- balanced[train.data,]
test.data.exe <- balanced[-train.data,]


table(train.data.exe$Comprou_anunciado)
prop.table(table(train.data.exe$Comprou_anunciado))

#Modelo e gráfico (neste caso, acertou tudo em uma iteração)

ml <- rpart(Comprou_anunciado ~ . , data= train.data.exe, method = "class", minsplit=2)
rpart.plot(ml, type = 4, digits = 2, fallen.leaves = F)

#Predição (avaliar o modelo)

pred <- predict(ml, test.data.exe, type= c("vector"))

table(test.data.exe$Comprou_anunciado, pred)

# c) Em seguida, classifique os seguintes potenciais novos clientes para receber a propaganda e explique o raciocínio empregado pela árvore nessas classificações:
#  c.1) Maria tem 55 anos, um altorendimento e uma família pequena. Além disso, já comprou outros produtos da empresa anteriormente.
     #Maria tem rendimento alto, logo de acordo com a 

# c.2) João é um adolescente com rendimento baixo e família pequena. Ele já comprou produtos da empresa.
    #João tem  família pequena e um baixo rendimento, logo não commpraria.