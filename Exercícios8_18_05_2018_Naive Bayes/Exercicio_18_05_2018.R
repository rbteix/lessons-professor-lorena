#Exerc�cio Naive Bayes

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


#b) Monte um modelo Naive Bayes para classificar os clientes, a partir dessa base de
#experi�ncia (lembre-se de desconsiderar o atributo ID).
model <- naiveBayes(Comprou_anunciado ~ ., data=data.exe)
model

#c) Em seguida, classifique os seguintes potenciais novos clientes para receber a propaganda:
# c.1) Maria tem 55 anos, um alto rendimento e uma fam�lia pequena. Al�m
#disso, j� comprou outros produtos da empresa anteriormente.
#c.2) Jo�o � um adolescente com rendimento baixo e fam�lia pequena. Ele j�comprou produtos da empresa.
maria <- c('Alto, C, Pequena, Sim')
joao <- c( "Baixo, A, Pequena, Sim")

#Teste
test <- rbind(maria, joao) 
NB_Predictions=predict(model,test)
NB_Predictions

# Como predi��o  o modelo afirma que ambos comprariam os produtos anunciados, logo s�o clientes com potencial para 
#receber as ofertas.
