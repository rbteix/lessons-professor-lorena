library(help="datasets")
View(iris)
View(swiss)


#Exercício 1: 

#Renda mensal: Quantitativo Racional
#Número de palavras de um texto: Quantitativo Racional
#Número de matrícula: Qualitativo Nominal
#Data de nascimento : Quantitativo Intervalar
#Código postal: Qualitativo Nominal
#Poaição em uma corrida: Qualitativo Ordinal


#Exercício 2:

#Dado o conjunto de dados {21, 2, 3, 4, 5, 80}, calcular:
#Média:
data <- c(1, 2, 3, 4, 5, 80)
data
mean(data)

#Mediana
median(data)

#Média Truncada, p = 33%
mean(data, trim = 0.33)


#Exercício 3:

# Obter os quatro primeiros momentos centrais para os dados:
# 3.2, 11.7, 13.64, 15.6, 15.89, 28.44, 29.07

stats <- c(3.2, 11.7, 13.64, 15.6, 15.89, 28.44, 29.07)
stats

#momento1: 
sum(stats - mean(stats))/ (length(stats) -1) 

#momento2, variância (stats):
var(stats)

#momento3, obliquidade (stats):
library(e1071)
skewness(stats)

#momento4, curtose(sstas):
kurtosis(stats)

library(ggplot2)
hist(stats, main= "Histograma", xlab = "stats", ylab =  "Frequência", col= "grey" )
     