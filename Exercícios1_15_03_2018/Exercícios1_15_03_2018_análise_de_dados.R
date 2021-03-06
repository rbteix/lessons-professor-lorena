library(help="datasets")
View(iris)
View(swiss)


#Exerc�cio 1: 

#Renda mensal: Quantitativo Racional
#N�mero de palavras de um texto: Quantitativo Racional
#N�mero de matr�cula: Qualitativo Nominal
#Data de nascimento : Quantitativo Intervalar
#C�digo postal: Qualitativo Nominal
#Poai��o em uma corrida: Qualitativo Ordinal


#Exerc�cio 2:

#Dado o conjunto de dados {21, 2, 3, 4, 5, 80}, calcular:
#M�dia:
data <- c(1, 2, 3, 4, 5, 80)
data
mean(data)

#Mediana
median(data)

#M�dia Truncada, p = 33%
mean(data, trim = 0.33)


#Exerc�cio 3:

# Obter os quatro primeiros momentos centrais para os dados:
# 3.2, 11.7, 13.64, 15.6, 15.89, 28.44, 29.07

stats <- c(3.2, 11.7, 13.64, 15.6, 15.89, 28.44, 29.07)
stats

#momento1: 
sum(stats - mean(stats))/ (length(stats) -1) 

#momento2, vari�ncia (stats):
var(stats)

#momento3, obliquidade (stats):
library(e1071)
skewness(stats)

#momento4, curtose(sstas):
kurtosis(stats)

library(ggplot2)
hist(stats, main= "Histograma", xlab = "stats", ylab =  "Frequ�ncia", col= "grey" )
     