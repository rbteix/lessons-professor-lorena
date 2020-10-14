# Decision trees

##################################################################################
# Classification and regression trees (CART)

library(rpart)

# separating training and test sets
train.indices <- sample(1:nrow(iris), 100)
iris.train <- iris[train.indices, ]
iris.test <- iris[-train.indices, ]

fit <- rpart(Species ~ ., data=iris.train, method="class") # treinando a árvore para prever a espécie, (~) especifica as predições, (.) inclui todos os atributos preditivos ou os melhores 
par(xpd = T) 
windows()
plot(fit,compress=TRUE,uniform=TRUE, main="Classification Tree for Iris")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#Teste de pacote de gráficos

library(rpart.plot)
windows()
rpart.plot(fit, type = 2, digits = 3, fallen.leaves = F)
#__________________________________________________________________________

prediction <- predict(fit, iris.test, type = "class") #jogando o conjunto de teste, retorna classes
table(iris.test$Species,prediction) 

# probabilities
prob <- predict(object=fit, newdata=iris.test, type="prob") #probabilidade para cada uma das classes, baseada na folha que ele caiu
prob # pode utilizar essas probabilidades para gerar curva Roc

# looking at parameters 
?rpart.control

# Removing pruning, for instance
fit <- rpart(Species ~ ., data=iris.train, method="class",control = rpart.control(minbucket = 1,cp=0)) #cp= custo de complexidade, sem poda (0) nesse caso
par(xpd = TRUE)
windows()
plot(fit,compress=TRUE,uniform=TRUE, main="Classification Tree for Iris")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
prediction <- predict(fit, iris.test, type = "class")
table(iris.test$Species,prediction)

# for regression
iris_reg.train <- iris.train[-ncol(iris)] # removing last collumn
iris_reg.test <- iris.test[-ncol(iris)] # removing last collumn
fit <- rpart(Petal.Width ~ ., data=iris_reg.train) #prver o primeiro a partir dos atributos que restaram
print(fit)
par(xpd = TRUE)
windows()
plot(fit,compress=TRUE,uniform=TRUE, main="Classification Tree for Iris_regression")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
p <- predict(object=fit, newdata=iris_reg.test) #previsão do teste de regressão
# NMSE
sum((p-iris_reg.test$Petal.Width)^2)/sum((iris_reg.test$Petal.Width-mean(iris_reg.test$Petal.Width))^2)


##################################################################################
# C5.0
# From https://rstudio-pubs-static.s3.amazonaws.com/195428_16074a4e980747c4bc05af6c0bb305a9.html

install.packages("C50")
install.packages("printr")
library(C50)
library(printr)

model <- C5.0(Species ~., data=iris.train,trials=1) #gerando modelo
summary(model)
plot(model)

results <- predict(object=model, newdata=iris.test, type="class")
table(iris.test$Species,results)

# probabilities
prob <- predict(object=model, newdata=iris.test, type="prob")
prob

# extracting rules
model <- C5.0(Species ~., data=iris.train,trials=1,rules=TRUE)
summary(model)
results <- predict(object=model, newdata=iris.test, type="class")
table(iris.test$Species,results)


# Other packages: caret, RWeka, ctree, party, tree