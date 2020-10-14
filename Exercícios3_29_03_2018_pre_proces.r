# Data Pre-processing using Iris dataset (R)
# The script includes:
# Sampling (without replacement)
# Making part of iris unbalanced and balancing (with undersampling and SMOTE)
# Introducing missing values and treating them (replacing by average values)
# Noise filtering
# Data discretization
# Normalization and standardization
# PCA analysis
# Feature selection (filter, embedded, wrapper)

############################################################################
# Sampling

# First lets take a look at the data histogram before sampling
library(ggplot2)
library(gridExtra)
library(grid)

# Function for iris histogram
myhist <- function(d){
    # Sepal length 
    HisSl <- ggplot(data=d, aes(x=Sepal.Length))+
    geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
    xlab("Sepal Length (cm)") +  
    ylab("Frequency") + 
    theme(legend.position="none")+
    ggtitle("Histogram of Sepal Length")+
    geom_vline(data=d, aes(xintercept = mean(Sepal.Length)),linetype="dashed",color="grey")

    # Sepal width
    HistSw <- ggplot(data=d, aes(x=Sepal.Width)) +
    geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
    xlab("Sepal Width (cm)") +  
    ylab("Frequency") + 
    theme(legend.position="none")+
    ggtitle("Histogram of Sepal Width")+
    geom_vline(data=d, aes(xintercept = mean(Sepal.Width)),linetype="dashed",color="grey")

    # Petal length  
    HistPl <- ggplot(data=d, aes(x=Petal.Length))+
    geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
    xlab("Petal Length (cm)") +  
    ylab("Frequency") + 
    theme(legend.position="none")+
    ggtitle("Histogram of Petal Length")+
    geom_vline(data=d, aes(xintercept = mean(Petal.Length)),
             linetype="dashed",color="grey")

    # Petal width
    HistPw <- ggplot(data=d, aes(x=Petal.Width))+
    geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
    xlab("Petal Width (cm)") +  
    ylab("Frequency") + 
    theme(legend.position="right" )+
    ggtitle("Histogram of Petal Width")+
    geom_vline(data=d, aes(xintercept = mean(Petal.Width)),linetype="dashed",color="grey")

    # Plot all visualizations
    grid.arrange(HisSl + ggtitle(""),
             HistSw + ggtitle(""),
             HistPl + ggtitle(""),
             HistPw  + ggtitle(""),
             nrow = 2,
             top = textGrob("Iris Frequency Histogram", 
                            gp=gpar(fontsize=15))
    )
    
}

# histogram for the original dataset
myhist(iris)
# some statistics
summary(iris)

# Now lets sample 10% of the dataset, randomly
iris2 <- iris[sample(nrow(iris),0.1*nrow(iris)),]

# Lets see the histogram of the sampled data
myhist(iris2)
# some statistics
summary(iris2)

# Exercise1: modify sampling rate to 0.5 and 0.7 and see what happens to the histograms and summary for iris2
iris2 <- iris[sample(nrow(iris),0.5*nrow(iris)),]
myhist(iris2)
summary(iris2)
# Exercise2: now sample with replacement

########################################################################################################
# Unbalanced data
library(ggplot2)

myplot <- function(d){
  ggplot(data=d, aes(x = Petal.Length, y = Petal.Width)) +
    geom_point(aes(color=Species, shape=Species)) +
    xlab("Petal Length") + 
    ylab("Petal Width") +
    ggtitle("Petal Length vs Width") 
}

# Part of iris data, two features just for easing plots, and two classes
irisPart <- subset(iris, select = Petal.Length:Species)
irisPart <- subset(irisPart,Species != "setosa")
irisPart <- droplevels(irisPart)
# Plotting
myplot(irisPart)

set.seed(2);
# making unbalanced version
irisUmb <- rbind(irisPart[ sample( which( irisPart$Species == "versicolor" ) , 10 ) , ],irisPart[which(irisPart$Species == "virginica" ),])
# Plotting
myplot(irisUmb)

# Lets see what happens to unbalanced data
library(class)

# 1nn with irisPart
knn1 <- knn.cv(irisPart[,1:2], irisPart$Species, k = 1)
table(knn1, irisPart$Species)

# 1nn with iris3
knn2 <- knn.cv(irisUmb[,1:2], irisUmb$Species, k = 1)
table(knn2, irisUmb$Species)

#Tabela da confusão
#Verdadeiro está na coluna e o predito na linha
# VP  FP
# FN  VN
# some functions for balancing data

library(unbalanced)

# for using the functions, the class must be a binary factor where the majority class is coded as 0 and the minority as 1
#
levels(irisUmb$Species)[levels(irisUmb$Species)=="virginica"] <- "0"
levels(irisUmb$Species)[levels(irisUmb$Species)=="versicolor"] <- "1"
output<-irisUmb$Species
input<-irisUmb[ ,-ncol(irisUmb)]

balance <- function(input,output,typ){
  data<-ubBalance(X= input, Y=output, type=typ, verbose=TRUE)
  balancedData<-cbind(data$X,data$Y)
  names(balancedData)[3] <- "Species"
  balancedData
}

#balance the dataset with undersampling
balanced1<-balance(input,output,"ubUnder")
dim(balanced1)
myplot(balanced1)

# balance dataset with SMOTE (creates new points in the minority class)
balanced2<-balance(input,output,"ubSMOTE")
dim(balanced2)
myplot(balanced2)

# Exercice: how does knn behaves with the pre-processed datasets?


##############################################################################
# Missing values

# Taking irisPart, which has reduced numbers of features and classes for better visualization
irisM <- irisPart

# taking iris2 and replacing **two** data values by unknown
inds <- sample(1:100, 2, replace=F)
irisM[inds,]

irisM$Petal.Length[inds] <- NA
View(irisM)

# replacing values by average
irisM$Petal.Length[is.na(irisM$Petal.Length)] <- mean(irisM$Petal.Length, na.rm = TRUE)
irisM[inds,]

# Exercise: what is the difference if the average is taken only for elements from the same class as the element? Which R command can do that?

##################################################################################################
# Noise filtering
library(NoiseFiltersR)
out <- ENN(Species~., data = irisPart, k = 3)
summary(out)

# highlighted points are those removed by ENN
ggplot(data=irisPart, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color=Species, shape=Species)) +
  xlab("Petal Length") + 
  ylab("Petal Width") +
  ggtitle("Petal Length vs Width") +
  geom_point(data=irisPart[out$remIdx, ], aes(x = Petal.Length, y = Petal.Width), colour="black", size=5)

# Exercice1: change the number of neighbors in ENN and see what happens
# Exercice2: how does knn behave with the pre-processed dataset?

#################################################################################################
# Data discretization
# Data Trabsformation

# one-to-c encoding
library(dummies)
df <- dummy(iris$Species)
View(df)

#Discretization

# data discretization of a particular feature (Petal Width)
x <- iris[,4]
hist(x, breaks=20, main="Equal Interval length") # breaking into 20 beans

# Exercice: choose another number of beans and compare the results

# Other discretization method (based on Chi-squared statistical test)
library(discretization)
#---cut-points
disc <- chi2(iris,0.5,0.05)
disc$cutp
#--discretized dataset using Chi2 algorithm
View(disc$Disc.data)

# Normalization and Standardization

# Removal of the class
iris2 = iris2[-ncol(iris)]

# normalized
doNorm <- function(x) {(x - min(x))/(max(x)-min(x))}
iris.normalized <- as.data.frame(lapply(iris2, doNorm))
View(iris.normalized)
summary(iris.normalized)

# standardize 
iris.scaled <- scale(iris2)
View(iris.scaled)
summary(iris.scaled)

# Exercice: plot histograms of the normalized and scaled datasets (using the class information, that must be reappended to the data frame)

######################################################################################################
# Dimensionality reduction

# aggregation
# PCA
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,
                 center = TRUE,
                 scale. = TRUE)
#peso
print(ir.pca$rotation)

#em porcentagem pc1 73% + 22% é suficiente
summary(ir.pca)

# plotting data with less features
pca <- as.data.frame(cbind(ir.pca$x[,1],ir.pca$x[,2]))
pca <- cbind(pca,iris$Species)
names(pca)[3] <- "Species"
ggplot(data=pca, aes(x = pca$V1, y = pca$V2)) +
  geom_point(aes(color=pca$Species, shape=pca$Species)) +
  xlab("PC1") + 
  ylab("PC2") +
  ggtitle("PC1 vc PC2")

# Exercice: how does knn behave with the pre-processed dataset?

# feature selection
library(RWeka)

# C4.5 - embedded
#gulosa
#arvore inteira
fit <- J48(Species~., data=iris)
fit

# filter - ranker
#univariada

InfoGainAttributeEval(Species ~ . , data = iris)

# wrapper
library(FSelector)
#avaliando e pegando a taxa de acerto, pegando o q ta igual e dividindo pelo numro total 
#critério
evaluator <- function(subset) { 
  p <- J48(as.simple.formula(subset, "Species"), data=iris) 
  e <- sum(iris$Species == predict(p))/nrow(iris) 
  print(subset) 
  print(e) 
  return(e)
} 
#combinação de passos, passo 1 avalia individual, passo 2 combinado da melhor desmpenho, passo 3 novamente e no 4 ve as melhores combinações
subset <- forward.search(names(iris)[-5], evaluator)
# Exercice: change previous line to subset <- backward.search(names(iris)[-5], evaluator)  and see what happens
f <- as.simple.formula(subset, "Species")
print(f)