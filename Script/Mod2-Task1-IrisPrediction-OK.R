#Install packages
install.packages("readr")
#Read libraries
library("readr")
#Import data set from local and copy link
IrisDataset <- read.csv("~/Desktop/Ubiqum/Módulo 2/Task1/iris.csv")
#Exploring data
attributes(IrisDataset)
summary(IrisDataset)
str(IrisDataset)
names(IrisDataset)
hist(IrisDataset$Petal.Length)
hist(IrisDataset$Petal.Width)
hist(IrisDataset$Sepal.Length)
hist(IrisDataset$Sepal.Width)
plot(IrisDataset$Petal.Width, IrisDataset$Petal.Length)
plot(IrisDataset$Sepal.Width, IrisDataset$Sepal.Length)
#Preprocessing
IrisDataset$Species<- as.numeric(IrisDataset$Species)
qqnorm(IrisDataset$Species)
qqnorm(IrisDataset$Petal.Length)
qqnorm(IrisDataset$Petal.Width)
summary(IrisDataset)
is.na(IrisDataset)
IrisDataset$Petal.Length[is.na(IrisDataset$Petal.Length)]<-mean(IrisDataset$Petal.Length,na.rm = TRUE)
IrisDataset$Petal.Width[is.na(IrisDataset$Petal.Width)]<-mean(IrisDataset$Petal.Width,na.rm = TRUE)
#Modelling
set.seed(123)
trainSize <- round(nrow(IrisDataset) * 0.8)
testSize<-nrow(IrisDataset) - trainSize
trainSize
testSize
#Training
training_indices<-sample(seq_len(nrow(IrisDataset)),size =trainSize)
trainSet<-IrisDataset[training_indices,]
testSet<-IrisDataset[-training_indices,]
IrisModel<-lm(Petal.Length ~ Petal.Width, trainSet)
summary(IrisModel)
#Testing
IrisPrediction<-predict(IrisModel, testSet)
IrisPrediction
#Metrics
plot(IrisPrediction, IrisDataset$Petal.Length)
