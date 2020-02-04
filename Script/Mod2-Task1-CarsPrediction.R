#After install packages, run library always
library(readr)
#Upload Data set from my working enviroment
CarsSet<- read.csv("~/Desktop/Ubiqum/Módulo 2/Task1/cars.csv")
#Exploring data
attributes(CarsSet)
summary(CarsSet)
str(CarsSet)
names(CarsSet)
CarsSet$name.of.car
CarsSet$speed.of.car
#Plots
hist(CarsSet$speed.of.car)
hist(CarsSet$distance.of.car)
plot(CarsSet$speed.of.car,CarsSet$distance.of.car)
plot(CarsSet$name.of.car,CarsSet$speed.of.car)
qqnorm(CarsSet$speed.of.car)
qqnorm(CarsSet$distance.of.car)
#Preprocessing Data (change variables, names, etc.)
CarsSet$distance.of.car<-as.numeric(CarsSet$distance.of.car)
hist(CarsSet$distance.of.car)
CarsSet$distance.of.car
names(CarsSet)<-c("name","speed","distance")
#NA Removing and Replacing
summary(CarsSet)
is.na(CarsSet)
na.omit(CarsSet$distance)
CarsSet$speed[is.na(CarsSet$speed)]<-mean(CarsSet$speed,na.rm = TRUE)
#Testing and Training Sizes
set.seed(123)
trainSize<-round(nrow(CarsSet)*0.7)
testSize<-nrow(CarsSet)-trainSize
training_indices<-sample(seq_len(nrow(CarsSet)),size =trainSize)
#Creating train and test Sets
trainSet<-CarsSet[training_indices,]
testSet<-CarsSet[-training_indices,] 
#Model
CarsModel<-lm(distance~ speed, trainSet)
summary(CarsModel)
#Predictions
CarsPrediction <- predict(CarsModel,testSet)
CarsPrediction
summary(CarsModel)