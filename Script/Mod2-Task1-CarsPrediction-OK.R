#Read library
library(readr)
#Upload Data set from my working enviroment
CarsSet<- read.csv ("~/Desktop/Ubiqum/Módulo 2/Task1/cars.csv")

#Exploring data
names(CarsSet)<-c("name","speed","distance")
attributes(CarsSet)
summary(CarsSet)
str(CarsSet)
CarsSet$name
CarsSet$speed
CarsSet$distance
#Plots
hist(CarsSet$speed)
hist(CarsSet$distance)
plot(CarsSet$speed,CarsSet$distance)
boxplot(CarsSet$speed, CarsSet$distance)
plot(CarsSet$name,CarsSet$speed)
qqnorm(CarsSet$speed)
qqnorm(CarsSet$distance)
#Preprocessing Data (change variables, names, etc.)
CarsSet$distance<-as.numeric(CarsSet$distance)
hist(CarsSet$distance)
CarsSet$distance
#NA Removing and Replacing

# is.na() tells you if values are NA (TRUE) or not (FALSE). You are looking for
# how many NA's you have in hour df. If you sum() is.na(), it will return how
# many TRUE you get -> how many NA's in the object.
sum(is.na(CarsSet)) # return 0, you do not have NA's !
is.na(CarsSet)
na.omit(CarsSet$distance) # Understood na.omit() ?
# The following line is AMAZING !
# But you do not need it because you do not have NA's. Will be usefull when you will have NA's
# Keep it !
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
# Fit the model
CarsModel<-lm(distance~0+speed,trainSet)
#Predictions
CarsPrediction <- predict(CarsModel,testSet)
CarsPrediction
#Metrics
summary(CarsModel)
summary(CarsPrediction)


# Where is the error analysis ?
