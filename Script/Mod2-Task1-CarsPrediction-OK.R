# Libraries
library(readr)
library(rstudioapi)
require("lattice")


# Relative path
setwd(dirname(getActiveDocumentContext()$path))


#### ---- Uploading and Exploring Data ---- ####

CarsSet<- read.csv ("../cars.csv")

names(CarsSet)<-c("name","speed","distance")
attributes(CarsSet)
summary(CarsSet)
str(CarsSet)
CarsSet$name
CarsSet$speed
CarsSet$distance

# Plots
hist(CarsSet$speed)
hist(CarsSet$distance)
plot(CarsSet$speed,CarsSet$distance)
boxplot(CarsSet$speed, CarsSet$distance)
plot(CarsSet$name,CarsSet$speed)
qqnorm(CarsSet$speed)
qqnorm(CarsSet$distance)


#### ---- Preprocessing ---- ####

CarsSet$distance<-as.numeric(CarsSet$distance) # Transforming data type

is.na(CarsSet) # Checking NA 
na.omit(CarsSet$distance) # Removing NA
CarsSet$speed[is.na(CarsSet$speed)]<-mean(CarsSet$speed,na.rm = TRUE) # Replacing by the mean

box_plot <- boxplot(CarsSet[, c("distance","speed")]) # Checking outliers
Carsok <- CarsSet[which(CarsSet$distance != box_plot$out),] # Removing outliers
boxplot(Carsok$distance)


#### ---- Modeling ---- ####

set.seed(123)

# Create Testing and Training Sizes
trainSize<-round(nrow(CarsSet)*0.7)
testSize<-nrow(CarsSet)-trainSize
training_indices<-sample(seq_len(nrow(CarsSet)),size =trainSize)

# Creating train and test Sets
trainSet<-CarsSet[training_indices,]
testSet<-CarsSet[-training_indices,] 

# First linear regression model
CarsModel<-lm(distance~ speed, trainSet)

# Metrics
summary(CarsModel)

# Second lm model - fitting results above 0 (nosense negative values for distance of a car)
CarsModel2 <-lm(distance~0+speed, trainSet)

# Metrics
summary(CarsModel2)

# Plot model
plot(CarsModel2)
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(CarsModel2)  # Plot the model information

#### ---- Predictions and error analysis ---- ####

CarsPrediction <- predict(CarsModel2,testSet)

sd(testSet$speed - CarsPrediction)

testSet$prediction <- CarsPrediction

Errors <- (CarsPrediction - testSet$distance)
testSet$Errors <- Errors

plot(testSet$Errors)

plot(testSet[, c("distance","prediction")])


# Errors visualization

xyplot(distance ~ speed, data = testSet, type = c("p","r"), col.line = "red")


ggplot(testSet, aes(x = speed, y = distance)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = speed, yend = prediction), alpha = .2) +
  
  # > Color adjustments made here...
  geom_point(aes(color = Errors)) + # Color mapped to abs(Errors)
  scale_color_continuous(low = "black", high = "red") +  # Colors to use here
  guides(color = FALSE) +  # Color legend removed
  # <
  
  geom_point(aes(y = prediction), shape = 1) +
  theme_bw()
