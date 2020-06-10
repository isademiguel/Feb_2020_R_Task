# Libraries
library("readr")
library("rstudioapi")
library("ggplot2")
library("e1071")

---- # Upload and explore data # ----

setwd(dirname(getActiveDocumentContext()$path)) # Set current working directory - Relative path
getwd()

IrisDataset <- read.csv("../iris.csv") # Read csv data set
print(IrisDataset)
attributes(IrisDataset)
summary(IrisDataset)
str(IrisDataset)
names(IrisDataset)
hist(IrisDataset$Petal.Length)
hist(IrisDataset$Petal.Width)
hist(IrisDataset$Sepal.Length)
hist(IrisDataset$Sepal.Width)
plot(IrisDataset$Petal.Width, IrisDataset$Petal.Length)
plot(IrisDataset$Sepal.Width, IrisDataset$Petal.Width)
par(mfrow=c(1, 2))  # divide graph area in 2 columns and rows
plot(density(IrisDataset$Sepal.Width), main="Density Plot: Petal Width", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(IrisDataset$Petal.Width), 2)))  # density plot 
polygon(density(IrisDataset$Petal.Width), col="red")
plot(density(IrisDataset$Petal.Length), main="Density Plot: Petal Length", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(IrisDataset$Petal.Length), 2)))  # density plot
polygon(density(IrisDataset$Petal.Length), col="red")

---- # Preprocessing # ----

# Transform data type
# IrisDataset$Species<- as.numeric(IrisDataset$Species)
qqnorm(IrisDataset$Species)
qqnorm(IrisDataset$Petal.Length)
qqnorm(IrisDataset$Petal.Width)

# Checking NAs
is.na(IrisDataset)
sum(is.na(IrisDataset))
IrisDataset$Petal.Length[is.na(IrisDataset$Petal.Length)]<-mean(IrisDataset$Petal.Length,na.rm = TRUE)
IrisDataset$Petal.Width[is.na(IrisDataset$Petal.Width)]<-mean(IrisDataset$Petal.Width,na.rm = TRUE)

# Checking duplicates
duplicates <- IrisDataset %>%
  duplicated() %>% 
  table()

duplicates

# Checking Outliers
par(mfrow=c(1, 1)) 
boxplot(IrisDataset$Sepal.Width)
boxplot(IrisDataset$Petal.Width)
boxplot(IrisDataset[, c("Sepal.Width","Sepal.Length")]) # Founded ouliers on sepal width and modelled apart but worse metrics founded
scatter.smooth(x=IrisDataset$Petal.Width, y=IrisDataset$Petal.Length, main="Petal.Length ~ Petal-Width")  


---- # Modeling # ---- 

# Testing and Training Sizes
set.seed(123)
trainSize <- round(nrow(IrisDataset) * 0.8)
testSize<-nrow(IrisDataset) - trainSize
trainSize
testSize

# Training and testing sets
training_indices<-sample(seq_len(nrow(IrisDataset)),size =trainSize)
trainSet<-IrisDataset[training_indices,]
testSet<-IrisDataset[-training_indices,]

# Linear Regression model
IrisModel<-lm(Petal.Length ~ Petal.Width, trainSet)
summary(IrisModel)

# Predictions on test
IrisPrediction<-predict(IrisModel, testSet)
IrisPrediction

# Metrics
plot(IrisModel)
summary(IrisPrediction)

# Errors
names(testSet)
testSet$prediction <- IrisPrediction
testSet$abs_error <- abs(testSet$Petal.Length - testSet$prediction)
testSet$rel_error <- (abs(testSet$Petal.Length - testSet$prediction) / testSet$Petal.Length) * 100
head(testSet)

#ErrorPlots

plot(testSet$Petal.Length, testSet$prediction)
ggplot(testSet, aes(Petal.Length, abs_error)) + geom_point(aes(colour = factor(Species)))

ggplot(IrisDataset, aes(x=Petal.Length, y=Petal.Width))+
    geom_point(size= 3, aes(colour = factor(Species))) +
    geom_smooth(method= "lm") +
    labs(title = "Petal Size by Species") + 
    facet_wrap ( ~  Species) #Split the frame by variable categories

ggplot(testSet, aes(x=Petal.Width, y=abs_error))+
  geom_point(size= 3, aes(colour = factor(Species))) +
  labs(title = "Errors by Species") + 
  facet_wrap ( ~  Species) # The bigger is the petal, the bigger the error. Virginica the one that has more.
