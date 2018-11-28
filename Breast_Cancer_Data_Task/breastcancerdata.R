#Installed the class package
installed.packages("class")
library(class)
#Installed the ggplot class
install.packages("ggplot2")
library(ggplot2)
#Setting the working directory 
setwd("C:/Users/Admin/Documents/R Projects/Breast_Cancer_Data_Task")
#Inputting the data file
RawData <- read.csv("BreastCancerData.csv", header=FALSE)
#The 'c' function puts individual Strings into a String vector. This String vector can
#be added to the DataFrame as a header row with the 'names' function.
names(RawData) <- c("ID","diagnosis","radius_mean","texture_mean","perimeter_mean","area_mean","smoothness_mean","compactness_mean","concavity_mean","concave points_mean","symmetry_mean","fractal_dimensions_mean","radius_se","texture_se","perimeter_se","area_se","smoothness_se","compactness_se","concavity_se","concave points_se","symmetry_se","fractal_dimensions_se","radius_worst","texture_worst","perimeter_worst","area_worst","smoothness_worst","compactness_worst","concavity_worst","concave points_worst","symmetry_worse","fractal_dimensions_worse")
#Removing the ID filed and creating a variable called Data_NoID
Data_NoID <- RawData[,-1]
#We also need a subset of the BCD data set without the Result column
#This allows it to be used for normalisation and validation purposes
Data_NoResults <- Data_NoID[,-1]
#There are many Feature Scaling functions defined in CRAN, but to learn the maths, it is
#good to built one yourself, like so:
FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) }
#Data normalised
Data_Normalised <- as.data.frame(lapply(Data_NoResults, FeatureScaling))
Data_Training <- Data_Normalised[1:400,]
Data_Test <- Data_Normalised[401:569,]
K_Value <- floor(sqrt(length(Data_Training[,1])))
Data_Predictions <- knn(Data_Training,Data_Test,Data_NoID[1:400,1], k=K_Value)
Data_Reference <- Data_NoID[401:569,1]
table(Data_Predictions,Data_Reference)