library(e1071)
data("Titanic")
Titanic_df=as.data.frame(Titanic)
#Creating data from table
repeating_sequence=rep.int(seq_len(nrow(Titanic_df)), Titanic_df$Freq) #This will repeat each combination equal to the frequency of each combination
#Create the dataset by row repetition created
Titanic_dataset=Titanic_df[repeating_sequence,]
#We no longer need the frequency, drop the feature
Titanic_dataset$Freq=NULL
Bayes_Model=naiveBayes(Survived ~., data=Titanic_dataset)
Bayes_Model
#Prediction on the dataset
NB_Predictions=predict(Naive_Bayes_Model,Titanic_dataset)
#Confusion matrix to check accuracy
table(NB_Predictions,Titanic_dataset$Survived)
install.packages("mlr")
library(mlr)
task = makeClassifTask(data )