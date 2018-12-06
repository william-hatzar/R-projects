install.packages("rpart")
install.packages("randomForest")
install.packages("rattle")
library(rpart)
library(randomForest)
library(rattle)
library(class)
library(ggplot2)
library(e1071)

setwd("C:/Users/Admin/Documents/R-projects/First Attempt")
TestData <- read.csv("train.csv")
cleanedtestdata<-as.data.frame(TestData)


cleanedtestdata$Id<-NULL
cleanedtestdata$LotFrontage<-NULL
cleanedtestdata$LotArea<-NULL
cleanedtestdata$Street<-NULL
cleanedtestdata$LotShape<-NULL
cleanedtestdata$LandContour<-NULL
cleanedtestdata$Utilities<-NULL
cleanedtestdata$LotConfig<-NULL
cleanedtestdata$LandSlope<-NULL
cleanedtestdata$Condition1<-NULL
cleanedtestdata$Condition2<-NULL
cleanedtestdata$HouseStyle<-NULL
cleanedtestdata$OverallCond<-NULL
cleanedtestdata$YearBuilt<-NULL
cleanedtestdata$YearRemodAdd<-NULL
cleanedtestdata$RoofStyle<-NULL
cleanedtestdata$RoofMatl<-NULL
cleanedtestdata$Exterior1st<-NULL
cleanedtestdata$Exterior2nd<-NULL
cleanedtestdata$MasVnrType<-NULL
cleanedtestdata$MasVnrArea<-NULL
cleanedtestdata$ExterQual<-NULL
cleanedtestdata$ExterCond<-NULL
cleanedtestdata$RoofMatl<-NULL
cleanedtestdata$BsmtCond<-NULL
cleanedtestdata$BsmtExposure<-NULL
cleanedtestdata$RoofMatl<-NULL
cleanedtestdata$BsmtFinSF1<-NULL
cleanedtestdata$BsmtFinSF2<-NULL
cleanedtestdata$BsmtFinType2<-NULL
cleanedtestdata$BsmtUnfSF<-NULL
cleanedtestdata$TotalBsmtSF<-NULL
cleanedtestdata$LowQualFinSF<-NULL
cleanedtestdata$RoofMatl<-NULL
cleanedtestdata$BsmtFullBath<-NULL
cleanedtestdata$BsmtHalfBath<-NULL
cleanedtestdata$HalfBath<-NULL
cleanedtestdata$BedroomAbvGr<-NULL
cleanedtestdata$KitchenAbvGr<-NULL
cleanedtestdata$Functional<-NULL
cleanedtestdata$FireplaceQu<-NULL
cleanedtestdata$GarageType<-NULL
cleanedtestdata$GarageArea<-NULL
cleanedtestdata$GarageQual<-NULL
cleanedtestdata$GarageCond<-NULL
cleanedtestdata$GarageYrBuilt<-NULL
cleanedtestdata$WoodDeckSF<-NULL
cleanedtestdata$OpenPorchSF<-NULL
cleanedtestdata$EnclosedPorch<-NULL
cleanedtestdata$X3SsnPorch<-NULL
cleanedtestdata$ScreenPorch<-NULL
cleanedtestdata$PoolArea<-NULL
cleanedtestdata$PoolQC<-NULL
cleanedtestdata$Fence<-NULL
cleanedtestdata$MiscFeature<-NULL
cleanedtestdata$MiscVal<-NULL
cleanedtestdata$MoSold<-NULL
cleanedtestdata$YrSold<-NULL
cleanedtestdata$SaleType<-NULL


#cleanedtestdata$MSZoning <- sub("A","1",cleanedtestdata$MSZoning)
#cleanedtestdata$MSZoning <- sub("C(all)","2",cleanedtestdata$MSZoning)
#cleanedtestdata$MSZoning <- sub("FV","3",cleanedtestdata$MSZoning)
#cleanedtestdata$MSZoning <- sub("I","4",cleanedtestdata$MSZoning)
#cleanedtestdata$MSZoning <- sub("RH","5",cleanedtestdata$MSZoning)
#cleanedtestdata$MSZoning <- sub("RL","6",cleanedtestdata$MSZoning)
#cleanedtestdata$MSZoning <- sub("RM","7",cleanedtestdata$MSZoning)

#cleaning ally
cleanedtestdata$Alley <- sub("Grvl",1,cleanedtestdata$Alley)
cleanedtestdata$Alley <- sub("Pave",2,cleanedtestdata$Alley)
cleanedtestdata$Alley <-as.integer(cleanedtestdata$Alley)
cleanedtestdata$Alley[is.na(cleanedtestdata$Alley)] <- 0
cleanedtestdata$Alley <-as.factor(cleanedtestdata$Alley)

#cleaning electrical

cleanedtestdata$Electrical<-sub("1",1,cleanedtestdata$Electrical)
cleanedtestdata$Electrical<-sub("FuseA",0,cleanedtestdata$Electrical)
cleanedtestdata$Electrical<-sub("FuseF",0,cleanedtestdata$Electrical)
cleanedtestdata$Electrical<-sub("FuseP",0,cleanedtestdata$Electrical)
cleanedtestdata$Electrical<-sub("Mix",0,cleanedtestdata$Electrical)

#change fireplaces to either do or dont have one

cleanedtestdata$Fireplaces<-sub(2,1,cleanedtestdata$Fireplaces)
cleanedtestdata$Fireplaces<-sub(3,1,cleanedtestdata$Fireplaces)
#change heating to gas or non gas

cleanedtestdata$Heating<-sub("GasA","Gas", cleanedtestdata$Heating)
cleanedtestdata$Heating<-sub("GasW","Gas", cleanedtestdata$Heating)
cleanedtestdata$Heating<-sub("Wall","NonGas", cleanedtestdata$Heating)
cleanedtestdata$Heating<-sub("Floor","NonGas", cleanedtestdata$Heating)
cleanedtestdata$Heating<-sub("OthW","NonGas", cleanedtestdata$Heating)
cleanedtestdata$Heating<-sub("Grav","NonGas", cleanedtestdata$Heating)
#change paveddrive to paved and not paved

cleanedtestdata$PavedDrive<-sub("P","N", cleanedtestdata$PavedDrive)





#replace na values
cleanedtestdata$Electrical[is.na(cleanedtestdata$Electrical)]<-0
#send back to factor
cleanedtestdata$Electrical<-as.factor(cleanedtestdata$Electrical)

#replace other na values by making the variable an integer, sending NA values to 0, then sending variable back to a factor
cleanedtestdata$BsmtQual<-as.integer(cleanedtestdata$BsmtQual)
cleanedtestdata$BsmtQual[is.na(cleanedtestdata$BsmtQual)]<-0
cleanedtestdata$BsmtQual<-as.factor(cleanedtestdata$BsmtQual)

cleanedtestdata$BsmtFinType1<-as.integer(cleanedtestdata$BsmtFinType1)
cleanedtestdata$BsmtFinType1[is.na(cleanedtestdata$BsmtFinType1)]<-0 
cleanedtestdata$BsmtFinType1<-as.factor(cleanedtestdata$BsmtFinType1)

cleanedtestdata$GarageFinish<-as.integer(cleanedtestdata$GarageFinish)
cleanedtestdata$GarageFinish[is.na(cleanedtestdata$GarageFinish)]<-0 
cleanedtestdata$GarageFinish<-as.factor(cleanedtestdata$GarageFinish)

cleanedtestdata$MSZoning<-as.integer(cleanedtestdata$MSZoning)
cleanedtestdata$Alley<-as.integer(cleanedtestdata$Alley)
cleanedtestdata$Neighborhood<-as.integer(cleanedtestdata$Neighborhood)
cleanedtestdata$BldgType<-as.integer(cleanedtestdata$BldgType)
cleanedtestdata$Foundation<-as.integer(cleanedtestdata$Foundation)
cleanedtestdata$BsmtQual<-as.integer(cleanedtestdata$BsmtQual)
cleanedtestdata$BsmtFinType1<-as.integer(cleanedtestdata$BsmtFinType1)
cleanedtestdata$Heating<-as.integer(cleanedtestdata$Heating)
cleanedtestdata$HeatingQC<-as.integer(cleanedtestdata$HeatingQC)
cleanedtestdata$CentralAir<-as.integer(cleanedtestdata$CentralAir)
cleanedtestdata$Electrical<-as.integer(cleanedtestdata$Electrical)
cleanedtestdata$KitchenQual<-as.integer(cleanedtestdata$KitchenQual)
cleanedtestdata$GarageFinish<-as.integer(cleanedtestdata$GarageFinish)
cleanedtestdata$PavedDrive<-as.integer(cleanedtestdata$PavedDrive)
cleanedtestdata$SaleCondition<-as.integer(cleanedtestdata$SaleCondition)

#SVM Attempt

svm_model <- svm( SalePrice ~ ., cleanedtestdata)
summary(svm_model)

#Fit a model
model_svm <- svm(cleanedtestdata, TestData)

#Use predictions on the data
pred <- predict(model_svm, TestData)

#Plot the predictions and plot to see our model
points (TestData$s, pred, col = "blue", pch= 4)

#Linear model has a residuals part which we can extract
error <- model$residuals
lm_error <=- sqrt(mean(error^2))

#For svm we have to
error_2 <- train$
  


