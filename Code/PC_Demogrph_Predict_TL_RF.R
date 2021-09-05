## Thanks for showing interest in our work!
## Please contact the author in case of any queries or discrepancies.


#setting the working directory
#setwd("")

#loading the required library packages
library("randomForest")
library("caTools")
library("caret")
library("pROC") #needed for ROC related calculations and plots

# The following package is needed only once for feature selection; 
# May not be loaded once unimportant variables have been excluded and the target is to only check the Random Forest Model
library("Boruta")  

#### Loading and formatting data ####

#Load data from the dataset saved in a CSV file
pctlData <- read.csv("", header = TRUE)

#Convert all character type columns to 'factor' type [demographic variables and TL_AVG in this case]
for(i in 1:12)
{
  pctlData[,i] <- as.factor(pctlData[,i])
}

#Check if all char type columns have been converted into factor type properly
str(pctlData)

#### Feature Selection through the Boruta Algorithm ####

## Important note: this block of code was run only once to determine the relative importance of the variables 
## w.r.t the dependent variable and the rest of the analyses was done properly. This code block is being provided
## in this file only to save space. Please contact the author in case of any queries

#For repeatable results
set.seed(141)

#Examining Boruta Feature Selection algorithm
brtaVIP <- Boruta(TL_AVG~., data = pctlData, doTrace = 2, ntree = 500)
brtaVIP
plot(brtaVIP)

##### Preparation and Data Analyses

## Important note: The structure of the dataframe was obtained after combining the results of the run of Boruta algorithm and 
## the 'Variable Importance Plot' (which, in turn was plotted after running the Random Forest Algorithm)
pctlDF1 <- data.frame("iPF_AVG_PP" = pctlData$iPF_AVG_PP,
                      "CS_AVG_PP" = pctlData$CS_AVG_PP,
                      "CP_BC" = pctlData$CP_BC,
                      "PA_AVG_CP" = pctlData$PA_AVG_CP,
                      "GENDER" = pctlData$GENDER,
                      "TL_AVG" = pctlData$TL_AVG
                    )

#To verify if the dataframe structure has formed properly
str(pctlDF1)

#For repeatable results
set.seed(141)

#Split the first dataframe into two portions (75%:25%) as training and test subsets
sampleDF1 = sample.split(pctlDF1$TL_AVG, SplitRatio = .75)
trainDF1 = subset(pctlDF1, sampleDF1 == TRUE)
testDF1  = subset(pctlDF1, sampleDF1 == FALSE)

#Building the randomforest model
Model_DF1 <- randomForest(TL_AVG~.,data=trainDF1)#, importance=TRUE)

#Understanding Models and the importance of variables
Model_DF1
varImpPlot(Model_DF1, main = "Variable Importance Plot", col="black", pch = 19)

#Predicting the test dataset using the model
testpredictionsDF1 <- predict(Model_DF1, newdata = testDF1)
confusionMatrix(testpredictionsDF1, testDF1$TL_AVG, positive = "High")

#ROC Plot and the AUC value of the RF Models
testpredictions2DF1 <- predict(Model_DF1, newdata = testDF1, type = "prob")
rf_ROC_DF1 <- roc(testDF1$TL_AVG, testpredictions2DF1[,2])
rf_AUC_DF1 <- auc(rf_ROC_DF1)

#Examine the output
print(c("AUC:", rf_AUC_DF1))
plot(rf_ROC_DF1, col = "red", main = "'Profession Change and Transformative Learning' Random Forest: ROC plot")