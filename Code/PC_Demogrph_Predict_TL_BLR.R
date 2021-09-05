## Thanks for showing interest in our work!
## Please contact the author in case of any queries or discrepancies.


#setting the working directory
setwd("")

#loading the required library packages
library("caTools")
library("caret")
library("pROC")

pctlData <- read.csv("", header = TRUE)

#convert all character type columns to 'factor' type [demographic variables and TL_AVG in this case]
for(i in 1:12)
{
  pctlData[,i] <- as.factor(pctlData[,i])
}

#Check if all char type columns have been converted into factor type
str(pctlData)

#Creating a dataframe for analysis (this was created after running the Boruta Algorithm - code provided in the file for RFA)
pctlDF1 <- data.frame("iPF_AVG_PP" = pctlData$iPF_AVG_PP,
                      "CS_AVG_PP" = pctlData$CS_AVG_PP,
                      "CP_BC" = pctlData$CP_BC,
                      "PA_AVG_CP" = pctlData$PA_AVG_CP,
                      "GENDER" = pctlData$GENDER,
                      "TL_AVG" = pctlData$TL_AVG
                    )

str(pctlDF1)

#For repeatable results
set.seed(122)

#Split the first dataframe into two: 75%:25%
sampleDF1 = sample.split(pctlDF1$TL_AVG, SplitRatio = .75)
trainDF1 = subset(pctlDF1, sampleDF1 == TRUE)
testDF1  = subset(pctlDF1, sampleDF1 == FALSE)

logit1 <- glm(TL_AVG~., data = trainDF1, family = "binomial")
summary(logit1)

testpredictionsDF1 <- predict(logit1, testDF1, type = "response")

#We now need a factor variable for calculating Confusion Matrix and other statistics
xFctr <- factor(c("High", "Low"))

for(j in 1:length(testpredictionsDF1))
{
  if(testpredictionsDF1[j] <= 0.5)
  {
    xFctr[j] <- "Low"
  }
  else
  {
    xFctr[j] <- "High"
  }
}

#Confusion Matrix and other statistics
confusionMatrix(xFctr, testDF1$TL_AVG, positive = "High")

#For ROC Plot of RF Models
testpredictions2DF1 <- predict(logit1, newdata = testDF1, type = "response")
logit_ROC_DF1 <- roc(testDF1$TL_AVG, testpredictions2DF1)
logit_AUC_DF1 <- auc(logit_ROC_DF1)

plot(logit_ROC_DF1, col = "red", main = "'Profession Change and Transformative Learning' Logistic Regression Model: ROC Plot")