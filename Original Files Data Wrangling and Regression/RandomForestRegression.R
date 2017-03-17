
install.packages("randomForest")

library(randomForest)
library(data.table)
library(forecast)
library(leaps)
library(dplyr)

#Writen to Accept the System Args from the Console
args <- commandArgs(trailingOnly = TRUE)
n <- as.numeric(args[1])
m <- as.numeric(args[2])

#Read the File from the Current Directory
QuarterData <- fread("Q11999.csv", sep = ',')
print (head(QuarterData))

Quarter2Data <- fread("Q11999.csv", sep = ',')
print (head(Quarter2Data))

QuarterDataFeature <- QuarterData
QuarterDataFeature <- QuarterDataFeature %>% mutate_if(is.character,as.factor)
#Can not handle categorical predictors with more than 53 categories - Error 
QuarterDataFeature <- within(QuarterDataFeature, rm("POSTAL_CODE",
                                                                        "LOAN_SEQUENCE_NUMBER",
                                                                        "SUPER_CONFORMING_FLAG",
                                                                        "PRODUCT_TYPE",
                                                                        "PROPERTY_STATE"))

QuarterData2Feature <- Quarter2Data
QuarterData2Feature <- QuarterData2Feature %>% mutate_if(is.character,as.factor)
#Can not handle categorical predictors with more than 53 categories - Error 
QuarterData2Feature <- within(QuarterData2Feature, rm("POSTAL_CODE",
                                                       "LOAN_SEQUENCE_NUMBER",
                                                       "SUPER_CONFORMING_FLAG",
                                                       "PRODUCT_TYPE",
                                                       "PROPERTY_STATE"))
  

summary(QuarterData)
#75% of the Sample Size 
#smp_size_r <- floor(0.75 * nrow(QuarterData_RandomForestData))

#set.seed(123)
#train_ind_r <- sample(seq_len(nrow(QuarterData_RandomForestData)),size = smp_size_r)

#Divide into Training and Testing Data
#train_r <- QuarterData_RandomForestData[train_ind_r,]
#test_r <- QuarterData_RandomForestData[-train_ind_r,]

train_r <- QuarterDataFeature
test_r <- QuarterData2Feature

# Fitting model
#randomForestfit <- randomForest(ORIGINAL_INTEREST_RATE~CREDIT_SCORE+ORIGINAL_UPB+ORIGINAL_LOAN_TERM+MORTGAGE_INSURANCE_PERCENTAGE_MI+
#                                 ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV+ORIGINAL_DEBT_TO_INCOME_DTI_RATIO+ORIGINAL_LOAN_TO_VALUE_LTV
#                                  ,data = train_r, ntree=5)

randomForestfit <- randomForest(ORIGINAL_INTEREST_RATE~CREDIT_SCORE+ORIGINAL_UPB+ORIGINAL_LOAN_TERM+
                                  ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV+ORIGINAL_DEBT_TO_INCOME_DTI_RATIO
                                  ,data = train_r, ntree=5)
summary(randomForestfit)
#Predict Output 
predictedrandomForest = predict(randomForestfit,test_r)
accuracy(predictedrandomForest, train_r$ORIGINAL_INTEREST_RATE)
