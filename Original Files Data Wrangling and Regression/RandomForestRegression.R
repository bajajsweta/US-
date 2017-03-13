
install.packages("randomForest")

library(randomForest)
library(data.table)
library(forecast)
library(leaps)
library(dplyr)

QuarterData_RandomForestData <- fread("Quarter2005.csv", sep = ',')
QuarterData_RandomForestData <- QuarterData_RandomForestData %>% mutate_if(is.character,as.factor)
#Can not handle categorical predictors with more than 53 categories - Error 
QuarterData_RandomForestData <- within(QuarterData_RandomForestData, rm("POSTAL_CODE",
                                                                        "LOAN_SEQUENCE_NUMBER",
                                                                        "SUPER_CONFORMING_FLAG",
                                                                        "PRODUCT_TYPE",
                                                                        "PROPERTY_STATE"))
  

summary(QuarterData_RandomForestData)
#75% of the Sample Size 
smp_size_r <- floor(0.75 * nrow(QuarterData_RandomForestData))

set.seed(123)
train_ind_r <- sample(seq_len(nrow(QuarterData_RandomForestData)),size = smp_size_r)

#Divide into Training and Testing Data
train_r <- QuarterData_RandomForestData[train_ind_r,]
test_r <- QuarterData_RandomForestData[-train_ind_r,]
?randomForest.formula
# Fitting model
randomForestfit <- randomForest(ORIGINAL_INTEREST_RATE~CREDIT_SCORE+ORIGINAL_UPB+ORIGINAL_LOAN_TERM+MORTGAGE_INSURANCE_PERCENTAGE_MI+
                                  ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV+ORIGINAL_DEBT_TO_INCOME_DTI_RATIO+PROPERTY_STATE+ORIGINAL_LOAN_TO_VALUE_LTV+
                                  LOAN_PURPOSE+PROPERTY_TYPE+FIRST_TIME_HOMEBUYER_FLAG+NUMBER_OF_UNITS,data = train_r, ntree=50)
summary(randomForestfit)
#Predict Output 
predictedrandomForest = predict(randomForestfit,test_r)
accuracy(predictedrandomForest, train_r$ORIGINAL_INTEREST_RATE)
