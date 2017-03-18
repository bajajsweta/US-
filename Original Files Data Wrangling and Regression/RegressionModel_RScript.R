#Install Packages in R
install.packages("data.table")
install.packages("forecast")
install.packages("leaps",repos='https://CRAN.R-project.org')
install.packages("dplyr")
install.packages("randomForest")
install.packages("FNN")
install.packages("neuralnet")

getwd()


#Import the Libraries
library(data.table)
library(forecast)
library(leaps)
library(dplyr)
library(randomForest)
library(FNN, verbose = TRUE)
library(neuralnet)



print("Loading Data for Quarter2005")


#Writen to Accept the System Args from the Console
args <- commandArgs(trailingOnly = TRUE)
n <- as.numeric(args[1])
m <- as.numeric(args[2])

#Read the File from the Current Directory
QuarterData <- fread(n, sep = ',')
print (head(QuarterData))

Quarter2Data <- fread(m, sep = ',')
print (head(Quarter2Data))


#FEATURE ENGINEERING
#create a separate dataframe
QuarterDataFeature <- QuarterData
QuarterDataFeature <- QuarterDataFeature %>% mutate_if(is.character,as.factor)
QuarterDataFeature <- within(QuarterDataFeature, rm("SELLER_NAME","SUPER_CONFORMING_FLAG",
                                                    "POSTAL_CODE","MSA","LOAN_SEQUENCE_NUMBER",
                                                    "SERVICER_NAME","PRODUCT_TYPE","FIRST_PAYMENT_DATE",
                                                    "MATURITY_DATE", "NUMBER_OF_BORROWERS"))

#create a separate dataframe
QuarterData2Feature <- Quarter2Data
QuarterData2Feature <- QuarterData2Feature %>% mutate_if(is.character,as.factor)
QuarterData2Feature <- within(QuarterData2Feature, rm("SELLER_NAME","SUPER_CONFORMING_FLAG",
                                                      "POSTAL_CODE","MSA","LOAN_SEQUENCE_NUMBER",
                                                      "SERVICER_NAME","PRODUCT_TYPE","FIRST_PAYMENT_DATE",
                                                      "MATURITY_DATE", "NUMBER_OF_BORROWERS"))

#Normalized Data
normalized <-  function(x) {
  (x-min(x))/(max(x)-min(x))
}

#Normalize the Data
QuarterDataFeature$CREDIT_SCORE <- normalized(QuarterDataFeature$CREDIT_SCORE)  
QuarterDataFeature$ORIGINAL_UPB <- normalized(QuarterDataFeature$ORIGINAL_UPB)
QuarterDataFeature$ORIGINAL_LOAN_TERM <- normalized(QuarterDataFeature$ORIGINAL_LOAN_TERM)
QuarterDataFeature$MORTGAGE_INSURANCE_PERCENTAGE_MI <- normalized(QuarterDataFeature$MORTGAGE_INSURANCE_PERCENTAGE_MI)
QuarterDataFeature$ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV <- normalized(QuarterDataFeature$ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV)
QuarterDataFeature$ORIGINAL_DEBT_TO_INCOME_DTI_RATIO <- normalized(QuarterDataFeature$ORIGINAL_DEBT_TO_INCOME_DTI_RATIO)
QuarterDataFeature$ORIGINAL_LOAN_TO_VALUE_LTV <- normalized(QuarterDataFeature$ORIGINAL_LOAN_TO_VALUE_LTV)

#Convert All the Factors to Numerics
QuarterDataFeature$PROPERTY_STATE <- as.numeric(QuarterDataFeature$PROPERTY_STATE)
QuarterDataFeature$LOAN_PURPOSE <- as.numeric(QuarterDataFeature$LOAN_PURPOSE)
QuarterDataFeature$PROPERTY_TYPE <- as.numeric(QuarterDataFeature$PROPERTY_TYPE)
QuarterDataFeature$FIRST_TIME_HOMEBUYER_FLAG <- as.numeric(QuarterDataFeature$FIRST_TIME_HOMEBUYER_FLAG)

#Normalize all the Numerics
QuarterDataFeature$PROPERTY_STATE <- normalized(QuarterDataFeature$PROPERTY_STATE)
QuarterDataFeature$LOAN_PURPOSE <- normalized(QuarterDataFeature$LOAN_PURPOSE)
QuarterDataFeature$PROPERTY_TYPE <- normalized(QuarterDataFeature$PROPERTY_TYPE)
QuarterDataFeature$FIRST_TIME_HOMEBUYER_FLAG <- normalized(QuarterDataFeature$FIRST_TIME_HOMEBUYER_FLAG)

#Normalize the Data
QuarterData2Feature$CREDIT_SCORE <- normalized(QuarterData2Feature$CREDIT_SCORE)  
QuarterData2Feature$ORIGINAL_UPB <- normalized(QuarterData2Feature$ORIGINAL_UPB)
QuarterData2Feature$ORIGINAL_LOAN_TERM <- normalized(QuarterData2Feature$ORIGINAL_LOAN_TERM)
QuarterData2Feature$MORTGAGE_INSURANCE_PERCENTAGE_MI <- normalized(QuarterData2Feature$MORTGAGE_INSURANCE_PERCENTAGE_MI)
QuarterData2Feature$ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV <- normalized(QuarterData2Feature$ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV)
QuarterData2Feature$ORIGINAL_DEBT_TO_INCOME_DTI_RATIO <- normalized(QuarterData2Feature$ORIGINAL_DEBT_TO_INCOME_DTI_RATIO)
QuarterData2Feature$ORIGINAL_LOAN_TO_VALUE_LTV <- normalized(QuarterData2Feature$ORIGINAL_LOAN_TO_VALUE_LTV)

#Convert All the Factors to Numerics
QuarterData2Feature$PROPERTY_STATE <- as.numeric(QuarterData2Feature$PROPERTY_STATE)
QuarterData2Feature$LOAN_PURPOSE <- as.numeric(QuarterData2Feature$LOAN_PURPOSE)
QuarterData2Feature$PROPERTY_TYPE <- as.numeric(QuarterData2Feature$PROPERTY_TYPE)
QuarterData2Feature$FIRST_TIME_HOMEBUYER_FLAG <- as.numeric(QuarterData2Feature$FIRST_TIME_HOMEBUYER_FLAG)
QuarterData2Feature$OCCUPANCY_STATUS <- as.numeric(QuarterData2Feature$OCCUPANCY_STATUS)

#Normalize all the Numerics
QuarterData2Feature$PROPERTY_STATE <- normalized(QuarterData2Feature$PROPERTY_STATE)
QuarterData2Feature$LOAN_PURPOSE <- normalized(QuarterData2Feature$LOAN_PURPOSE)
QuarterData2Feature$PROPERTY_TYPE <- normalized(QuarterData2Feature$PROPERTY_TYPE)
QuarterData2Feature$FIRST_TIME_HOMEBUYER_FLAG <- normalized(QuarterData2Feature$FIRST_TIME_HOMEBUYER_FLAG)

# VARIABLE SELECTION
# Four algorithms exhaustive search, forward, backward selection, stepwise regression are explored here

# Forward Search
# nvmax is the maximum size of subsets
regfit_forward = regsubsets(ORIGINAL_INTEREST_RATE~., data = QuarterDataFeature, really.big=TRUE, method="forward")
regsummary_forward = summary(regfit_forward)

regsummary_forward$adjr2
regsummary_forward$rss

coef(regfit_forward,8)

plot(regfit_forward,scale="r2") 
plot(regsummary_forward$rss, xlab='Number of variables',ylab='RSS', type='l',col="green") + title ("Forward")
plot(regsummary_forward$adjr2, xlab='Number of variables',ylab='Adjr2', type='l',col="green") + title ("Forward")
plot(regsummary_forward$bic, xlab='Number of variables',ylab='BIC', type='l',col="green") + title ("Forward")
plot(regsummary_forward$cp, xlab='Number of variables',ylab='CP', type='l',col="green") + title ("Forward")

# Backward Search
regfit_backward = regsubsets(ORIGINAL_INTEREST_RATE~., data = QuarterDataFeature, really.big=TRUE, method="backward")
regsummary_backward = summary(regfit_backward)

regsummary_backward$adjr2
regsummary_backward$rss

coef(regfit_backward,8)

plot(regfit_backward,scale="r2") 
plot(regsummary_backward$rss, xlab='Number of variables',ylab='RSS', type='l',col="blue") + title ("Backward")
plot(regsummary_backward$adjr2, xlab='Number of variables',ylab='Adjr2', type='l',col="blue") + title ("Backward")
plot(regsummary_backward$bic, xlab='Number of variables',ylab='BIC', type='l',col="blue") + title ("Backward")
plot(regsummary_backward$cp, xlab='Number of variables',ylab='CP', type='l',col="blue") + title ("Backward")

# Exhaustive Search
regfit_exhaustive = regsubsets(ORIGINAL_INTEREST_RATE~., data = QuarterDataFeature, really.big=TRUE)
regsummary_exhaustive = summary(regfit_exhaustive)

regsummary_exhaustive$adjr2
regsummary_exhaustive$rss

coef(regfit_exhaustive,8)

plot(regfit_exhaustive,scale="r2") 
plot(regsummary_exhaustive$rss, xlab='Number of variables',ylab='RSS', type='l',col="red") + title ("Exhaustive")
plot(regsummary_exhaustive$adjr2, xlab='Number of variables',ylab='Adjr2', type='l',col="red") + title ("Exhaustive")
plot(regsummary_exhaustive$bic, xlab='Number of variables',ylab='BIC', type='l',col="red") + title ("Exhaustive")
plot(regsummary_exhaustive$cp, xlab='Number of variables',ylab='CP', type='l',col="red") + title ("Exhaustive")


# MODEL THE DATA
#75% of the Sample Size 

#smp_size <- floor(0.75 * nrow(QuarterDataFeature))

#set.seed(140)
#train_ind <- sample(seq_len(nrow(QuarterDataFeature)),size = smp_size)

#Divide into Training and Testing Data
#train <- QuarterDataFeature[train_ind,]
#test <- QuarterDataFeature[-train_ind,]
print("Divide into Training and Testing Data")
train <- QuarterDataFeature
test <- QuarterData2Feature

# Use the lm Function to Apply Linear Regression
#lm.fit = lm(ORIGINAL_INTEREST_RATE~CREDIT_SCORE+ORIGINAL_UPB+ORIGINAL_LOAN_TERM+MORTGAGE_INSURANCE_PERCENTAGE_MI+
#              ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV+ORIGINAL_DEBT_TO_INCOME_DTI_RATIO+PROPERTY_STATE+ORIGINAL_LOAN_TO_VALUE_LTV+
#             LOAN_PURPOSE+PROPERTY_TYPE+FIRST_TIME_HOMEBUYER_FLAG+NUMBER_OF_UNITS, 
#          data = train)

print("Performing Linear Regression")

lm.fit = lm(ORIGINAL_INTEREST_RATE~CREDIT_SCORE+ORIGINAL_UPB+ORIGINAL_LOAN_TERM+
              ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV+ORIGINAL_DEBT_TO_INCOME_DTI_RATIO+PROPERTY_STATE+
              LOAN_PURPOSE+PROPERTY_TYPE+FIRST_TIME_HOMEBUYER_FLAG+NUMBER_OF_UNITS, 
            data = train)
summary(lm.fit)
pred = predict(lm.fit, test)
accuracy(pred, test$ORIGINAL_INTEREST_RATE)

print ("Accuracy")

print(accuracy(pred, test$ORIGINAL_INTEREST_RATE))

print("Performing Random Forest Algorithm")

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

print ("Accuracy")

print(accuracy(predictedrandomForest, train_r$ORIGINAL_INTEREST_RATE))

print("Performing Neural Net Algorithm")

QuarterDataFeature <- QuarterData
QuarterDataFeature <- QuarterDataFeature %>% mutate_if(is.character,as.factor)
QuarterDataFeature <- within(QuarterDataFeature, rm("SELLER_NAME","SUPER_CONFORMING_FLAG",
                                                    "POSTAL_CODE","MSA","LOAN_SEQUENCE_NUMBER",
                                                    "SERVICER_NAME","PRODUCT_TYPE","FIRST_PAYMENT_DATE",
                                                    "MATURITY_DATE", "NUMBER_OF_BORROWERS"))


#Convert All the Factors to Numerics
QuarterDataFeature$PROPERTY_STATE <- as.numeric(QuarterDataFeature$PROPERTY_STATE)
QuarterDataFeature$LOAN_PURPOSE <- as.numeric(QuarterDataFeature$LOAN_PURPOSE)
QuarterDataFeature$PROPERTY_TYPE <- as.numeric(QuarterDataFeature$PROPERTY_TYPE)
QuarterDataFeature$FIRST_TIME_HOMEBUYER_FLAG <- as.numeric(QuarterDataFeature$FIRST_TIME_HOMEBUYER_FLAG)
QuarterDataFeature$OCCUPANCY_STATUS <- as.numeric((QuarterDataFeature$OCCUPANCY_STATUS))
QuarterDataFeature$CHANNEL <- as.numeric((QuarterDataFeature$CHANNEL))
QuarterDataFeature$PREPAYMENT_PENALTY_MORTGAGE_PPM_FLAG <- as.numeric(QuarterDataFeature$PREPAYMENT_PENALTY_MORTGAGE_PPM_FLAG)


#75% of the Sample Size 
smp_size <- floor(0.75 * nrow(QuarterDataFeature))

set.seed(140)
train_ind <- sample(seq_len(nrow(QuarterDataFeature)),size = smp_size)

#Normalize the Data or Scale the Data
maxs <- apply(QuarterDataFeature, 2, max) 
mins <- apply(QuarterDataFeature, 2, min)

scaled <- as.data.frame(scale(QuarterDataFeature, center = mins, scale = maxs - mins))

#Divide into Training and Testing Data
train <- QuarterDataFeature[train_ind,]
test <- QuarterDataFeature[-train_ind,]

n <- names(train)
f <- as.formula(paste("ORIGINAL_INTEREST_RATE~", paste(n[!n %in% "ORIGINAL_INTEREST_RATE"], collapse = " + ")))

relu <- function(x) sapply(x, function(z) max(0,z))

neuralnet <- neuralnet(f,data=train,hidden=c(5,3),linear.output=T)
neuralnet$result.matrix

plot(neuralnet)

m <- names(test)
test_cols <- m[!m %in% "ORIGINAL_INTEREST_RATE"]
pr.nn <- compute(neuralnet,test[,test_cols])

pr.nn_ <- pr.nn$net.result*(max(QuarterDataFeature$ORIGINAL_INTEREST_RATE)-min(QuarterDataFeature$ORIGINAL_INTEREST_RATE))+min(QuarterDataFeature$ORIGINAL_INTEREST_RATE)

test.r <- (test$ORIGINAL_INTEREST_RATE)*(max(QuarterDataFeature$ORIGINAL_INTEREST_RATE)-min(QuarterDataFeature$ORIGINAL_INTEREST_RATE))+min(QuarterDataFeature$ORIGINAL_INTEREST_RATE)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test)

print(paste(MSE.nn))


print("Performing KNN Algorithm")


#FEATURE ENGINEERING
#create a separate dataframe
QuarterDataFeature <- QuarterData
QuarterDataFeature <- QuarterDataFeature %>% mutate_if(is.character,as.factor)
QuarterDataFeature <- within(QuarterDataFeature, rm("SELLER_NAME","SUPER_CONFORMING_FLAG",
                                                    "POSTAL_CODE","MSA","LOAN_SEQUENCE_NUMBER",
                                                    "SERVICER_NAME","PRODUCT_TYPE","FIRST_PAYMENT_DATE",
                                                    "MATURITY_DATE", "NUMBER_OF_BORROWERS","PREPAYMENT_PENALTY_MORTGAGE_PPM_FLAG",
                                                    "CHANNEL","QuarterDataFeature$ORIGINAL_LOAN_TO_VALUE_LTV",
                                                    "MORTGAGE_INSURANCE_PERCENTAGE_MI"))


summary(QuarterDataFeature)
#create a separate dataframe
QuarterData2Feature <- Quarter2Data
QuarterData2Feature <- QuarterData2Feature %>% mutate_if(is.character,as.factor)
QuarterData2Feature <- within(QuarterData2Feature, rm("SELLER_NAME","SUPER_CONFORMING_FLAG",
                                                      "POSTAL_CODE","MSA","LOAN_SEQUENCE_NUMBER",
                                                      "SERVICER_NAME","PRODUCT_TYPE","FIRST_PAYMENT_DATE",
                                                      "MATURITY_DATE", "NUMBER_OF_BORROWERS","PREPAYMENT_PENALTY_MORTGAGE_PPM_FLAG",
                                                      "CHANNEL","QuarterDataFeature$ORIGINAL_LOAN_TO_VALUE_LTV",
                                                      "MORTGAGE_INSURANCE_PERCENTAGE_MI"))



#Normalized Data
normalized <-  function(x) {
  (x-min(x))/(max(x)-min(x))
}

#Normalize the Data
QuarterDataFeature$CREDIT_SCORE <- normalized(QuarterDataFeature$CREDIT_SCORE)  
QuarterDataFeature$ORIGINAL_UPB <- normalized(QuarterDataFeature$ORIGINAL_UPB)
QuarterDataFeature$ORIGINAL_LOAN_TERM <- normalized(QuarterDataFeature$ORIGINAL_LOAN_TERM)
#QuarterDataFeature$MORTGAGE_INSURANCE_PERCENTAGE_MI <- normalized(QuarterDataFeature$MORTGAGE_INSURANCE_PERCENTAGE_MI)
QuarterDataFeature$ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV <- normalized(QuarterDataFeature$ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV)
QuarterDataFeature$ORIGINAL_DEBT_TO_INCOME_DTI_RATIO <- normalized(QuarterDataFeature$ORIGINAL_DEBT_TO_INCOME_DTI_RATIO)
#QuarterDataFeature$ORIGINAL_LOAN_TO_VALUE_LTV <- normalized(QuarterDataFeature$ORIGINAL_LOAN_TO_VALUE_LTV)

#Convert All the Factors to Numerics
QuarterDataFeature$PROPERTY_STATE <- as.numeric(QuarterDataFeature$PROPERTY_STATE)
QuarterDataFeature$LOAN_PURPOSE <- as.numeric(QuarterDataFeature$LOAN_PURPOSE)
QuarterDataFeature$PROPERTY_TYPE <- as.numeric(QuarterDataFeature$PROPERTY_TYPE)
QuarterDataFeature$FIRST_TIME_HOMEBUYER_FLAG <- as.numeric(QuarterDataFeature$FIRST_TIME_HOMEBUYER_FLAG)
QuarterDataFeature$OCCUPANCY_STATUS <- as.numeric(QuarterDataFeature$OCCUPANCY_STATUS)

#Normalize all the Numerics
QuarterDataFeature$PROPERTY_STATE <- normalized(QuarterDataFeature$PROPERTY_STATE)
QuarterDataFeature$LOAN_PURPOSE <- normalized(QuarterDataFeature$LOAN_PURPOSE)
QuarterDataFeature$PROPERTY_TYPE <- normalized(QuarterDataFeature$PROPERTY_TYPE)
QuarterDataFeature$FIRST_TIME_HOMEBUYER_FLAG <- normalized(QuarterDataFeature$FIRST_TIME_HOMEBUYER_FLAG)

#Normalize the Data
QuarterData2Feature$CREDIT_SCORE <- normalized(QuarterData2Feature$CREDIT_SCORE)  
QuarterData2Feature$ORIGINAL_UPB <- normalized(QuarterData2Feature$ORIGINAL_UPB)
QuarterData2Feature$ORIGINAL_LOAN_TERM <- normalized(QuarterData2Feature$ORIGINAL_LOAN_TERM)
#QuarterData2Feature$MORTGAGE_INSURANCE_PERCENTAGE_MI <- normalized(QuarterData2Feature$MORTGAGE_INSURANCE_PERCENTAGE_MI)
QuarterData2Feature$ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV <- normalized(QuarterData2Feature$ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV)
QuarterData2Feature$ORIGINAL_DEBT_TO_INCOME_DTI_RATIO <- normalized(QuarterData2Feature$ORIGINAL_DEBT_TO_INCOME_DTI_RATIO)
#QuarterData2Feature$ORIGINAL_LOAN_TO_VALUE_LTV <- normalized(QuarterData2Feature$ORIGINAL_LOAN_TO_VALUE_LTV)

#Convert All the Factors to Numerics
QuarterData2Feature$PROPERTY_STATE <- as.numeric(QuarterData2Feature$PROPERTY_STATE)
QuarterData2Feature$LOAN_PURPOSE <- as.numeric(QuarterData2Feature$LOAN_PURPOSE)
QuarterData2Feature$PROPERTY_TYPE <- as.numeric(QuarterData2Feature$PROPERTY_TYPE)
QuarterData2Feature$FIRST_TIME_HOMEBUYER_FLAG <- as.numeric(QuarterData2Feature$FIRST_TIME_HOMEBUYER_FLAG)
QuarterData2Feature$OCCUPANCY_STATUS <- as.numeric(QuarterData2Feature$OCCUPANCY_STATUS)

#Normalize all the Numerics
QuarterData2Feature$PROPERTY_STATE <- normalized(QuarterData2Feature$PROPERTY_STATE)
QuarterData2Feature$LOAN_PURPOSE <- normalized(QuarterData2Feature$LOAN_PURPOSE)
QuarterData2Feature$PROPERTY_TYPE <- normalized(QuarterData2Feature$PROPERTY_TYPE)
QuarterData2Feature$FIRST_TIME_HOMEBUYER_FLAG <- normalized(QuarterData2Feature$FIRST_TIME_HOMEBUYER_FLAG)

# MODEL THE DATA
#75% of the Sample Size 
#smp_size <- floor(0.75 * nrow(QuarterDataFeature))

#set.seed(140)
#train_ind <- sample(seq_len(nrow(QuarterDataFeature)),size = smp_size)

#Divide into Training and Testing Data
#train <- QuarterDataFeature[train_ind,]
#test <- QuarterDataFeature[-train_ind,]
train <- QuarterDataFeature
test <- QuarterData2Feature

summary(train)

KNN_reg <- knn.reg(train= train, test=test, y=train$ORIGINAL_INTEREST_RATE, k=3,algorithm = c("brute") )

preds = as.data.frame(KNN_reg$pred)

print (summary(KNN_reg))

saveRDS(KNN_reg, "KNN_reg.rds")
my_model <- readRDS("KNN_reg.rds")














