#Install Packages in R
install.packages("data.table")
install.packages("forecast")
install.packages("leaps")
install.packages("dplyr")

#Import the Libraries
library(data.table)
library(forecast)
library(leaps)
library(dplyr)

print("Loading Data...")

#Read the File from the Current Directory
QuarterData <- fread("Quarter2005.csv", sep = ',')
print (head(QuarterData))

print("Loaded Data")

print("Performing Variable Selection")
# VARIABLE SELECTION
# Four algorithms exhaustive search, forward, backward selection, stepwise regression are explored here
print("Forward Search")
# Forward Search
# nvmax is the maximum size of subsets
regfit_forward = regsubsets(ORIGINAL_INTEREST_RATE~., data = QuarterDataFeature, really.big=TRUE, method="forward")
regsummary_forward = summary(regfit_forward)

print(regsummary_forward$adjr2)
print(regsummary_forward$rss)

print(coef(regfit_forward,8))

plot(regfit_forward,scale="r2") 
plot(regsummary_forward$rss, xlab='Number of variables',ylab='RSS', type='l',col="green") + title ("Forward")
plot(regsummary_forward$adjr2, xlab='Number of variables',ylab='Adjr2', type='l',col="green") + title ("Forward")
plot(regsummary_forward$bic, xlab='Number of variables',ylab='BIC', type='l',col="green") + title ("Forward")
plot(regsummary_forward$cp, xlab='Number of variables',ylab='CP', type='l',col="green") + title ("Forward")

print("Backward Search")
# Backward Search
regfit_backward = regsubsets(ORIGINAL_INTEREST_RATE~., data = QuarterDataFeature, really.big=TRUE, method="backward")
regsummary_backward = summary(regfit_backward)

print(regsummary_backward$adjr2)
print(regsummary_backward$rss)

print(coef(regfit_backward,8))

plot(regfit_backward,scale="r2") 
plot(regsummary_backward$rss, xlab='Number of variables',ylab='RSS', type='l',col="blue") + title ("Backward")
plot(regsummary_backward$adjr2, xlab='Number of variables',ylab='Adjr2', type='l',col="blue") + title ("Backward")
plot(regsummary_backward$bic, xlab='Number of variables',ylab='BIC', type='l',col="blue") + title ("Backward")
plot(regsummary_backward$cp, xlab='Number of variables',ylab='CP', type='l',col="blue") + title ("Backward")


print("Exhaustive Search")

# Exhaustive Search
regfit_exhaustive = regsubsets(ORIGINAL_INTEREST_RATE~., data = QuarterDataFeature, really.big=TRUE)
regsummary_exhaustive = summary(regfit_exhaustive)

print(regsummary_exhaustive$adjr2)
print(regsummary_exhaustive$rss)

print(coef(regfit_exhaustive,8))

plot(regfit_exhaustive,scale="r2") 
plot(regsummary_exhaustive$rss, xlab='Number of variables',ylab='RSS', type='l',col="red") + title ("Exhaustive")
plot(regsummary_exhaustive$adjr2, xlab='Number of variables',ylab='Adjr2', type='l',col="red") + title ("Exhaustive")
plot(regsummary_exhaustive$bic, xlab='Number of variables',ylab='BIC', type='l',col="red") + title ("Exhaustive")
plot(regsummary_exhaustive$cp, xlab='Number of variables',ylab='CP', type='l',col="red") + title ("Exhaustive")


print("Feature Selection")
#FEATURE ENGINEERING
#create a separate dataframe
QuarterDataFeature <- QuarterData
print("Convert to Factors")
QuarterDataFeature <- QuarterDataFeature %>% mutate_if(is.character,as.factor)
print("Remove certain Factors")
QuarterDataFeature <- within(QuarterDataFeature, rm("SELLER_NAME","SUPER_CONFORMING_FLAG",
                                                    "POSTAL_CODE","MSA","LOAN_SEQUENCE_NUMBER",
                                                    "SERVICER_NAME","PRODUCT_TYPE","FIRST_PAYMENT_DATE",
                                                    "MATURITY_DATE", "NUMBER_OF_BORROWERS"))

print("Convert to Numeric for Regressing")
#Convert All the Factors to Numerics
QuarterDataFeature$PROPERTY_STATE <- as.numeric(QuarterDataFeature$PROPERTY_STATE)
QuarterDataFeature$LOAN_PURPOSE <- as.numeric(QuarterDataFeature$LOAN_PURPOSE)
QuarterDataFeature$PROPERTY_TYPE <- as.numeric(QuarterDataFeature$PROPERTY_TYPE)
QuarterDataFeature$FIRST_TIME_HOMEBUYER_FLAG <- as.numeric(QuarterDataFeature$FIRST_TIME_HOMEBUYER_FLAG)


# MODEL THE DATA
#75% of the Sample Size 
smp_size <- floor(0.75 * nrow(QuarterDataFeature))

set.seed(140)
train_ind <- sample(seq_len(nrow(QuarterDataFeature)),size = smp_size)

print("Divide into Training and Testing Data")
train <- QuarterDataFeature[train_ind,]
test <- QuarterDataFeature[-train_ind,]

print("Use the lm Function to Apply Linear Regression")
lm.fit = lm(ORIGINAL_INTEREST_RATE~CREDIT_SCORE+ORIGINAL_UPB+ORIGINAL_LOAN_TERM+MORTGAGE_INSURANCE_PERCENTAGE_MI+
            ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV+ORIGINAL_DEBT_TO_INCOME_DTI_RATIO+PROPERTY_STATE+ORIGINAL_LOAN_TO_VALUE_LTV+
            LOAN_PURPOSE+PROPERTY_TYPE+FIRST_TIME_HOMEBUYER_FLAG+NUMBER_OF_UNITS, 
            data = train)

summary(lm.fit)
pred = predict(lm.fit, test)
accuracy(pred, train$ORIGINAL_INTEREST_RATE)

print(accuracy(pred, train$ORIGINAL_INTEREST_RATE))

