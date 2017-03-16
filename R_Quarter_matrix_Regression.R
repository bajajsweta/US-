@@ -1,36 +1,77 @@
  #Install Packages in R
install.packages("data.table")
install.packages("forecast")
install.packages("leaps")
#install.packages("forecast")
install.packages("leaps",repos='https://CRAN.R-project.org')
install.packages("dplyr")

getwd()


#Import the Libraries
library(data.table)
library(forecast)
#library(forecast)
library(leaps)
library(dplyr)

print("Loading Data...")
print("Loading Data for Quarter2005")

#Read the File from the Current Directory
QuarterData <- fread("Sample_Original_Validated_ata1_Q42013.csv", sep = ',')

QuarterData2 <- fread("Sample_Original_Validated_ata1_Q12014.csv.csv", sep = ',')
print (head(QuarterData))
print (head(QuarterData2))

print("Loaded Data")

print("Performing Variable Selection")
#Writen to Accept the System Args from the Console
args <- commandArgs(trailingOnly = TRUE)
n <- as.numeric(args[1])


#FEATURE ENGINEERING
#create a separate dataframe
QuarterDataFeature <- QuarterData
QuarterDataFeature <- QuarterDataFeature %>% mutate_if(is.character,as.factor)
QuarterDataFeature <- within(QuarterDataFeature, rm("SELLER_NAME","SUPER_CONFORMING_FLAG",
                                                    "POSTAL_CODE","MSA","LOAN_SEQUENCE_NUMBER",
                                                    "SERVICER_NAME","PRODUCT_TYPE","FIRST_PAYMENT_DATE",
                                                    "MATURITY_DATE", "NUMBER_OF_BORROWERS"))

QuarterDataFeature2 <- QuarterData2
QuarterDataFeature2 <- QuarterDataFeature2 %>% mutate_if(is.character,as.factor)
QuarterDataFeature2 <- within(QuarterDataFeature2, rm("SELLER_NAME","SUPER_CONFORMING_FLAG",
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

QuarterDataFeature2$CREDIT_SCORE <- normalized(QuarterDataFeature2$CREDIT_SCORE)  
QuarterDataFeature2$ORIGINAL_UPB <- normalized(QuarterDataFeature2$ORIGINAL_UPB)
QuarterDataFeature2$ORIGINAL_LOAN_TERM <- normalized(QuarterDataFeature2$ORIGINAL_LOAN_TERM)
QuarterDataFeature2$MORTGAGE_INSURANCE_PERCENTAGE_MI <- normalized(QuarterDataFeature2$MORTGAGE_INSURANCE_PERCENTAGE_MI)
QuarterDataFeature2$ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV <- normalized(QuarterDataFeature2$ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV)
QuarterDataFeature2$ORIGINAL_DEBT_TO_INCOME_DTI_RATIO <- normalized(QuarterDataFeature2$ORIGINAL_DEBT_TO_INCOME_DTI_RATIO)
QuarterDataFeature2$ORIGINAL_LOAN_TO_VALUE_LTV <- normalized(QuarterDataFeature2$ORIGINAL_LOAN_TO_VALUE_LTV)

#Convert All the Factors to Numerics
QuarterDataFeature$PROPERTY_STATE <- as.numeric(QuarterDataFeature$PROPERTY_STATE)
QuarterDataFeature$LOAN_PURPOSE <- as.numeric(QuarterDataFeature$LOAN_PURPOSE)
QuarterDataFeature$PROPERTY_TYPE <- as.numeric(QuarterDataFeature$PROPERTY_TYPE)
QuarterDataFeature$FIRST_TIME_HOMEBUYER_FLAG <- as.numeric(QuarterDataFeature$FIRST_TIME_HOMEBUYER_FLAG)

QuarterDataFeature2$PROPERTY_STATE <- as.numeric(QuarterDataFeature2$PROPERTY_STATE)
QuarterDataFeature2$LOAN_PURPOSE <- as.numeric(QuarterDataFeature2$LOAN_PURPOSE)
QuarterDataFeature2$PROPERTY_TYPE <- as.numeric(QuarterDataFeature2$PROPERTY_TYPE)
QuarterDataFeature2$FIRST_TIME_HOMEBUYER_FLAG <- as.numeric(QuarterDataFeature2$FIRST_TIME_HOMEBUYER_FLAG)

#Normalize all the Numerics
QuarterDataFeature$PROPERTY_STATE <- normalized(QuarterDataFeature$PROPERTY_STATE)
QuarterDataFeature$LOAN_PURPOSE <- normalized(QuarterDataFeature$LOAN_PURPOSE)
QuarterDataFeature$PROPERTY_TYPE <- normalized(QuarterDataFeature$PROPERTY_TYPE)
QuarterDataFeature$FIRST_TIME_HOMEBUYER_FLAG <- normalized(QuarterDataFeature$FIRST_TIME_HOMEBUYER_FLAG)

QuarterDataFeature2$PROPERTY_STATE <- normalized(QuarterDataFeature2$PROPERTY_STATE)
QuarterDataFeature2$LOAN_PURPOSE <- normalized(QuarterDataFeature2$LOAN_PURPOSE)
QuarterDataFeature2$PROPERTY_TYPE <- normalized(QuarterDataFeature2$PROPERTY_TYPE)
QuarterDataFeature2$FIRST_TIME_HOMEBUYER_FLAG <- normalized(QuarterDataFeature2$FIRST_TIME_HOMEBUYER_FLAG)

# VARIABLE SELECTION
# Four algorithms exhaustive search, forward, backward selection, stepwise regression are explored here
#print("Forward Search")

# Forward Search
# nvmax is the maximum size of subsets
#regfit_forward = regsubsets(ORIGINAL_INTEREST_RATE~., data = QuarterDataFeature, really.big=TRUE, method="forward")
#regsummary_forward = summary(regfit_forward)

#print(regsummary_forward$adjr2)
#print(regsummary_forward$rss)
#regsummary_forward$adjr2
#regsummary_forward$rss

#print(coef(regfit_forward,8))
#coef(regfit_forward,8)

#plot(regfit_forward,scale="r2") 
#plot(regsummary_forward$rss, xlab='Number of variables',ylab='RSS', type='l',col="green") + title ("Forward")
#@@ -38,15 +79,14 @@ plot(regsummary_forward$adjr2, xlab='Number of variables',ylab='Adjr2', type='l'
 #                        plot(regsummary_forward$bic, xlab='Number of variables',ylab='BIC', type='l',col="green") + title ("Forward")
  #                       plot(regsummary_forward$cp, xlab='Number of variables',ylab='CP', type='l',col="green") + title ("Forward")
                         
   #                      print("Backward Search")
                         # Backward Search
    #                     regfit_backward = regsubsets(ORIGINAL_INTEREST_RATE~., data = QuarterDataFeature, really.big=TRUE, method="backward")
     #                    regsummary_backward = summary(regfit_backward)
                         
      #                   print(regsummary_backward$adjr2)
       #                  print(regsummary_backward$rss)
        #                 regsummary_backward$adjr2
         #                regsummary_backward$rss
                         
          #               print(coef(regfit_backward,8))
           #              coef(regfit_backward,8)
                         
            #             plot(regfit_backward,scale="r2") 
             #            plot(regsummary_backward$rss, xlab='Number of variables',ylab='RSS', type='l',col="blue") + title ("Backward")
              #           @@ -54,17 +94,14 @@ plot(regsummary_backward$adjr2, xlab='Number of variables',ylab='Adjr2', type='l
               #                                   plot(regsummary_backward$bic, xlab='Number of variables',ylab='BIC', type='l',col="blue") + title ("Backward")
                #                                  plot(regsummary_backward$cp, xlab='Number of variables',ylab='CP', type='l',col="blue") + title ("Backward")
                                                  
                                                  
                 #                                 print("Exhaustive Search")
                                                  
                                                  # Exhaustive Search
                  #                                regfit_exhaustive = regsubsets(ORIGINAL_INTEREST_RATE~., data = QuarterDataFeature, really.big=TRUE)
                   #                               regsummary_exhaustive = summary(regfit_exhaustive)
                                                  
                    #                              print(regsummary_exhaustive$adjr2)
                     #                             print(regsummary_exhaustive$rss)
                      #                            regsummary_exhaustive$adjr2
                       #                           regsummary_exhaustive$rss
                                                  
                        #                          print(coef(regfit_exhaustive,8))
                         #                         coef(regfit_exhaustive,8)
                          #                        
                           #                       plot(regfit_exhaustive,scale="r2") 
                            #                      plot(regsummary_exhaustive$rss, xlab='Number of variables',ylab='RSS', type='l',col="red") + title ("Exhaustive")
                             #                     @@ -73,26 +110,6 @@ plot(regsummary_exhaustive$bic, xlab='Number of variables',ylab='BIC', type='l',
                              #                    plot(regsummary_exhaustive$cp, xlab='Number of variables',ylab='CP', type='l',col="red") + title ("Exhaustive")
                                                  
                                                  
                               #                   print("Feature Selection")
                                                  #FEATURE ENGINEERING
                                                  #create a separate dataframe
                                                  QuarterDataFeature <- QuarterData
                                                  QuarterDataFeature2 <- QuarterData2
                                                  print("Convert to Factors")
                                                  QuarterDataFeature <- QuarterDataFeature %>% mutate_if(is.character,as.factor)
                                                  QuarterDataFeature2 <- QuarterDataFeature2 %>% mutate_if(is.character,as.factor)
                                                  print("Remove certain Factors")
                                                  QuarterDataFeature <- within(QuarterDataFeature, rm("SELLER_NAME","SUPER_CONFORMING_FLAG",
                                                  "POSTAL_CODE","MSA","LOAN_SEQUENCE_NUMBER",
                                                  "SERVICER_NAME","PRODUCT_TYPE","FIRST_PAYMENT_DATE",
                                                  "MATURITY_DATE", "NUMBER_OF_BORROWERS"))
                                                  QuarterDataFeature2 <- within(QuarterDataFeature2, rm("SELLER_NAME","SUPER_CONFORMING_FLAG",
                                                                                                      "POSTAL_CODE","MSA","LOAN_SEQUENCE_NUMBER",
                                                                                                      "SERVICER_NAME","PRODUCT_TYPE","FIRST_PAYMENT_DATE",
                                                                                                      "MATURITY_DATE", "NUMBER_OF_BORROWERS"))
                                                  
                                                  
                                                  print("Convert to Numeric for Regressing")
                                                  #Convert All the Factors to Numerics
                                                  QuarterDataFeature$PROPERTY_STATE <- as.numeric(QuarterDataFeature$PROPERTY_STATE)
                                                  QuarterDataFeature$LOAN_PURPOSE <- as.numeric(QuarterDataFeature$LOAN_PURPOSE)
                                                  QuarterDataFeature$PROPERTY_TYPE <- as.numeric(QuarterDataFeature$PROPERTY_TYPE)
                                                  QuarterDataFeature$FIRST_TIME_HOMEBUYER_FLAG <- as.numeric(QuarterDataFeature$FIRST_TIME_HOMEBUYER_FLAG)
                                                  
                                                  QuarterDataFeature2$PROPERTY_STATE <- as.numeric(QuarterDataFeature2$PROPERTY_STATE)
                                                  QuarterDataFeature2$LOAN_PURPOSE <- as.numeric(QuarterDataFeature2$LOAN_PURPOSE)
                                                  QuarterDataFeature2$PROPERTY_TYPE <- as.numeric(QuarterDataFeature2$PROPERTY_TYPE)
                                                  QuarterDataFeature2$FIRST_TIME_HOMEBUYER_FLAG <- as.numeric(QuarterDataFeature2$FIRST_TIME_HOMEBUYER_FLAG)
                                                  
                                                  
                                                  # MODEL THE DATA
                                                  #75% of the Sample Size 
                                                  smp_size <- floor(0.75 * nrow(QuarterDataFeature))
                                                  @@ -100,19 +117,25 @@ smp_size <- floor(0.75 * nrow(QuarterDataFeature))
                                                  set.seed(140)
                                                  train_ind <- sample(seq_len(nrow(QuarterDataFeature)),size = smp_size)
                                                  
                                                  smp_size2 <- floor(0.75 * nrow(QuarterDataFeature2))
                                                  @@ -100,19 +117,25 @@ smp_size <- floor(0.75 * nrow(QuarterDataFeature2))
                                                  set.seed(140)
                                                  train_ind2 <- sample(seq_len(nrow(QuarterDataFeature2)),size = smp_size)
                                                  
                                                  
                                                  
                                                  print("Divide into Training and Testing Data")
                                                  #Divide into Training and Testing Data
                                                  train <- QuarterDataFeature[train_ind,]
                                                  test <- QuarterDataFeature[-train_ind,]
                                                  
                                                  train2 <- QuarterDataFeature[train_ind2,]
                                                  test2 <- QuarterDataFeature[-train_ind2,]
                                                  
                                                  print("Use the lm Function to Apply Linear Regression")
                                                  # Use the lm Function to Apply Linear Regression
                                                  lm.fit = lm(ORIGINAL_INTEREST_RATE~CREDIT_SCORE+ORIGINAL_UPB+ORIGINAL_LOAN_TERM+MORTGAGE_INSURANCE_PERCENTAGE_MI+
                                                  ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV+ORIGINAL_DEBT_TO_INCOME_DTI_RATIO+PROPERTY_STATE+ORIGINAL_LOAN_TO_VALUE_LTV+
                                                  LOAN_PURPOSE+PROPERTY_TYPE+FIRST_TIME_HOMEBUYER_FLAG+NUMBER_OF_UNITS, 
                                                  ORIGINAL_COMBINED_LOAN_TO_VALUE_CLTV+ORIGINAL_DEBT_TO_INCOME_DTI_RATIO+PROPERTY_STATE+ORIGINAL_LOAN_TO_VALUE_LTV+
                                                  LOAN_PURPOSE+PROPERTY_TYPE+FIRST_TIME_HOMEBUYER_FLAG+NUMBER_OF_UNITS, 
                                                  data = train)
                                                  
                                                  summary(lm.fit)
                                                  pred = predict(lm.fit, test2)
                                                  accuracy(pred, train2$ORIGINAL_INTEREST_RATE)
                                                  
                                                  print(accuracy(pred, train2$ORIGINAL_INTEREST_RATE))
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  