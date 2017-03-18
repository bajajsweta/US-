#Install Packages in R
install.packages("data.table")
install.packages("forecast")
install.packages("leaps")
install.packages("dplyr")
install.packages("neuralnet")


#Import the Libraries
library(data.table)
library(forecast)
library(leaps)
library(dplyr)
library(neuralnet)

QuarterData <- fread("Sample_Original_Validated_ata1_Q11999.csv", sep = ',')

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

