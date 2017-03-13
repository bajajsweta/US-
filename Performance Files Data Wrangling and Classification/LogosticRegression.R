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

print("Loading Data for Quarter2005")

#Read the File from the Current Directory
QuarterData <- fread("Quarter2005.csv", sep = ',')

#Writen to Accept the System Args from the Console
args <- commandArgs(trailingOnly = TRUE)
n <- as.numeric(args[1])

#FEATURE ENGINEERING
#create a separate dataframe
QuarterDataFeature <- QuarterData
QuarterDataFeature <- QuarterDataFeature %>% mutate_if(is.character,as.factor)

# MODEL THE DATA
#75% of the Sample Size 
smp_size <- floor(0.75 * nrow(QuarterDataFeature))

set.seed(140)
train_ind <- sample(seq_len(nrow(QuarterDataFeature)),size = smp_size)

#Divide into Training and Testing Data
train <- QuarterDataFeature[train_ind,]
test <- QuarterDataFeature[-train_ind,]


# Use the lm Function to Apply Linear Regression
glm.fit = glm(ORIGINAL_INTEREST_RATE~CREDIT_SCORE+ORIGINAL_LOAN_TERM+
              +PROPERTY_STATE+LOAN_PURPOSE+PROPERTY_TYPE+FIRST_TIME_HOMEBUYER_FLAG+NUMBER_OF_UNITS, 
              data = train, family=binomial(link="logit"))
summary(glm.fit)