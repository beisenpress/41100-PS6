# 41100 PS5

setwd("/Users/ben/dropbox/Chicago Booth/41100 Regressions/Homework 6")

##################### Question 1 ###########################
# 1 Community Crime
# The file CommunityCrime.csv has violent crime rates for 1994 communities across the US and 25
# descriptive variables. Our goal is to find a good-performing parsimonious model to predict the 
# log crime rate. The demographic variables include:

# • householdsize: mean people per household
# • PctUnemployed: % of people 16 and over unemployed
# • PctFam2Par: % of families (w/ kids) having two parents • PctRecentImmig: % immigrated within 
# 3 years
# • PctHousOccup: % of housing occupied
# • RentMedian: rental housing – median rent
# • PctUsePubTrans: % of people who use public transit

# The rest of the 25 variables are similar, and can be interpreted from their names in the data.

# First split the data into training and testing samples (size and sampling scheme is up to you).

# Import data
crime.0 <- read.csv("CommunityCrime.csv")

# Add a variable for the log of the Violent Crime Rate
crime.0$LogViolentCR <- log(crime.0$ViolentCR)

# Drop the community name and non-logged ViolentCR variables
crime.1 <- crime.0[,c(-1,-2)]

# Set seed so the results are replicable 
set.seed(9)

# Select a random sample of rows
samples <- sort(sample.int(nrow(crime.1), 0.80*nrow(crime.1)))

# Subset the data into training and test datasets.
crime.train <- crime.1[samples,] 
crime.test <- crime.1[-samples,]

# (a) Using the training data, build a model for log crime rate by using forward stepwise selection 
# guided by both AIC and BIC to search through all main effects.

# Create a null model with no independent variables
null <- lm(LogViolentCR ~ 1, data=crime.train)

# Create a full model with all the main effects. 
full <- lm(LogViolentCR ~ ., data=crime.train)

# Create a model using forward stepwise selection
crime.reg.AIC1 <- step(null, scope=formula(full), direction="forward", k=2)
crime.reg.BIC1 <- step(null, scope=formula(full), direction="forward", k=log(nrow(crime.train)))


# (b) Redo (a) allowing for all possible interactions. What has changed?

# Create a full model with all the main effects and interactions
full2 <- lm(LogViolentCR ~ . + .^2, data=crime.train)

# Create a model using forward stepwise selection
crime.reg.AIC2 <- step(null, scope=formula(full2), direction="forward", k=2)
crime.reg.BIC2 <- step(null, scope=formula(full2), direction="forward", k=log(nrow(crime.train)))

# (c) Still within the training data, use the LASSO to select a model from all main effects and
# interactions.

# install.packages("glmnet") #just run this once (on each computer)
library(glmnet)

# Create a dataset without the independent variable
# Use the full data, rather than training.  We will select the training data later
crime.2 <- crime.1[,c(-26)]

X <- model.matrix(~.^2, crime.2) 
#need to subtract the intercept
X <- X[,-1]

cvfit <- cv.glmnet(x = X[samples,], y = crime.train$LogViolentCR, family="gaussian", alpha=1, standardize=FALSE)

betas <- coef(cvfit, s = "lambda.1se")
model <- which(betas[2:length(betas)]!=0)
#we need to EXCLUDE the intercept, because glmnet and lm() both put one in automatically


#re-run regular linear model with the selected variables
crime.reg.lasso <- lm(crime.train$LogViolentCR ~ X[,model])

summary(crime.reg.lasso)


# (d) Compute the BIC-based model probabilities for all the models found thus far, and the 
# model which includes all main effects. Plot the fit for each model against the true value of 
# log crime rate.

# Put the BIC values for all the regressions into a vector
# The syntax c(A=1,B=2) will return a list of named numbers (i.e. 1 is named A, 2 is named B)

BIC <- c(crime.reg.AIC1=extractAIC(crime.reg.AIC1, k=log(nrow(crime.train)))[2],
         crime.reg.BIC1=extractAIC(crime.reg.BIC1, k=log(nrow(crime.train)))[2],
         crime.reg.AIC2=extractAIC(crime.reg.AIC2, k=log(nrow(crime.train)))[2],
         crime.reg.BIC2=extractAIC(crime.reg.BIC2, k=log(nrow(crime.train)))[2],
         crime.reg.lasso=extractAIC(crime.reg.lasso, k=log(nrow(crime.train)))[2])
BIC

# Apply the formula e^((-1/2)*BIC) to each element of the array. 
eBIC <- exp(-0.5*(BIC-min(BIC)))
eBIC

# Calculate the probabliliy by dividing each eBIC by the sum of all eBIC values
probs <- eBIC/sum(eBIC)
round(probs, 5)

# BIC says the regression from BIC forward stepwise selection has a 99% change of being right

# (e) Use the test data to compare out-of-sample MSE performance. Compare your results with what 
# you found in (d).

# Calculate errors for all but LASSO using the predict function
crime.error.AIC1 <- predict(crime.reg.AIC1, newdata=crime.test)-crime.test$LogViolentCR
crime.error.BIC1 <- predict(crime.reg.BIC1, newdata=crime.test)-crime.test$LogViolentCR
crime.error.AIC2 <- predict(crime.reg.AIC2, newdata=crime.test)-crime.test$LogViolentCR
crime.error.BIC2 <- predict(crime.reg.BIC2, newdata=crime.test)-crime.test$LogViolentCR

# Creat a data frame to to make predictions with LASSO
refitting.data <- data.frame(crime.1$LogViolentCR, X[,model])
names(refitting.data)[1] <- "LogViolentCR"

# Calculate errors for LASSO using predict and the data frame
crime.error.lasso <- predict(crime.reg.lasso, newdata=refitting.data[-samples,])-crime.test$LogViolentCR


#comparing the errors
c(crime.mse.AIC1=mean(crime.error.AIC1^2), crime.mse.BIC1=mean(crime.error.BIC1^2), 
  crime.mse.AIC2=mean(crime.error.AIC2^2), crime.mse.BIC2=mean(crime.error.BIC2^2), 
  crime.mse.lasso=mean(crime.error.lasso^2))

# The regression using AIC on interactions has the lowest MSE.

# (f) Use cv.glmnet to fit Ridge regression on all main effects and interactions. Compute the 
# out- of-sample MSE and compare to (e). (You may want to consult ?predict.cv.glmnet.)

# Do cv.glmnet with alpha = 0 for ridge regression
cvfit2 <- cv.glmnet(x = X[samples,], y = crime.train$LogViolentCR, family="gaussian", alpha=0, standardize=FALSE)

betas2 <- coef(cvfit2, s = "lambda.1se")
model2 <- which(betas2[2:length(betas2)]!=0)
#we need to EXCLUDE the intercept, because glmnet and lm() both put one in automatically


#re-run regular linear model with the selected variables
crime.reg.ridge <- lm(crime.train$LogViolentCR ~ X[,model])

summary(crime.reg.ridge)

# Get prediction errors
crime.error.ridge <- predict(crime.reg.ridge, newdata=refitting.data[-samples,])-crime.test$LogViolentCR

#comparing the errors
c(crime.mse.AIC1=mean(crime.error.AIC1^2), crime.mse.BIC1=mean(crime.error.BIC1^2), 
  crime.mse.AIC2=mean(crime.error.AIC2^2), crime.mse.BIC2=mean(crime.error.BIC2^2), 
  crime.mse.lasso=mean(crime.error.lasso^2), crime.mse.ridge=mean(crime.error.ridge^2))