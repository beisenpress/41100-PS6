# 41100 PS6

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

# Create a dataset without the independent variable.
# Use the full data, rather than training.  We will select the training data later
crime.2 <- crime.1[,c(-26)]

# The model.matrix command creates a matrix of all possible interactions.
# Run model.matrix on the dataset without the independent varaible.
X <- model.matrix(~.^2, crime.2) 

# Remove the interctpt from the matrix. The cv.glmnet command will create an intercept later.
X <- X[,-1]

# Use cv.glmnet to run the LASSO. Only run it on the training data.
cvfit.lasso <- cv.glmnet(x = X[samples,], y = crime.train$LogViolentCR, family="gaussian", alpha=1, standardize=FALSE)

# Extract the Beta coefficients from the LASSO results.
# Two options for the "s" paramater are lambda.1se" and "lambda.min" The lecture did not cover the difference.
betas.lasso.1se <- coef(cvfit.lasso, s = "lambda.1se")

# Get the column numbers of the non-zero beta coefficients.
# Again, EXCLUDE the intercept. We just want the dependent variables. lm() will add an intercept later.
# The code [2:length(betas.lasso.1se)] excludes the intercept because the intercept is the first element of betas.lasso.1se.
model.lasso.1se <- which(betas.lasso.1se[2:length(betas.lasso.1se)]!=0)

# Create a data frame containing the variables selected by LASSO plus the independent variable.
# Note this is an extra step relative to the lecture code.
# This approach lets us calculate MSE later.
lasso.data.1se <- data.frame(crime.1$LogViolentCR, X[,model.lasso.1se])

# Rename the indendent variable
names(lasso.data.1se)[1] <- "LogViolentCR"

# Run a normal regression using the dataset that contains only the LASSO variables.
# Only use the training data rows for the regression.
crime.reg.lasso.1se <- lm(LogViolentCR ~ ., data=lasso.data.1se[samples,])

summary(crime.reg.lasso.1se)


# (d) Compute the BIC-based model probabilities for all the models found thus far, and the 
# model which includes all main effects. Plot the fit for each model against the true value of 
# log crime rate.

# Put the BIC values for all the regressions into a vector
# The syntax c(A=1,B=2) will return a list of named numbers (i.e. 1 is named A, 2 is named B)

BIC <- c(crime.reg.AIC1=extractAIC(crime.reg.AIC1, k=log(nrow(crime.train)))[2],
         crime.reg.BIC1=extractAIC(crime.reg.BIC1, k=log(nrow(crime.train)))[2],
         crime.reg.AIC2=extractAIC(crime.reg.AIC2, k=log(nrow(crime.train)))[2],
         crime.reg.BIC2=extractAIC(crime.reg.BIC2, k=log(nrow(crime.train)))[2],
         crime.reg.lasso.1se=extractAIC(crime.reg.lasso.1se, k=log(nrow(crime.train)))[2])
BIC

# Apply the formula e^((-1/2)*BIC) to each element of the array. 
eBIC <- exp(-0.5*(BIC-min(BIC)))

# Calculate the probabliliy by dividing each eBIC by the sum of all eBIC values
probs <- eBIC/sum(eBIC)
round(probs, 5)

# BIC says the regression from BIC forward stepwise selection has a 99% change of being right

# (e) Use the test data to compare out-of-sample MSE performance. Compare your results with what 
# you found in (d).

# Calculate errors using the predict function and the test data
crime.error.AIC1 <- predict(crime.reg.AIC1, newdata=crime.test)-crime.test$LogViolentCR
crime.error.BIC1 <- predict(crime.reg.BIC1, newdata=crime.test)-crime.test$LogViolentCR
crime.error.AIC2 <- predict(crime.reg.AIC2, newdata=crime.test)-crime.test$LogViolentCR
crime.error.BIC2 <- predict(crime.reg.BIC2, newdata=crime.test)-crime.test$LogViolentCR
crime.error.lasso.1se <- predict(crime.reg.lasso.1se, newdata=lasso.data.1se[-samples,])-crime.test$LogViolentCR

#comparing the errors
MSE <- c(crime.mse.AIC1=mean(crime.error.AIC1^2), crime.mse.BIC1=mean(crime.error.BIC1^2), 
  crime.mse.AIC2=mean(crime.error.AIC2^2), crime.mse.BIC2=mean(crime.error.BIC2^2), 
  crime.mse.lasso.1se=mean(crime.error.lasso.1se^2))

round(MSE, 5)

# The regression using AIC on interactions has the lowest MSE.

# (f) Use cv.glmnet to fit Ridge regression on all main effects and interactions. Compute the 
# out- of-sample MSE and compare to (e). (You may want to consult ?predict.cv.glmnet.)

# Use cv.glmnet with alpha = 0 for ridge regression
cvfit.ridge <- cv.glmnet(x = X[samples,], y = crime.train$LogViolentCR, family="gaussian", alpha=0, standardize=FALSE)

# Calculate errors using the predict function and the test data
crime.error.ridge.1se <- predict(cvfit.ridge, newx = X[-samples,], s = "lambda.1se") - crime.test$LogViolentCR

#comparing the errors
MSE <- c(crime.mse.AIC1=mean(crime.error.AIC1^2), crime.mse.BIC1=mean(crime.error.BIC1^2), 
         crime.mse.AIC2=mean(crime.error.AIC2^2), crime.mse.BIC2=mean(crime.error.BIC2^2), 
         crime.mse.lasso.1se=mean(crime.error.lasso.1se^2), 
         crime.mse.ridge.1se=mean(crime.error.ridge.1se^2))

round(MSE, 5)

# AIC with interactions still wins.