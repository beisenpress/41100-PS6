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
# Remove the non-logged version of Violent CR as well as the community name varialbe
full <- lm(LogViolentCR ~ . - communityname - ViolentCR, data=crime.train)

# Create a model using forward stepwise selection
crime.reg.AIC1 <- step(null, scope=formula(full), direction="forward", k=2)
crime.reg.BIC1 <- step(null, scope=formula(full), direction="forward", k=log(nrow(crime.train)))



# (b) Redo (a) allowing for all possible interactions. What has changed?

# Create a full model with all the main effects and interactions
# Remove the non-logged version of Violent CR as well as the community name varialbe
# full <- lm(LogViolentCR ~ . + .^2, data=crime.train)

# Create a model using forward stepwise selection
crime.reg.AIC1 <- step(null, scope=formula(full), direction="forward", k=2)
crime.reg.BIC1 <- step(null, scope=formula(full), direction="forward", k=log(nrow(crime.train)))
