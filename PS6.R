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

# a) Using the training data, build a model for log crime rate by using forward stepwise selection 
# guided by both AIC and BIC to search through all main effects.