#### 37301 Data Driven Marketing ######
#### Homework 7 #######
#### Author: Ben Eisenpress #########

# Load libraries
library(psych)
library(lattice)
library(plyr)
library(mfx)

# Set working directory
setwd("/Users/ben/Dropbox/Chicago Booth/37103 Data Driven Marketing/Homework 7/37103-HW7")
# Load Data
load("Catalog-Data.RData")

########################################################
################## Question 1 ##########################

# Inspect the data
summary(catalog_DF)

# Create an indicator variable that splits the data into estimation and validation subsets
# Set the seed so that the analysis comes out the same every time
set.seed(5807)
catalog_DF$validation_sample = rbinom(nrow(catalog_DF), 1, 0.5)

# Use the indicator variable to split the data into two dataframes
catalog_DF_est <- subset(catalog_DF, validation_sample == 0)
catalog_DF_valid <- subset(catalog_DF, validation_sample == 1)

# Run a logistic regression on all variables using the estimation data
reg1 <- glm(buytabw ~ . - customer_no - validation_sample, family = binomial(link = "logit"), data = catalog_DF_est)
summary(reg1)

reg1.mfx <- logitmfx(buytabw ~ . - customer_no - validation_sample, data = catalog_DF_est, atmean = FALSE)
reg1.mfx

########################################################
################## Question 2 ##########################

# Add predictions to the validation data
catalog_DF_valid$Pr <- predict(reg1, newdata = catalog_DF_valid,  type = "response")

########################################################
################## Question 3 ##########################

# Create a botplot of predictions by actual purchase in the validation data
boxplot(Pr ~ buytabw, data = catalog_DF_valid, col = "hotpink1", xlab = "Customer did not buy (0) or bought (1)",
        ylab = "Predicted purchase probability")

# Source the createBins function for creating quartiles
source("createBins.R")

########################################################
################## Question 4 ##########################

# Create deciles of predicted probabilities
catalog_DF_valid$pr_index <- createBins(catalog_DF_valid$Pr, 10)

# Reverse the quartile order, so quartile 1 is the highest probability
catalog_DF_valid$pr_index <- 11 - catalog_DF_valid$pr_index

# Create a table that contains the number of observations in each bucket
table_n_obs = aggregate(buytabw ~ pr_index, FUN = length, data = catalog_DF_valid)
colnames(table_n_obs)[2] = "n_obs"

# Create a table that contains the number of buyers in each bucket
table_n_buyers = aggregate(buytabw ~ pr_index, FUN = sum, data = catalog_DF_valid)
colnames(table_n_buyers)[2] = "n_buyers"

# Create a table that contains the predicted response rate in each bucket
table_response1 = aggregate(Pr ~ pr_index, FUN = mean, data = catalog_DF_valid)
colnames(table_response1)[2] = "pred_response"

# Create a table that contains the actual response rate in each bucket
table_response2 = aggregate(buytabw ~ pr_index, FUN = mean, data = catalog_DF_valid)
colnames(table_response2)[2] = "true_response"

# Merge summary data into one dataframe
score_DF = merge(table_n_obs, table_n_buyers, by = "pr_index")
score_DF = merge(score_DF, table_response1, by = "pr_index")
score_DF = merge(score_DF, table_response2, by = "pr_index")


########################################################
################## Question 5 ##########################

# Calculate the mean true response rate across all customers
mean_response <- mean(catalog_DF_valid$buytabw)

# Calculate true lift for each quintile bucket
score_DF$lift <- 100 * score_DF$true_response/mean_response

# Plot Lift

# Create a quartile rank variable
score_DF$quartile_rank <- 11 - score_DF$pr_index

# Plot the lift factor for each segment
plot(score_DF$quartile_rank, score_DF$lift, type = "o", pch = 21, lwd = 0.4, bg = "skyblue1", 
     xlab = "Quartile of Predicted Response", ylab = "Lift (Calculated using Actual Response)", main = "Lift by Quartile")
# Draw a horizontal line at 100
abline(h = 100)
# Add Gridlines
grid()

# Plot Cumulative Lift


# Sort the data by response rate, descending
score_DF <- score_DF[order(score_DF$pr_index),]

# Create a variable for the cumulative number of observations and buyers
score_DF$cum_obs <- cumsum(score_DF$n_obs)
score_DF$cum_buy <- cumsum(score_DF$n_buyers)

# Calculate cumulative average response rate
score_DF$cum_response <- score_DF$cum_buy / score_DF$cum_obs

# Calculate the cumulative lift
score_DF$cum_lift <- 100 * score_DF$cum_response / mean_response

# Transform the count of cumulative observations into a percentage
score_DF$cum_mailed <- 100 * score_DF$cum_obs/sum(score_DF$n_obs)

# Plot the cumulative lift
plot(score_DF$cum_mailed, score_DF$cum_lift, type = "o", pch = 21, lwd = 0.4, bg = "skyblue1", 
     xlab = "Cumulative Percent of Customers Targeted", ylab = "Cumulative Lift", main = "Cumulative Lift")
# Add Gridlines
grid()

# Plot Cumulative Gains

# Calculate cumulative percent of buyers captured by targeting n percent of customers
score_DF$cum_buy_pct <- 100* score_DF$cum_buy / sum(score_DF$n_buyers)

# Plot the cumulative buyers reached
plot(score_DF$cum_mailed, score_DF$cum_buy_pct, type = "o", pch = 21, lwd = 0.4, bg = "skyblue1",
     xlim = c(0,100), ylim = c(0,100),
     xlab = "Cumulative Percent of Customers Targeted", ylab = "Cumulative Percent of Buyers Captured", main = "Cumulative Gains")
# Add a 45 degree line
abline(a = 0, b = 1)
# Add Gridlines
grid()

########################################################
################## Question 6 ##########################

# Calcualte predicted profit for each customer
catalog_DF_valid$pred_profit <- 26.9 * catalog_DF_valid$Pr - 1.4

# Make a histogram of predicted profits
histogram(catalog_DF_valid$pred_profit, type = "count", xlab = "Predicted Profit", main = "Histogram of Predicted Profits")

# Calcuate the fraction of customers expected to be profitable
length(which(catalog_DF_valid$pred_profit >= 0)) / length(catalog_DF_valid$pred_profit) 

# Rank customers based on expected profitability
catalog_DF_valid$p_rank <- 1 + length(catalog_DF_valid$pred_profit) - rank(catalog_DF_valid$pred_profit)

# Turn rank into a percentage
catalog_DF_valid$p_rank_pct <- 100 * catalog_DF_valid$p_rank / length(catalog_DF_valid$p_rank)

# Calculate Realized Proifts
catalog_DF_valid$true_profit <- 26.9 * catalog_DF_valid$buytabw - 1.4

# Sort the data by predicted profit
catalog_DF_valid <- catalog_DF_valid[order(-catalog_DF_valid$pred_profit),]

# Calculate cumulative true profit for targeting the n customers with highest expected profit
catalog_DF_valid$cum_true_profit <- cumsum(catalog_DF_valid$true_profit)

# Calculate total true profit
total_true_profit <- sum(catalog_DF_valid$true_profit)

# Plot the cumulative profit for a given number of customers targeted
plot(catalog_DF_valid$p_rank_pct, catalog_DF_valid$cum_true_profit, type = "l", lwd = 0.4, bg = "skyblue1",
     xlab = "Cumulative Percent of Customers Targeted", ylab = "Cumulative Profit", 
     main = "Cumulative Profit")
# Add Gridlines
grid()

########################################################
################## Question 7 ##########################

# Calculate total true profit
total_true_profit <- sum(catalog_DF_valid$true_profit)

# Calculate totao true profit for customers predicted to be profitable
max_true_profit <- sum(subset(catalog_DF_valid$true_profit, catalog_DF_valid$pred_profit > 0))

max_true_profit / total_true_profit - 1
