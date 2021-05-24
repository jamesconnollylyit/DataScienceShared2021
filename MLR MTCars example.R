# Multiple linear regression with interactions -----------------------
# We use : to indicate an interaction between predictor variables
# Refer to earlier slides for syntax
fit <- lm(mpg ~ hp + wt + hp:wt, data = mtcars)
summary(fit)
# Pr(>|t|) column that the interaction between horsepower and
# car weight is significant. What does this mean ? A significant interaction between two
# predictor variables tells you that the relationship between one predictor and the
# response variable depends on the level of the other predictor

# You can visualise interactions using the effect() function in the effects package.
# This means we can change values for wt and view the change graphically
install.packages("effects")
library(effects)
plot(effect("hp:wt", fit,, list(wt = c(2.2, 3.2, 4.2))), multiline = TRUE)

# evaluating the statistical assumptions in a regression analysis. 
# The most common approach is to apply the plot() function
# to the object returned by the lm() . 
# Doing so produces four graphs that are useful
# for evaluating the model fit.
fit <- lm(weight ~ height, data = women)
# 4 plots in one graph
par(mfrow = c(2, 2))
plot(fit)

# Diagnostic plots for the quadratic fit.
fit2 <- lm(weight ~ height + I(height ^ 2), data = women)
par(mfrow = c(2, 2))
plot(fit2)

# Dropping point 13 and 15
newfit <- lm(weight ~ height + I(height ^ 2), data = women[-c(13, 15),])
# plot 4 charts in one
par(mfrow = c(2, 2))
plot(newfit)

# let ’s apply the basic approach to the states multiple regression problem
states <- as.data.frame(state.x77[, c("Murder", "Population",
                                      "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
par(mfrow = c(2, 2))
plot(fit)
# the model assumptions appear to be well satisfied
# with the exception that Nevada is an outlier

# Global validation of linear model assumption
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)

library(car)
states <- as.data.frame(state.x77[, c("Murder", "Population",
                                      "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
qqPlot(fit, labels = row.names(states), id.method = "identify",
       simulate = TRUE, main = "Q-Q Plot")

# Create Training and Test data for the states dataset
# setting seed to reproduce results of random sampling
set.seed(200)

# sample chooses a random sample
# from 1:all records from states, 80% of rows
no_of_records <- sample(1:nrow(states), 0.8 * nrow(states))
# model training data
training_data <- states[no_of_records,]
training_data
# test data
testing_data <- states[-no_of_records,]
testing_data

# Build the model on training data
# lm(formula, data) where
# formula describes the model to be fit
lr_model <- lm(Murder ~ Illiteracy, data = training_data)

# model summary
summary(lr_model)

# predict distance from testing data
murder_predicted <- predict(lr_model, testing_data)

# make actuals_predicteds dataframe.
actuals_preds <- data.frame(cbind(actuals = testing_data$Murder, 
                                  predicted = murder_predicted))
head(actuals_preds)

correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

# Min - max accuracy
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

# MAPE
mape <- mean(abs((actuals_preds$predicted - actuals_preds$actuals)) / actuals_preds$actuals)
mape
