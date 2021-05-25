# Multiple Linear regression (MLR)

# check model assumptions
# Linearity: There is a linear relation among the variables
# Normality: Residuals are normally distributed
# Homoscedasticity: Residuals have constant variance
# No collinearity: Variables are not linear combinations of each other
# Independence: Residuals are independent or at least not correlated 

# use the state.x77 dataset - base package
help("state.x77")
head(state.x77, 15)
class(state.x77)
# convert to a data frame
states <- as.data.frame(state.x77)
class(states)
head(states, 15)
str(states)

colnames(states)[colnames(states) == "Life Exp"] <- "Life_Exp"
colnames(states)[colnames(states) == "HS Grad"] <- "HS_Grad"
# check for NA's
# using VIM or Mice

# Check for linearity
# variables chosen that will be used for the model
names(states)
# could remove a subset of the data first
# choose the vars and then show them i nthe pairs() function
variables_of_interest <- c("Murder", 
                           "Population", 
                           "HS_Grad", 
                           "Illiteracy", 
                           "Income", 
                           "Life_Exp", 
                           "Area", 
                           "Frost")
pairs(states[variables_of_interest])

library(psych)
pairs.panels(states, 
             smooth = TRUE, # If TRUE, draws loess smooths  
             scale = FALSE, # If TRUE, scales the correlation text font  
             density = TRUE, # If TRUE, adds density plots and histograms  
             ellipses = TRUE, # If TRUE, draws ellipses   
             method = "spearman",# Correlation method (also "pearson" or "kendall") 
             pch = 21, # pch symbol   
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit 
             cor = TRUE, # If TRUE, reports correlations
             jiggle = FALSE, # If TRUE, data points are jittered  
             factor = 2, # Jittering factor  
             hist.col = 4, # Histograms color   
             stars = TRUE,
             ci = TRUE) # If TRUE, adds confidence intervals 

# Dependent var = x-axis
# Independent var = y-axis

attach(states)
scatter.smooth(x = Murder, 
               y = Population, 
               main = "Murder ~ Population", 
               xlab = "Murder (per 100,000)", ylab = "Population (estimate)")

# Chck nuberically the correlation of these variables
# Values of -0.2 < x < 0.2 - low correlation

cor(Murder, Population) # Medium correlation. Value = 0.3436428.
# The correlation test shows that the correlation between the murder and 
# population variables = 0.3436428 indicating a medium correlation.

scatter.smooth(x = Murder, 
               y = Frost, 
               main = "Murder ~ Frost", 
               xlab = "Murder (per 100,000)", ylab = "Frost (mean min temp below freezing)")

cor(Murder, Frost)

paste("correltion for murder and frost: ", cor(Murder, Frost))
paste("correltion for murder and illiteracy: ", cor(Murder, Illiteracy))
paste("correltion for murder and Population: ", cor(Murder, Population))
paste("correltion for murder and HS grade: ", cor(Murder, HS_Grad))
paste("correltion for murder and Income: ", cor(Murder, Income))
paste("correltion for murder and Life expectancy: ", cor(Murder, Life_Exp))
paste("correltion for murder and area: ", cor(Murder, Area))

# Decied to remove the "area" varibale
states <- subset(states, select = -c(Area))
head(states)

# Check for outliers
opar <- par(no.readonly = TRUE)
attach(states)
par(mfrow = c(1, 2)) # charts shown in 4 rows x 2 cols

boxplot(Murder, 
        main = "Murder", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Murder)$out))

boxplot(Population, 
        main = "Population", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Population)$out))

boxplot(HS_Grad, 
        main = "Graduation", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(HS_Grad)$out))
                        
boxplot(Illiteracy, 
         main = "Illiteracy", 
        sub = paste("Outlier rows: ", 
                   boxplot.stats(Illiteracy)$out))
        
boxplot(Income, 
         main = "Income", 
         sub = paste("Outlier rows: ", 
                     boxplot.stats(Income)$out))
        
boxplot(Frost, 
          main = "Frost", 
          sub = paste("Outlier rows: ", 
                    boxplot.stats(Frost)$out))
                
          boxplot(Life_Exp, 
          main = "Life expectancy", 
          sub = paste("Outlier rows: ", 
                    boxplot.stats(Life_Exp)$out))

par(opar)

# use boxplot.stats(0 function to extract the outliers
outlier_values <- boxplot.stats(Population)$out
paste("Population outliers: ", paste(outlier_values, collapse = ", "))

# repeat fo the income var
outlier_values <- boxplot.stats(Income)$out
paste("Income outliers: ", paste(outlier_values, collapse = ", "))
      

# Remove outliers
states <- subset(states, 
                 Population != 21198 
                 & Population != 11197 
                 & Population != 18076 
                 & Population != 11860 
                 & Population != 12237 & Population != 10735)

# remove income outliers
states <- subset(states, Income != 6315)

attach(states)
boxplot(Population, 
        main = "Population", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Population)$out))

boxplot(Income, 
        main = "Income", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Income)$out))

# Decide whether to delete a few more outliers and what this results in

# Check normality
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 4))
plot(density(Population), 
     main = "Density plot : Population", 
     ylab = "Frequency", xlab = "Population",
     sub = paste("Skewness : ", round(e1071::skewness(Population), 2)))

# fill the area under the plot
polygon(density(Population), col = "red")

plot(density(Murder), 
     main = "Density plot : Murder", 
     ylab = "Frequency", xlab = "Murder",
     sub = paste("Skewness : ", round(e1071::skewness(Murder), 2)))
polygon(density(Murder), col = "red")

plot(density(HS_Grad), 
     main = "Density plot : HS grade", 
     ylab = "Frequency", xlab = "HS grade",
     sub = paste("Skewness : ", round(e1071::skewness(HS_Grad), 2)))

# fill the area under the plot
polygon(density(states$HS_Grad), col = "red")

plot(density(Illiteracy), 
     main = "Density plot : Illiteracy", 
     ylab = "Frequency", xlab = "Illiteracy",
     sub = paste("Skewness : ", round(e1071::skewness(Illiteracy), 2)))
polygon(density(Illiteracy), col = "red")

plot(density(Income), 
     main = "Density plot : Income", 
     ylab = "Frequency", xlab = "Income",
     sub = paste("Skewness : ", round(e1071::skewness(Income), 2)))

# fill the area under the plot
polygon(density(Income), col = "red")

plot(density(Frost), 
     main = "Density plot : Frost", 
     ylab = "Frequency", xlab = "Feost",
     sub = paste("Skewness : ", round(e1071::skewness(Frost), 2)))

# fill the area under the plot
polygon(density(Frost), col = "red")
par <- opar

paste("Skewness for illiteracy : ", round(e1071::skewness(Illiteracy), 2))
paste("Skewness for population : ", round(e1071::skewness(Population), 2))
paste("Skewness for murder : ", round(e1071::skewness(Murder), 2))
paste("Skewness for HS grad : ", round(e1071::skewness(HS_Grad), 2))
paste("Skewness for income : ", round(e1071::skewness(Income), 2))
paste("Skewness for frost : ", round(e1071::skewness(Frost), 2))

# Create a table of the values with a decision alongside



# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical

# check normality using qqnorm() function
opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2)) # divide graph area in 2 columns
hist(Murder, main = "Normality proportion of Murder", xlab = "Murder rate")

qqnorm(Murder)
qqline(Murder)
par <- opar

attach(states)
mlr_model <- lm(Murder ~ Illiteracy + Population + HS_Grad + Income + Frost, data = states)
summary(mlr_model)

# Create training and testing data - see notes 
# on Blackboard for detailed description
set.seed(1)
no_rows_data <- nrow(states)
data_sample <- sample(1: no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)
data_sample
training_data <- states[data_sample, ]
testing_data <- states[-data_sample, ]

# Build the model based on training data
model <- lm(Murder ~ Population + Illiteracy + Income + Frost + Life_Exp + HS_Grad, data = training_data)
summary(model)

confint(model)










