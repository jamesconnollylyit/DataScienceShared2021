# Insurance dataset 
# Load the dataset into a data frame first

insurance_data <- read.csv("insurance.csv", na = "")
str(insurance_data)

# several variables need to be converted
# sex - male = 0, female = 1
# Smoker - yes = 1, no = 0
# Region contains 4 categories
# N = 4, so we need n-1 indicator variables
# = 3 indicator variables
# Code variables in alphabetical order
head(insurance_data$region, 15)

# Convert variables as described above
names(insurance_data)
attach(insurance_data)

insurance_data$sex <- factor(sex,
                             levels = c("male", "female"), 
                             ordered = FALSE)

insurance_data$smoker <- factor(smoker, levels = c("yes", "no"), ordered = FALSE)

insurance_data$region <- factor(region,  
                             levels = c("northeast", "northwest", "southeast", "southwest"), 
                             ordered = FALSE)

str(insurance_data)

# Initial investigation of data variables
install.packages("psych")
library(psych)

# Seems there could be a positive correlation between 
# smoker and charges, perhaps charges and age
# and BMI and charges
pairs.panels(insurance_data,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# if we build the model now, R will automatically split the
# factor variables
# Alternatively we will control this process

set.seed(1)
model <- lm(formula = charges ~ age + sex + bmi + children + smoker + region, data = insurance_data )
summary(model)


# Instead we can create our own variables
insurance_data$male <- ifelse(sex == "male", 1, 0)
insurance_data$female <- ifelse(sex == "female", 1, 0)
insurance_data$smokes <- ifelse(smoker == "yes", 1, 0)
insurance_data$ne <- ifelse(region == "northeast", 1, 0)
insurance_data$nw <- ifelse(region == "northwest", 1, 0)
insurance_data$se <- ifelse(region == "southeast", 1, 0)
insurance_data$sw <- ifelse(region == "southwest", 1, 0)

names(insurance_data)
# drop the unneeded variables
# Im going to use age, male, bmi, smokes, ne, nw, se and sw
insurance_data <- insurance_data[c(1,3,4,7:14)]

# round the BMI value and charges
insurance_data$bmi <- round(bmi, 1)
insurance_data$charges <- round(charges, 2)

# Now lets check the assumptions

# Linearity
# We can examine whether there is a linear
# correlation between both variables
attach(insurance_data)
scatter.smooth(x=charges,
               y=age,
               main="Insurance charges ~ age",
               xlab="Insurance charge (,000)",
               ylab="Age (years)")

scatter.smooth(x=charges,
               y=male,
               main="Insurance charges ~ males",
               xlab="Insurance charge (,000)",
               ylab="Gender (male)")

scatter.smooth(x=charges,
               y=bmi,
               main="Insurance charges ~ BMI",
               xlab="Insurance charge (,000)",
               ylab="BMI")

scatter.smooth(x=charges,
               y=smokes,
               main="Insurance charges ~ smokes",
               xlab="Insurance charge (,000)",
               ylab="BMI")

scatter.smooth(x=charges,
               y=ne,
               main="Insurance charges ~ BMI",
               xlab="Insurance charge (,000)",
               ylab="BMI")

# No information available for plot of continuous varaible against 
# ca categorical variable

plot(charges, ne, pch = 19, col = "lightblue", main = "Insurance charges ~ NE", xlab = "Insurance charge (,000)", ylab = "North East")

# Check the correlations between both variables
paste("Correlation for charges and age: ",cor(charges, age))
paste("Correlation for charges and males: ",cor(charges, male))
paste("Correlation for charges and BMI: ",cor(charges, bmi))
paste("Correlation for charges and smokes: ",cor(charges, smokes))
paste("Correlation for charges and children: ",cor(charges, children))
paste("Correlation for charges and NE: ",cor(charges, ne))
paste("Correlation for charges and NW: ",cor(charges, nw))
paste("Correlation for charges and SE: ",cor(charges, se))
paste("Correlation for charges and SW: ",cor(charges, sw))

# Then check for outliers, normality, colinearity


with(insurance_data,
     qqplot(charges, male, 
            main = "Comparing 2 samples of activity data", 
            xlab = "Active temp = yes",
            ylab =  "Active temp = no"))
abline(0,1)

# Check for collinearity of the model
model <- lm(formula = charges ~ age + bmi + children + male + smokes + ne + nw + se)
summary(model)
qqPlot(model, labels = row.names(insurance_data), id.method = "identify",
       simulate = TRUE, main = "Q-Q Plot")


# Collinearity statistics measure the relationship between multiple variables.  
# The "tolerance" is an indication of the percent of variance in the predictor 
# that cannot be accounted for by the other predictors, 
# hence very small values indicate that a predictor is redundant

# The VIF scores should be close to 1 but under 5 is fine and 10+ 
# indicates that the variable is not needed and can be removed from the model. 

library(car)
vif(model)
 
hist(resid(model),main='Histogram of residuals',xlab='Standardised Residuals',ylab='Frequency')

# The fitted values and residuals plot to check the assumption of homoscedasticity.
plot(model, which = 1)

