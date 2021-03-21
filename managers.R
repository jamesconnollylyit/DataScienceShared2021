# Create the managers data frame
# Refer to notes "creating a dataset - problem" on Blackboard 
# for more information

# Enter data into vectors before constructing the data frame
date_col <- c("10/15/18", "01/11/18", "10/21/18", "10/28/18", "05/01/18")
country_col <- c("US", "US", "IRL", "IRL", "IRL")
gender_col <- c("M", "F", "F", "M", "F")
age_col <- c(32, 45, 25, 39, 99)
q1_col <- c(5, 3, 3, 3, 2)
q2_col <- c(4, 5, 5, 3, 2)
q3_col <- c(5, 2, 5, 4, 1)
q4_col <- c(5, 5, 5, NA, 2) # NA is inserted in place of the missing data for this attribute
q5_col <- c(5, 5, 2, NA, 1)

# Construct a data frame using the data from all the vectors
managers_data <- data.frame(date_col, 
                            country_col, 
                            gender_col, 
                            age_col, 
                            q1_col, 
                            q2_col, 
                            q3_col, 
                            q4_col, 
                            q5_col)
# View content of the new managers_data data frame
managers_data

column_names <- c("Date", 
                  "Country", 
                  "Gender", 
                  "Age", 
                  "Q1", 
                  "Q2", 
                  "Q3", 
                  "Q4", 
                  "Q5")

# Add column names to the managers_data dataframe
colnames(managers_data) <- column_names

managers_data

# Recode the incorrect 'age' to NA
managers_data$Age[managers_data$Age == 99] <- NA
managers_data

# 2 options to create a new variable
# 1 - create a new vector and store the logical check in it
# 2 - create the new var when doing the logical check

# In this example we're creating the new variable when
# performing the logical check
# We are also recoding age record containing 'NA' to Elder

managers_data$AgeCat[managers_data$Age >= 45] <- "Elder"
managers_data$AgeCat[managers_data$Age >= 26 & managers_data$Age <= 44] <- "Middle Aged"
managers_data$AgeCat[managers_data$Age <= 25] <- "Young"
managers_data$AgeCat[is.na(managers_data$Age)] <- "Elder"
managers_data

# recode age_cat so that it is ordinal and factored
# with the order young, middle aged, elder
age_cat <- factor(managers_data$AgeCat, order = TRUE, levels = c("Young", "Middle Aged", "Elder"))
age_cat

# replace manager_data age_cat variable with
# the factored variable we created above

managers_data$AgeCat <-age_cat
managers_data

# Look at the structure of the data frame
str(managers_data)

# -------------------------------------------------------------------------------
# Dealing with missing data 
# -------------------------------------------------------------------------------

# There are several methods we can use to extract out 
# missing data before we decide whether to delete it

# We can remove all missing data with this code.
# Removes any rows that contains NA - listwise deletion
new_data <- na.omit(managers_data)
new_data

# We can use complete.cases to show rows where data is available
# Here's an example of how to do this
complete_data <- complete.cases(managers_data)
complete_data
# Show sum of missing rows
sum(complete_data)

# We can also list the rows of data
# that do not have missing values
# Note that the ',' and no number inside square brackets means "all columns"
complete_data <- managers_data[complete.cases(managers_data),]
complete_data

# List rows with missing values
managers_data[!complete.cases(managers_data),]

# We can examine individual variables and count
# the number of missing values
# Eg find sum of all missing values in the age attribute
sum(is.na(managers_data$Age))

# Find the mean of missing values from the Age attribute
# This shows that 20% of data is missing in the Age attribute
mean(is.na(managers_data$Age))

# Find the mean of rows with no incomplete data
# Note that we dont need to add the ',' as we're only
# looking for an overall mean of rows with missing values
# This example indicates that 40% of data is missing in
# the managers_data data frame
mean(!complete.cases(managers_data))

#----------------------------------------------------------------------------
# Graphically display missing data
#----------------------------------------------------------------------------

# Use the mice package to show patterns in missing data
install.packages("mice")
library(mice)
md.pattern(managers_data)

# Use VIM package to show missing values
install.packages("VIM")
library(VIM)
missing_values <- aggr(managers_data, prop = FALSE, numbers = TRUE)
# Show summary of the contents of missing_values
summary(missing_values)
# See this link for more info https://www.rdocumentation.org/packages/VIM/versions/4.8.0/topics/aggr 

