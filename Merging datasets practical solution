# Merging datasets ------------------------------------------------------
#
# Answers to practical problem
#
# -----------------------------------------------------------------------

# Read in the dataset first
# Data needs to be in the workign directory
getwd()
# Move data from blackboard to WD in files window
new_managers_data <- read.csv("MoreData.csv")
managers_data <- read.csv("managers.csv")
str(new_managers_data)

# Show headers of the data
names(new_managers_data)
include_list <- new_managers_data[c("Date", "Country", "Gender", "Age", "Q1", "Q2", "Q3", "Q4", "Q5")]
include_list
# OR
include_list <- new_managers_data[c(6,3,7,4,8:12)]
include_list

# We can't use rbind() function yet
rbind(managers_data, include_list)

# We can create blank columns first and then import into our data frame
blank_vectors <- c("AgeCat", "Answer total", "mean value")
include_list [ , blank_vectors] <- NA
include_list

# OR we can calculate values
attach(include_list)
include_list$AgeCat[Age >= 45] <- "Elder"
include_list$AgeCat[Age >= 26 & Age <= 44] <- "Middle Aged"
include_list$AgeCat[Age <= 25] <- "Young"
include_list$AgeCat[is.na(Age)] <- "Elder"
detach(include_list)

include_list

# Then calculate answer total

attach(include_list)
include_list$`Answer total` <- Q1 + Q2 + Q3 + Q4 + Q5

# Calculate mean value for each row
include_list$`mean value` <- rowMeans(include_list[5:9])

# Now we need to convert the date field to date
str(include_list)

# Examine the date field
str(include_list$Date)

# Default date format in R is yyyy-mm-dd

# Define position of date contents first 
# in the imported data frame eg using %d, %m etc
# R then puts the date field into default yyyy-mm-dd
# format when using Date data type
restructured_date <- as.Date(include_list$Date, "%m/%d/%Y")
restructured_date
# Check date structure
str(restructured_date)

# We can convert the date structure to improve readability
# to display a diffent date output
# But this is not used when storing the date values in a data frame
date_format <- "%m/%d/%Y"
new_date_format <- "%d/%m/%Y"
formatted_date <- format(as.Date(restructured_date, format = date_format), new_date_format)
formatted_date
# Date is now a character as it is not in the
# default date structure
str(formatted_date)

include_list$Date = restructured_date
include_list
str(include_list)

# Check date field of managers data frame
managers$Date
# Needs converting - currently in US format
converted_date <- as.Date(managers$Date, format = "%Y-%d-%m")
converted_date
class(converted_date)

# Now assign this to the managers data frame
managers$Date <- converted_date
str(managers)

# Now we can combine both datasets with
# rbind function
managers <- rbind(managers, include_list)
managers
str(managers)

# Set AgeCat with ordered factors
managers$AgeCat <- factor(managers$AgeCat, levels = c("Young", "Middle Aged", "Elder"), ordered = TRUE)
