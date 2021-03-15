# First step
x <- 2
x
y <- 5
y

# Check class of x
class(x)

# Check if x is numeric
is.numeric(x)

date1 <- as.Date("2019-03-08")
date1
class(date1)
as.numeric(date1)


date2 <- as.POSIXct("2019-03-08 09:00")
class(date2)
as.numeric(date2)

# Add 2 vectors
p <- c(3, 5, 6, 8)
q <- c(3, 3, 3)
p + q

# Vector arithmitic
x <- c(10.4, 5.6, 3.1, 6.4, 21.7)
y <- c(x, 0, x)
z <- 2*x + y + 1
z
min(z)
max(z)
range(z)
prod(x)

mean_of_z <- mean(z)
sort(mean_of_z)
sorted_z <- sort(z)
class(sorted_z)

# Seq example
seq3 <- seq(-100, 100, by=0.6)
seq3
mean((seq3))
# Sort seq3 in descending order
sort(seq3, decreasing = TRUE)


z <- c(1:3,NA); result <- is.na(z)
z

is.na(seq3)

#Factor vectors
vec1 <- c("Hockey", "Football", "Baseball", "Curling", "Rugby", 
          "Hurling", "Basketball", "Tennis", "Cricket", "Lacrosse")
vec2 <- c(vec1, "Hockey", "Lacrosse", "Hockey", "Water Polo", "Hockey", "Lacrosse")
vec2
vec3 <- vec2 [c(1,3,6)]
vec3
vec3_factor <- as.factor(vec3)
class(vec3)
