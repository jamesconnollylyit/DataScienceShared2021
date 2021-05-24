states <- as.data.frame(state.x77)

# Add the states name as a variable
states$name <- state.name
head(states)


colnames(states)[colnames(states) == "Life Exp"] <- "Life_Exp"
colnames(states)[colnames(states) == "HS Grad"] <- "HS_Grad"

states <- subset(states, select = -c(Area))

set.seed(1)
no_rows_data <- nrow(states)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- states[sample, ]
testing_data <- states[-sample, ]

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=training_data)
summary(fit)

confint(fit)

library(car)
qqPlot(fit, labels=row.names(states), id.method="identify", simulate=TRUE, main="Q-Q Plot")

training_data["Michigan",]
training_data["Maryland",]

fitted(fit)["Michigan"]
fitted(fit)["Maryland"]

student_fit <- rstudent(fit)
hist(student_fit, 
     breaks=10, 
     freq=FALSE, 
     xlab="Studentized Residual", 
     main="Distribution of Errors")

rug(jitter(student_fit), col="brown")
curve(dnorm(x, mean=mean(student_fit), sd=sd(student_fit)), add=TRUE, col="blue", lwd=2)
lines(density(student_fit)$x, density(student_fit)$y, col="red", lwd=2, lty=2)
legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), lty=1:2, col=c("blue","red"), cex=.7)

outlierTest(fit)
?outlier.test


