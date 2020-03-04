library(datasets)
data(mtcars)

str(mtcars)
head(mtcars)

#Converting the 'am' variable to a factor with 2 lvls: 'Manual' as 1, and 'Automatic' as 0
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))

#Simple analysis for finding mean of mpg for each transmission type
mean.mpg <- aggregate(mpg ~ am, mtcars, mean)

boxplot(mpg ~ am, data = mtcars, xlab = "Transmission Type", ylab = "MPG (Miles Per Gallon)", main = "MPG (Miles Per Gallon) by Transmission Type")

#Hypothesis Testing
t.test(mtcars$mpg ~ mtcars$am)
# P-value is 0.001374 < 0.05 so we reject the null hypothesis that there is no mpg diff. in transmission types

#Do a linear model of outcome mpg and regressor am.
fit <- lm(mpg ~ am, data = mtcars)
summary(fit)
#Can see that coefficient for automatic is 17.147, and coefficient for manual is 24.392
#Same as the means we calculated from above
#R^2 is 0.3598, which explains only 36% of the variance

fit2 <- lm(mpg ~ am + cyl + hp + wt, data = mtcars)
summary(fit2)
#Here the R^2 value is 0849 which explains 85% of the variance
#This model is better optimized, as it probably includes variables that are correlated with mpg. 

par(mfrow = c(2,2))
plot(fit2)