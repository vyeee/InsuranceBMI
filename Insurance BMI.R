insurance <- read.csv("C:\\Users\\annem\\Downloads\\insurance.csv", header=TRUE, stringsAsFactors =FALSE)
head(insurance)

tempdir()
# [1] "C:\Users\XYZ~1\AppData\Local\Temp\Rtmp86bEoJ\Rtxt32dcef24de2"
dir.create(tempdir())
#summary
summary(insurance)

#BMI Distribution
h <-hist(insurance$bmi,
         col ="lightblue1", breaks=15, main="BMI Distribution", xlab="BMI")
abline(v=mean(insurance$bmi),
       col="dodgerblue3",
       lty=2,
       lwd=2)
xfit <- seq(min(insurance$bmi), max(insurance$bmi), length = 40)
yfit <- dnorm(xfit, mean = mean(insurance$bmi), sd = sd(insurance$bmi))
yfit <- yfit * diff(h$mids[1:2]) * length(insurance$bmi)

lines(xfit, yfit, col ="black", lwd = 2)

#Age Distribution
h <-hist(insurance$age,
         col ="lightblue1", breaks=15, main="Age Distribution", xlab="Age(Years)")
abline(v=mean(insurance$age),
       col="dodgerblue3",
       lty=2,
       lwd=2)
xfit <- seq(min(insurance$age), max(insurance$age), length = 40)
yfit <- dnorm(xfit, mean = mean(insurance$age), sd = sd(insurance$age))
yfit <- yfit * diff(h$mids[1:2]) * length(insurance$age)

lines(xfit, yfit, col ="black", lwd = 2)

#Charges Distribution
h <-hist(insurance$charges,
         col ="lightblue1", breaks=15, main="Charges Distribution", xlab="RM")
abline(v=mean(insurance$charges),
       col="dodgerblue3",
       lty=2,
       lwd=2)
xfit <- seq(min(insurance$charges), max(insurance$charges), length = 40)
yfit <- dnorm(xfit, mean = mean(insurance$charges), sd = sd(insurance$charges))
yfit <- yfit * diff(h$mids[1:2]) * length(insurance$charges)

lines(xfit, yfit, col ="black", lwd = 2)

#Scatter Plot
plot(insurance$smoker, insurance$charges)
plot(insurance$bmi, insurance$charges)
plot(insurance$smoker, insurance$charges)

#MLR smoking + age + bmi
model2 <- lm(insurance$charges~insurance$smoker+insurance$age+insurance$bmi)
print(summary(model2))
plot(model2)
qqnorm(model2)

#MLR smoking + age + bmi+ children + gender +region
model3 <- lm(insurance$charges~insurance$smoker+insurance$age+insurance$bmi+insurance$Gender+insurance$children+insurance$region)
print(summary(model3))
plot(model3)
qqnorm(model3)

#MLR smoking + age + bmi+ children + region
model4 <- lm(insurance$charges~insurance$smoker+insurance$age+insurance$bmi+insurance$children+insurance$region)
print(summary(model4))
plot(model4)
qqnorm(model4)

#ANOVA
anova(model2, model4)

#VIF
install.packages("caTools")    # For Linear regression 
install.packages('car')        # To check multicollinearity 
install.packages("quantmod")
install.packages("MASS")
install.packages("corrplot")   # plot correlation plot

library(caTools)
library(car)
library(quantmod)
library(MASS)
library(corrplot)
vif(model4)
