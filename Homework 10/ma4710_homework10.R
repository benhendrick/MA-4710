# MA 4710 Homework 10

# Problem 8.4
Muscle <- read.table("~/GitHub/MA-4710/Homework 10/Muscle.txt", quote="\"", comment.char="")
names(Muscle) <- c("y","x")

## Part A
muscle.fit <- lm(y~x+I(x^2), data = Muscle)
plot(Muscle$x, Muscle$y)
x <- seq(40,80, by=0.01)
y <- muscle.fit$coefficients[1] + muscle.fit$coefficients[2]*x + muscle.fit$coefficients[3]*x^2
lines(x,y)
summary(muscle.fit)$r.squared

## Part B
summary(muscle.fit)

## Part C
x <- 48
y <- muscle.fit$coefficients[1] + muscle.fit$coefficients[2]*x + muscle.fit$coefficients[3]*x^2
newdata <- data.frame(y=y, x=x)
predict(muscle.fit, newdata, interval = "confidence")

## Part D
predict(muscle.fit, newdata, interval = "predict")

## Part E
muscle.fit.simple <- lm(y~x, data = Muscle)
anova(muscle.fit.simple, muscle.fit)

## Part F
# Use quadratic equation

## Part F
sqrt(summary(muscle.fit)$r.squared)


# Problem 8.5
# Part A
muscle.fit.resid <- residuals(muscle.fit)
plot(fitted(muscle.fit), muscle.fit.resid, main = "Fitted vs Residuals",
     xlab = "Fitted Values", ylab = "Residuals")
plot(muscle.fit.resid, main = "x vs Residuals",
     xlab = "x", ylab = "Residuals")
qqnorm(muscle.fit.resid)
qqline(muscle.fit.resid)

## Part B
anova(muscle.fit)


# Problem 8.16
gpa2 <- read.delim("~/GitHub/MA-4710/Homework 10/gpa2.txt", header=FALSE)
names(gpa2) <- c("y", "x1", "x2")

## Part A
gpa2.fit <- lm(y~x1+x2, data = gpa2)
summary(gpa2.fit)

## Part B
gpa2.fit <- lm(y~x1+x2, data = gpa2)

## Part C
gpa2.fit.simple <- lm(y~x1, data = gpa2)
anova(gpa2.fit.simple, gpa2.fit)
 
## Part D
gpa2.fit.resid <- resid(gpa2.fit)
plot(gpa2$x1*gpa2$x2, gpa2.fit.resid, main = "Residuals vs X1X2",
     xlab = "X1X2", ylab = "Residuals")


# Problem 8.20
## Part A
gpa2.fit2 <- lm(y~x1+x2+x1*x2, data = gpa2)

## Part B
anova(gpa2.fit, gpa2.fit2)
