# MA 4710 Homework 8

# Problem 6.5
brand <- read.table("~/GitHub/MA-4710/Homework 8/brand.txt", quote="\"", comment.char="")
names(brand) <- c("Yi","Xi1", "Xi2")

# Part A
pairs(brand, main = "Scatterplot Matrix")
cor(brand) # No correlation between Xi1 and Xi2

# Part B
brand.lm <- lm(Yi ~ Xi1 + Xi2, data=brand)

# Part C
brand.lm.resid <-resid(brand.lm)
boxplot(brand.lm.resid, main = "Box Plot of Residuals", ylab= "Residuals")

# Part D
# Resid vs Yi
plot(x = brand.lm.resid, y = brand$Yi,
     xlab = "Residuals", ylab = "Yi", 
     main = "Residuals vs. Yi")

# Resid vs Xi1
plot(x = brand.lm.resid, y = brand$Xi1,
     xlab = "Residuals", ylab = "X1",
     main = "Residuals vs. Xi1")

# Resid vs. Xi2
plot(x = brand.lm.resid, y = brand$Xi2,
     xlab = "Residuals", ylab = "X2",
     main = "Residuals vs. Xi2")

# Resid vs. Xi1 * Xi2
plot(x = brand.lm.resid, y = brand$Xi1 * brand$Xi2,
     xlab = "Residuals", ylab = "X1X2",
     main = "Residuals vs. X1X2")

qqnorm(brand.lm.resid)
qqline(brand.lm.resid)

# Part E
library(lmtest)
bptest(Yi ~ Xi1 + Xi2, data = brand)

# Part F
brand.lm.innter <- brand.lm
brand.lm.outer <- lm(Yi ~ factor(Xi1) + factor(Xi2), data = brand)
anova(brand.lm.innter, brand.lm.outer)#$`Pr(>F)`

# Problem 6.6

# Part A
summary(brand.lm)#$r.squared

# Part B
# Use part A output

# Part C


# Problem 6.7

# Part A
summary(brand.lm)$r.squared

# Part B
yHat <- predict(brand.lm)
yBar <- mean(brand$Yi)
sum((yHat-yBar)^2)/sum((brand$Yi-yBar)^2)


# Problem 6.8

# Part A

