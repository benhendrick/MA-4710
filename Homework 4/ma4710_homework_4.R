# MA 4710 Homework 4

# Problem 3.3
CH01PR19 <- read.table("/var/folders/6j/_n_8wtg912559sqd775wfctw0000gn/T//RtmpywvxZ6/data155fd0291c0", quote="\"", comment.char="")
names(CH01PR19)[1] <- "GPA"
names(CH01PR19)[2] <- "ACT"

## Part A
boxplot(CH01PR19$ACT)

## Part B
gpa.lm <- lm(ACT ~ GPA, data = CH01PR19)
gpa.resid <- resid(gpa.lm)
stripchart(gpa.resid, method = "stack", offset = .5, at = .15, pch = 19, main = "Dotplot", xlab = "lotsize")

## Part C
plot(gpa.lm)

## Part D
cor(gpa.resid,fitted(gpa.lm))
# Critical value at n = 100 is 0.987; reject normality

## Part E
library(car)
gpa.group <- as.factor(1*(CH01PR19$ACT>26))
leveneTest(gpa.resid,gpa.group,center=median)



# Problem 3.6 
CH01PR22 <- read.table("/var/folders/6j/_n_8wtg912559sqd775wfctw0000gn/T//RtmpywvxZ6/data155f27e2f5cb", quote="\"", comment.char="")
names(CH01PR22)[1] <- "time"
names(CH01PR22)[2] <- "hardness"

## Part A
plastic.lm <- lm(time ~ hardness, data = CH01PR22)
plastic.resid <- resid(plastic.lm)
boxplot(plastic.resid)

## Part B
plot(plastic.lm)

## Part C
cor(plastic.resid, fitted(plastic.lm))

## Part E
plastic.group <- as.factor(1*(CH01PR22$hardness>24))
leveneTest(plastic.resid,plastic.group,center=median)

