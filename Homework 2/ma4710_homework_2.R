# Problem 1.19
CH01PR19 <- read.table("~/GitHub/MA-4710/Homework 2/CH01PR19.txt", quote="\"", comment.char="")

# Part A
model <- lm(V1~V2, data = CH01PR19)
model

# Part B
plot(CH01PR19)
abline(model)

# Part C
2.1145 + 0.03883*30

# Part D
# it's the slope

# Problem 1.23
CH01PR19.2 <- read.table("~/GitHub/MA-4710/Homework 2/CH01PR19-2.txt", quote="\"", comment.char="")

# Part A
model <- lm(V1~V2, data = CH01PR19.2)
resids <- residuals(model)
sum(resids)

# Part B
var(CH01PR19.2$V1)
sd(CH01PR19.2$V1) # expressed in GPA

# Problem 2.4
CH01PR19.3 <- read.table("~/GitHub/MA-4710/Homework 2/CH01PR19-3.txt", quote="\"", comment.char="")

# Part A
model <- lm(V1~V2, data = CH01PR19.3)
confint(model, level = 0.99)
