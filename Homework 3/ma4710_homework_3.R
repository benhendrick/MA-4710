# Problem 2.13
CH01PR19 <- read.table("/var/folders/6j/_n_8wtg912559sqd775wfctw0000gn/T//Rtmp8EbqFk/datacdf786779fb", quote="\"", comment.char="")

# Part A
model <- lm(V1 ~ V2, data = CH01PR19)
confint(model)

# Part B
predict(model, CH01PR19, interval="predict")[28,]

# Part C
abs(1.959355-4.443063 )

# Part D
yh = 2.11405 + (28)*0.03883
w2 = qf(.95,2,118)
yh + sqrt(w2)*yh
yh - sqrt(w2)*yh


# Problem 2.23

# Part A
anova(model)

# Part B

# Part C


# Problem 2.26
CH01PR22 <- read.table("/var/folders/6j/_n_8wtg912559sqd775wfctw0000gn/T//Rtmp8EbqFk/datacdf58d91d97", quote="\"", comment.char="")

# Part A
model <- lm(V1~V2, data = CH01PR22)
anova(model)

# Part B

# Part C
plot(model)

# Part D
summary(model)
