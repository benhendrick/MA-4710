# MA 4710 Homework 9

# Problem 7.3
brand <- read.table("~/GitHub/MA-4710/Homework 8/brand.txt", quote="\"", comment.char="")
names(brand) <- c("Yi","Xi1", "Xi2")

# Part A
brand.fit1 <- lm(Yi ~ Xi1, data = brand)
brand.fit2 <- lm(Yi ~ Xi2, data = brand)
brand.fit3 <- lm(Yi ~ Xi1 + Xi2, data = brand)

anova(brand.fit3) 

# Part B
#H_0: beta_2 = 0

f <- (anova(brand.fit3)[2,2]/anova(brand.fit3)[2,1])/
               ((anova(brand.fit3)[2,2]+anova(brand.fit3)[3,2])/
                  (anova(brand.fit3)[2,1]+anova(brand.fit3)[3,1]))

df(0.01, anova(brand.fit3)[2,1],(anova(brand.fit3)[2,1]+anova(brand.fit3)[3,1]))


# Problem 7.12
R2_Y1 <- anova(brand.fit1)[1,2]/(anova(brand.fit1)[1,2]+anova(brand.fit1)[2,2])
R2_Y2 <- anova(brand.fit2)[1,2]/(anova(brand.fit2)[1,2]+anova(brand.fit2)[2,2])
R2_12 <- (anova(brand.fit3)[1,2]+anova(brand.fit3)[2,2])/
          (anova(brand.fit3)[1,2]+anova(brand.fit3)[2,2]+anova(brand.fit3)[3,2])
R2_Y12 <- anova(brand.fit3)[1,2]/(anova(brand.fit3)[1,2]+anova(brand.fit3)[3,2])
R2_Y21 <- anova(brand.fit3)[2,2]/(anova(brand.fit3)[2,2]+anova(brand.fit3)[3,3])
R2 <- (anova(brand.fit3)[1,2]+anova(brand.fit3)[2,2])/
  (anova(brand.fit3)[1,2]+anova(brand.fit3)[2,2]+anova(brand.fit3)[3,2])


# Problem 7.16

# Part A
n <- dim(brand)[1]
brand.trans <- data.frame(1/sqrt(n-1)*scale(brand))
names(brand.trans) <- c("tYi", "tXi1", "tXi2")
brand.trans.fit <- lm(tYi ~ -1 + tXi1 + tXi2, data = brand.trans) 

# Part B
brand.trans.fit$coefficients # transformed X1 increases 0.8924 per standard deviation

# Part C


# Problem 7.24

# Part A
brand.simple <- lm(Yi ~ Xi1, data = brand)
brand.simple$coefficients

# Part B

