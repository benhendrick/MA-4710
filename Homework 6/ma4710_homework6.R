# MA4710 Homework 6
# Benjamin Hendrick
# Feb 29, 2015

# Problem 4.5
# Part A
filePath <- "~/GitHub/MA-4710/Homework 6/CH01PR22.txt"
CH01PR22 <- read.table(filePath, quote="\"", comment.char="")
names(CH01PR22)[1] <- "hardness"
names(CH01PR22)[2] <- "time"

plastic.lm <- lm(hardness ~ time, data = CH01PR22)
plastic.coef <- summary(plastic.lm)$coefficients         
alpha <- 0.1

B <- qt(1-alpha/(2*2),plastic.lm$df.residual) 
BCI <- cbind(plastic.coef[,1]-B*plastic.coef[,2],
             plastic.coef[,1]+B*plastic.coef[,2]) 
colnames(BCI) <- c("Lower Bound","Upper Bound") 
BCI


# Problem 4.9
# Part A

CI <- predict(plastic.lm,newdata=data.frame(time=c(20,30,40)),se.fit=TRUE) 
g <- 3
B <- qt(1-alpha/(2*g),plastic.lm$df.residual) 
BBand <- cbind( CI$fit - B * CI$se.fit, CI$fit + B * CI$se.fit )
BBand

# Part C

CI <- predict(plastic.lm,newdata=data.frame(time=c(30,40)),se.fit=TRUE)
              
W <- sqrt(2*qf(0.90,length(plastic.lm$coefficients),plastic.lm$df.residual))
WHBand <- cbind( CI$fit - W * CI$se.fit, CI$fit + W * CI$se.fit )
WHBand

g <- 2
B <- qt(1-alpha/(2*g),plastic.lm$df.residual) 
BBand <- cbind( CI$fit - B * CI$se.fit, CI$fit + B * CI$se.fit )
BBand

# Problem 5.1
A <- matrix(c(1,2,3,4,6,8),3,2)
B <- matrix(c(1,1,2,3,4,5),3,2)
C <- matrix(c(3,5,8,4,1,0),2,3)

A+B
A-B
A %*% C
A %*% t(B)
t(B) %*% A

# Problem 5.5
filePath <- "~/GitHub/MA-4710/Homework 6/CH05PR05.txt"
CH05PR05 <- read.table(filePath, quote="\"", comment.char="")
names(CH05PR05)[1] <- "city"
names(CH05PR05)[2] <- "loans"

Y <- matrix(CH05PR05$loans, 6,1)
X <- matrix(c(1,1,1,1,1,1,CH05PR05$city), 6,  2)

t(Y) %*% Y
X %*% t(X)
t(X) %*% Y

# Problem 5.13
(t(X) %*% X)^(-1)