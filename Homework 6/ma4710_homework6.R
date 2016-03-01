# MA4710 Homework 6
# Benjamin Hendrick
# Feb 29, 2015

# Problem 4.5
# Part A
filePath <- "~/GitHub/MA-4710/Homework 6/CH01PR22.txt"
CH01PR22 <- read.table(filePath, quote="\"", comment.char="")
names(CH01PR22)[1] <- "time"
names(CH01PR22)[2] <- "hardness"

plastic.lm <- lm(time ~ hardness, data = CH01PR22)
plastic.coef <- summary(plastic.lm)$coefficients         
alpha <- 0.1

B <- qt(1-alpha/(2*2),plastic.lm$df.residual) 
BCI <- cbind(plastic.coef[,1]-B*plastic.coef[,2],
             plastic.coef[,1]+B*plastic.coef[,2]) 
colnames(BCI) <- c("Lower Bound","Upper Bound") 
BCI


# Problem 4.9
# Part A

CI <- predict(plastic.lm,newdata=data.frame(hardness=c(20,30,40)),se.fit=TRUE) 
g <- 3
B <- qt(1-alpha/(2*g),plastic.lm$df.residual) 
BBand <- cbind( CI$fit - B * CI$se.fit, CI$fit + B * CI$se.fit )
BBand

# Part C


# Problem 5.1
# See Mathematica notebook "Problem 5.5.nb"


# Problem 5.5
filePath <- "~/GitHub/MA-4710/Homework 6/CH05PR05.txt"
CH05PR05 <- read.table(filePath, quote="\"", comment.char="")
names(CH01PR22)[1] <- "city"
names(CH01PR22)[2] <- "loans"
