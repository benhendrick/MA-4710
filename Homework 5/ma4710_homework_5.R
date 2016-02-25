# MA 4710 Homework 5
# Benjamin Hendrick
# Started 2/24/2016

# Problem 3.4
CH03PR03 <- read.table("~/GitHub/MA-4710/Homework 5/CH03PR03.txt", quote="\"", comment.char="")
names(CH03PR03) <- c("gpa","act","intel","rank")

# Part F
gpa.lm <- lm(gpa~act, data= CH03PR03)
gpa.resid <- resid(gpa.lm)

plot(x = CH03PR03$intel, y = gpa.resid,
     xlab = "Intelligence Score",
     ylab = "Residuals",
     main = "Residuals of Intelligence Score")

plot(x = CH03PR03$rank, y = gpa.resid,
     xlab = "Class Rank",
     ylab = "Residuals",
     main = "Residuals of Rank")


# Problem 3.15
CH03PR15 <- read.table("~/GitHub/MA-4710/Homework 5/CH03PR15.txt", quote="\"", comment.char="")
names(CH03PR15) <- c("conc", "time")

# Part A
chem.lm <- lm(conc~time, data = CH03PR15)

# Parts B and C
anova(chem.lm)


# Problem 3.16
# Part A
plot(x = CH03PR15$time, y = CH03PR15$conc, 
     xlab = "Time",
     ylab = "Concentration of Solution",
     main = "Concentration of Solution over Time") # try log transformation

# Part B
gmean <- exp(mean(log(CH03PR15$conc))) 
sse <- NULL 
lambda <- NULL 
i <- 1 
for (lam in seq(-0.2,0.2,0.1)){ 
  if (lam != 0){
  tY <- (CH03PR15$conc^lam - 1) / (lam*gmean^(lam-1))
} else {
  tY <- log(CH03PR15$conc)*gmean
} 
  test <- anova(lm(tY~CH03PR15$time)) 
  sse[i] <- test['Residuals','Sum Sq'] 
  lambda[i] <- lam 
  i <- i+1
} 
plot(lambda,sse,type="o",
     main="Box-Cox Transform",
     xlab = "Lambda",
     ylab = "SSE") 
lambda[which.min(sse)] # Log transformation

# Part C
CH03PR15$logConc <- log(CH03PR15$conc)
logConc.lm <- lm(logConc~time, data = CH03PR15)

# Part D
plot(x = CH03PR15$time,
     y = CH03PR15$logConc,
     xlab = "Time",
     ylab = "Solution Concentration (Log Scale)",
     main = "Solution Concentration over Time")
abline(logConc.lm)

# Part E
plot(logConc.lm, which=c(1,2))

