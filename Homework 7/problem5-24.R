# Problem 5.1
filePath <- "~/GitHub/MA-4710/Homework 6/CH05PR05.txt"
CH05PR05 <- read.table(filePath, quote="\"", comment.char="")
names(CH05PR05)[1] <- "city"
names(CH05PR05)[2] <- "loans"

finance.lm <- lm(city~loans, data = CH05PR05)
finance.lm$coefficients
