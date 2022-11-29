require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")


#The data set contains variables on 200 students. The outcome variable is prog, program type. The predictor variables are social economic status, ses, a three-level categorical variable and writing score, write, a continuous variable. Let’s start with getting some descriptive statistics of the variables of interest.

## check data 
with(ml, table(ses, prog))
##         prog
## ses      general academic vocation
##   low         16       19       12
##   middle      20       44       31
##   high         9       42        7
## check mean
with(ml, do.call(rbind, tapply(write, prog, function(x) c(M = mean(x), SD = sd(x)))))##
## group by prog and calculate the mean and sd
##              M    SD
## general  51.33 9.398
## academic 56.26 7.943
## vocation 46.76 9.319

#Multinomial logistic regression
ml$prog2 <- relevel(ml$prog, ref = "academic")
test <- multinom(prog2 ~ ses + write, data = ml)

summary(test)

summary(test)$coefficients
z <- summary(test)$coefficients/summary(test)$standard.errors

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2


## extract the coefficients from the model and exponentiate
exp(coef(test))

#You can calculate predicted probabilities for each of our outcome levels using the fitted function. We can start by generating the predicted probabilities for the observations in our dataset and viewing the first few rows

head(pp <- fitted(test))


#Next, if we want to examine the changes in predicted probability associated with one of our two variables, we can create small datasets varying one variable while holding the other constant. We will first do this holding write at its mean and examining the predicted probabilities for each level of ses.

dses <- data.frame(ses = c("low", "middle", "high"), write = mean(ml$write))
predict(test, newdata = dses, "probs")

Another way to understand the model using the predicted probabilities is to look at the averaged predicted probabilities for different values of the continuous predictor variable write within each level of ses.

dwrite <- data.frame(ses = rep(c("low", "middle", "high"), each = 41), write = rep(c(30:70),
    3))

## store the predicted probabilities for each value of ses and write
pp.write <- cbind(dwrite, predict(test, newdata = dwrite, type = "probs", se = TRUE))

## calculate the mean probabilities within each level of ses
by(pp.write[, 3:5], pp.write$ses, colMeans)
## pp.write$ses: high
## academic  general vocation
##   0.6164   0.1808   0.2028
## --------------------------------------------------------
## pp.write$ses: low
## academic  general vocation
##   0.3973   0.3278   0.2749
## --------------------------------------------------------
## pp.write$ses: middle
## academic  general vocation
##   0.4256   0.2011   0.3733

## melt data set to long for ggplot2
lpp <- melt(pp.write, id.vars = c("ses", "write"), value.name = "probability")
head(lpp)  # view first few rows

## plot predicted probabilities across write values for each level of ses
## facetted by program type
ggplot(lpp, aes(x = write, y = probability, colour = ses)) + geom_line() + facet_grid(variable ~
    ., scales = "free")

