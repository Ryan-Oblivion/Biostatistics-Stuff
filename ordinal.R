 require(foreign)
 require(ggplot2)
 require(MASS)
 require(Hmisc)
 require(reshape2)
dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")


#The data set has a dependent variable known as apply. It has 3 levels namely “unlikely”, “somewhat likely”, and “very likely”, coded in 1, 2, and 3 respectively. 3 being highest and 1 being lowest. This situation is best for using ordinal regression because of presence of ordered categories. Pared (0/1) refers to at least one parent has a graduate degree; public (0/1) refers to the type of undergraduate institute.

m <- polr(apply ~ pared + public + gpa, data = dat, Hess=TRUE)
summary(m)

#We see the usual regression output coefficient table including the value of each coefficient, standard errors, t values, estimates for the two intercepts, residual deviance and AIC. AIC is the information criteria. Lesser the better.
ctable <- coef(summary(m))

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)

# confidence intervals
> ci <- confint(m)

exp(coef(m))

## OR and CI
> exp(cbind(OR = coef(m), ci))

##               OR     2.5 %   97.5 %
## pared  2.8510579 1.6958376 4.817114
## public 0.9429088 0.5208954 1.680579
## gpa    1.8513972 1.1136247 3.098490

One of the assumptions underlying ordered logistic (and ordered probit) regression is that the relationship between each pair of outcome groups is the same.  In other words, ordered logistic regression assumes that the coefficients that describe the relationship between, say, the lowest versus all higher categories of the response variable are the same as those that describe the relationship between the next lowest category and all higher categories, etc.  This is called the proportional odds assumption or the parallel regression assumption.  Because the relationship between all pairs of groups is the same, there is only one set of coefficients (only one model).

> exp(coef(m))

##     pared    public       gpa
## 2.8510579 0.9429088 1.8513972

## OR and CI
> exp(cbind(OR = coef(m), ci))

##               OR     2.5 %   97.5 %
## pared  2.8510579 1.6958376 4.817114
## public 0.9429088 0.5208954 1.680579
## gpa    1.8513972 1.1136247 3.098490

Interpretation
1. One unit increase in parental education, from 0 (Low) to 1 (High), the odds of “very likely” applying versus “somewhat likely” or “unlikely” applying combined are 2.85 greater .

2. The odds “very likely” or “somewhat likely” applying versus “unlikely” applying is 2.85 times greater .

3. For gpa, when a student’s gpa moves 1 unit, the odds of moving from “unlikely” applying to “somewhat likely” or “very likley” applying (or from the lower and middle categories to the high category) are multiplied by 1.85.

Let’s now try to enhance this model to obtain better prediction estimates.


