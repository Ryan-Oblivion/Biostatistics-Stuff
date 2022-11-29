#https://www.theanalysisfactor.com/generalized-linear-models-in-r-part-6-poisson-regression-count-variables/

#The data set consists of counts of high school students diagnosed with an infectious disease within a period of days from an initial outbreak.

cases <-
structure(list(Days = c(1L, 2L, 3L, 3L, 4L, 4L, 4L, 6L, 7L, 8L,
8L, 8L, 8L, 12L, 14L, 15L, 17L, 17L, 17L, 18L, 19L, 19L, 20L,
23L, 23L, 23L, 24L, 24L, 25L, 26L, 27L, 28L, 29L, 34L, 36L, 36L,
42L, 42L, 43L, 43L, 44L, 44L, 44L, 44L, 45L, 46L, 48L, 48L, 49L,
49L, 53L, 53L, 53L, 54L, 55L, 56L, 56L, 58L, 60L, 63L, 65L, 67L,
67L, 68L, 71L, 71L, 72L, 72L, 72L, 73L, 74L, 74L, 74L, 75L, 75L,
80L, 81L, 81L, 81L, 81L, 88L, 88L, 90L, 93L, 93L, 94L, 95L, 95L,
95L, 96L, 96L, 97L, 98L, 100L, 101L, 102L, 103L, 104L, 105L,
106L, 107L, 108L, 109L, 110L, 111L, 112L, 113L, 114L, 115L),
    Students = c(6L, 8L, 12L, 9L, 3L, 3L, 11L, 5L, 7L, 3L, 8L,
        4L, 6L, 8L, 3L, 6L, 3L, 2L, 2L, 6L, 3L, 7L, 7L, 2L, 2L, 8L,
	    3L, 6L, 5L, 7L, 6L, 4L, 4L, 3L, 3L, 5L, 3L, 3L, 3L, 5L, 3L,
	        5L, 6L, 3L, 3L, 3L, 3L, 2L, 3L, 1L, 3L, 3L, 5L, 4L, 4L, 3L,
		    5L, 4L, 3L, 5L, 3L, 4L, 2L, 3L, 3L, 1L, 3L, 2L, 5L, 4L, 3L,
		        0L, 3L, 3L, 4L, 0L, 3L, 3L, 4L, 0L, 2L, 2L, 1L, 1L, 2L, 0L,
			    2L, 1L, 1L, 0L, 0L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 0L, 0L,
			        0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L)), .Names = c("Days", "Students"
				), class = "data.frame", row.names = c(NA, -109L))

attach(cases)

head(cases)
    Days Students
    1    1      6
    2    2      8
    3    3     12
    4    3      9
    5    4      3
    6    4      3
    The mean and variance are different (actually, the variance is greater). Now we plot the data.

plot(Days, Students, xlab = "DAYS", ylab = "STUDENTS", pch = 16)

model1 <- glm(Students ~ Days, poisson)

summary(model1)

Call:
glm(formula = Students ~ Days, family = poisson)

Deviance Residuals:
     Min        1Q    Median        3Q       Max
     -2.00482  -0.85719  -0.09331   0.63969   1.73696

Coefficients:
             Estimate Std. Error z value Pr(>|z|)
	     (Intercept)  1.990235   0.083935   23.71   <2e-16 ***
	     Days        -0.017463   0.001727  -10.11   <2e-16 ***
	     ---
	     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 215.36  on 108  degrees of freedom
    Residual deviance: 101.17  on 107  degrees of freedom
    AIC: 393.11

Number of Fisher Scoring iterations: 5


The negative coefficient for Days indicates that as days increase, the mean number of students with the disease is smaller.

This coefficient is highly significant (p < 2e-16).

We also see that the residual deviance is greater than the degrees of freedom, so that we have over-dispersion. This means that there is extra variance not accounted for by the model or by the error structure.


Now let’s fit a quasi-Poisson model to the same data.

model2 <- glm(Students ~ Days, quasipoisson) summary(model2) Call: glm(formula = Students ~ Days, family = quasipoisson) Deviance Residuals: Min 1Q Median 3Q Max -2.00482 -0.85719 -0.09331 0.63969 1.73696 Coefficients: Estimate Std. Error t value Pr(>|t|)
(Intercept)  1.990235   0.074789   26.61   <2e-16 ***
Days        -0.017463   0.001539  -11.35   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for quasipoisson family taken to be 0.7939441)

    Null deviance: 215.36  on 108  degrees of freedom
    Residual deviance: 101.17  on 107  degrees of freedom
    AIC: NA

Number of Fisher Scoring iterations: 5

model2$coefficients
(Intercept)     Days
 1.99023497 -0.01746317

timeaxis <-seq 0="" 150="" 1="" pre="">
timeaxis <-seq (0,150,0.1)


Y <- predict(model2, list(Days = timeaxis))

plot(Days, Number, xlab = "DAYS", ylab = "STUDENTS", pch = 16)

lines(timeaxis, exp(Y), lwd = 2, col = "blue")

Z <- predict(model2, list(Days = timeaxis), type = "response")

plot(Days, Number, xlab = "DAYS", ylab = "NUMBER", pch = 16)

lines(timeaxis, Z, lwd = 2, col = "red")

Let’s calculate the impact on the number of cases arising from a one day increase along the time axis. First we take the exponential of the coefficients.

coeffs <- exp(coef(model2))

coeffs
(Intercept)        Days
  7.3172529   0.9826884
  We calculate the 95% confidence interval (upper and lower confidence limits) as follows:

CI <- exp(confint.default(model2))

CI
              2.5 %    97.5 %
	      (Intercept) 6.3195674 8.4724454
	      Days        0.9797296 0.9856562
	      We can calculate the change in number of students presenting with the disease for each additional day, as follows:

1 - 0.9826884
[1] 0.0173116
The reduction (rate ratio) is approximately 0.02 cases for each additional day.

