##### Hypothesis Testing in two samples

# Libraries
library(foreign)
library(readr)
library(haven)
library(nortest)

# Import Datasets
battary = read.spss("battary.sav", to.data.frame = TRUE)
str(battary)
View(battary)

endorph <- read_sav("endorph.sav")
View(endorph)

salary <- read_csv("salary.csv")
View(salary)

Arxaiologiko <- read_sav("Arxaiologiko.sav")
View(Arxaiologiko)

# Data Cleaning
levels(battary$marka) <- c(1,2)



#### Parametric Tests

## Intependent Samples

  # H0: μ1=μ2
  # Η1: μ1!=μ2

  # Before t-test we check : i)  if the samples follow the normal distribution
                            #ii) if the variances are equal

lillie.test(battary$life)

# Variance Equality Test

  # H0: (σ1)^2/(σ2)^2 = 1
  # Η1: not H0

life_marka_vartest = var.test(x= battary[battary$marka =="1", "life"],
                              y = battary[battary$marka == "2", "life"],
                              alternative ="two.sided",
                              conf.level = 0.95)
life_marka_vartest

 # or
life_marka_vartest1 <- var.test(formula = life ~ marka,
                                data = battary,
                                alternative = "two.sided",
                                conf.level = 0.95)
life_marka_vartest1

# t-test for the means

  # H0: μ1-μ2=0
  # Η1: μ1-μ2!=0

life_marka_test <- t.test(formula = life ~ marka,
                          data = battary,
                          alternative = "two.sided",
                          paired = FALSE,
                          var.equal = TRUE,
                          conf.level = 0.95)
life_marka_test

## Dependent Samples 

lillie.test(endorph$before)
lillie.test(endorph$after)

# The samples follow the normal distribution

# T-test for the means
before_after_test <- t.test(x = endorph$before,
                            y = endorph$after,
                            alternative = "two.sided",
                            paired = TRUE,
                            conf.level = 0.95)
before_after_test

#### Non-Parametric Tests

  # When the characteristics dont follow the normal distribution..

## Independent Samples 

lillie.test(salary$salbeg)

# Wilcoxon-Mann-Whitney
salbeg_sex_test <- wilcox.test(formula = salbeg ~ sex,
                               data = salary,
                               alternative = "two.sided",
                               paired = FALSE,
                               conf.level = 0.95)
salbeg_sex_test
  # The mean salbeg of men and women are not equal (p-value < 0.05)

salnow_sex_test <- wilcox.test(formula = salnow ~ sex,
                               data = salary,
                               alternative = "two.sided",
                               paired = FALSE,
                               conf.level = 0.95)
salnow_sex_test
  # The mean salnow of men and women are not equal (p-value < 0.05)

# Kolmogorov-Smirnov Test
salbeg_sex_kstest <- ks.test(x = salary$salbeg[salary$sex == "0"],
                             y=  salary$salbeg[salary$sex == "1"],
                             alternative = "two.sided"
                             )
salbeg_sex_kstest

## Dependent Samples 

lillie.test(Arxaiologiko$c14)
lillie.test(Arxaiologiko$newmethod)
  # We will use non-parametric tests

# Wilcoxon Test 
c_14_newmethod_wilcoxtest <- wilcox.test(x = Arxaiologiko$c14,
                                         y = Arxaiologiko$newmethod,
                                         alternative = "two.sided",
                                         paired = TRUE,
                                         conf.level = 0.95)
c_14_newmethod_wilcoxtest

# Chi-Square Test --> for categorical data

  # H0: The two characteristics are independent
  # H1: the two characteristics are dependent

chitest <- chisq.test(salary$minority,salary$sex)
chitest
 # We dont reject the H0 ,so the variables are independent
chitest$observed

# Chi-Square Test - Goodness of fit test 

tulip <- c(81,50,27)
res <- chisq.test(tulip,p = c(1/3,1/3,1/3))
res
  # The p-value of the test is less than a = 0.05
  # We can conclude that the colors are significantly not 
  # commonly distributed 