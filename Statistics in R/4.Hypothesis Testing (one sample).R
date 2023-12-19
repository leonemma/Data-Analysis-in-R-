#install.packages("foreign")
#install.packages("nortest")
#install.packages("ggpubr")
#install.packages("readr")
#install.packages("randtests")
library(foreign)
library(nortest)
library(ggpubr)
library(readr)
library(randtests)
country_15 = read.spss("country_15.sav", to.data.frame =
                         TRUE)

salary <- read_csv("salary.csv")

View(salary)
View(country_15)
str(country_15)
str(salary)

### Hypothesis Test (One Sample)

## Normal Distribution Tests

# Kolmogorov - Smirnov Test
nortest_urban <- lillie.test(country_15$URBAN)
nortest_urban

  # p-value = 0.9775 > 0.05 = a => H0 approved => [URBAN] --> Normal Distribution 

hist(country_15$URBAN,col = "blue")

nortest_lifeexpm <- lillie.test(country_15$LIFEEXPM)
nortest_lifeexpm

  # p-value = 0.9775 > 0.05 = a => H0 approved => [LIFEEXPM] --> Normal Distribution 

hist(country_15$LIFEEXPM,col = "Red")

# Shapiro - Wilk Test
nortest_urban2 <- shapiro.test(country_15$URBAN)
nortest_urban2

nortest_urban2$statistic
nortest_urban2$p.value
nortest_urban2$method
nortest_urban2$data.name

nortest_lifeexpm2 <- shapiro.test(country_15$LIFEEXPM)
nortest_lifeexpm2

# Visual Tests (QQ - Plots)
qqnorm(y = country_15$URBAN,
       main = "Q-Q plot for Urban population")
       qqline(y = country_15$URBAN)

       
ggqqplot(
    data = country_15$URBAN,
    conf.int = TRUE,
    conf.int.level = 0.95,
    title = "Q-Q plot for Urban population"
        )

ggqqplot(data = country_15$LIFEEXPM,
         conf.int = T,
         conf.int.level = 0.95,
         main = "Q-Q plot for Male Life Expectancy population")         


## Hypothesis Testing

  # For the mean
  # H0: μ = μ0
  # Η1: μ != μ1

# Parametric Tests (if the distribution is normal)
urbantest = t.test(x = country_15$URBAN,
                   alternative = "two.sided",
                   mu = 50,
                   conf.level = 0.95)

urbantest

lifeexpm_test <- t.test(x = country_15$LIFEEXPM,
                        alternative = "two.sided",
                        mu = 67,
                        conf.level = 0.95)
lifeexpm_test

# Non-Parametric Tests(if the distribution is not normal)

nortest_salbeg <- lillie.test(salary$salbeg)
nortest_salbeg
  # The distribution of the variable [salbeg] is not normal (p-value-->0 < 0.05)

# Runs Test

  #H0: The sample is random
  #H1: THe sample is not random

  # It's applied in a factor variable with 2 levels and it's applied also in a 
  # numeric variable dividing into 2 categories based on its median or its mean

salbeg_randtest <- runs.test(x = salary$salbeg,
                             alternative = "two.sided",
                             threshold = median(salary$salbeg))
salbeg_randtest

  # p-value --> 0 => H0 Rejected => The beginning salary is not random

salnow_randtest <- runs.test(x = salary$salbeg,
                             alternative = "two.sided",
                             threshold = median(salary$salbeg))
salnow_randtest

  # p-value --> 0 => H0 Rejected => The salary now ,is not random

# Kolmogorov - Smirnov Test 
  # It's applied in numeric variables and tests if the variables follows some known distribution
  #H0: F(x)=F0(x)
  #H1: F(x)!=F0(x)

salbeg_uniftest = ks.test(x = salary$salbeg, y = "dunif")
salbeg_uniftest

?distributions

# Binomial Test
sex_binomtest = binom.test(
                            x = table(salary$sex),
                            n = length(salary$sex),
                            p = 0.60
                          )
sex_binomtest
