#### ANALYSIS OF VARIANCE (ANOVA) & LINEAR REGRESSION

library(nortest)
library(DescriptiveStats.OBeu)
library(ggpubr)
library(haven)

country_15 <- read_sav("country_15.sav")
View(country_15)

coag = c(62, 60, 63, 59, 63, 67, 71, 64, 65, 66, 68, 66, 71,
         67, 68, 68, 56, 62, 60, 61, 63, 64, 63, 59)

diet = factor( rep( LETTERS [1:4], c(4, 6, 6, 8)))

coag.df = data.frame(diet, coag)
View(coag.df)
str(coag.df)

# Normal Distribution Test -> for the numeric coag
lillie.test(coag)

# Variance Equality Test
var_ab = var.test(x = coag.df[coag.df$diet == "A","coag"],
                  y = coag.df[coag.df$diet == "B","coag"],
                  alternative = "two.sided",
                  conf.level = 0.95)
var_ab

var_bc = var.test(x = coag.df[coag.df$diet == "B","coag"],
                  y = coag.df[coag.df$diet == "C","coag"],
                  alternative = "two.sided",
                  conf.level = 0.95)
var_bc

var_cd = var.test(x = coag.df[coag.df$diet == "C","coag"],
                  y = coag.df[coag.df$diet == "D","coag"],
                  alternative = "two.sided",
                  conf.level = 0.95)
var_cd

compare.stats(df = coag.df,
              group_var = "diet",
              values = "coag")

boxplot(formula = coag ~ diet,
        data = coag.df,
        col = rainbow(4)
        )

### ANOVA 
coag_aov <- aov(formula = coag ~ diet,
                data = coag.df)
coag_aov
summary(coag_aov)

# Normal Distribution Test for the Residuals
shapiro.test(coag_aov$residuals)

ggqqplot(data = coag_aov$residuals,conf.int = TRUE,conf.int.level = 0.95,title = "Q-Q plot for the residuals")


# If we reject the null hypothesis in ANOVA,in order to see which mean
# is different we use the Tukey method

# Tukey Method
mca.coag = TukeyHSD(x = coag_aov,
                    which = "diet",
                    conf.level = 0.95
)
mca.coag

### LINEAR REGRESSION --> in 2 numeric variables

# Birthrat ~ Lifeexpf 
plot(formula  = BIRTHRAT ~ LIFEEXPF,
     data = country_15,
     xlab = "Birth Rate",
     ylab = "Life Expectancy of Females")

lifeexpf_birthrat_lm <- lm(formula = LIFEEXPF ~ BIRTHRAT,
                           data = country_15)
lifeexpf_birthrat_lm
  # LIFEEXPF = 89.9852 - 0.6973 * BIRTHRAT

summary(lifeexpf_birthrat_lm)

plot(x = country_15$BIRTHRAT,
     y = country_15$LIFEEXPF,
     xlab = "Birth Rate",
     ylab = "Female Life Expectancy")
abline(lifeexpf_birthrat_lm,col = "red")

# Normal Distribution Test for the Residuals
shapiro.test(lifeexpf_birthrat_lm$residuals)

ggqqplot(lifeexpf_birthrat_lm$residuals,conf.int = T,conf.int.level = 0.95)

## Future Predictions
predictions <- predict(object = lifeexpf_birthrat_lm,
                       newdata = data.frame(BIRTHRAT = c(51,52,53)))
predictions
