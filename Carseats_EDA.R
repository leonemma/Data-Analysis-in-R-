library(ISLR)
library(nortest)
library(ggpubr)
library(ggplot2)
library(corrplot)
library(Metrics)

View(Carseats)
?Carseats

names(Carseats)
any(is.na(Carseats))

dim(Carseats)
str(Carseats)
summary(Carseats)

# Data Cleaning
any(is.na(Carseats))
 
 # Education 

Carseats$Education <-factor(Carseats$Education,levels = c(10,11,12,13,14,15,16,17,18),ordered = T)
attach(Carseats)
class(Education)

# Data Analysis

# Sales 
lillie.test(Sales)
ggqqplot(Sales,conf.int = T,conf.int.level = 0.95,title = "Q-Q plot for Sales")


## Numeric Variables
par(mfrow = c(3,3))
hist(Sales,col = "lightgreen")
hist(CompPrice,col = "Lightblue")
hist(Income,col = "Lightblue")
hist(Advertising,col = "lightgreen")
hist(Population,col = "lightgreen")
hist(Price,col = "lightgreen")
hist(Age,col = "lightgreen")

Carseats$Education <-factor(Carseats$Education,levels = c(10,11,12,13,14,15,16,17,18),ordered = T)
Carseats$ShelveLoc <-factor(Carseats$ShelveLoc,levels = c("Bad","Medium","Good"),ordered = T)

attach(Carseats)
class(Education)
par(mfrow = c(1,1))

## Factor Variables

# Education
education_table <- table(Education)
education_table
education_bar <- barplot(education_table,
                         horiz = F,
                         col = rainbow(length(levels(Education))),
                         main = "Education Level Distribution",
                         xlab = "Education Level",
                         ylab = "Frequency",
                         ylim = c(0,60),
                         cex.names = 0.8)

# Urban 
urban_table <- table(Urban)
urban_table

Urban_bar <- barplot(urban_table,
                     col = rainbow(length(levels(Urban))),
                     main = "Urban Distribution",
                     cex.names = 0.8)

# US
us_table <- table(US)
us_proptable <- prop.table(us_table)
round(us_proptable,2)

us_bar <- barplot(us_table,
                  col = c("lightgreen","7"),
                  main = "US Distribution",
                  cex.names = 0.9)

# Shevlock 
shelvock_table <- table(ShelveLoc)

shelvock_proptable <- prop.table(shelvock_table)
round(shelvock_proptable,2)

shevlock_bar <- barplot(shelvock_table,
                        col = rainbow(length(levels(ShelveLoc))),
                        cex.names = 0.8,
                        ylim = c(0,250))

piepercent <- paste(round(shelvock_proptable,2),"%",sep = " ")
pie(shelvock_table,
    labels = piepercent,
    col = rainbow(length(levels(ShelveLoc))),
    main = "ShelveLoc Distribution")
legend(x = "topright",
       legend = names(shelvock_table),
       fill = rainbow(length(levels(ShelveLoc))))


# Linear Correlation for numeric variables

Carseats1 <- Carseats[,-7]
Carseats2 <- Carseats1[,-c(8,9,10)]

cor_matrix <- cor(Carseats2)
corrplot(cor_matrix,method = "color",
         type = "upper", tl.cex = 0.8, tl.col = "black", 
         diag = FALSE, addCoef.col = "black", tl.srt = 45)
title(main = "Correlation Heatmap")

# Sales ~ Price
cor(Price,Sales)
ggplot(Carseats,aes(x = Price,y = Sales,col = ShelveLoc))+
    geom_jitter(alpha = 0.8)+
    stat_smooth(method = "lm",se = F,col = "red")
    #+facet_grid(.~ShelveLoc)

# Price ~ CompPrice
ggplot(Carseats,aes(x = Price,y = CompPrice))+
    geom_jitter(alpha = 0.8)+
    stat_smooth(method = "lm",se = F,col = "green")


# Numeric ~ Categorical 

# Sales ~ Urban
boxplot(formula = Sales ~ Urban,
        data = Carseats,
        col = rainbow(2),
        main = "Sales by Urban")

ggplot(Carseats,aes(x = Urban,y = Sales,col = Urban))+
    geom_boxplot()

var_test <- var.test(x = Carseats$Sales[Urban == "Yes"],
                     y = Carseats$Sales[Urban == "No"],
                     alternative = "two.sided",
                     conf.level = 0.95)
var_test

sales_urban <- t.test(formula = Sales ~ Urban,
                      data = Carseats,
                      var.equal = T,
                      alternative = "two.sided",
                      conf.level = 0.95)
sales_urban

# Sales ~ US

boxplot(formula = Sales ~ US,
        data = Carseats,
        col = rainbow(2),
        main = "Sales by US")

var_test <- var.test(x = Carseats$Sales[US == "Yes"],
                     y = Carseats$Sales[US == "No"],
                     alternative = "two.sided",
                     conf.level = 0.95)
var_test

sales_us <- t.test(formula = Sales ~ US,
                      data = Carseats,
                      var.equal = T,
                      alternative = "two.sided",
                      conf.level = 0.95)
sales_us
 
# Sales ~ ShelveLoc  (ANOVA)

lillie.test(Sales[ShelveLoc == "Bad"])
ggqqplot(Sales[ShelveLoc == "Bad"],conf.int = T,conf.int.level = 0.95)

lillie.test(Sales[ShelveLoc == "Medium"])
ggqqplot(Sales[ShelveLoc == "Medium"],conf.int = T,conf.int.level = 0.95)

lillie.test(Sales[ShelveLoc == "Good"])
ggqqplot(Sales[ShelveLoc == "Good"],conf.int = T,conf.int.level = 0.95)

 # The distribution are normal

bad_medium_vartest <- var.test(x = Sales[ShelveLoc == "Bad"],
                               y = Sales[ShelveLoc == "Medium"],
                               alternative = "two.sided",
                               conf.level = 0.95)

medium_good_vartest <- var.test(x = Sales[ShelveLoc == "Medium"],
                                y = Sales[ShelveLoc == "Good"],
                                alternative = "two.sided",
                                conf.level = 0.95)

bad_medium_vartest
medium_good_vartest
 # The variances are equal

 # We can apply the anova
sales_shelveloc_anova <- aov(formula = Sales ~ ShelveLoc,
                             data = Carseats
                             )

summary(sales_shelveloc_anova)

# The p value of the ShelveLoc variable is low (p < 0.001)
# so it appears that the ShelveLoc has a real impact on the Sales.

ggplot(Carseats,aes(x = ShelveLoc,y = Sales,col = ShelveLoc))+
    geom_boxplot()

# Residuals Distribution
shapiro.test(sales_shelveloc_anova$residuals)
ggqqplot(sales_shelveloc_anova$residuals,conf.int = T,conf.int.level = 0.95)

ggplot(Carseats,aes(x = ShelveLoc,y = Sales,fill = ShelveLoc))+
    geom_boxplot()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_fill_manual(values = c("brown3","chartreuse2","cyan3"))

# Tukey HSD 
TukeyHSD(sales_shelveloc_anova,conf.level = 0.95)


# Sales ~ Education

ggplot(Carseats,aes(x = Education,y = Sales,fill = Education))+
    geom_boxplot()+
    scale_fill_manual(values = rainbow(9))

# Create a list to store the normality test results for each level of the factor variable
normality_test_results <- list()

# Loop through each level of the factor variable and perform the normality test
for (level in levels(Carseats$Education)) {
    subset_data <- Carseats[Carseats$Education == level, "Sales_log"]
    normality_test_results[[level]] <- shapiro.test(subset_data)
}

for (level in levels(Carseats$Education)) {
    print(paste("Level:", level))
    print(normality_test_results[[level]])
}
# Not all the populations follow the normal distribution...


## Statistical Learning 

set.seed(1)
row.number <- sample(1:nrow(Carseats), 0.8*nrow(Carseats))
train = Carseats[row.number,]
test = Carseats[-row.number,]
dim(train)
dim(test)

model1 <- lm(Sales~.-Education-Urban-US,data = train)
summary(model1)
par(mfrow = c(2,2))
plot(model1)

 # Remove the less significant feature
model2 <- update(model1,~.-Population,data = train)
summary(model2)
plot(model2)

 # Plot the residual plot with all predictors.
attach(train)
require(gridExtra)
plot1 = ggplot(train, aes(CompPrice, residuals(model2))) + geom_point() + geom_smooth()
plot2 = ggplot(train, aes(Income, residuals(model2))) + geom_point() + geom_smooth()
plot3 = ggplot(train, aes(Advertising, residuals(model2))) + geom_point() + geom_smooth()
plot4 = ggplot(train, aes(Price, residuals(model2))) + geom_point() + geom_smooth()
plot5 = ggplot(train, aes(Age, residuals(model2))) + geom_point() + geom_smooth()
grid.arrange(plot1,plot2,plot3,plot4,plot5,ncol=3,nrow=2)

model3 <- glm(Sales~CompPrice+Income+Advertising+Price+Age+I(Price^2),data = train)
summary(model3)

model4 <- glm(Sales~CompPrice+Income+Advertising+Price+Age,data = train)
summary(model4)

model5 <- glm(Sales~CompPrice+Income+Advertising+Price+Age+I(CompPrice^2)+I(Price^2)+I(Advertising^2)+I(Age^2)+I(Income^2),data = train)
summary(model5)


 # Predicted (fitted) values in the test
fitted_values <- predict(model2,newdata = test)

 # Create a data frame with observed and fitted values in test
data_for_train <- data.frame(Observed = train$Sales,Fitted = model2$fitted.values)
data_for_test <- data.frame(Observed = test$Sales, Fitted = fitted_values)

# Scatterplot for predicted and real values in test set
ggplot(data_for_test, aes(x = Fitted, y = Observed)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Diagonal reference line
    labs(title = "Observed vs. Fitted Values in Test Set",
         x = "Observed Values",
         y = "Fitted Values") +
    theme_minimal()

mse(pred1,test$Sales)

# Scatterplot for the fitted values in train set 
ggplot(data_for_train, aes(x = Observed, y = Fitted)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Diagonal reference line
    labs(title = "Observed vs. Fitted Values in Train Set",
         x = "Observed Values",
         y = "Fitted Values") +
    theme_minimal()

