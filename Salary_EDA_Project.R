library(readr)
library(dplyr)
library(scales)
library(corrplot)
library(ggplot2)
library(nortest)
library(ggpubr)
library(randtests)
library(FSA)  #dunnTest function
library(Metrics)

salary <- read_csv("~/salary.csv")
names(salary)
str(salary)

## Data Cleaning

any(is.na(salary))


salary <- salary[,-1]

salary$sex <- as.factor(salary$sex)
levels(salary$sex) <- c("Male","Female")

salary$minority <- as.factor(salary$minority)

salary$sexrace <- as.factor(salary$sexrace)

salary$jobcat <- as.factor(salary$jobcat)

## Statistical Analysis

summary(salary)

# Numeric Variables

par(mfrow = c(2,3))
hist(salary$salbeg,col = 'darkcyan')

hist(salary$time,col = 'darkcyan')

hist(salary$age,col = 'darkcyan')

hist(salary$salnow,col = 'darkcyan')

hist(salary$edlevel,col = 'darkcyan')

hist(salary$work,col = 'darkcyan')

# Factor Variables

sex_table <- table(salary$sex)
sex_freqtable <- prop.table(sex_table)

jobcat_table <- table(salary$jobcat)
jobcat_freqtable <- prop.table(jobcat_table)

minority_table <- table(salary$minority)
minority_freqtable <- prop.table(minority_table)

sexrace_table <- table(salary$sexrace)
sexrace_freqtable <- prop.table(sexrace_table)

piepercent <- paste(round(sex_freqtable,2),"%",sep = " ")
piepercent1 <- paste(round(jobcat_freqtable,2),"%",sep = " ")
piepercent2 <- paste(round(minority_freqtable,2),"%",sep = " ")
piepercent3 <- paste(round(sexrace_freqtable,2),"%",sep = " ")

colors <- brewer_pal(palette = "Blues")(2)

 # Sex
pie(sex_table,
    main = "Sex Distribution",
    labels = piepercent,
    col = colors)
legend(x = "topright",
       legend = names(sex_table),
       fill = colors)

 # Jobcat
barplot(jobcat_table,
        main = "Jobcat Category Distribution",
        xlab = "Jobcat Category",
        ylab = "Frequency",
        cex.names = 1.0,
        ylim = c(0,250),
        col = rainbow(length(levels(salary$jobcat))))
text(x = barplot(jobcat_table, plot = FALSE), y = jobcat_table + 2, labels = jobcat_table, pos = 3)

 # Minority
pie(minority_table,
    main = "Minority Distribution",
    labels = piepercent2,
    col = colors)
legend(x = "topright",
       legend = names(minority_table),
       fill = colors)

 # Sexrace
barplot(sexrace_table,
        main = "Sexrace Distribution",
        xlab = "Sexrace",
        ylab = "Frequency",
        cex.names = 1.0,
        ylim = c(0,250),
        col = rainbow(length(levels(salary$sexrace))))
text(x = barplot(sexrace_table, plot = FALSE), y = sexrace_table + 2, labels = sexrace_table, pos = 3)

# Salnow ~ Variables 

attach(salary)

salary1 <- salary[,-c(2,8,9,10)]
cor_matrix <- cor(salary1)
corrplot(cor_matrix,method = "color",
         type = "upper", tl.cex = 0.8, tl.col = "black", 
         diag = FALSE, addCoef.col = "black", tl.srt = 45)
title(main = "Correlation Heatmap")

 # Salnow ~ Salbeg 
l1<- ggplot(salary,aes(x = salbeg,y = salnow))+
  geom_jitter(alpha = 0.6)+
  stat_smooth(method = "lm",se = F,col = "red")+
  labs(title = "Beginning Salary and Salary Now" ,subtitle =  "Linear Correlation")+
  coord_equal()+
  scale_y_continuous("Salary Now",limits = c(5000,40000))+
  scale_x_continuous("Beginning Salary",limits = c(0,25000))

 # Salnow ~ Sex - Minority - Sexrace
p0 <- ggplot(salary,aes(x = sex,y = salnow,fill = sex))+
  geom_boxplot()+
  labs(title = "Salary ~ Sex Distribution")

p1 <- ggplot(salary,aes(x = minority,y = salnow,fill = minority))+
  geom_boxplot()+
  labs(title = "Salary ~ Minority Distribution")


p2 <- ggplot(salary,aes(x = sexrace,y = salnow,fill = sexrace))+
  geom_boxplot()+
  labs(title = "Salary ~ Sexrace Distribution")


p3 <- ggplot(salary,aes(x = jobcat,y = salnow,fill = jobcat))+
  geom_boxplot()+
  labs(title = "Salary ~ Job Category Distribution")


# Salnow - Normality Test

lillie.test(salnow)
ggqqplot(salnow,conf.int = T,conf.int.level = 0.95)

salnow_randtest <- runs.test(salnow)
salnow_randtest
hist(salnow,col = "darkgreen")
# The sample is not random


# Non - Parametric Tests

# Wilcoxon Tests

# 1)
salnow_sex <- wilcox.test(formula = salnow ~ sex,
                          data = salary,
                          alternative = 'two.sided',
                          paired = F,
                          conf.level = 0.95)
salnow_sex
p0
# Men have higher salaries than women

salary%>%
  group_by(sex)%>%
  summarise(avg_salnow = mean(salnow))%>%
  arrange(-avg_salnow)

# 2)
salnow_minority <- wilcox.test(formula = salnow ~ minority,
                               data = salary,
                               alternative = 'two.sided',
                               paired = F,
                               conf.level = 0.95)
salnow_minority
p1
# The minority 0 have higher salaries than 1 (it seems that the extremes make the difference)
salary%>%
  group_by(minority)%>%
  summarise(avg_salnow = mean(salnow))%>%
  arrange(-avg_salnow)

# Kruskal Wallis Test

# 1)
kruskal.test(salnow~sexrace,data = salary)
p2
# There is a difference among the salaries due to sexrace

dunn_test_result <- dunnTest(salnow, sexrace, method = "bonferroni")
print(dunn_test_result)
# It seems the sexrace 1 makes the biggest impact because have the higher salaries

salary%>%
  group_by(sexrace)%>%
  summarise(avg_salnow = mean(salnow))%>%
  arrange(-avg_salnow)

# 2)
kruskal.test(salnow~jobcat,data = salary)
p3
# There is a difference among the salaries due to job category

salary%>%
  group_by(jobcat)%>%
  summarise(avg_salnow = mean(salnow))%>%
  arrange(-avg_salnow)

## Statistical Learning

# Salnow ~ Salbeg - Linear Regression

round(cor(salnow,salbeg),2)

salbeg_salnow_lm <- lm(salnow~salbeg,data = salary)
salbeg_salnow_lm
summary(salbeg_salnow_lm)

ggqqplot(salbeg_salnow_lm$residuals,conf.int = T,conf.int.level = 0.95)

salbeg_salnow_lm$coefficients
 # salnow = 771.282 + 1.909 * salbeg
  
lillie.test(salbeg_salnow_lm$residuals)
# The errors do not follow the gaussian distribution

l1


 # Multiple Regession

set.seed(1)
row.number <- sample(1:nrow(salary), 0.8*nrow(salary))
train = salary[row.number,]
test = salary[-row.number,]
dim(train)
dim(test)

hist(salnow)
hist(log(salnow))
model1 <- glm(log(salnow)~.-sex-sexrace-minority-jobcat-age,data = train)
summary(model1)

par(mfrow = c(2,2))
plot(model1)

require(gridExtra)
plot2=ggplot(train, aes(salbeg, residuals(model1))) + geom_point() + geom_smooth()
plot3=ggplot(train, aes(time, residuals(model1))) + geom_point() + geom_smooth()
plot4=ggplot(train, aes(edlevel, residuals(model1))) + geom_point() + geom_smooth()
plot5=ggplot(train, aes(work, residuals(model1))) + geom_point() + geom_smooth()
grid.arrange(plot2,plot3,plot4,plot5,ncol=2,nrow=2)

model2 <- glm(log(salnow)~salbeg+time+edlevel+work+I(salbeg^2)+I(edlevel^2),data = train)
summary(model2)
plot(model2)
summary(model2$residuals)

model3 <- update(model2,~.-edlevel-I(edlevel^2),data = train)
summary(model3)

pred1 <- predict(model1,newdata = test)
pred1
mse(pred1,log(test$salnow))
