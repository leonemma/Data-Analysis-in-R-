library(readr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(ggplot2)

Student_Mental_health <- read_csv("~/Student Mental health.csv")
View(Student_Mental_health)

students <- Student_Mental_health
names(students)
colnames(students) <- c('timestamp','gender','age','course','year','cpga','maritalstat','depressed','anxious','panicAttacks','therapy')
View(students)

str(students)

# Data Cleaning

students$year <- str_replace(students$year,"Year"," ")
students$year <- str_replace(students$year,"year"," ")
head(students$year,11)

students$course <- str_replace(students$course,"Kirkhs","KIRKHS")
students$course <- str_replace(students$course,"Islamic education","Islamic Education")

students$course <- str_replace(students$course,"Law","Laws")

students$course <- str_replace(students$course,"Lawss","Laws")
students$course <- str_replace(students$course,"Engineering","Engine")
students$course <- str_replace(students$course,"engin","Engine")
students$course <- str_replace(students$course,"BCS","Banking Studies")
students$course <- str_replace(students$course,"Benl","BENL")
students$course <- str_replace(students$course,"Biotechnology","BIT")
students$course <- str_replace(students$course,"fatwa","")
students$course <- str_replace(students$course,"BCS","Banking Studies")
students$course <- str_replace(students$course,"Resources","Sciences")
students$course <- str_replace(students$course,"islam","Islam")
students$course <- str_replace(students$course,"psychology","Psychology")
students$course <- str_replace(students$course,"koe","Koe")
students$course <- str_replace(students$course,"KOE","Koe")
students$course <- str_replace(students$course,"BIT","IT")
students$course <- str_replace(students$course,"Fiqh ","Fiqh")


View(students)
students <- students[-44,]
# Data Types

students$gender <- as.factor(students$gender)

students$timestamp <- dmy_hm(students$timestamp)
class(students$timestamp)

factor_vars <- names(students[7:11])

for (vars in factor_vars) {
  students[[vars]] <- as.factor(students[[vars]])
}

students$course <- as.factor(students$course)
length(levels(students$course))

students$year <- as.factor(students$year)
students$cpga <- as.factor(students$cpga)

length(levels(students$cpga))
levels(students$cpga)

levels(students$cpga) <- c("Bad","Medium","Good","Very Good","Excellent")

glimpse(students)
length(levels(students$course))
levels(students$course)

# Data Analysis

attach(students)
sum(is.na(students))
summary(students)
 # Age
summary(age)
hist(age,col = "cyan4")

 # Gender
gender_table <- table(gender)
gender_bar <- barplot(gender_table,
                      col = rainbow(2),
                      xlab = "Gender",
                      ylab = "Frequency",
                      main = "Gender Distribution",
                      cex.names = 0.8)
gender_proptable <- prop.table(gender_table)
piepercent <- paste(round(gender_proptable,2),"%")
gender_pie <- pie(gender_table,
                  labels = piepercent,
                  col = rainbow(length(levels(gender))),
                  main = "Gender Distribution"
                  )
legend(x = "topright",
       legend = names(gender_table),
       fill = rainbow(length(levels(gender))))

 # Course
course_table <- table(course)

 # Year
year_table <- table(year)

year_barplot <- barplot(year_table,
                        col = rainbow(length(levels(year))),
                        xlab = "Year",
                        ylab = "Frequency",
                        main = "Year Distribution",
                        cex.names = 1,
                        ylim = c(0,50),
                        )

year_proptable <- prop.table(year_table)
piepercent1 <- paste(round(year_proptable,2),"%")
year_pie <- pie(year_table,
                labels = piepercent1,
                col = rainbow(length(levels(year))),
                main = "Year Distribution")
legend(x = "topright",
       legend = paste("Year",names(year_table)),
       fill = rainbow(length(levels(year))))

 # cpga
cpga_table <- table(cpga)
cpga_proptable <- prop.table(cpga_table)

cpga_bar <- barplot(cpga_table,
                    col = rainbow(length(levels(cpga))),
                    xlab = "CPGA",
                    ylab = "Frequency",
                    main = "CPGA Distribution",
                    ylim = c(0,50),
                    cex.names = 1)
piepercent2 <- paste(round(cpga_proptable,2),"%")
cpga_pie <- pie(cpga_table,
                labels = piepercent2,
                col = rainbow(length(levels(cpga))),
                main = "CPGA Distribution")
legend(x = "topright",
       legend = names(cpga_table),
       fill = rainbow(length(levels(cpga))))

 # Maritalstat
maritalstat_table <- table(maritalstat)
maritalstat_proptable <- prop.table(maritalstat_table)

maritalstat_bar <- barplot(maritalstat_table,
                           col = c("lightgreen","brown"),
                           )
piepercent3 <- paste(round(maritalstat_proptable,2),"%")
maritalstat_pie <- pie(maritalstat_table,
                       labels = piepercent3,
                       col = c(2,4)
                       )
legend(x = "topright",
       legend = names(maritalstat_table),
       fill = c(2,4))

 # Depressed
depressed_table <- table(depressed)
depressed_proptable <- prop.table(depressed_table)
round(depressed_proptable,2)

depressed_bar <- barplot(depressed_table,
                         col = c("darkgoldenrod2","darkorchid3"),
                         main = "Depression Distribution",
                         xlab = "Depressed",
                         ylab = "Frequency",
                         ylim = c(0,70),
                         cex.names = 1.1,
                         cex.axis = 1.1)

 # Anxious
anxious_table <- table(anxious)
anxious_proptable <- prop.table(anxious_table)
round(anxious_proptable,2)

anxious_bar <- barplot(anxious_table,
                         col = c("darkgoldenrod2","darkorchid3"),
                         main = "Anxiety Distribution",
                         xlab = "Anxious",
                         ylab = "Frequency",
                         ylim = c(0,70),
                         cex.names = 1.1,
                         cex.axis = 1.1)

 # Panic Attacks
panic_table <- table(panicAttacks)
panic_proptable <- prop.table(panic_table)
round(panic_proptable,2)

panic_bar <- barplot(panic_table,
                     col = c("darkgoldenrod2","darkorchid3"),
                     main = "Panic Attacks Distribution",
                     xlab = "Panic Attacks",
                     ylab = "Frequency",
                     ylim = c(0,70),
                     cex.names = 1.1,
                     cex.axis = 1.1)

 # Therapy
therapy_table <- table(therapy)
therapy_proptable <- prop.table(therapy_table)
round(therapy_proptable,2)

therapy_bar <- barplot(therapy_table,
                         col = c("darkgoldenrod2","darkorchid3"),
                         main = "Therapy Distribution",
                         xlab = "Therapy",
                         ylab = "Frequency",
                         ylim = c(0,110),
                         cex.names = 1.1,
                         cex.axis = 1.1,
                         )

 #  Marital status - Depression
maritalstat_depressed_table <- table(maritalstat,depressed)
maritalstat_depressed_table

ggplot(students, aes(x = maritalstat, fill = depressed)) +
           geom_bar(position = "dodge") +
           labs(x = "marital status", y = "Count") +
           ggtitle("Relationship marital status and depression")


 # Marital Status - Anxiety
maritalstat_anxiety_table <- table(maritalstat,anxious)
maritalstat_anxiety_table
maritalstat_anxiety_proptable <- prop.table(maritalstat_anxiety_table)
maritalstat_anxiety_proptable

ggplot(students, aes(x = maritalstat, fill = anxious)) +
          geom_bar(position = "dodge") +
          labs(x = "Marital Status", y = "Count") +
          ggtitle("Marital Status and Anxiety Relationship") 
?geom_text



# Chi-Squared 
 # CPGA - Anxiety
cpga_anxiety_chi <- chisq.test(cpga,anxious)
cpga_anxiety_chi

 # CPGA - Depression
cpga_depression_chi <- chisq.test(cpga,depressed)
cpga_depression_chi

 # Anxiety - Depression
anxiety_depression_chi <- chisq.test(anxious,depressed)
anxiety_depression_chi
 
anxiety_depression_table <- table(anxious,depressed)
anxiety_depression_table

ggplot(students, aes(x = depressed, fill = anxious)) +
           geom_bar(position = "dodge") +
           labs(x = "Depression", y = "Count") +
           ggtitle("Depression and Anxiety Relationship") 

 # Depression - Therapy
depression_therapy_table <- table(depressed,therapy)
depression_therapy_table

ggplot(students, aes(x = depressed, fill = therapy)) +
          geom_bar(position = "dodge") +
          labs(x = "Depressed", y = "Count") +
          ggtitle("Therapy and Depression Relationship") 

 # Anxiety - Therapy
anxiety_therapy_table <- table(anxious,therapy)
anxiety_therapy_table

ggplot(students, aes(x = anxious, fill = therapy)) +
          geom_bar(position = "dodge") +
          labs(x = "Anxious", y = "Count") +
          ggtitle("Therapy and Anxiety Relationship") 

 # Panic Attacks - Anxiety - Depression

levels(students$depressed) <- c("Not Depressed","Depressed")
levels(students$depressed)

ggplot(students, aes(x = panicAttacks, fill = anxious)) +
  geom_bar(position = "dodge") +
  facet_grid(~students$depressed,scales = "free_x",space = "free_x") +
  labs(x = "Panic Attacks", y = "Count")+
  ggtitle("Relationship between Depression,Anxiety & Panic Attacks")+
  theme(strip.placement = "outside")


 # Depression by Age
class(age)  
students$age <- as.factor(students$age)
attach(students)

ggplot(students, aes(x = age, fill = depressed)) +
  geom_bar(position = "dodge") +
  labs(x = "Age", y = "Count") +
  ggtitle("Depression by Age") 

table(age)
# Age does not follow the Gaussian Distribution and the sample is not
# representive...

 # Gender - Depression

ggplot(students, aes(x = depressed, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(x = "Depression", y = "Count") +
  ggtitle("Depression and Anxiety Relationship") 

table(gender)
# We cannot take a conclusion cause the sample consists of women mainly

chisq.test(gender,depressed)





