## Descriptive Statistics

# Import dataset
library(readr)
salary <- read_csv("salary.csv")
View(salary)

# Data Cleaning

salary <- as.data.frame(salary)
class(salary)

salary$id <- as.character(salary$id)
salary$sex <- as.factor(salary$sex)
salary$jobcat <- as.factor(salary$jobcat)
salary$edlevel <-as.factor(salary$edlevel)
salary$minority <- as.factor(salary$minority)
salary$sexrace <- as.factor(salary$sexrace)

# Levels of a factor variable
levels(salary$sex) <- c("male","female")
levels(salary$edlevel) <- c("level 1","level 2","level 3","level 4",
                            "level 5","level 6","level 7","level 8",
                            "level 9","level 10")
levels(salary$sexrace) <- c("white males","minority males","white females",
                            "minority females")

levels(salary$minority) <- c("white","nonwhite")
levels(salary$jobcat) <- c("clerical",
                           "office trainee",
                           "security officer",
                           "college trainee",
                           "exempt employee",
                           "MBA trainee",
                           "technical employee")

## Factor Variables - Descriptive Statistics
# Table Frequency

freq.sex <- table(salary$sex)
freq.sex

freq.sex.df <- as.data.frame(freq.sex)
View(freq.sex.df)

colnames(freq.sex.df) <- c("Gender","Frequency")

# Relative Frequency Table

rel.freq.sex <- prop.table(freq.sex)
rel.freq.sex
round(rel.freq.sex,2)
rel.freq.sex.df <- as.data.frame(rel.freq.sex)
colnames(rel.freq.sex.df) <- c("Gender","Relative Frequency")

sex.df <-cbind(freq.sex.df,rel.freq.sex.df[2])
sex.df

# Table/Relative Table for the variables : Minority,Jobcat

minority_table <- table(salary$minority)
minority_table

jobcat_table <- table(salary$jobcat)
jobcat_table

minority_rel_table <- prop.table(minority_table)
round(minority_rel_table,2)

jobcat_rel_table <- prop.table(jobcat_table)
round(jobcat_rel_table,2)

# Save as a csv file
write.csv(sex.df,"sex.csv")

# Barplot 
barplot(freq.sex,
        main = "Sex Distribution",
        xlab = "Sex",
        ylab = "Frequency",
        horiz = FALSE,
        cex.names = 0.8,
        col = c(2,4))

barplot(minority_table,
        main = "Minority Distribution",
        xlab = "Minority",
        ylab = "Frequency",
        col = c(5,3),
        horiz = F,
        cex.names = 1)

# Pie
piepercent <- paste(round(rel.freq.sex,2),"%",sep = " ")

pie(freq.sex,
    main = "Sex Distribution",
    labels = piepercent,
    col = rainbow(length(levels(salary$sex))),
     )
legend(x = "topright",
       legend = names(freq.sex),
       fill = rainbow(length(levels(salary$sex))))

piepercent1 <- paste(round(minority_rel_table,2),"%",sep = " ")

pie(minority_table,
    main = "Minority Distribution",
    col = rainbow(length(levels(salary$minority))),
    labels = piepercent1)
legend(x = "topright",
       legend = names(minority_table),
       fill = rainbow(length(levels(salary$minority))))

piepercent2 <- paste(round(jobcat_rel_table,2),"%",sep = " ")

pie(jobcat_table,
    labels = piepercent2,
    col = rainbow(length(levels(salary$jobcat))),
    main = "Jobcat Distribution")
legend(x = "topleft",
       legend = names(jobcat_table),
       fill = rainbow(length(levels(salary$jobcat))))

# Numeric Variables - Descriptive Statistics

mean(salary$salbeg)
median(salary$salbeg)
quantile(salary$salbeg,c(0.25,0,75)) # Tetarthmoria

#install.packages("DescTools")
#library(DescTools)
#Mode(salary$salbeg)

max(salary$salbeg)
min(salary$salbeg)
IQR(salary$salbeg) # Endotetarthmoriako Platos

# Histogram ---> for numeric variables
h <- hist(salary$salbeg,
          main = "Histogram of Beginning Salary",
          breaks = "scott",
          xlab = "salary",
          col = "blue",
          freq = T)
points(x = h$mids, y = h$counts,
       col="red", pch=20)
lines(x= h$mids, y= h$counts,
      col="black")

# Boxplot
b1 <- boxplot(x = salary$salbeg,
              main = "Boxplot of Beginning Salary",
              col = "red",
              )

# Two Categorical Variables - Descriptive Statistics

# Crosstabulation Matrix

jobcat_sex_table <- table(salary$jobcat,salary$sex)
jobcat_sex_table

jobcat_sex_table2 <- addmargins(jobcat_sex_table) # sum of the previous
jobcat_sex_table2

jobcat_minority_table <- table(salary$jobcat,salary$minority)
jobcat_minority_table

jobcat_minority_table2 <- addmargins(jobcat_minority_table)
jobcat_minority_table2

jobcat_minority_rel_table <- prop.table(jobcat_minority_table)
round(jobcat_minority_rel_table,2)

t(jobcat_sex_table)
t(jobcat_minority_table)

jobcat_sex_bar <- barplot(t(jobcat_sex_table),
       horiz = F,
        beside = T,
        col = rainbow(length(levels(salary$sex))),
        xlab = "Job Distribution",
        ylab = "Frequency",
        ylim = c(0,150),
        cex.names = 0.8,
        main = "Job Category - Gender Distribution",
        legend.text = levels(salary$sex)
        )
text(x = jobcat_sex_bar,
     y = t(jobcat_sex_table),
     labels = t(jobcat_sex_table),
     pos =3
)

jobcat_minority_bar <- barplot(jobcat_minority_table,
                               horiz = F,
                               beside = T,
                               xlab = "Jobcat Distribution",
                               ylab = "Frequency",
                               main = "Job Category - Minority Distribution",
                               ylim = c(0,200),
                               cex.names = 0.8,
                               col = rainbow(length(levels(salary$minority))),
                               legend.text = levels(salary$minority))
text(x=jobcat_minority_bar,
     y=t(jobcat_minority_table),
     labels = t(jobcat_minority_table),
     pos= 3)


  
 jobcat_sex.bar <- barplot(height = t(jobcat_sex_table),
          beside = FALSE,
          horiz = FALSE,
          col = rainbow(length(levels(salary$sex))),
          main = "Job Category- Gender Distribution",
          xlab = "Job Category",
          ylab = "Frequency",
          ylim = c(0,250),
          cex.names = 0.8,
          legend.text = levels(salary$sex)
          )                               
  text(
    x = jobcat_sex.bar,
    y = jobcat_sex_table[,1]/2,
    labels = jobcat_sex_table[,1],
    pos = 3
  )  
  
# Compare Stats

library(DescriptiveStats.OBeu)
compare.stats(df=salary,
              group_var = "jobcat",
              values = "salbeg")  

compare.stats(
               df=salary,
               group_var=c("jobcat", "minority"),
               values = "salbeg",
               m_functions = "mean")

compare.stats(
             df = salary,
             group_var = c("jobcat", "minority"),
             values = c("salbeg", "salnow"),
             m_functions = "mean")

compare.stats(
  df = salary,
  group_var = "jobcat",
  values = c("salbeg", "salnow"),
  m_functions = c("mean", "median"))

# Descriptive Statistics in Numeric Variable ~ Factor Variable

salbeg_sex_box <- boxplot(formula = salbeg ~ sex,
                          data = salary,
                          col = c(2,4),
                          main = "Beginning Salary by Sex"
                          )
salnow_sex_box <- boxplot(formula = salnow ~ sex,
                          data = salary,
                          col = rainbow(length(levels(salary$sex))),
                          main = "Salary by Sex now")

hist(
  formula = salbeg~sex,
  data = salary,
  breaks = "scott",
  xlab = "Beginning Salary",
  col = "lightblue")
