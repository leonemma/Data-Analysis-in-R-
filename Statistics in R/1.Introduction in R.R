# Vectors 

numeric_vector <- c(1,-3,11)

character_vector <- c("George","John","Paul")

boolean_vector <- c(T,F,FALSE)

# Vector Addition
c(1,5,7) + c(1,-3,2)
# or
a <- c(1,2,3)
b <- c(2,5,0)
c <- a + b
sum(a)
sum(b)
sum(c)
a>b
a[1]
b[3]

d <- c(1:10)
d

# Matrices
a1 <- matrix(1:12,nrow = 3,ncol = 4,byrow = T)
a1
a2 <- matrix(1:12,nrow = 3,ncol = 4,byrow = F)
a2

rownames(a1) <- c("Row A","Row B","Row C")
colnames(a1) <- c("Column 1","Column 2","Column 3","Column 4")
a1

# Row/Column Addition in a Matrix
row_D <- c(2,4,5,7)
col_E <- c(2,8,5)

a1 <- cbind(a1,col_E)
a1

# ?rbind

rowSums(a1)
colSums(a1)
a1
a1[2,3]
a1[,2]
a1[1,]

# Data Frame

library(readr)
salary <- read_csv("salary.csv")
View(salary)

# First 6 rows
head(salary)

# Last 10 rows
tail(salary,10)

# Variable names 
names(salary) 
#or
colnames(salary)

# Class of variable
class(salary$salbeg)
mode(salary$age)

# Display the structure of the data
str(salary)

# Display the main statistics of the data
summary(salary)

# To call a column 
salary[,5]
salary$age

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

# Missing values Check
any(is.na(salary))

# Reduce Missing Values
na.omit(salary)

# Add new variables
new_salary_beg <- salary$salbeg + 12.5 *sqrt(2010)
new_salary_beg

salary$new_salary_beg <- new_salary_beg
View(salary)

# 
# salary$new_salary_beg <- salary$salbeg + 12.5 * sqrt(2010)

# Data Transformation
salary$age_class <- ifelse(salary$age < 30,"1",ifelse(salary$age>=60,"3","2"))
View(salary)
class(salary$age_class)
salary$age_class <- as.factor(salary$age_class)
class(salary$age_class)

salary$salnow_class <- ifelse(salary$salnow >=13768,"high","low")
View(salary)

# Subset choice
sub <- subset(salary,salary$age>35)
View(sub)

sub2 <- subset(salary,salary$sex == "male")
View(sub2)

sub3 <- subset(salary,salary$age < 35 & salary$sex =="male" & salary$salbeg < 7000)
dim(sub3)




