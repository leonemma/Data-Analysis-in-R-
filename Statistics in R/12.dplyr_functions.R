library(ISLR)
library(dplyr)

View(Carseats)
names(Carseats)

str(Carseats)
summary(Carseats)

sub1 <- subset(Carseats,Carseats$Population==10)
sub1

Carseats %>%
  mutate(New_Sales = (Carseats$Sales)^2)%>%
  summarise(Sales = Carseats$Sales,
            New_Sales,
            Populations = Carseats$Population,
            Age = Carseats$Age)%>%
  filter(Carseats$Population < 100)

         
Carseats%>%
  group_by(Carseats$Urban)%>%
  summarise(Sales_Avg = mean(Sales))%>%
  arrange(Sales_Avg)

