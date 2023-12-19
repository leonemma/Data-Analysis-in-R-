library(tidyverse)
library(ggplot2)

iris
view(iris)
summary(iris)
attach(iris)

p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) 
  
p + geom_point()
p + geom_jitter()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6)



ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6) +
  facet_grid(. ~ Species)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,col = Species)) +
  geom_jitter(alpha = 0.6) +
  facet_grid(. ~ Species)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6) +
  facet_grid(. ~ Species) +
  stat_smooth(method = "lm", se = F, col = "red")

# limits
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species))+
  geom_point(position = "jitter") +
  scale_x_continuous("Sepal Length", limits = c(2, 8)) +
  scale_color_discrete("Species")

# break
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species))+
  geom_point(position = "jitter") +
  scale_x_continuous("Sepal Length", limits = c(2, 8), breaks = seq(2, 8, 3)) +
  scale_color_discrete("Species")

# expand
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point(position = "jitter") +
  scale_x_continuous("Sepal Length", limits = c(2, 8), breaks = seq(2,8,3),expand = (0,0))+ 
  scale_color_discrete("Species")

# labels
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point(position = "jitter") +
  scale_x_continuous("Sepal Length",limits =c(2, 8),breaks = seq(2, 8, 3),expand = c(0,0))+ 
  scale_color_discrete("Species",labels = c("Setosa", "Versicolour","Virginica"))

# labs
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point(position = "jitter") +
  labs(x = "Sepal Length", y = "Sepal Width", col = "Species")

levels(iris$Species) <- c("Setosa", "Versicolor", "Virginica")
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6) +
  facet_grid(. ~ Species) +
  stat_smooth(method = "lm", se = F, col = "red") +
  scale_y_continuous("Sepal Width (cm)",limits = c(2, 5),expand = c(0, 0)) +
  scale_x_continuous("Sepal Length (cm)",limits = c(4, 8),expand = c(0, 0)) +
  coord_equal()

#coord_equal() +
#  theme(panel.background = element_blank(),
#        plot.background = element_blank(),
#        legend.background = element_blank(),
#        legend.key = element_blank(),
#        strip.background = element_blank(),
#        axis.text = element_text(colour = "black"),
#        axis.ticks = element_line(colour = "black"),
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        axis.line = element_line(colour = "black"),
#        strip.text = element_blank(),
#        panel.spacing = unit(1, "lines")
#  )

ggplot(iris,aes(x = Sepal.Length,y = Sepal.Width,col = Species,shape = Species))+
  geom_jitter(alpha = 0.7)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point(position = "jitter") +
  scale_x_continuous("Sepal Length") +
  scale_color_discrete("Species")

ggplot(iris, aes(x = Sepal.Width)) +
  geom_histogram(binwidth = 0.1)

ggplot(iris, aes(x = Sepal.Width, fill = Species)) +
  geom_histogram(binwidth = 0.1)

ggplot(iris, aes(x = Sepal.Width, fill = Species)) +
  geom_histogram(binwidth = 0.1, position = "dodge")

ggplot(iris, aes(x = Sepal.Width, fill = Species)) +
  geom_histogram(binwidth = 0.1, position = "fill")


ggplot(iris,aes(x = Species,y = Sepal.Length,col = Species,fill(Species)))+
  geom_boxplot()



