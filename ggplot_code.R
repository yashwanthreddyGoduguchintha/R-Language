# Clear Global environment
rm(list=ls())

library(ggplot2)
library(dplyr) 

mtcars <- mtcars
?mtcars

# Length of Unique Values
sapply(mtcars, function(x) length(unique(x))) 


qplot(x = mpg, y = wt, data = mtcars, geom = "point")

#Change the color by a continuous numeric variable
qplot(mpg, wt, data = mtcars, color = cyl)

# Change color and shape by groups (factor)
mtcars$cyl <- as.factor(mtcars$cyl)

qplot(mpg, wt, data = mtcars, colour = cyl, shape = cyl)

# Change the size of points according to the values of a continuous variable
qplot(mpg, wt, data = mtcars, size = mpg)

##########

wdata = data.frame(gender = factor(rep(c("F", "M"), each=200)), 
                   weight = c(rnorm(200, 55), rnorm(200, 58)))

head(wdata)

mu <- wdata %>%
  group_by(gender) %>% 
  summarise(grp.mean = mean(weight))

mu

a <- ggplot(wdata, aes(x = weight))

#• For one continuous variable:
## geom_area() for area plot
## geom_density() for density plot
## geom_dotplot() for dot plot
## geom_histogram() for histogram plot

##For one discrete variable:
## geom_bar() for bar plot

# Basic plot
# Change line and fill colors 
ggplot(wdata, aes(x = weight)) + geom_area(stat = "bin", color = "black", fill="#00AFBB")

# Basic plot
a + geom_density()

#ggplot(wdata, aes(x = weight)) + geom_density()

# Change line color and fill color, add mean line
a + geom_density(color = "black", fill = "gray") + 
  geom_vline(aes(xintercept=mean(weight)),
  color="#FC4E07", linetype="dashed", size=1)

#Change line colors by gender
a + geom_density(aes(color = gender))

# Change fill color by gender
# Use semi-transparent fill: alpha = 0.4
a + geom_density(aes(fill = gender), alpha=0.4)

# Add mean lines and color by gender

a + geom_density(aes(color = gender), alpha=0.4)+
  geom_vline(data = mu, aes(xintercept = grp.mean, color=gender),linetype = "dashed",size=1)


##A Histogram represents the distribution of a continuous variable by dividing 
## into bins and counting the number of observations in each bin. 
## The function geom_histogram() is used to create a histogram plot.

# Basic plot
a + geom_histogram()
# Change the number of bins
a + geom_histogram(bins = 50)

# Change line color and fill color, add mean line
a + geom_histogram(color = "black", fill = "gray",bins = 50)+ 
  geom_vline(aes(xintercept=mean(weight)),color="#FC4E07", linetype="dashed", size=1) +
  theme_minimal()

# Change line colors by gender
a + geom_histogram(aes(color = gender), fill = "white") 
# Position adjustment: "identity" (overlaid)
a + geom_histogram(aes(color = gender), fill = "white", alpha = 0.6, position="identity")


##The function geom_bar() can be used to visualize one discrete variable.

ggplot(mtcars, aes(cyl)) +
  geom_bar(fill = "steelblue") + 
  theme_minimal()

#Scatter plots: Continuous X and Y

b <- ggplot(mtcars, aes(x = wt, y = mpg))

# Basic scatter plot
b + geom_point(color = "#00AFBB")
# Change the point size, and shape
b + geom_point(color = "#00AFBB", size = 2, shape = 23)

# Control point size by continuous variable values
b + geom_point(aes(size=qsec), color = "#00AFBB")

# Change point shapes and colors
b + geom_point(aes(shape = cyl, color = cyl))

#############

e <- ggplot(mtcars, aes(x = cyl, y = wt))

# Basic box plot
e + geom_boxplot()
# Rotate the box plot
e + geom_boxplot() + coord_flip()

# Use single colors
e + geom_boxplot(color = "black", fill = "steelblue") 

# Change outline colors by cyl (groups)
e + geom_boxplot(aes(color = cyl))


df <- data.frame(dose=c("D0.5", "D1", "D2"), len=c(4.2, 10, 29.5))

f <- ggplot(df, aes(x = dose, y = len))

# Basic bar plot
f + geom_bar(stat = "identity")

# Change fill color and add labels at the top (vjust = -0.3)
f + geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = len), vjust = -0.3, size = 3.5) + 
  theme_minimal()

# Change bar plot line colors by groups
f + geom_bar(aes(color = dose), stat="identity", fill="white")
# Change bar plot fill colors by groups
f + geom_bar(aes(fill = dose), stat="identity")

df2 <- data.frame(supp=rep(c("VC", "OJ"), each=3), 
                  dose=rep(c("D0.5", "D1", "D2"),2), 
                  len=c(6.8, 15, 33, 4.2, 10, 29.5))
head(df2)

g <- ggplot(data=df2, aes(x=dose, y=len, fill=supp)) 

# Stacked bar plot
g + geom_bar(stat = "identity")

# Use position=position_dodge()
g + geom_bar(stat="identity", position=position_dodge())

ggplot(data=df2, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(stat="identity", position = position_dodge())+ 
  geom_text(aes(label = len),vjust = 1.6,
            position = position_dodge(0.9), size = 3.5)

ggplot(data=df2, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(stat="identity", position = position_dodge())+ 
  geom_text(aes(label = len),vjust = 1.6,
            position = position_dodge(0.9), size = 3.5) +
  xlab("Dose") + ylab("length") + 
  ggtitle("Length Vs Dose") + theme_minimal()

###########