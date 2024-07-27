#Intro to R and RStudio 
#Romaric A. Mouafo Tchinda & Berea Etherton
#Jul 27th, 2024

#---------------------------Basics--------------------------#

#R acts as a calculator, for example
1+1
2*3

#Make sure you're aware of where your parentheses go:
4+(11-8)/2
(4+(11-8))/2

#Objects
#Lets denote some objects using the "<-" operator 
A <- 10
print(A)
A
B <- 30
print(B)
C <- B-A
print(C)

#PRACTICE: denote an object named "D" that has the value 100


#Note: notice that the Environment Tab (on the upper right) is showing the values of your objects

#Objects can also be strings/vectors/arrays
#Note: the "c(......)" here stands for concatenate, or to join things together
A.vector <- c(1,2,3,4)
A.vector

#We can preform math on our vectors of numbers
A.vector*10
A.vector+6

#Vectors can be lists of numbers, letters, or characters
B.vector<-c("A","B","C","D")
B.vector
C.vector<-c(A,B,C,D)
C.vector

#To create vectors we can also use the following operators:
A.vector<-c(1,2,3,4)
A.vector.again<-seq(1,4)
A.vector.again.again<-1:4
A.vector;A.vector.again;A.vector.again.again
#all three of these operators do the same thing!

#We can join vectors together using the following operators:
rbind(A.vector,B.vector) #rbind() stands for row bind
cbind(A.vector,B.vector) #cbind() stands for column bind

#PRACTICE: create a vector called "C.vector" containing four different numbers,
#and column bind it to BOTH A.vector and B.vector


#now save this into an object called "combined.vectors"



#To remove objects you no longer want, use the rm() functions
rm(A.vector.again,A.vector.again.again)
#notice the objects disappear from your working environment


#--------------------------Matrices--------------------------------------#

#matrices are an important aspect of most statistical/scientific analyses
#to create a matrix, we use the matrix() function

matrix1<-matrix(1:16,ncol=4,nrow=4)
#this says we put the vector 1 through 16 into a matrix with 4 columns and 4 rows
matrix1
#the dimensions of our matrix:
dim(matrix1)
#the length of our matrix, or total elements present
length(matrix1) 
#and VERY important, the class of our object
class(matrix1)

#knowing the class of your object is very important, for example
vector_1<-c("1","2","3","4")
vector_2<-c(1,2,3,4)
class(vector_1)
class(vector_2)

#notice how even know the entries are the same, vector_1 is a character string, and vector_2 is a numeric string
vector_1*10
vector_2*10

#....back to matrices
matrix1
#lets select the 1st element in this matrix by using subscripting, or the [] operator
matrix1[1]

#we can select multiple elements too:
matrix1[c(1,2,3)]
matrix1[1:5]
matrix1[c(1,3,5)]

#say we want the element in the 2nd row, in the 4th column
matrix1[2,4]

#lets pull all the elements from the first row
matrix1[1,]

#now the first column
matrix1[,1]

#now lets pull only the second and third element from the first row
matrix1[1,2:3]



#---------------------------Real Data Example------------------------------------#

#lets work with some real data 
#first we need to know where our working directory is, or where all of our data and code is being stored
getwd()
#if this working directory isn't where you want R to pull your files from, do the following:
#go to Session (above), Set Working Directory, Choose Directory, and choose where Rstudio should be looking for files
#it's easiest to have everything in one place (scripts and files)

#lets download some data from 
#https://raw.githubusercontent.com/jbrownlee/Datasets/master/daily-min-temperatures.csv
#right click, and save the file in your working directory (getwd())

#lets load this data set into our environment 
install.packages("readr")
library("readr")
daily_min_temperatures <- read.csv("daily-min-temperatures.csv")

daily_min_temperatures <- read.csv("https://raw.githubusercontent.com/jbrownlee/Datasets/master/daily-min-temperatures.csv")

View(daily_min_temperatures)

#this data set is too big for us to manually parse through 
#lets analyze the data a bit
head(daily_min_temperatures) #this shows the first few entries 
class(daily_min_temperatures)

#for data frames, we can use the "$" or subset operators & [] operators
daily_min_temperatures$Date
daily_min_temperatures$Date[2]
daily_min_temperatures$Temp[4]

#for data frames, it is often better to use the $ operator, for example:
class(daily_min_temperatures[,1])
class(daily_min_temperatures$Date)
class(daily_min_temperatures[,2])
class(daily_min_temperatures$Temp)

#notice how we see the true class of the data only when we use the $ operator
range(daily_min_temperatures$Date)
range(daily_min_temperatures$Temp)

#check the mean & median
mean(daily_min_temperatures$Temp)
median(daily_min_temperatures$Temp)

plot(daily_min_temperatures$Date,daily_min_temperatures$Temp)
# Check for NA values
print(sum(is.na(daily_min_temperatures$Date))) # Should be 0
print(sum(is.na(daily_min_temperatures$Temp))) # Should be 0

class(daily_min_temperatures$Temp)
class(daily_min_temperatures$Date)

plot(as.Date(daily_min_temperatures$Date),daily_min_temperatures$Temp)
#lets clean up our scatterplot a bit
#lets see what the plot() function can do 
help(plot) #then select the Generic X-Y Plotting
#use help(".....") to figure out the details of any function in R

plot(as.Date(daily_min_temperatures$Date),daily_min_temperatures$Temp,
     xlab = "Date",ylab="Temperature",main="The Daily Minimum Temperature from 1981-1990",
     type="l",col="blue")

#PRACTICE: using the $ and [] operators, select the 3rd and 4th variables
#from the Temperature column in daily_min_temperatures



#-------------------------Introduction to tidyverse------------------------------------#

# I. Data wrangling and basic plots using tidyverse

# First, install the package
install.packages("tidyverse")
# Second, load the package using library()
library(tidyverse)

# Load iris data
data(iris)
# Returns the first or last parts of a object
head(iris)
# Display the internal structure of an R object
str(iris)
# Return a vector of character strings giving the names of the columns in the dataset
ls(iris)

## Key dplyr functions in dplyr package for data manipulation
# 1. tibble

# turns an existing object, such as a data frame or matrix, into a so-called tibble
df <- as_tibble(iris)
df

# 2. Filter: allows to subset observations based on their values

# Filter rows with filter()
filter(df, Species == "versicolor")
# EXERCISE
# Now filter observations from species virginica 


# Using filter() to choose specific observations  with logical operators
# Example 1: choose observations with petal length greater than 2
filter(df, Petal.Length > 2)
# Example 2: choose observations with petal length greater than 6 and sepal lenght greater than 7
filter(df, Petal.Length > 6 & Sepal.Length > 7)
# EXERCISE
# Now filter those observations with Sepal.Width smaller than 2 and Species versicolor


# 3. arrange(): works similarly to filter() except that instead of selecting rows, it changes their order. 

# default is ascending order
arrange(df, Sepal.Length, Petal.Width)
# to order in descending order
arrange(df, desc(Sepal.Length))
# EXERCISE
# What happens if we arrange a categorical variable?


# 4. select(): select columns and allows to zoom in on a subset of data based on the names of the variables

# subsetting columns of interest
select(df, Species, Petal.Width, Petal.Length)
# EXERCISE
#  Select the following columns Species and Sepal.Width

# 5. mutate() : adds new variables and preserves existing one

# Create a new column with log transformed values of sepal length
mutate(df, log.Sepal.length = log(Sepal.Length))
# EXERCISE
# Calculate the exponential values of Sepal.Length plus Petal.Length
# To calculate exponential values we can use the function exp()


# 6. group_by(): allows to group / roll up dataset by multiple variables

# group dataset by species, and display the number of entries
# How many observations are for each species of flower?
group_by(df, Species) %>% count()

# %>%: Pipe function in R- allows to pass the output from one operation as input to the next, without need to create object at each step


# 7. summarise() : provides summary information 

# Calculate the mean of petal length
summarise(df, mean(Petal.Length))
## EXERCISE
# Now calculate the standard deviation using the sd() function


# # mean of petal length for each species
df %>%
  group_by(Species) %>%
  summarise(mean(Petal.Length))

## EXERCISE: 
# Create a summary table containing: 
# mean and standard deviation for petal length, petal width, sepal length, and
# arranging the output in descending order for Species

df %>%
  group_by(Species) %>%
  summarise(mean(Petal.Length),
            mean(Petal.Width),
            mean(Sepal.Length),
            sd(Petal.Length),
            sd(Petal.Width),
            sd(Sepal.Length)) %>%
  arrange(desc(Species))

# II. Frequently Used Plots
# Default function in R is plot()
plot(x=iris$Sepal.Length, y=iris$Sepal.Width)

# 1) Scatterplot

ggplot(data=df, 
       aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color=Species, shape=Species)) +
  xlab("Sepal Length") +
  ylab("Sepal Width") +
  ggtitle("Sepal Length-Width")

# 2) Box Plot
box <- ggplot(data=df, aes(x=Species,
                           y=Sepal.Length))
box +
  geom_boxplot(aes(fill=Species)) +
  ylab("Sepal Length") +
  ggtitle("Iris Boxplot")

# Adding a dot for the means of each flower species
box +
  geom_boxplot(aes(fill=Species)) +
  ylab("Sepal Length") +
  ggtitle("Iris Boxplot")+
  stat_summary(fun.y=mean, geom="point",
              shape=1, size=4)

# Note that fun.y is deprecated, so can update to fun
box +
  geom_boxplot(aes(fill=Species)) +
  ylab("Sepal Length") +
  ggtitle("Iris Boxplot") +
  stat_summary(fun=mean, geom="point",
               shape=1, size=4)

# 3) Histogram
ggplot(data=df, aes(x=Sepal.Width)) +
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) +
  xlab("Sepal Width") +
  ylab("Frequency") +
  ggtitle("Histogram of Sepal Width")

# 4) bar plot
ggplot(data=df, aes(x=Species))+ 
  geom_bar(aes(fill=Species)) +
  xlab("Species") +
  ylab("Count") +
  ggtitle("Bar plot of Sepal Length") 

# 5) Faceting: forms a matrix of panels defined by row and column faceting variables.
# For each species create a scatterplot using Sepal.Length and Sepal.Width
ggplot(data=df, aes(Sepal.Length, y=Sepal.Width,
                    color=Species)) +
  geom_point(aes(shape=Species), size=1.5) +
  xlab("Sepal Length") +
  ylab("Sepal Width") +
  ggtitle("Faceting")+
  facet_grid(. ~Species)

