iris
mtcars
setwd("C:/Users/Dell/Desktop/Programming for Data Science")

# R PROGRAMMING BASICS

# R is a powerful programming language used
# for statistical computing,data analysis,
# and machine learning. 

# Installation and Setup
# Download and install R from CRAN.

# https://www.youtube.com/watch?v=U4LIjQCa0hc 
# Use an IDE like RStudio for an enhanced programming experience.

#Data Types
#1. Numeric
#2. Integer
#3. Character
#4. Logical
#5. Filter
#6. Complex
#7. Raw

# Assign values to variables using <-:
x <- 10
y <- 20
z <- x + y
print(z)  # Output: 30

# Comments start with #:
# This is a comment

# DATA STRUCTURES

# VECTORS
vec <- c(1, 2, 3, 4, 5)
print(vec)

# MATRICES
mat <- matrix(1:9, nrow = 3, ncol = 3)
print(mat)

# DATA FRAME
df <- data.frame(Name = c("VIT", "IIT"), 
                 Age = c(40, 50))
print(df)

# READING AND WRITING DATA

data <- read.csv("test.txt")
iris
head(iris)
tail(iris)

write.csv(data, "output.txt")

# INSTALLING THE PACKAGES
install.packages("ggplot2")
library(ggplot2)

# CREATE A SIMPLE PLOT
plot(1:10, main = "R Programming for Data Science",
     xlab = "X-axis", ylab = "Y-axis")

# Assigning Values to Variables
# assignment operators <-, =, or
# the combination -> 
#         for assigning values to variables.

# Using '<-' operator
x <- 10
x
# Using '=' operator
y = "Hello"
y
# Using '->' operator
20 -> z
z

# VARIABLE NAMING RULES
# Variable names must start with a letter or a dot (.) followed by a letter.
# Subsequent characters can include letters, numbers, underscores (_), and dots (.).
# Avoid using reserved words or function names as variable names (e.g., if, for, while).
# Variable names are case-sensitive (myVar and MyVar are different).
# Example:
# Valid
my_var <- 5
.var1 <- "test"
age <- 30

# Invalid
1var <- 100    # Cannot start with a number
var! <- "oops" # Special characters are not allowed
# Checking the Value of a Variable
print(x)
# or
x
# Listing Variables in the Environment
ls()
# Removing Variables
rm(x)
x
# Different Data Types
# Numeric
a <- 3.14

# Integer
b <- 5L  # The 'L' specifies an integer

# Character (String)
c <- "Hello, R!"

# Logical (Boolean)
d <- TRUE

# Vector
e <- c(1, 2, 3, 4)

# List
f <- list(name = "Dia", age = 10)

# Data Frame
g <- data.frame(ID = c(1, 2), Score = c(95, 88))
a
b
c
d
e
f
g

# Builtin Functions
# Mathematical functions
abs(-5)         # Absolute value: 5
sqrt(16)        # Square root: 4
log(100, 10)    # Logarithm with base 10: 2
exp(1)          # Exponential of 1: 2.718
sum(c(1, 2, 3)) # Sum of elements: 6
mean(c(2, 4, 6))# Mean of elements: 4
# Character String Function
nchar("hello")     # Number of characters: 5
toupper("hello")   # Convert to uppercase: "HELLO"
tolower("WORLD")   # Convert to lowercase: "world"
paste("AI", "R", sep = "-") # Concatenate: "AI-R"
substr("Learning", 1, 4)    # Extract substring: "Lear"

# Logical Functions
all(c(TRUE, TRUE))     # Check if all are TRUE: TRUE
any(c(TRUE, FALSE))    # Check if any is TRUE: TRUE
is.numeric(42)         # Check if numeric: TRUE
is.character("AI")     # Check if character: TRUE

#Data Manuplation Function
length(c(1, 2, 3))     # Length of vector: 3
sort(c(3, 1, 2))       # Sort elements: 1, 2, 3
table(c("a", "b", "a"))# Frequency table
unique(c(1, 1, 2, 3))  # Unique elements: 1, 2, 3

# Syntax of Built-in Functions
# function_name(arguments)
# function_name: The name of the built-in function (e.g., sum, mean).
# arguments: Inputs or parameters the function requires.

result <- mean(c(1, 2, 3, 4, 5))
print(result)  # Output: 3

# Help for buildin functions
?mean       # Opens help documentation for the `mean` function
help(mean)  # Same as above

# Buildin Functions for Data Frames
data <- data.frame(name = c("Dia", "Raj"), age = c(10, 40))

data
iris
dim(data)         # Dimensions: 2 rows, 2 columns
colnames(data)    # Column names: "name", "age"
rownames(data)    # Row names
summary(data)     # Summary statistics for data frame

# Builtin Plotting Function
x <- c(1, 2, 3)
y <- c(4, 5, 6)
plot(x, y)       # Creates a scatterplot
hist(x)          # Creates a histogram
boxplot(x)       # Creates a boxplot
# Conditional statements
#IF
x <- 5
if (x > 0) {
  print("x is positive")
}
#IFELSE
x <- -3
if (x > 0) {
  print("x is positive")
} else {
  print("x is negative or zero")
}

x <- 0
if (x > 0) {
  print("x is positive")
} else if (x == 0) {
  print("x is zero")
} else {
  print("x is negative")
}


#Switch Statement
day <- 3
result <- switch(day,
                 "1" = "Monday",
                 "2" = "Tuesday",
                 "3" = "Wednesday",
                 "4" = "Thursday",
                 "5" = "Friday",
                 "Weekend")

print(result)

#Practice Problems:
#  Activity 1: Print a Personalized Message
# Print "Hello, [Ritchie]!"
print("Hello, [Ritchie]!")
# Activity 2: Perform Basic Arithmetic
# Assign two numbers
num1 <- 15
num2 <- 5
# Perform addition, subtraction, multiplication, and division
sum <- num1 + num2
difference <- num1 - num2
product <- num1 * num2
quotient <- num1 / num2
# Print the results
print(paste("Sum:", sum))
print(paste("Difference:", difference))
print(paste("Product:", product))
print(paste("Quotient:", quotient))

#Activity 3: Work with Vectors
# Create a vector of five numbers
# Find the sum, mean,
# and maximum value
numbers <- c(10, 20, 30, 40, 50)
sum_numbers <- sum(numbers)
mean_numbers <- mean(numbers)
max_value <- max(numbers)
# Print the results
print(paste("Sum:", sum_numbers))
print(paste("Mean:", mean_numbers))
print(paste("Maximum:", max_value))

#Activity 4: Conditional Statements
#use if and ifelse.
# Assign a number
num <- 25

# Check if the number is even or odd
if (num %% 2 == 0) {
  print("The number is even")
} else {
  print("The number is odd")
}
# Use ifelse for a vector
vec <- c(1, 2, 3, 4, 5)
result <- ifelse(vec %% 2 == 0, "Even", "Odd")
print(result)

#Activity 5: Create and Access a Data Frame
# Create a data frame with student details
students <- data.frame(
  Name = c("Alice", "Bob", "Charlie"),
  Age = c(20, 22, 21),
  Marks = c(85, 90, 95)
)

# Access the 'Name' column
print(students$Name)

# Filter students with Marks greater than 90
high_scorers <- students[students$Marks > 90, ]
print(high_scorers)
# Activity 6: Create and Plot Data,
# Basic plotting using R.
# Create data for plotting
x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 6, 8, 10)
# Create a scatter plot
plot(x, y, type = "o", col = "blue",
     main = "Line Plot", 
     xlab = "X-axis", ylab = "Y-axis")

matrix(1:9, nrow = 3, ncol = 3)  
matrix(1:9, 3, 3)  
