
# : 2 - Data Structures
# author: Joseph Rickert
# date: "September 28, 2015"

## Basic Data Structures
# R has 5 basic data structures:     
# 1. one dimensional atomic vectors  
# 2. two dimensional matrices  
# 3. multi-dimensional arrays  
# 4. lists  
# 5. data frames  
# 
# Note that there are no scalars in R.

## Atomic Vectors

v <- 10           # or v = 10 
v
u <- c(12,55,43,9)
u

# Atomic vectors have 3 basic properties   
# 1. Type - determine with the typeof() function   
# 2. Length - determine with the length() function   
# 3. Attributes - metadata - look at with attributes()   

# The four common types of atomic vectors are:   
# 1. double   
# 2. integer   
# 3. character   
# 4. logical   

typeof(v)
length(v)
length(u)

# Integer vectors
u1 <- c(12L,55L,43L,9L)    # specify integer type
typeof(u1)
u  <- as.integer(u)        # coerce u to be an integer vector
typeof(u)

# Character vectors
c1 <- c("abc","def","any old string")       # form a character vector
typeof(c1)
length(c1)

c2 <- c("2","3")  
c2
u2 <- as.integer(c2)        # coerce c2 to be an integer
u2
typeof(u2)

# Logical vectors
u3 <- c(TRUE,T,FALSE,F)    # form a logical vector
typeof(u3)

u4 <- runif(10) > .5		# create a logical vector  
u4  
typeof(u4)  

## Attributes
# As stated above attributes are meta-data associated with 
# R objects such as atomic, vectors and arrays. 
# Many R objects have a class attribute, a character vector 
# giving the names of the classes from which the object inherits. 
# If the object does not have a class attribute, it has an 
# implicit class,  "matrix", "array" or "integer" for example. 
# Objects can be given names and arbitrary attributes.

u5 <- seq(1,50,by=5)        # R has many ways to generate sequences
names(u5) <- letters[1:10]  # Index into the stored chacter vector "letters"
attr(u5,"custom attribute") <- "This is a vector"
u5
typeof(u5)
class(u5)                # 
names(u5)
attributes(u5)

## Vector Operations

v1 <- rnorm(10)          # vector of Normal random variates
v1[5]                    # Index into a vector
#
v2 <- 1:10               # another way to generate a sequence
v2
#
s <- v1 + v2             # add vectors element by element
s
#
p <- v1 * v2             # multiply element by element
p
#
s[v2 > 5]                # index into one vector using a function on another vector

## Matrices

m1 <- matrix(1:100,nrow=10,ncol=10,byrow=TRUE)  
m1  
typeof(m1)  
class(m1)  
length(m1)  
dim(m1)  
m1[5,5]  					# index into the matrix	  	
#

##Some Elementary Matrix Functions

m1^2						# square elements
#
sqrt(m1)
#
m2 <- matrix(1:100,nrow=10,ncol=10)
m2
#
m1 + m2						# add 2 matrices elementwise
#
m1 * m2						# multiply 2 matrices elementwise
#
m1 %*% m2					# matrix multiplication
#
c3 <- subset(letters,letters!="z")
c3
m3 <- matrix(c3,nrow=5)		# matrix of characters
m3
typeof(m3)
class(m3)

#Outer Product
 
x <- y <- 1:9
names(x) <- x
x
names(y) <- paste(y,":",sep="")
y
# 
y %o% x						# Multiplication table
outer(y, x, "^")			# Table of Powers

## Arrays
#Multidimensional arrays are basic data types   

A <- array(runif(40),c(5,4,2))
A
B <- array(A,c(2,5,4))   # reshape A
B
 
## Lists

lst <- list(v,v1,v2,c1,list(m1,m2),c3,m3, A,B)
lst
typeof(lst)
class(lst)
length(lst)
lst[[5]][2]  				# Index into a list


## Data Frames
# Data frames are the most common data structure in R. 
# They mirror the ways that statisticians think about data: 
# each row an observation and each column a variable.

# Here we build a data frame "by hand". 
# The data comes from the cystfbr data set in the ISwR package.
age <-c(7,7,8,8,8,9,11,12,12,13,13,14,14,15,16,17,17,17,17,19,19,20,23,23,23)
length(age)
typeof(age)
summary(age)
#
sex <- c("f","m","f","m","f","f","m","m","f","m",
         "f","m","f","m","m","m","f","m","f","m","f","f","f","f","f")
length(sex)
typeof(sex)
#
pemax <- c(95,85,100,85,95,80,65,110,70,95,110,90,100,80,134,134,165,120,130,85,85,160,165,95,195)
typeof(pemax)
#
height <- c(109,112,124,125,127,130,139,150,146,155,
            156,153,160,158,160,153,174,176,171,156,174,178,180,175,179)
typeof(height)
#
weight <- c(13.1,12.9,14.1,16.2,21.5,17.5,30.7,28.4,25.1,31.5,
            39.9,42.1,45.6,51.2,35.9,34.8,44.7,60.1,42.6,37.2,54.6,64.0,73.8,51.1,71.5)
typeof(weight)
#
bmp <- c(68,65,64,67,93,68,89,69,67,68,89,90,93,93,66,70,70,92,69,72,86,86,97,71,95)
typeof(bmp)
#
dF <- data.frame(pemax,sex,height,weight,bmp)
head(dF)
dim(dF)
sapply(dF,typeof)
sapply(dF,class)

# Notice that sex is now of type "integer"!!! 
# The data.frame() function automatically turned it 
# into an object of class "factor". 
# This is most likely what a statistician would want done. 
# Factors are "categorical variables".

# Data scientists and software developers, however, usually don't 
# like R messing with their strings. 
# In this case, sex got made into a factor because the default 
# for the parameter stringsAsFactors in the data.frame() function 
# is TRUE.

args(data.frame)

#As the documentation data.frame() states:
# stringsAsFactors	
# logical: should character vectors be converted to factors?
# The ‘factory-fresh’ default is TRUE, but this can be changed 
# by setting options(stringsAsFactors = FALSE).

## EXERCISE
# 1. Explore the data frame dF
# 2 Compute summary statistics
# Fit a linear model: pemax ~ sex + height + weight + bmp




