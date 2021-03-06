
# A First Look at R"
# author: "Joseph Rickert"
# date: "10/19/15"
# output: html_document

## A Simple Regression Example
# This simple regression example comes from Peter Dalgaard's classic book: 
# Statistics and Computing, Introductory Statistics with R (Springer 2002)  

library(ISwR)             # Load the ISwR package written for the book
data(package="ISwR")      # Look at the data sets that come with the package

# The thuesen data set which comes with the ISwR package 
# has 24 rows and two columns containing ventricular shortening 
# velocity and blood glucose for type 1 diabetic patients. 
# Lets load the data set into the R environment and have a look at it. 
# Later on we will see how to read data from files and databases into R.

# The data set is structured as a data frame, the most common data structure
# for doing statistical analyses in R.

data(thuesen)            # loads the data
thuesen                  # prints it to the console

# Very useful functions for examining large data sets are:

head(thuesen)            # print the first 6 lines
tail(thuesen,2)          # print last 2 lines
dim(thuesen)             # look at the dimension of the data frame
class(thuesen)           # Find out what kind of object thuesen is

## Basic Statistics
summary(thuesen$blood.glucose) # summary statistics for a variable
summary(thuesen)

## Vectorized Code
# This next bit of code shows off a very powerful feature of the R language: 
# how many functions are "vectorized". The function sapply() takes the funcion class() that we just used on the data frame and applies it to all of the columns of the data frame.

sapply(thuesen,class)   # Find out what kind of animals the variables are
sapply(thuesen,summary) # Get numerical summaries of the variables

## Plots in R
# R has three systems for static graphics: base graphics, lattice and ggplot2. 
# For now we will see how easy it is to produce bare bones plots with the base 
# graphics system.

# Note that you can produce a basic scatter plot either by giving the 
# plot() function a formula or a pair of coordinates 

#
par(mfrow=c(2,2))        # put multiple plots on a single graph
hist(thuesen$blood.glucose)
boxplot(thuesen$blood.glucose)
hist(thuesen$short.velocity)
plot(thuesen$blood.glucose,thuesen$short.velocity,   # index into the data frame 
pch = 19, col="red",
main = "Scatter Plot (cleaned up vari names)",
xlab = "blood glucose",
ylab = "short velocity")   
#

## Linear Regression in R
# Now, let's build a simple regression model, examine the results 
# of the model and plot the points and the regression line.

model <- lm(short.velocity ~ blood.glucose, data=thuesen) # build the model
summary(model) 
# look at results 
par(mfrow=c(1,1))
plot(x = thuesen$blood.glucose,
     y = thuesen$short.velocity,
     xlab = "blod glucose (mmol / l)",
     ylab = "circumferential shortening velocity (%/s)",
     main = "Thuesen Data Set",
     col = "blue",pch=19)
abline(model,col="red")             # adds the regression line

#Produce the standard ANOVA table for the model      
anova(model)   

## Regression Diagnostics
# It is easy to get regression diagnostic plots. 
# The same plot function that plots points either with a formula or 
# with the coordinates also has a "method" for dealing with a model object.

par(mfrow=c(2,2))                   # Set up for multiple plots on the same figure
plot(model,col="blue")              # Look at some model diagnostics

## The Model Object
# Finally, let's look at the model object. R packs everything that goes with the model, 
# the fornula, and results into the object. You can pick out what you need by 
# indexing into the model object.

str(model)
# Index into the model object and pull out the coefficients

model$coefficients
beta_0 <- model$coefficients[1]
beta_0
beta_1 <- model$coefficients[2]
beta_1

