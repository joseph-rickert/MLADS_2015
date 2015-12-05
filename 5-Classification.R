# title: 5 - Classification Tree
# author: Joseph Rickert
# date: "12/3/15"

## Load required packages
library(rattle)       # data set plus plot for decision trees
library(rpart)				# CART Decision Trees
library(colorspace)		# used to generate colors for plots
library(rpart.plot)          # plot decision trees
library(ROCR)				  # ROC 
library(caret)        # just for confusion matrix

## Some Convenience Functions
# Function to divide data into training, and test sets 
splitData <- function(data=dF,pctTrain=0.7)
{
  # fcn to create indices to divide data into random 
  # training, validation and testing data sets
  N <- nrow(data)										
  trainInd <- sample(N, pctTrain*N)								
  testInd <- setdiff(seq_len(N),trainInd)	
  indexList <- list(trainInd,testInd)
  return(indexList)
} 

# Function to generate the confusion matrix and percent correct
score <- function(model,target=dF[testInd, 21],predict=pr){
  results.test <- table(target,predict,dnn=c("Actual", "Predicted"))
  pct.test.correct <- round(100 * sum(diag(results.test)) / sum(results.test),2)
  results <- list(results.test,pct.test.correct)
  (results)
}

## Read the Data and Prepare the Training and Test Sets
# Get the weather data and select the subset for modeling

data(weather)
head(weather,2)
# Select variables for the model
dF <- subset(weather,select=c(MinTemp:RainToday,RainTomorrow))
set.seed(42)									 # Set seed

indices <- splitData(dF)       # Generate indidices to split data
trainInd <- unlist(indices[1]) # Training data
testInd <- unlist(indices[2])  # Test data

dim(dF[trainInd,]); head(dF[trainInd,],2)
dim(dF[testInd,]); head(dF[testInd,],2)

## Build a Tree Model with rpart   

# The rpart algorithm based on recursive partitioning       
# (See section 11.2 of Data Mining with Rattle and R by williams)   
# The rpart Algorithm:  
#   1. -   Partition the data set according to some criterion of "best" partition   
#   2. -   Do the same for each of the two new subsets   
#   3. -  Once a partition is made, stick with it (greedy approach)   
# 
# Measures of "best" partition:   
#   1. -    information gain (the default)   
#   2. -    Gini   
# 
# Information Gain Algorithm:   
#   For all possible splits (partitions)   
#    1. -    Split data, D, into to subsets S1 and S2 where D = S1 U S2   
#    2. -    Calculate information I1 and I2 associated with S1 and S2   
#    3. -    Compute total information of split: Info(D,S1,S2) = (|D1|/D)*I1 + (|D2|/|D|)*I2   
#    4. -    Compute the information gain of the split: info(D) - info(D,S1,S2)   
#    5. -    Select split with greatest information gain

### Build a classification tree model   
# Set up the control function for the tree
ctrl <- rpart.control(minsplit = 20,  # Min obs at node for splitting
                      cp = 0.05,     # Complexity parameter
                      maxcompete = 4, # No. competor splits in retained
                      maxsurrogate = 5, # No. surrogate splits retained
                      usesurrogate = 2, # How to use surrogate splits
                      xval = 10,        # No. of cross validations
                      surrogatestyle = 0, # How to select surrogates
                      maxdepth = 30       # Max depth of tree
)


form <- formula(RainTomorrow ~ .)				# Describe the model to R
model <- rpart(formula=form,
               data=dF[trainInd,],
               control = ctrl)	# Build the model
model	

### Interpreting the Model Results     

# Every line of the output the follows will have   
#  1. node: a node number   
#  2. split: the logic for how the node splits the data   
#  3. n: the number of observations considered at that split    
#  4. loss: the number of incorrectly classified observations   
#  5. the majority class at that node   
#  6. yprob: the distribution of classes at that node   
# 
# So for the second line above: Pressure3pm>=1011.9 204 16 No (0.92156863 0.07843137)   
#  1. node: 2)      
#  2. split: if Pressure3pm > 1011.9 go left down tree   
#  3. n: 204 obversations went down this branch      
#  4. loss: 16 misclassified observations   
#  5. Most observations were No   
#  6. 92% of obs have target var No, 8% are yes   

### Examine the results    
# Two different formats for extracting results
printcp(model)
summary(model)

leaf <-model$where						   # find out in which leaf each observation ended up
head(leaf,20)

### Plot the Tree
# First a basic tree plot and then a fancy one
prp(model,type=1,extra=100)   # basic plot
fancyRpartPlot(model)         # a fancy plot
title(main="Decision Tree weather.csv $ RainTomorrow",line=3)

### Evaluate model performance on the test set 
# Run the tree model on the test set 
pr <- predict(model, dF[testInd, ], type="class")

# Evaluate the model
score(model)                          # Basic confusion matrix 
confusionMatrix(pr,dF[testInd,21])   # Preview of caret


### Draw the ROC Curve
# First, create a prediction object.
pred <- prediction(as.vector(as.numeric(pr)), dF[testInd,21])
perf <- performance(pred,"tpr","fpr")
plot(perf, main="ROC curve")


### Explore an unpruned tree
# The complexity parameter sets the minimum benefit that must be 
# gained at each split of the decision tree. (default = .01) Typical behavior with cp=0 is to see the error reate decrease at first and then begin to increase.

control <- rpart.control(cp=0,
                         minsplit=10,
                         minbucket=0,
                         maxdepth=20,
                         usesurrogate=0,
                         maxsurrogate=0)
model2 <- rpart(formula=form,control=control,data=dF[trainInd,])
print(model2$cptable)
plotcp(model2,col="blue")
grid(col="blue")
