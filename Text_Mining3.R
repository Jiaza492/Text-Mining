# Ziang Jia
# STAT W4240 
# Homework 04
# Problem 3
# Nov 11th
# The following code analyzes the federalist papers

#################
# Pre-work
#################
## work direction
setwd("D://R Workspace//hw04");

## first include the relevant libraries
library(tm);
source("hw04.R");
source("hw04_q2.R");

## define a function
# check the precentage of correction
prec.correct <- function(result.hamilton.test, result.madison.test){
  count = 0;
  sum = length(result.hamilton.test)+length(result.madison.test);
  for(i in 1:length(result.hamilton.test)){
    class = result.hamilton.test[i];
    if(class == "hamilton"){
      count = count+1;
    }
  }
  for(j in 1:length(result.madison.test)){
    class = result.madison.test[j];
    if(class == "madison"){
      count = count +1;
    }
  }
  return(count/sum);
}

## define a function
# true positive
truePos <- function(result.hamilton.test){
  count.true_positive = 0;
  for(i in 1:length(result.hamilton.test)){
    class = result.hamilton.test[i];
    if(class == "hamilton"){
      count.true_positive = count.true_positive + 1;
    }
  }
  return(count.true_positive/length(result.hamilton.test));
}

# define a function
# true negative
trueNeg <- function(result.madison.test){
  count.true_neg = 0;
  sum = length(result.madison.test);
  for(i in 1:length(result.madison.test)){
    class = result.madison.test[i];
    if(class == "madison"){
      count.true_neg = count.true_neg + 1;
    }
  }
  return(count.true_neg/sum);
}

## define a function
# false_positive
falsePos <- function(result.madison.test){
  sum = length(result.madison.test);
  count.false_positive = 0;
  for(i in 1:length(result.madison.test)){
    class = result.madison.test[i];
    if(class == "hamilton"){
      count.false_positive = count.false_positive + 1;
    }
  }
  return(count.false_positive/sum);
}

## define a function 
# false_negative
falseNeg<-function(result.hamilton.test){
  sum = length(result.hamilton.test);
  count.false_neg = 0;
  for(i in 1:length(result.hamilton.test)){
    class = result.hamilton.test[i];
    if(class == "madison"){
      count.false_neg = count.false_neg + 1;
    }
  }
  return(count.false_neg/sum);
}

#################
# Problem 3
################# 
## find the precentage of correctly classified papers
View(result.hamilton.test);
View(result.madison.test);
# check the precentage of correction
print(sprintf('The percentage of correctly classified paper is: %s', prec.correct(result.hamilton.test, result.madison.test)));
# true positive
print(sprintf("True Positive:%s",truePos(result.hamilton.test)));
# true nagetive
print(sprintf("True Negative:%s",trueNeg(result.madison.test)));
# false_positive
print(sprintf("False Positive:%s",falsePos(result.madison.test)));
# false_negative
print(sprintf("False Negaitive:%s",falseNeg(result.hamilton.test)));