# Ziang Jia
# STAT W4240 
# Homework 04
# Problem 2
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

## define a function 
# Naive Bayes Classification Procedure
naiveBayes <- function(logp.hamilton.train, logp.madison.train,
                       log.prior.hamilton, log.prior.madison , dtm.test){
  # initial result vector
  result = c();
  # calculate the words probability vector of each Federalist in dtm.test
  for(i in 1: nrow(dtm.test)){
    # probability = frenquency
    temp= dtm.test[i,];
    # directly calculate log likelihood of each Federalist i in dtm.test
    # the number of word * prob that its belongs to class k(k = 1, 2)
    lh.hamilton = t(temp) %*% logp.hamilton.train + log.prior.hamilton;
    # print(lh.hamilton);
    lh.madison = t(temp) %*% logp.madison.train + log.prior.madison;
    # print(lh.madison)
    if(lh.hamilton >= lh.madison){
      class = "hamilton";
    }else{
      class = "madison";
    }
    result = cbind(result, class); 
  }
  return(result);
}

## test such naiveBayes classifier with both dtm.hamilton.test & dtm.madison.test
# calculate prior
num.files_list.train = length(hamilton.train)+length(madison.train);
log.prior.hamilton = log(length(hamilton.train)/num.files_list.train);
log.prior.madison = log(length(madison.train)/num.files_list.train);

# classify procedure
result.hamilton.test = naiveBayes(logp.hamilton.train, logp.madison.train,
                                  log.prior.hamilton, log.prior.madison , dtm.hamilton.test);

result.madison.test = naiveBayes(logp.hamilton.train, logp.madison.train,
                                  log.prior.hamilton, log.prior.madison , dtm.madison.test);



