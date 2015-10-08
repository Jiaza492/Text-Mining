# Ziang Jia
# STAT W4240 
# Homework 04
# Problem 4
# Nov 11th
# The following code analyzes the federalist papers

#################
# Pre-work
#################
## work direction
setwd("D://R Workspace//hw04");

## first include the relevant libraries
library(tm);
library(cvTools);
source("hw04.R");
source("hw04_q2.R");
source("hw04_q3.R");
set.seed(1);
# define a function 
## 5-fold Cross Validation Trails
crossV5 <- function(dtm){
  num = nrow(dtm);
  folds = cvFolds(num,K = 5,R = 0,type="random");
  sub_list = list();
  for(i in 1:5){
    index.test = folds$subsets[which(folds$which==i)];
    index.train = folds$subsets[-which(folds$which==i)];
    dtm.test = dtm[index.test,];
    dtm.train = dtm[index.train,];
    sub_list[[i]] = list(dtm.train, dtm.test);
  }
  trail_list = list(sub_list[[1]],sub_list[[2]],sub_list[[3]],sub_list[[4]], sub_list[[5]]);
  return(trail_list);
}

## define a function 
# find the error rate of cross validation for each u[i]
error.CV <- function(cv.hamilton,cv.madison,mu){
  k = length(cv.hamilton);
  correct.class = c();
  false.neg = c();
  false.pos = c();
  for(i in 1:k){
    # For each fold, find dtm.train and dtm.test for hamilton and madison respectively.
    temp.sample.hamilton = cv.hamilton[[i]];
    dtm.hamilton.train = temp.sample.hamilton[[1]];
    dtm.hamilton.test = temp.sample.hamilton[[2]];
    temp.sample.madison = cv.madison[[i]];
    dtm.madison.train = temp.sample.madison[[1]];
    dtm.madison.test = temp.sample.madison[[2]];
    # calculate each dtm's log prob vector
    logp.hamilton.test = log.pvec(dtm.hamilton.test, mu);
    logp.hamilton.train = log.pvec(dtm.hamilton.train, mu);
    logp.madison.test = log.pvec(dtm.madison.test, mu);
    logp.madison.train = log.pvec(dtm.madison.train, mu);
    # calculate prior
    num.files_list.train = nrow(dtm.hamilton.train)+nrow(dtm.madison.train);
    log.prior.hamilton = log(nrow(dtm.hamilton.train)/num.files_list.train);
    log.prior.madison = log(nrow(dtm.madison.train)/num.files_list.train);
    # do naiveBayes on both madison.test and hamilton.test
    result.hamilton.test = naiveBayes(logp.hamilton.train, logp.madison.train,
                                      log.prior.hamilton, log.prior.madison , dtm.hamilton.test);
    
    result.madison.test = naiveBayes(logp.hamilton.train, logp.madison.train,
                                     log.prior.hamilton, log.prior.madison , dtm.madison.test);
    # calculate corrrect classification rate
    correct.class[i] = prec.correct(result.hamilton.test, result.madison.test);
    # print(correct.class[i]);
    false.neg[i] = falseNeg(result.hamilton.test);
    # print(false.neg[i]);
    false.pos[i] = falsePos(result.madison.test);
    # print(false.pos[i]);
  }
  list.each.mu = list(correctClass = sum(correct.class)/k, 
                      falseNeg = sum(false.neg)/k, falsePos = sum(false.pos)/k);
  return (list.each.mu);
}

## define a function 
# find the true error rate each u[i]
error.true <- function(dtm.hamilton.train,dtm.madison.train,dtm.hamilton.test,dtm.madison.test,mu){
  k = length(mu);
  correct.class = c();
  false.neg = c();
  false.pos = c();
  for(i in 1:k){
    # For each mu
    # calculate each dtm's log prob vector
    logp.hamilton.test = log.pvec(dtm.hamilton.test, mu[i]);
    logp.hamilton.train = log.pvec(dtm.hamilton.train, mu[i]);
    logp.madison.test = log.pvec(dtm.madison.test, mu[i]);
    logp.madison.train = log.pvec(dtm.madison.train, mu[i]);
    # calculate prior
    num.files_list.train = nrow(dtm.hamilton.train)+nrow(dtm.madison.train);
    log.prior.hamilton = log(nrow(dtm.hamilton.train)/num.files_list.train);
    log.prior.madison = log(nrow(dtm.madison.train)/num.files_list.train);
    # do naiveBayes on both madison.test and hamilton.test
    result.hamilton.test = naiveBayes(logp.hamilton.train, logp.madison.train,
                                      log.prior.hamilton, log.prior.madison , dtm.hamilton.test);
    
    result.madison.test = naiveBayes(logp.hamilton.train, logp.madison.train,
                                     log.prior.hamilton, log.prior.madison , dtm.madison.test);
    # calculate corrrect classification rate
    correct.class[i] = prec.correct(result.hamilton.test, result.madison.test);
    # print(correct.class[i]);
    false.neg[i] = falseNeg(result.hamilton.test);
    # print(false.neg[i]);
    false.pos[i] = falsePos(result.madison.test);
    # print(false.pos[i]);
  }
  list.each.mu = list(correctClass = correct.class, 
                      falseNeg = false.neg, falsePos = false.pos);
  return (list.each.mu);
}

#################
# Problem 4
################# 
## a)
## initial a vector of u
D = nrow(dictionary);
u = c(1/D, 10/D, 100/D, 1000/D, 10000/D);

## Test on 5-folds cross validation
# do 5-folds cross validation on two training sets.
cv.hamilton = crossV5(dtm.hamilton.train);
cv.madison = crossV5(dtm.madison.train);
# error rate for each mu 
error.cv.mu = list();
correctRate.error.cv = c();
falNeg.error.cv = c();
falPos.error.cv = c();
for(j in 1:length(u)){
  error.cv.mu[[j]] = error.CV(cv.hamilton,cv.madison,u[j]);
  temp.error.cv.mu = error.CV(cv.hamilton,cv.madison,u[j]);
  correctRate.error.cv[j] = temp.error.cv.mu[[1]];
  falNeg.error.cv[j] = temp.error.cv.mu[[2]];
  falPos.error.cv[j] = temp.error.cv.mu[[3]];
}

# plot mu aganiest correct rate, false Pos, false Neg, respectively
plot(log(u),correctRate.error.cv,type = 'b',main = "CV:Correctly Classified Rate");
plot(log(u),falNeg.error.cv,type = 'b',main = "CV:False Negative Rate");
plot(log(u),falPos.error.cv,type = 'b',main = "CV:False Positive Rate");

## c)
error.true.mu = error.true(dtm.hamilton.train,dtm.madison.train,dtm.hamilton.test,dtm.madison.test,u);
# plot 
plot(log(u),error.true.mu$correctClass,type = 'b',main = "Correctly Classified Rate");
plot(log(u),error.true.mu$falseNeg,type = 'b',main = "False Negative Rate");
plot(log(u),error.true.mu$falsePos,type = 'b',main = "False Positive Rate");

# ROC
plot(sort(rbind(rbind(0,error.true.mu$falsePos),1)), sort(rbind(rbind(0,c(1,1,1,1,1)-error.true.mu$falseNeg),1)),xlab = "false Positive",ylab ="true Positive",type="b",main="ROC Curve")
abline(0,1)

# error percentage 
correctClass.error = -1*(error.true.mu$correctClass[4]-correctRate.error.cv[4])/error.true.mu$correctClass[4];
falseNeg.error = -1*(error.true.mu$falseNeg[4]-falNeg.error.cv[4])/error.true.mu$falseNeg[4];
falsePos.error = -1*(error.true.mu$falsePos[4]-falPos.error.cv[4])/error.true.mu$falsePos[4];