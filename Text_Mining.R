# Ziang Jia
# STAT W4240 
# Homework 04 
# Problem 1
# Nov 11th
# The following code analyzes the federalist papers

#################
# Pre-work
#################
## work direction
setwd("D://R Workspace//hw04")

## first include the relevant libraries
library(tm);

## define a funcion 
# This code uses tm to preprocess the papers into a format useful for NB
preprocess.dir <- function(dirName){
  
  # the directory must have all the relevant text files
  ds = DirSource(dirName)
  # Corpus will make a tm document corpus from this directory
  fp = Corpus( ds )
  # make all words lower case
  fp = tm_map( fp , content_transformer(tolower));
  # remove all punctuation
  fp = tm_map( fp , removePunctuation);
  # remove stopwords like the, a, and so on.  
  fp = tm_map( fp, removeWords, stopwords("english"));
  # remove stems like suffixes
  fp = tm_map( fp, stemDocument)
  # remove extra whitespace
  fp = tm_map( fp, stripWhitespace)  
  # now write the corpus out to the files for our future use.
  # MAKE SURE THE _CLEAN DIRECTORY EXISTS
  writeCorpus( fp , sprintf('%s_clean',dirName) )
}

## define a function
# read the files into a file list
read.dir <- function(dirName) {
  # Initial a filenames list
  files_list = list();
  # Get a list of filenames in the directory
  filenames = dir(dirName,full.names=TRUE);
  for (i in 1:length(filenames)){
    files_list[[i]] = scan(filenames[i],what="",quiet=TRUE);
  }
  return(files_list)
}

## define a function
# Make dictionary sorted by number of times a word appears in corpus 
# This returns a dataframe that is sorted by the number of times a word appears 
sorted.dic.df <- function(files_list){
  
  # initial a list of vectors to one big vetor
  full_dic = unlist(files_list) 
  # Tabulates the full dictionary
  tab_dic = tabulate(factor(full_dic)) 
  # Find unique values
  dictionary = unique(full_dic) 
  # Sort them alphabetically
  dictionary = sort(dictionary)
  dictionary.df = data.frame(word = dictionary, count = tab_dic)
  sort.dictionary.df = dictionary.df[order(dictionary.df$count,decreasing=TRUE),];
  return(sort.dictionary.df)
}

# define a function
# Make a document-term matrix, which counts the number of times each dictionary element is used in a document
document.term.matrix <- function(files_list,dictionary){
  # initial document term matrix(rows are documents and columns are words)
  dtm = mat.or.vec(length(files_list),nrow(dictionary)); # A matrix filled with zeros
  # count each word in each file in files_list
  for (i in 1:nrow(dtm)){
    temp.file = files_list[[i]]
    for (j in 1:length(temp.file)){
      ind = which(dictionary == temp.file[j])[[1]]
      dtm[i,ind] = dtm[i,ind] + 1
    }
  }
  return(dtm);
}

# define a function 
# compute a probability vector for a dtm
log.pvec <- function(dtm,mu){
  # Sum up the number of instances per word
  num.word_inst = colSums(dtm)
  # Sum up number of words
  num.words = sum(num.word_inst)
  # Incorporate mu and normalize
  log.pvec = log(num.word_inst + mu) - log(mu*ncol(dtm) + num.words)
  return(log.pvec)
}


#################
# Problem 1a
#################
## use function preprocess.directory to clean up corpus
# hamilton_test
preprocess.dir("fp_hamilton_test");
# hamilton_train
preprocess.dir("fp_hamilton_train");
# madison_test
preprocess.dir("fp_madison_test");
# madison_train
preprocess.dir("fp_madison_train");


#################
# Problem 1b
#################
## To read in data from the directories:
# hamilton.test
hamilton.test = read.dir("fp_hamilton_test_clean");
# hamilton.train
hamilton.train = read.dir("fp_hamilton_train_clean");
# madison.test
madison.test = read.dir("fp_madison_test_clean");
# madison.train
madison.train = read.dir("fp_madison_train_clean");


#################
# Problem 1c
#################
## concatenating the individual lists into a single large one for all files
allfiles_list = list();
allfiles_list = c(hamilton.test, hamilton.train,madison.test, madison.train);

## create a dictionary with allfiles_list
dictionary = sorted.dic.df(allfiles_list);

#################
# Problem 1d
#################
## create document.term.matrix for each files_list
# dtm.hamilton.test
dtm.hamilton.test = document.term.matrix(hamilton.test,dictionary);
# dtm.hamilton.train
dtm.hamilton.train = document.term.matrix(hamilton.train,dictionary);
# dtm.hamilton.test
dtm.madison.test = document.term.matrix(madison.test,dictionary);
# dtm.hamilton.test
dtm.madison.train = document.term.matrix(madison.train,dictionary);


#################
# Problem 1e
#################
## compute log probability likelihood vector for each dtm
mu = 100/nrow(dictionary);
# hamilton.test
logp.hamilton.test = log.pvec(dtm.hamilton.test, mu);
# hamilton.train
logp.hamilton.train = log.pvec(dtm.hamilton.train, mu);
# madison.test
logp.madison.test = log.pvec(dtm.madison.test, mu);
# madison.train
logp.madison.train = log.pvec(dtm.madison.train, mu);


#################
# End of Script
#################


