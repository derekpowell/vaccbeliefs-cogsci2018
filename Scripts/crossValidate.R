# author: Derek Powell
# created: 11-02-2017
# last updated: 1/11/18, 5:19 PM
# Also available as gist:
# devtools::source_gist("8838d867daa4185c9c09187a6b02f96b", filename="crossValidate.R")

library(tidyverse)
library(multidplyr)
library(modelr)
library(purrr)

cross_validate <- function(df, model_func, score_func, nFolds=10, nRuns=10) {
  # Function to perform cross validation and return results
  # uses serial processing, for faster processing use multi_cross_validate
  # df: data to examine
  # model_func: model to use for train and test
  # score_func: scoring function to measure model performance
  # cluster: multidplyer cluster
  # nFolds: number of folds for k-fold cross validation
  # nRuns: number of runs to average over
  
  for (rep in 1:nRuns) {
    repDF <- df %>% crossv_kfold(nFolds) %>% mutate(run = rep)
    
    if (rep == 1) {
      mainDF <- repDF
    }
    
    if (rep > 1) {
      mainDF <- rbind(repDF, mainDF)
    }
  }
  
  mainDF %>%
    mutate(train = map(train, as.data.frame)) %>%
    mutate(model = map(train, model_func)) %>% 
    mutate(test = map(test, as.data.frame)) %>%
    mutate(score = map2_dbl(model, test, score_func)) #%>%
    # group_by(run) %>% 
    # summarize(score=mean(score)) # should add weighting for uneven kfolds
}


cv_compare <- function(df, model_funcs, model_names, score_func, nFolds=10, nRuns=10) {
  # Function to perform cross validation with several functions and compare results
  # uses serial processing, for faster processing use multi_cv_compare
  # df: data to examine
  # model_funcs: list of model to use for train and test
  # model_names: list of model names to label results
  # score_func: scoring function to measure model performance
  # cluster: multidplyer cluster
  # nFolds: number of folds for k-fold cross validation
  # nRuns: number of runs to average over
  
  for (i in 1:length(model_funcs)) {
    model_func <- model_funcs[i]
    model_name <- model_names[i]
    cvResult <- cross_validate(df, model_func, score_func, nFolds, nRuns) %>%
      mutate(method=model_name)
    
    if (i == 1) {
      allResults <- cvResult
    }
    if (i > 1) {
      allResults <- bind_rows(allResults, cvResult)
    }
    
  }
  
  return(allResults) 
}


make_cluster_env <- function(nCores, cluster_packages) {
  # create cluster and load its environment with packages
  cluster <- create_cluster(cores = nCores) %>%
    cluster_library(cluster_packages)
  
  return(cluster)
}


multi_cross_validate <- function(df, model_func, score_func, cluster, nFolds=10, nRuns=10) {
  # Function to perform cross validation and return results using multiple CPU cores
  # df: data to examine
  # model_func: model to use for train and test
  # score_func: scoring function to measure model performance
  # cluster: multidplyer cluster
  # nFolds: number of folds for k-fold cross validation
  # nRuns: number of runs to average over
  
  for (rep in 1:nRuns) {
    # print(rep)
    repDF <- df %>% crossv_kfold(nFolds) %>% mutate(run = rep)

        if (rep == 1) {
      mainDF <- repDF
    }
    if (rep > 1) {
      mainDF <- rbind(repDF,mainDF)
    }
  }
  
  cluster %>%
    cluster_assign_value("model_func", model_func) %>%
    cluster_assign_value("score_func", score_func)
  
  nCores <- nrow(summary(cluster))
  group <- rep(1:nCores, length.out = nrow(mainDF))
  mainDF <- bind_cols(tibble(group), mainDF)
  byGroup <- partition(mainDF, group, cluster=cluster)
  
  result <- byGroup %>%
    mutate(train = map(train, as.data.frame)) %>%
    mutate(model = map(train, model_func)) %>%
    mutate(test = map(test, as.data.frame)) %>%
    mutate(score = map2_dbl(model, test, score_func)) %>%
    # group_by(run) %>% 
    # summarize(score=mean(score)) %>%
    collect()
  
  return(result)
}


multi_cv_compare <- function(df, model_funcs, model_names, score_func, cluster, nFolds=10, nRuns=10) {
  # Function to perform cross validation with several functions and compare results
  # uses multiprocessing for faster results
  # df: data to examine
  # model_funcs: list of model to use for train and test
  # model_names: list of model names to label results
  # score_func: scoring function to measure model performance
  # cluster: multidplyer cluster
  # nFolds: number of folds for k-fold cross validation
  # nRuns: number of runs to average over
  
  for (i in 1:length(model_funcs)) {
    model_func <- model_funcs[i]
    model_name <- model_names[i]
    cvResult <- multi_cross_validate(df, model_func, score_func, cluster, nFolds, nRuns) %>%
      mutate(method=model_name)
    
    if (i == 1) {
      allResults <- cvResult
    }
    
    if (i > 1) {
      allResults <- bind_rows(allResults, cvResult)
    }
    
  }
  return(allResults) 
}

cv_summary <- function(cvData) {
  x <- cvData %>% group_by(method,run) %>% summarize(score=mean(score))
  return(x)
}

# # uncomment below to test

# library(ggplot2)
# library(bnlearn)
# data(coronary)
# 
# modelFuncs <- c(hc, tabu, mmhc, rsmax2)
# modelLabels <- c( "hc","tabu","mmhc","rsmax2")
# 
# # using serial processing
# cvResults <- multi_cv_compare(coronary, modelFuncs, modelLabels, logLik, nRuns=5)
# 
# ggplot(cvResults, aes(x = method, y = score)) + geom_boxplot() +
#   labs(y = "Log Likelihood", x = "Algorithm")
# 
# # using parallel processing
# nCores <- parallel::detectCores()
# myCluster <- make_cluster_env(nCores, c("tidyverse","bnlearn"))
# cvResultsMulti <- multi_cv_compare(coronary, modelFuncs, modelLabels, logLik, myCluster, nRuns=5)
# 
# ggplot(cvResultsMulti, aes(x = method, y = score)) + geom_boxplot() +
#   labs(y = "Log Likelihood", x = "Algorithm")