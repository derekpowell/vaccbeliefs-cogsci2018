# bnTheoryBlacklist.R
# Author: derek powell
# created: 1/17/18, 12:56 PM

# this script will take a dataframe with variables for the "order" of nodes
# and create a blacklist that prevents violations of that order

library(tidyverse)

# testData <- data.frame(node=c("diseaseRare", 
#                               "diseaseSevere", 
#                               "hb", 
#                               "infantImmLimCap", 
#                               "infantImmWeak", 
#                               "medSkept", 
#                               "nat", 
#                               "overpar", 
#                               "parentExpert", 
#                               "vaccDanger", 
#                               "vaccEff", 
#                               "vaccIntent", 
#                               "vaccStrain",
#                               "vaccTox"),
#                        order = c(
#                                  3,
#                                  3,
#                                  1,
#                                  3,
#                                  3,
#                                  2,
#                                  1,
#                                  2,
#                                  2,
#                                  3,
#                                  3,
#                                  4,
#                                  3,
#                                  3
#                                ))

make_theory_blacklist <- function(data) {
  
  result <- NULL
  
  for (targNode in data$node) {
    ord <- data %>% dplyr::filter(node==targNode) %>% dplyr::select(order) %>% as.numeric()
    d <- data %>% dplyr::filter(order < ord) %>% dplyr::mutate(from=targNode, to=node) %>% dplyr::select(from, to)
    
    if (is.null(result)) {
      result <- d
    }
    else {
      result <- dplyr::bind_rows(result, d)
    }
  }
  
  result <- dplyr::distinct(result, to, from)
  
  return(result)
}

combine_blacklists <- function(bList1, bList2) {

  dplyr::bind_rows(bList1, bList2) %>% dplyr::distinct(to, from)

  }

# 
# result <- NULL
# 
# for (targNode in testData$node) {
#   ord <- testData %>% filter(node==targNode) %>% select(order) %>% as.numeric()
#   d <- testData %>% filter(order < ord) %>% mutate(from=targNode, to=node) %>% select(from, to)
#   
#   if (is.null(result)) {
#     result <- d
#   }
#   else {
#     result <- bind_rows(result, d)
#   }
# }
# 
# result <- distinct(result, to, from)