# Author: Derek (but just rearranging kara's code into functions)
# Last Updated: 11/21/17, 4:26 PM

# NOTE: only blacklist code works

# source("../study1/vacc_import_data.R")

library(tidyverse)

# 
# bd_to_bn_whitelist <- function(bdSummaryObj, threshold=.995) {
#   # creates a whitelist for bnlearn from BDgraph adjacency matrix
#   
#   s <- bdSummaryObj
#   s[lower.tri(s)] = t(s)[lower.tri(s)]
#   
#   fromDF <- s %>%
#     data.frame() %>%
#     rownames_to_column("from") %>%
#     gather(to, posterior, -from) %>%
#     filter(posterior >= threshold) %>%
#     data.frame() %>%
#     dplyr::select(from, to)
#   toDF <- bdSummaryObj %>%
#     data.frame() %>%
#     rownames_to_column("to") %>%
#     gather(from, posterior, -to) %>%
#     filter(posterior >= threshold) %>%
#     data.frame() %>%
#     dplyr::select(from, to)
#   whitelist <- bind_rows(fromDF,toDF)
# }

bd_to_bn_blacklist <- function(BDtrimmed) {
  # creates a blacklist for bnlearn from BDgraph adjacency matrix
  
    s <- as.matrix(BDtrimmed)
    s[lower.tri(s)] = t(s)[lower.tri(s)]
  
    blacklist <- s %>%
      data.frame() %>%
      rownames_to_column("from") %>%
      gather(to, present, -from) %>%
      filter(present==0) %>%
      filter(from!=to) %>%
      data.frame() %>%
      dplyr::select(from, to)
    
}
