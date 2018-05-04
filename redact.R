# Author: Derek Powell
# Date: 2/10/18, 11:26 AM
# ---
# Script to redact workerIds and ip addresses from qualtrics files.
# Script looks for "date_private/" directory, saves resulting data in "data" directory.
# Personal info is replaced with a "hash" using xxhash64,
# a super fast hash algo w/ short resulting hashes (confirmed appropriate for this use)

suppressMessages(library(tidyverse))
suppressMessages(library(digest))

currentDir <- getwd()

make_salted <- function(x) {
  paste0(x, "alpha") # arbitrary "salt" (optional)
}

make_hash <- function(x){

  x <- make_salted(x)

  xxhash <- partial(digest, algo="xxhash64")
  sapply(x, xxhash)
}

parent_directory <- function(dirString) {
  ind <- as.numeric(gregexpr("/",dirString)[[1]])
  substring(dirString,0,ind[length(ind)]-1)
}

# privateDirs <- paste0(currentDir,"/Data_private/")
privateDirList <- list.files(path=currentDir, pattern="/*_private", recursive=TRUE, include.dirs=TRUE)

for (d in privateDirList) {
  privateDir <- paste0(currentDir,"/",d)
  workingDir <- parent_directory(privateDir)
  fileList <- list.files(privateDir, pattern="*.csv")
  
  for (f in fileList) {
    inFile <- paste0(privateDir, "/",f)  
    df <- read.csv(inFile, stringsAsFactors = FALSE)
    
    if ("workerId" %in% colnames(df)) {
      df <- df %>% mutate(workerId=make_hash(workerId))
      print(paste(f,"--- workerId variable redacted"))
    }
    
    else {print(paste(f, "--- no workerId variable present"))}
    
    if ("IPAddress" %in% colnames(df)) {
      df <- df %>% mutate(IPAddress=make_hash(IPAddress))
      print(paste(f,"--- IPAddress variable redacted"))
    }
    
    else {print(paste(f, "--- no IPAddress variable present"))}
    
    if ("LocationLatitude" %in% colnames(df)) {
      df <- df %>% mutate(LocationLatitude = round(as.numeric(LocationLatitude),2))
      print(paste(f,"--- LocationLatitude variable rounded"))
    }
    
    else {print(paste(f, "--- no LocationLatitude variable present"))}
    
    if ("LocationLongitude" %in% colnames(df)) {
      df <- df %>% mutate(LocationLongitude = round(as.numeric(LocationLongitude),2))
      print(paste(f,"--- LocationLongitude variable rounded"))
    }
    
    else {print(paste(f, "--- no LocationLatitude variable present"))}
    
    if (!"Data" %in% list.files(workingDir)) {
      dir.create(paste0(workingDir,"/data"))
    }
    
    outFile <- paste0(workingDir,"/data/", f)
    
    write.csv(df, file=outFile)
  }
}
