## Author: Sebastián Castillo
## Date: 2019-01-05

## Load CRAN modules 
library(downloader)
library(dplyr)
library(knitr)
library(tm)
library(stringr)


## Create Dir
if(!file.exists("./projectData")){
  dir.create("./projectData")
}

## Download the dataset and unzip folder
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if(!file.exists("./projectData/Coursera-SwiftKey.zip")){
  download.file(url, destfile="./projectData/Coursera-SwiftKey.zip", mode = "wb")
}


## Check if zip has already been unzipped?
if(!file.exists("./projectData/final")){
  unzip(zipfile="./projectData/Coursera-SwiftKey.zip",exdir="./projectData")
}

## List all the files of /final/en_US Dataset folder
path <- file.path("./projectData/final" , "en_US")
files <- list.files(path, recursive=TRUE)

# Lets make a file connection of the twitter data set
con <- file("./projectData/final/en_US/en_US.twitter.txt", "r") 
lineTwitter<-readLines(con) 

# 1 file size?
file.size("en_US.blogs.txt")

# 2 lines of text?
length(lineTwitter)

# 3 length of the longest line 
# Twitter data
longTwitter <- length(lineTwitter)
max(nchar(lineTwitter))
# Blog data
setwd("~/R/Processing_Text_Project/projectData/final/en_US")
list.files()
lineBlogs <- readLines("en_US.blogs.txt")
longBlogs <- max(nchar(lineBlogs))
# News data
lineNews <- readLines("en_US.news.txt") 
longNews <- max(nchar(lineNews))

# 4 
lineTwitter <- tolower(lineTwitter)
length(grep("love",lineTwitter)) / length(grep("hate",lineTwitter))

# 5 
biostatsTwitter <- grep("biostats",lineTwitter)
lineTwitter[biostatsTwitter]

# 6 
sentenceTwitter <- str_detect(lineTwitter, "A computer once beat me at chess, but it was no match for me at kickboxing")
sum(sentenceTwitter)

# Close the connection handle when you are done
close(con)
