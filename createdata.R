library(doParallel)
registerDoParallel(makeCluster(4))
library(stringr)
library(dplyr)
library(caret)
library(tau)
library(data.table)
library(downloader)
library(knitr)
library(tm)


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

blog_file <- file("~/R/Processing_Text_Project/projectData/final/en_US/en_US.blogs.txt", open = "rb")
blog <- readLines(blog_file, encoding= "UTF-8", warn = F)
close(blog_file)
rm(blog_file)

#head(blog)

preprocdata <- function(x) {
  x <- iconv(x, from="UTF-8", to="latin1", sub=" ")
  x <- tolower(x)
  x <- str_replace_all(x, "([iu]n)-([a-z])", "\\1\\2")
  x <- str_replace_all(x, "([0-9])(st|nd|rd|th)", "\\1")
  x <- str_replace_all(x, " \\'|\\' ", " ")
  x <- str_replace_all(x, "[^a-z.' ]", " ")
  x <- str_replace_all(x, "([abiep])\\.([cdegm])\\.", "\\1\\2")
  x <- str_replace_all(x, "([a-z])\\.([a-z])", "\\1 \\2")
  x <- str_replace_all(x, "( [a-z])\\. ", "\\1 ")
  x <- str_replace_all(x, " (m[rs]|mrs)\\.", " \\1 ")
  x <- str_replace_all(x, " (dr|st|rd|av|ave|blvd|ct)\\.", " \\1 ")
  x <- str_replace_all(x, "\\.$", "")
  x <- str_replace_all(x, "^ +| +$|", "")
  x <- str_replace_all(x, " {2,}", " ")
  x <- str_replace_all(x, " *\\. *","\\.")
  x <- str_split(x, "\\.")
  x <- unlist(x)
  x <- x[x != ""]
  x
}

blog <- preprocdata(blog)

# head(blog)

twitter_file <- file("~/R/Processing_Text_Project/projectData/final/en_US/en_US.twitter.txt", open = "rb")
twitter <- readLines(twitter_file, encoding= "UTF-8", warn = F)
close(twitter_file)
rm(twitter_file)

twitter <- preprocdata(twitter)

#head(twitter)

news_file <- file("~/R/Processing_Text_Project/projectData/final/en_US/en_US.news.txt", open = "rb")
news <- readLines(news_file, encoding= "UTF-8", warn = F)
close(news_file)
rm(news_file)

news <- preprocdata(news)

head(news)

# Create corpus
corpus <- c(blog, twitter, news)
rm(blog, twitter, news)

# Sample corpus to train our algorithm
set.seed(666)
sample_data <- createDataPartition(y = 1:length(corpus), p = 0.15, list = FALSE)
train <- corpus[sample_data]
rm(corpus, sample_data)

# Check for "bad" tokens
sum(str_detect(train, "www"))

# More cleaning task
train <- train %>% 
  str_replace_all("www [a-z]+ [a-z]+", "") %>% 
  str_replace_all(" ([a-z])\\1+ |^([a-z])\\1+ | ([a-z])\\1+$|^([a-z])\\1+$", " ") %>% 
  str_replace_all( "([a-z])\\1{2,}", "\\1\\1") %>% 
  str_replace_all( "\\'+([a-z]+)\\'+", "\\1") %>% 
  str_replace_all( "\\'+ \\'+", " ") %>% 
  str_replace_all( "(\\'+ )+|( \\'+)+|^\\'+|\\'+$", " ") %>% 
  str_replace_all( "^[a-z]+$", "") %>% 
  str_replace_all( "( [^ai])+ |^([^ai] )+|( [^ai])+$", " ") %>% 
  str_replace_all( "^ +| +$|", "") %>% 
  str_replace_all( " {2,}", " ") %>% 
  str_replace_all( " +$|^ +", "")
train <- train[train != ""]

#head(train)
#?textcnt()

train1 <- textcnt(train, method = "string", split = "[[:space:]]", n = 1L, decreasing = T)
train2 <- textcnt(train, method = "string", split = "[[:space:]]", n = 2L, decreasing = T)
train3 <- textcnt(train, method = "string", split = "[[:space:]]", n = 3L, decreasing = T)
rm(train)

# Explore our ngrams 
head(train1)
head(train1)
head(train1)

unigram_dt <- data.table(text = names(train1), as.matrix(train1))
setnames(unigram_dt, "V1", "count")
setnames(unigram_dt, "text", "n0")
tot <- sum(unigram_dt$count)

unigram_dt <- unigram_dt %>% 
  mutate(freq = round(count/tot, 7)) %>% 
  select(-count)

unigram_dt <- as.data.table(unigram_dt)
setkeyv(unigram_dt, c("n0", "freq"))
saveRDS(unigram_dt, "unigram_dt.rds")
rm(tot, unigram_dt)


bigram_dt <- data.table(text = names(train2), as.matrix(train2))
setnames(bigram_dt, "V1", "count")
bigram_dt[, c("n1", "n0")  := do.call(Map, c(f = c, strsplit(text, " ")))]
bigram_dt <- mutate(bigram_dt, freq = round(count/train1[n1][[1]], 7))
bigram_dt$text <- NULL
bigram_dt$count <- NULL
bigram_dt <- as.data.table(bigram_dt)
setkey(bigram_dt, n1)
bigram_dt <- bigram_dt[,lapply(.SD, function(x) head(x, 5)), by = key(bigram_dt)]
setkeyv(bigram_dt, c("n1", "freq", "n0"))
saveRDS(bigram_dt, "bigram_dt.rds")
rm(bigram_dt)
rm(train1)

trigram_dt <- data.table(text = names(train3), as.matrix(train3))
setnames(trigram_dt, "V1", "count")
trigram_dt <- filter(trigram_dt, count > 1)
trigram_dt <- as.data.table(trigram_dt)
str(trigram_dt)
trigram_dt[, c("n2", "n1", "n0")  := do.call(Map, c(f = c, strsplit(text, " ")))]
trigram_dt <- mutate(trigram_dt, freq = round(count/train2[paste(n2, n1)][[1]], 7))
trigram_dt$text <- NULL
trigram_dt$count <- NULL
trigram_dt <- as.data.table(trigram_dt)
setkeyv(trigram_dt, c("n2", "n1"))
trigram_dt <- trigram_dt[,lapply(.SD, function(x) head(x, 5)),by = key(trigram_dt)]
setkeyv(trigram_dt, c("n2", "n1", "freq", "n0"))
saveRDS(trigram_dt, "trigram_dt.rds")
rm(trigram_dt, train2, train3)

# Load the badwords dataset from google https://code.google.com/archive/p/badwordslist/downloads
badwords <- readLines("badwords.txt", warn = F)
badwords <- tolower(badwords)
badwords <- str_replace_all(badwords, "\\(", "\\\\(")
saveRDS(badwords, "badwords_dt.rds")
rm(badwords)

