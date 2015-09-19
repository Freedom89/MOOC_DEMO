#set working directory
setwd("~/Dropbox/IDA_MOOC/ML_app/capstone quiz script")

Quiz 1
============
  
Question 1
------------
file.info("en_US.blogs.txt")$size / 1024^2

Question 2
------------
# ptm <- proc.time()
# blogs <- readLines("en_US.blogs.txt", encoding = 'UTF-8')
# news <- readLines("en_US.news.txt", encoding='UTF-8')
# twitter <- readLines("en_US.twitter.txt", encoding = 'UTF-8')
# time_run_base <- proc.time()- ptm  
#  
# run_time results 
# user  system elapsed 
# 128.983   2.709 141.792 
# 
#  saveRDS(twitter,"twit.RData")
#  saveRDS(news,"news.RData")
#  saveRDS(blogs,"blogs.RData")

ptm_2 <- proc.time()
blogs <- readRDS("blogs.RData")
news <- readRDS("news.RData")
twitter <- readRDS("twit.RData")
time_run_RDS <- proc.time() - ptm_2 #total time run is 12.194 seconds - USER 
time_run_RDS

length(twitter)

Question 3
------------
max(nchar(blogs))
max(nchar(news))
max(nchar(twitter))

Question 4
------------
love_count <- sum(grepl("love", twitter))
hate_count <- sum(grepl("hate", twitter))
love_count / hate_count

Question 5
------------
biostats <- grep("biostats", twitter)
twitter[biostats]

Question 6
------------
sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing", twitter))
