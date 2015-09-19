blogs <- readRDS("blogs.RData")
news <- readRDS("news.RData")
twitter <- readRDS("twit.RData")

library(stringr)

aggregate_text<-function(string2,options){
  
  #string2 - input text before word choice
  #options - define all multiple choice options 
  
  storage<-c() #create a temp vector to store values 
  for(i in 1:4){
    string<-paste(string2,options[i]) #get the entire string 
    print(string) #print to see the process 
    location_blogs<-grep(string,blogs) #find the number of occurences in blogs 
    location_news<-grep(string, news) #find the number of occurences in news 
    location_twitter<-grep(string, twitter) #find the number of occurences in twitter
    storage<-c(storage,length(location_blogs)+length(location_news)+length(location_twitter)) 
    #store the sum of all the occurences and append into storage vector
  }
  return(storage) #return the results 
}

#question1
string<-c("a case of")
options<-c("soda","cheese","pretzels","beer")
answer<-aggregate_text(string,options)
answer

#a faster way to do it 
aggregate_text_parallel <- function(string3,options){
  
  #string3 - input string before 
  
  string_search <- paste0(string3, " ([[:alnum:]]+)")
  #extract all phrases that contains string3
  #remove all NA entries 
  temp_news<-str_extract(news, string_search) ; temp_news <- temp_news[!is.na(temp_news)]
  temp_blogs <- str_extract(blogs, string_search) ;temp_blogs <- temp_blogs[!is.na(temp_blogs)]
  temp_twit <- str_extract(twitter,string_search) ;temp_twit <- temp_twit[!is.na(temp_twit)]
  
  #create a summary of the results 
  table_results <- table(c(temp_news,temp_blogs,temp_twit))
  options_new <- paste0(string3," ",options)
  #match the options 
  location_options <- is.element(names(table_results),options_new) 
  print(table_results)
  print("results based on options are....")
  return(table_results[location_options])
}

#question1 with new function
string<-c("a case of")
options<-c("soda","cheese","pretzels","beer")
aggregate_text_parallel(string,options)

#question2
string<-c("would mean the")
options<-c("world","universe","most","best")
aggregate_text_parallel(string,options)


####
#question9
string<-c("faith during the")
options<-c("bad","sad","worse","hard")
aggregate_text_parallel(string,options)

string<-c("during the")
options<-c("bad","sad","worse","hard")
aggregate_text_parallel(string,options)

string<-c("the")
options<-c("bad","sad","worse","hard")
aggregate_text_parallel(string,options)

#question10
string<-c("you must be")
options<-c("insensitive","insane","callous","asleep")
aggregate_text_parallel(string,options)

string<-c("must be")
options<-c("insensitive","insane","callous","asleep")
aggregate_text_parallel(string,options)

string<-c("be")
options<-c("insensitive","insane","callous","asleep")
aggregate_text_parallel(string,options)

#by this, the answer should be asleep, but looking at the text 
#if this isnt the cutest thing you've ever seen, then you must be ... 
#requires more complex modeling of NLP 
