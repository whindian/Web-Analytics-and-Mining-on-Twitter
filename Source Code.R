#load libs needed
library(rtweet)
library(caret)
library(tidyverse)
library(tm)
library(stringdist)
library(proxy)
library(RecordLinkage)
library(proxy)   
library(wordcloud)
library(cluster)
library(stringi)
library(dendextend)
library(SnowballC)
library(textstem)
library(clusterCrit)
library("ape")
library(quanteda)
library(ggplot2)
library(plotly)
library(igraph)
library(textstem)
library(dplyr)
library(tidytext)

#set WD
setwd("C:\\Users\\James Bloor\\Desktop\\BU\\CS688\\Week 6")

#Part 1A)
following <- get_friends(c("ATLUTD", "JosefMartinez17", "MFparkhurst", "atlutdpup"))

#Part 1B)
counts <- table(following$user_id)
following.reduced <- following %>% filter(user_id %in% names(counts[counts > 2]))
table(following.reduced$user_id)
# you might use > 2 here to reduce the output

#You may also notice rtweet's get_friends() function returns Twitter user numbers 
#instead of Twitter screen names. If using R, convert the numbers to screen names by

following.names <- left_join(following.reduced, distinct(following.reduced,user_id) %>%
                               mutate(names = lookup_users(user_id)$screen_name), by = 'user_id') %>% select(-user_id)
following.matrix <- as.matrix(following.names)
(following.matrix)


#Part 1C)
friends <- c("ATLUTD", "JosefMartinez17", "MFparkhurst", "atlutdpup")
undirected_twitter <- graph_from_data_frame(following.matrix, directed = F)
V(undirected_twitter)[friends]$color = 'red'
plot(undirected_twitter)


directed_twitter <- graph_from_data_frame(following.matrix, directed = T)
V(directed_twitter)[friends]$color = 'red'
plot(directed_twitter)


#Part 1D)
distances(undirected_twitter,"MLS","JosefMartinez17",algorithm="dijkstra")
V(undirected_twitter)$name[shortest_paths(undirected_twitter,"MLS","JosefMartinez17")$vpath[[1]]]

#Part 1E)
#undirected
undirected_centrality <- data.frame(
  name=V(undirected_twitter)$name,
  degree=degree(undirected_twitter,normalized=T),
  closeness=closeness(undirected_twitter,normalized=T),
  betweenness=betweenness(undirected_twitter,normalized=T),
  eigen=eigen_centrality(undirected_twitter,scale=F)$vector
)
undirected_centrality

#directed
directed_centrality <- data.frame(
  name=V(directed_twitter)$name,
  degree=degree(directed_twitter,normalized=T),
  closeness=closeness(directed_twitter,normalized=T),
  betweenness=betweenness(directed_twitter,normalized=T),
  eigen=eigen_centrality(directed_twitter,scale=F)$vector
)
directed_centrality
#need to answer rest of question in Word Doc

#Part 2A)
Braves_tweets <- search_tweets("Braves", n =50, include_rts = FALSE,lang = "en") 
Braves_tweets <- (Braves_tweets$text)

#Part 2B)
write.csv(Braves_tweets, file="Braves_tweets.csv")
Braves_tweets <- read.csv("Braves_tweets.csv",header=T)
Braves_tweets <- Braves_tweets[c(-1)]
#Part 2C)
Braves_tweets <- as.data.frame(Braves_tweets)

#Pre-process titles we have
Braves_tweetsScorp <- VCorpus(VectorSource(Braves_tweets))


#lowercase
Braves_tweetsScorp <- tm_map(Braves_tweetsScorp, content_transformer(tolower)) 
head(Braves_tweetsScorp[[1]]$content)
#filter out braves
Braves_tweetsScorp <- tm_map(Braves_tweetsScorp, removeWords, "braves") 
head(Braves_tweetsScorp[[1]]$content)
#remove punctuation
Braves_tweetsScorp <- tm_map( Braves_tweetsScorp, removePunctuation )
Braves_tweetsScorp <- tm_map( Braves_tweetsScorp, content_transformer(function(x) gsub("'","",x)) )
head(Braves_tweetsScorp[[1]]$content) 
# get rid of http links
Braves_tweetsScorp <- tm_map( Braves_tweetsScorp, content_transformer(function(x) gsub("http\\S+","",x)) )
head(Braves_tweetsScorp[[1]]$content) 
## remove stopwords
Braves_tweetsScorp <- tm_map(Braves_tweetsScorp, removeWords, stopwords("english"))
head(Braves_tweetsScorp[[1]]$content)
# Stemming
Braves_tweetsScorp <- tm_map(Braves_tweetsScorp, stemDocument)
head(Braves_tweetsScorp[[1]]$content)
# Lemmatization
Braves_tweetsScorp <- tm_map(Braves_tweetsScorp, content_transformer(lemmatize_strings))
head(Braves_tweetsScorp[[1]]$content)
#remove numbers
Braves_tweetsScorp <- tm_map(Braves_tweetsScorp,removeNumbers)
head(Braves_tweetsScorp[[1]]$content)
#remove special characters
Braves_tweetsScorp <- tm_map( Braves_tweetsScorp, content_transformer(function(x) gsub("[^[:alnum:]]", " ", x)) )
head(Braves_tweetsScorp[[1]]$content)

#make data df
Braves_tweets_dataframe <- data.frame(text=unlist(sapply(Braves_tweetsScorp, `[`, "content")), 
                        stringsAsFactors=F)
#make bigram
Braves_tweets_Bigrams <- Braves_tweets_dataframe %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)


#Part 2D)
#count the Bigrams
Braves_tweets_Bigrams_Counts <- Braves_tweets_Bigrams %>% dplyr::count(bigram, sort = TRUE)
head(Braves_tweets_Bigrams_Counts)

#Part 2E)
#make a df so we can plot our words
Braves_tweets_Bigrams = Braves_tweets_Bigrams %>%
  separate(bigram, c("Word1", "Word2"), sep = " ")
head(Braves_tweets_Bigrams)

Braves_tweets_Bigrams <- Braves_tweets_Bigrams %>%
  filter(!Word1 %in% stop_words$word) %>%
  filter(!Word2 %in% stop_words$word)
head(Braves_tweets_Bigrams)

#time to plot our data and make sure that we make the counts visible in one way
Braves_tweets_Bigrams <- Braves_tweets_Bigrams %>%
  count(Word1, Word2, sort = TRUE)
head(Braves_tweets_Bigrams)
#Want to filter down the data so we can see trends and not have too mnay nodes
Braves_tweets_Bigrams <- filter(Braves_tweets_Bigrams, Braves_tweets_Bigrams$n > 1)
Braves_tweets_Bigrams_Words <- Braves_tweets_Bigrams[c(1,2)]
Braves_tweets_Bigrams_Words <- as.matrix(Braves_tweets_Bigrams_Words)
#Braves_tweets_Bigrams <- as.matrix(Braves_tweets_Bigrams)
G<- graph_from_edgelist(Braves_tweets_Bigrams_Words, directed = F)
E(G)$weight <- Braves_tweets_Bigrams[,3]
plot(G,edge.width = E(G)$weight)







