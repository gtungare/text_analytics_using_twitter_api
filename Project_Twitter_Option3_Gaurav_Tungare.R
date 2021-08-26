#################################################
####
###  CS688 Term Project - Gaurav Tungare
###
##################################################

##################################################################################
## load rtweet package
library(rtweet)
library(tidytext)
library(tidyverse)
library(dplyr) 
library(tidyr)
library(ggplot2)
library(SnowballC)
library(purrr)
library(igraph)
library(ggraph)

rm(list=ls()) 
#####

gainer1 <- search_tweets(
  "tesla", n = 100, include_rts = FALSE
)

#######
gainer2 <- search_tweets(
  "Moderna", n = 100, include_rts = FALSE
)
######

gainer3 <- search_tweets(
  "Coinbase", n = 100, include_rts = FALSE
)

gainertweets1 = gainer1 %>% select(screen_name,text,created_at)
gainertweets2 = gainer2 %>% select(screen_name,text,created_at)
gainertweets3 = gainer3 %>% select(screen_name,text,created_at)

######

loser1 <- search_tweets(
  "AMC", n = 100, include_rts = FALSE
)

loser2 <- search_tweets(
  "peloton", n = 100, include_rts = FALSE
)

loser3 <- search_tweets(
  "johnson&johnson", n = 100, include_rts = FALSE
)


losertweets1 = loser1 %>% select(screen_name,text,created_at)
losertweets2 = loser2 %>% select(screen_name,text,created_at)
losertweets3 = loser3 %>% select(screen_name,text,created_at)



### binding the tweets 

comb_gainer_save <- rbind(gainertweets1,gainertweets2,gainertweets3)

comb_loser_Save <- rbind(losertweets1,losertweets2,losertweets3)

###################################

getwd()

setwd("C:/Users/gaura/Documents/")

#### Saving object to local

saveRDS(comb_gainer_save, file = "comb_gainer.RDS") 
saveRDS(comb_loser_Save, file = "comb_loser.RDS") 

#### retrieve object from local
comb_gainer <- readRDS("comb_gainer.RDS")
comb_loser <- readRDS("comb_loser.RDS")


#################################

#####################################################

# Preprocessing clean up

###################################################

view(comb_gainer$stripped_text)

comb_gainer$stripped_text[2]

## Remove the twitter # tags names
comb_gainer$stripped_text <- gsub("#[A-Za-z0-9]+|@[A-Za-z0-9]+|\\w+(?:\\.\\w+)*/\\S+","",comb_gainer$text)

comb_gainer$stripped_text <- gsub("@[A-Za-z0-9]+|@[A-Za-z0-9]+|\\w+(?:\\.\\w+)*/\\S+","",comb_gainer$stripped_text)
## Remove other UTFs
comb_gainer$stripped_text <- gsub("[^\u0020-\u007F]+","",comb_gainer$stripped_text)
# Remove http elements manually
comb_gainer$stripped_text <- gsub("http\\S+","",comb_gainer$stripped_text)
## Remove all "()*&^%$#@.?<>;'!," charaters
comb_gainer$stripped_text <- gsub("[\\()*&^%$#@.?<>=+;'!,_/~|:-]","",comb_gainer$stripped_text)
## lower case
comb_gainer$stripped_text <- tolower(comb_gainer$stripped_text)
## Remove all the non-alphanumeric
comb_gainer$stripped_text <- gsub("[^[:alnum:]]", " ", comb_gainer$stripped_text)
## Remove all the numeric characters
comb_gainer$stripped_text <- gsub("[0-9]", " ", comb_gainer$stripped_text)



## Remove the twitter # tags names
comb_loser$stripped_text <- gsub("#[A-Za-z0-9]+|@[A-Za-z0-9]+|\\w+(?:\\.\\w+)*/\\S+","",comb_loser$text)
comb_loser$stripped_text <- gsub("@[A-Za-z0-9]+|@[A-Za-z0-9]+|\\w+(?:\\.\\w+)*/\\S+","",comb_loser$stripped_text)
## Remove other UTFs
comb_loser$stripped_text <- gsub("[^\u0020-\u007F]+","",comb_loser$stripped_text)
# Remove http elements manually
comb_loser$stripped_text <- gsub("http\\S+","",comb_loser$stripped_text)
## Remove all "()*&^%$#@.?<>;'!," charaters
comb_loser$stripped_text <- gsub("[\\()*&^%$#@.?<>=+;'!,_/~|:-]","",comb_loser$stripped_text)
## lower case
comb_loser$stripped_text <- tolower(comb_loser$stripped_text)
## Remove all the non-alphanumeric
comb_loser$stripped_text <- gsub("[^[:alnum:]]", " ", comb_loser$stripped_text)
## Remove all the numeric characters
comb_loser$stripped_text <- gsub("[0-9]", " ", comb_loser$stripped_text)



#################################################

# List of the most frequent terms for Gainer & Loose stock.  Compare them. 

###############################################

# view(comb_gainer)
# comb_loser

cleaned_comb_gainer_twt = comb_gainer %>% 
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text) %>%
  anti_join(stop_words)


cleaned_comb_loser_twt = comb_loser %>% 
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text) %>%
  anti_join(stop_words)


cleaned_comb_gainer_twt %>% filter(nchar(cleaned_comb_gainer_twt$word)>3) %>% select(word) %>%
  #filter(!word %in% stop_words$wword()
  #      !word %in% str_remove_all(stop_words$word, "'")) %>%
  dplyr::count(word, sort = TRUE) %>%
  top_n(10)


  cleaned_comb_loser_twt  %>% filter(nchar(cleaned_comb_loser_twt$word)>3) %>% select(word) %>%
  #filter(!word %in% stop_words$word,
   #      !word %in% str_remove_all(stop_words$word, "'")) %>%
  dplyr::count(word, sort = TRUE) %>%
  top_n(10)

#view(cleaned_Cntry_A_twts_tab)
#view(cleaned_Cntry_B_twts_tab)


par(mfrow=c(2,1))

cleaned_comb_gainer_twt %>%  filter(nchar(cleaned_comb_gainer_twt$word)>3) %>%
  dplyr::count(word, sort = TRUE) %>%
  top_n(10) %>%
  #mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "green") +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "Count",
       y = "words",
       title = "Word counts found on gainer stock")


cleaned_comb_loser_twt %>% filter(nchar(cleaned_comb_loser_twt$word)>5) %>%
  dplyr::count(word, sort = TRUE) %>%
  top_n(10) %>%
  #mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "blue") +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "Count",
       y = "words",
       title = "Word counts found on loser stock")


###########################################################
#
# Show top word pairs (bigrams) for each country as described in the lecture
#
#
############# ################################################


### --- Explore words that occur together in pairs!
## note we return back to original text in twt.kag

## For Gainer Stock

comb_gainer_pairs = comb_gainer %>%
  select(stripped_text) %>%
  unnest_tokens(pairs, stripped_text,token = "ngrams", n = 2)

comb_gainer_pairs_counts <- comb_gainer_pairs %>%
  dplyr::count(pairs, sort = TRUE)


comb_gainer_pairs_separate = comb_gainer_pairs %>%
  separate(pairs, c("Word1", "Word2"), sep = " ")

comb_gainer_pair_clean <- comb_gainer_pairs_separate %>%
  filter(!Word1 %in% stop_words$word) %>%
  filter(!Word2 %in% stop_words$word)  %>% filter(!is.na(Word1)) %>%
  filter(!is.na(Word2))

#### Loser stock 

cleaned_comb_loser_pairs = comb_loser %>%
  select(stripped_text) %>%
  unnest_tokens(pairs, stripped_text,token = "ngrams", n = 2)

cleaned_comb_loser_pairs_count <- cleaned_comb_loser_pairs %>%
  dplyr::count(pairs, sort = TRUE)

cleaned_comb_loser_pairs_separate = cleaned_comb_loser_pairs %>%
  separate(pairs, c("Word1", "Word2"), sep = " ")

comb_loser_pairs_clean <- cleaned_comb_loser_pairs_separate %>%
  filter(!Word1 %in% stop_words$word) %>%
  filter(!Word2 %in% stop_words$word)  %>% filter(!is.na(Word1)) %>%
  filter(!is.na(Word2))       

##############

# new bigram counts:
comb_gainer_pair_counts <- comb_gainer_pair_clean %>% filter(!is.na(Word1)) %>%
  filter(!is.na(Word2)) %>%
  dplyr::count(Word1, Word2, sort = TRUE) 

comb_loser_pair_counts <- comb_loser_pairs_clean %>% filter(!is.na(Word1)) %>%
  filter(!is.na(Word2)) %>%
  dplyr::count(Word1, Word2, sort = TRUE) 

# plot Gainer tweet word network

comb_gainer_pair_counts %>% 
  filter(n >= 2) %>% #only choose pairs that have 2 or more counts
  top_n(n=50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n,fill = "blue")) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Gainer Stock Tweets",
       subtitle = "Pairs",
       x = "", y = "") +
  theme_bw()


# plot loser tweet word network

comb_loser_pair_counts %>% 
  filter(n >= 2) %>% #only choose pairs that have 2 or more counts
  top_n(n=50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Loser Stock Tweets",
       subtitle = "Pairs",
       x = "", y = "") +
  theme_bw()


##########################################
#
# 6)  Compute the sentiment score (as described in the lecture) for all the tweets for each country. Compare the sentiments for the two countries. Do the results make sense?
#
#########################################

bing_comb_gainer_twt =cleaned_comb_gainer_twt %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_comb_loser_twt = cleaned_comb_loser_twt %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  ungroup()


bing_comb_gainer_twt %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  #mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets for Gainer stock",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()

bing_comb_loser_twt %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  #mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets for Loser stock",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()

#######################################
######
#
######################################

## using the fuction
sentiment_bing = function(twt){
  #Step 1;  perform basic text cleaning (on the tweet), as seen earlier
  twt_tbl = tibble(text = twt) %>% 
    mutate(
      # Remove http elements manually
      stripped_text = gsub("http\\S+","",text)
    ) %>% 
    unnest_tokens(word,stripped_text) %>% 
    anti_join(stop_words, by="word") %>%  #remove stop words
    inner_join(get_sentiments("bing"), by="word") %>% # merge with bing sentiment
    dplyr::count(word, sentiment, sort = TRUE) %>% 
    ungroup() %>% 
    ## Create a column "score", that assigns a -1 one to all negative words, and 1 to positive words. 
    mutate(
      score = case_when(
        sentiment == 'negative'~ n*(-1),
        sentiment == 'positive'~ n*1)
    )
  ## Calculate total score
  sent.score = case_when(
    nrow(twt_tbl)==0~0, # if there are no words, score is 0
    nrow(twt_tbl)>0~sum(twt_tbl$score) #otherwise, sum the positive and negatives
  )
  ## This is to keep track of which tweets contained no words at all from the bing list
  zero.type = case_when(
    nrow(twt_tbl)==0~"Type 1", # Type 1: no words at all, zero = no
    nrow(twt_tbl)>0~"Type 2" # Type 2: zero means sum of words = 0
  )
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
}

#comb_gainer$stripped_text
# Sentiment chart for Contry A only

Gainer_sent = lapply(comb_gainer$stripped_text,function(x){sentiment_bing(x)}) # -- takes a bit of time

Gainer_sentiment = tibble(
  Stock = 'Gainer',
  score = unlist(map(Gainer_sent,'score')),
  type = unlist(map(Gainer_sent,'type'))
)

head(Gainer_sentiment)

##GGplot with Type1 and Type 2 for Gainer Stock 
ggplot(Gainer_sentiment,aes(x=score)) +
  geom_histogram(bins = 15, alpha = .6,fill='green') + theme_bw() +
  ggtitle("Sentiment Score Analysis w/ Type1 and Type 2 - Gainer Stock") 

##GGplot with Type 2 for Gainer Stock 
ggplot(Gainer_sentiment %>% filter(type != "Type 1"),aes(x=score)) +
  geom_histogram(bins = 15, alpha = .6,fill='green') + theme_bw()+
  ggtitle("Sentiment Score Analysis w/ Type 2 only -  Gainer Stock")



# takes a few minutes 
loser_sent = lapply(comb_loser$stripped_text,function(x){sentiment_bing(x)})

loser_sentiment = tibble(
  Stock = 'Loser',
  score = unlist(map(loser_sent,'score')),
  type = unlist(map(loser_sent,'type'))
)


##GGplot with Type1 and Type 2 for Loser Stock
ggplot(loser_sentiment,aes(x=score)) +
  geom_histogram(bins = 15, alpha = .6,fill='blue') + theme_bw()+
  ggtitle("Sentiment Score Analysis w/ Type1 and Type 2 - Loser Stock")

##GGplot with Type 2 only for Loser Stock
ggplot(loser_sentiment %>% filter(type != "Type 1"),aes(x=score)) +
  geom_histogram(bins = 15, alpha = .6,fill='blue') + theme_bw()  +
  ggtitle("Sentiment Score Analysis w/ Type 2 only - Lose Stock")

total_stock_sentiment = bind_rows(Gainer_sentiment,
loser_sentiment)

total_stock_sentiment = bind_rows(
  tibble(
    Stock = 'Gainer',
    score = unlist(map(Gainer_sentiment,'score')),
    type = unlist(map(Gainer_sentiment,'type'))
  ),
  tibble(
    Stock = 'Loser',
    score = unlist(map(loser_sentiment,'score')),
    type = unlist(map(loser_sentiment,'type'))
  )
)



#### combined ggplot with Type1 and Type 2
ggplot(total_stock_sentiment ,aes(x=score, fill = Stock)) + geom_histogram(bins = 15, alpha = .6) +
  facet_grid(~Stock) + theme_bw() +
  ggtitle("Sentiment Score Analysis w/ Type1 and Type 2 classification ")

## Now lets excluded the Type 1 tweets (tweets with no words in the bing list)

#### combined ggplot with Type 2 only
ggplot(total_stock_sentiment %>% filter(type != "Type 1"),aes(x=score, fill = Stock)) + geom_histogram(bins = 15, alpha = .6) +
  facet_grid(~Stock) + theme_bw()+
  ggtitle("Sentiment Score Analysis w/ Type 2 only classification")

total_stock_sentiment %>% filter(type != "Type 1") %>% group_by(Stock) %>% 
  dplyr::summarise(
    Count = n(),
    Mean = mean(score),
    SD = sd(score),
    max = max(score),
    min = min(score)
  )

############################################
#
#Create ONE appropriate data visualization that shows the single-day change in
#stock prices for the stocks you selected and on the date you selected
#
##########################################

library(quantmod)
library(lubridate)
library(ggplot2)

################ Fo Gainer

MRNA <- getSymbols("MRNA", auto.assign=F)
MRNA.matrix <- as.matrix(MRNA)
times <- ymd(rownames(MRNA.matrix))
MRNA.df <- data.frame(date=times,price=MRNA.matrix[,"MRNA.Adjusted"])
MRNA.df.since2018 <- MRNA.df[MRNA.df$date>"2021-04-15",]
MRNA.df.since2018$Stock <- "Moderna"

COIN <- getSymbols("COIN", auto.assign=F)
COIN.matrix <- as.matrix(COIN)
times <- ymd(rownames(COIN.matrix))
COIN.df <- data.frame(date=times,price=COIN.matrix[,"COIN.Adjusted"])
COIN.df.since2018 <- COIN.df[COIN.df$date>"2021-04-15",]
COIN.df.since2018$Stock <- "Coinbase"

TSLA1 <- getSymbols("TSLA", auto.assign=F)
TSLA.matrix <- as.matrix(TSLA1)
times <- ymd(rownames(TSLA.matrix))
TSLA.df <- data.frame(date=times,price=TSLA.matrix[,"TSLA.Adjusted"])
TSLA.df.since2018 <- TSLA.df[TSLA.df$date>"2021-04-15",]
TSLA.df.since2018$Stock <- "Tesla"

termcombplot1  <- rbind(MRNA.df.since2018,COIN.df.since2018,TSLA.df.since2018)

## Gainer Stock Prices 
ggplot(data=termcombplot1,aes(x=date,y=price,colour=Stock),ylim=c(0,1000)) +
  geom_line(size=2) + 
  labs(title="Gainer Stock Prices") + theme_bw() +
geom_hline(yintercept = 0) + ylab("Price in Dollar")



################ Fo Gainer

MRNA <- getSymbols("MRNA", auto.assign=F)
MRNA.matrix <- as.matrix(MRNA)
times <- ymd(rownames(MRNA.matrix))
MRNA.df <- data.frame(date=times,price=MRNA.matrix[,"MRNA.Adjusted"])
MRNA.df.since2018 <- MRNA.df[MRNA.df$date>"2020-04-15",]
MRNA.df.since2018$Stock <- "Moderna"

COIN <- getSymbols("COIN", auto.assign=F)
COIN.matrix <- as.matrix(COIN)
times <- ymd(rownames(COIN.matrix))
COIN.df <- data.frame(date=times,price=COIN.matrix[,"COIN.Adjusted"])
COIN.df.since2018 <- COIN.df[COIN.df$date>"2020-04-15",]
COIN.df.since2018$Stock <- "Coinbase"

TSLA1 <- getSymbols("TSLA", auto.assign=F)
TSLA.matrix <- as.matrix(TSLA1)
times <- ymd(rownames(TSLA.matrix))
TSLA.df <- data.frame(date=times,price=TSLA.matrix[,"TSLA.Adjusted"])
TSLA.df.since2018 <- TSLA.df[TSLA.df$date>"2020-04-15",]
TSLA.df.since2018$Stock <- "Tesla"

termcombplot1  <- rbind(MRNA.df.since2018,COIN.df.since2018,TSLA.df.since2018)

## Gainer Stock Prices 
ggplot(data=termcombplot1,aes(x=date,y=price,colour=Stock),ylim=c(0,1000)) +
  geom_line(size=2) + 
  labs(title="Gainer Stock Prices : Variation for Entire Year") + theme_bw() +
  geom_hline(yintercept = 0) + ylab("Price in Dollar") +
  ylim(c(0,900))



##### For Loser stock

AMC <- getSymbols("AMC", auto.assign=F)
AMC.matrix <- as.matrix(AMC)
times <- ymd(rownames(AMC.matrix))
AMC.df <- data.frame(date=times,price=AMC.matrix[,"AMC.Adjusted"])
AMC.df.since2018 <- AMC.df[AMC.df$date>"2021-04-15",]
AMC.df.since2018$Stock <- "AMC"

JNJ <- getSymbols("JNJ", auto.assign=F)
JNJ.matrix <- as.matrix(JNJ)
times <- ymd(rownames(JNJ.matrix))
JNJ.df <- data.frame(date=times,price=JNJ.matrix[,"JNJ.Adjusted"])
JNJ.df.since2018 <- JNJ.df[JNJ.df$date>"2021-04-15",]
JNJ.df.since2018$Stock <- "J&J"

PTON <- getSymbols("PTON", auto.assign=F)
PTON.matrix <- as.matrix(PTON)
times <- ymd(rownames(PTON.matrix))
PTON.df <- data.frame(date=times,price=PTON.matrix[,"PTON.Adjusted"])
PTON.df.since2018 <- PTON.df[PTON.df$date>"2021-04-15",]
PTON.df.since2018$Stock <- "Peloton"

termcombplot2  <- rbind(PTON.df.since2018,JNJ.df.since2018,AMC.df.since2018)

## Loser Stock Prices 
ggplot(data=termcombplot2,aes(x=date,y=price,colour=Stock),ylim=c(0,1000)) +
  geom_line(size=2) + 
  labs(title="Loser Stock Prices") + theme_bw() +
  geom_hline(yintercept = 0) + ylab("Price in Dollar")



##### For Loser stock - Entire Year 

AMC <- getSymbols("AMC", auto.assign=F)
AMC.matrix <- as.matrix(AMC)
times <- ymd(rownames(AMC.matrix))
AMC.df <- data.frame(date=times,price=AMC.matrix[,"AMC.Adjusted"])
AMC.df.since2018 <- AMC.df[AMC.df$date>"2020-04-15",]
AMC.df.since2018$Stock <- "AMC"

JNJ <- getSymbols("JNJ", auto.assign=F)
JNJ.matrix <- as.matrix(JNJ)
times <- ymd(rownames(JNJ.matrix))
JNJ.df <- data.frame(date=times,price=JNJ.matrix[,"JNJ.Adjusted"])
JNJ.df.since2018 <- JNJ.df[JNJ.df$date>"2020-04-15",]
JNJ.df.since2018$Stock <- "J&J"

PTON <- getSymbols("PTON", auto.assign=F)
PTON.matrix <- as.matrix(PTON)
times <- ymd(rownames(PTON.matrix))
PTON.df <- data.frame(date=times,price=PTON.matrix[,"PTON.Adjusted"])
PTON.df.since2018 <- PTON.df[PTON.df$date>"2020-04-15",]
PTON.df.since2018$Stock <- "Peloton"

termcombplot2  <- rbind(PTON.df.since2018,JNJ.df.since2018,AMC.df.since2018)

## Loser Stock Prices 
ggplot(data=termcombplot2,aes(x=date,y=price,colour=Stock),ylim=c(0,1000)) +
  geom_line(size=2) + 
  labs(title="Loser Stock Prices - Variation for Entire Year") + theme_bw() +
  geom_hline(yintercept = 0) + ylab("Price in Dollar")
