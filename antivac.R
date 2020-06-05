
#Fething Data From Twitter


#Loading Libraries
library(tidyr)
library(rtweet)
library(ggplot2)
library(dplyr)
library(mapsapi)
library(tidytext)
library(devtools)
library(widyr)
library(igraph)
library(ggraph)
library(stopwords)
library(ggthemes)

#Pass a suite of keys to the API
antivax <- "antiVac"
api_key <- *********
api_secret_key <- *********
access_token <- ********
access_token_secret <- ********

# create token named "twitter_token"
token <- create_token(
  app = "antiVac",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

# post a tweet from R
post_tweet("Look, i'm tweeting from R in my #rstats #earthanalytics class! @EarthLabCU")
## your tweet has been posted!


# find recent tweets with #rstats but ignore retweets -EN
tweets_en2 <- search_tweets("vaccine", n = 9000,
                               include_rts = FALSE)

#tweets_en2 <- apply(tweets_en2,2,as.character)
#write.csv(tweets_en2, "data/tweets2.csv")

en <- tweets_en2 %>% 
  select(text) %>% 
  unnest_tokens(word, text) 


#load list of stop words
en_clean <- en %>%  
  anti_join(stop_words2)

# plot the top 15 words -- notice any issues?
en_clean %>%
  count(word, sort = TRUE) %>%
  top_n(7) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Yesterday Tweets") +
  theme_wsj() +
  scale_colour_wsj("colors4")

en_paired <- tweets_en %>% 
  select(text) %>% 
  unnest_tokens(paired_words, text, token = "ngrams", n = 2)

en_paired %>% 
  count(paired_words, sort = TRUE)

separate <- en_paired %>% 
  separate(paired_words, c("word1", "word2"), sep = " ")

en_paired_sep <- separate %>% 
  filter(!word1 %in% stop_words2$word) %>% 
  filter(!word2 %in% stop_words2$word)

en_counts <- en_paired_sep %>% 
  count(word1, word2, sort = TRUE)

# (plotting graph edges is currently broken)
en_counts %>%
  filter(n >= 24) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  # geom_edge_link(aes(edge_alpha = n, edge_width = n))
  # geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the word Vaccine",
       subtitle = "Text mining twitter data ",
       x = "", y = "")


#search tweets by location ####
# this requires that you set up a Google Cloud Platform account
# make sure Geocoding API & Places API are activated
# introduce here your key:
set.api.key ******
Sys.getenv ******
# result should be true
# search tweets from usa
search_usa <- search_tweets(
  "lang:en",
  geocode = lookup_coords("vaccine",
                          apikey = "AIzaSyAxabVKflNuVLpLUgsaI5JnMsOy4DaApGg"),
  n = 10
)

read.csv("data/english.csv", header = FALSE, sep = ",")

as.numeric(as.character(english$Vaccine))
English <- english[!is.na(as.numeric(as.character(english$Vaccine))),]
as.numeric(English$Vaccine)


English <- English %>%
  ungroup(Vaccine) %>%
  mutate(Vaccine = factor(Vaccine,levels = c(1,2,3),
                      labels = c("1","2","3") ) )


eng <- English %>% 
  group_by(Vaccine) %>% 
  count()

##SENTIMENT ANALYSIS
sentiment <- en_clean %>%
  inner_join(get_sentiments("bing")) 

ggplot(sentiment, aes(word, sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~word, ncol = 2, scales = "free_x")


tweets_bigrams <- tweets_en2 %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

en_bigram <- tweets_en2 %>% 
  select(text) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) 


negation_words <- c("not", "no", "never", "without")

negated_words <- en_clean %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)




