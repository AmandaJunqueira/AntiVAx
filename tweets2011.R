# setting the connection
token <- create_token(
  app <- "antiVac",
  consumer_key <- api_key,
  consumer_secret <-"xx",
  access_token <- "xxx",
  access_secret <- "xx")
keywords <- c("vaccine", 
              "#vaccine")
final_query <- "vaccine"

#search tweets on vaccine from 01/2019 to 04/2019
tweets2019 <- search_fullarchive(q = final_query,
                                    n = 1200,
                                    fromDate = 201901010000,
                                    toDate = 201904010000,
                                    env_name = "dev",
                                    safedir = NULL,
                                    parse = TRUE,
                                    token = token)
#writing the file
tweets_2019 <- apply(tweets2019,2,as.character)
write.csv(tweets_2019, "data/tweets2019.csv")

#search tweets on vaccine from 01/2011 to 04/2011
tweets201 <- search_fullarchive(q = final_query,
                                 n = 1200,
                                 fromDate = 201101010000,
                                 toDate = 201104010000,
                                 env_name = "dev",
                                 safedir = NULL,
                                 parse = TRUE,
                                 token = token)
#write file
tweets_2011 <- apply(tweets2011,2,as.character)
write.csv(tweets_2011, "data/tweets2011.csv")

#separate the words
en_2011 <- tweets201 %>% 
  select(text) %>% 
  unnest_tokens(word, text) 

#load list of stop words
en_2011 <- en_2011 %>%  
  anti_join(stop_words2)

# plot the top 15 words 
en_2011 %>%
  count(word, sort = TRUE) %>%
  top_n(7) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "January/2011") +
  theme_wsj()


twits_sentiment <- en_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)







###graph pre-process
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




