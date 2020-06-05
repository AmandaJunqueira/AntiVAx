###2019###


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
#tweets_2019 <- apply(tweets2019,2,as.character)
#write.csv(tweets_2019, "data/tweets2019.csv")

#separate the words
tweets2019clean <- tweets2019 %>% 
  select(text) %>% 
  unnest_tokens(word, text) 

#load list of stop words
tweets2019clean <- tweets2019clean %>%  
  anti_join(stop_words2)

# plot the top 15 words 
tweets2019clean %>%
  count(word, sort = TRUE) %>%
  top_n(7) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "January/2019") +
  theme_wsj()


###graph pre-process
nineteen_paired <- tweets2019 %>% 
  select(text) %>% 
  unnest_tokens(paired_words, text, token = "ngrams", n = 2)

nineteen_paired %>% 
  count(paired_words, sort = TRUE)

separated_nineteen <- nineteen_paired %>% 
  separate(paired_words, c("word1", "word2"), sep = " ")

separated_nineteen <- separated_nineteen %>% 
  filter(!word1 %in% stop_words2$word) %>% 
  filter(!word2 %in% stop_words2$word)

nineteen_counts <- separated_nineteen %>% 
  count(word1, word2, sort = TRUE)

# (plotting graph edges is currently broken)
nineteen_counts %>%
  filter(n >= 100) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  # geom_edge_link(aes(edge_alpha = n, edge_width = n))
  # geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the word Vaccine",
       subtitle = "Text mining twitter data ",
       x = "", y = "")

###SENTIMENT ANALYSIS

select(tweets2019, text)) 






