
# 01 Load data and liberaries ---------------------------------------------
install.packages("readr")
install.packages("ggraph")
install.packages("igraph")
install.packages("widyr")
library(tuber)
library(tidyverse)
library(ggraph)
library(httr)
library(igraph)
library(readr)
library(widyr)



# 01 Channel-Level aggragation --------------------------------------------
trump_videos <- read_csv("data_preprocessed/trump_videos.csv")

# Aggregate video titles by channel and ideology
channel_docs <- trump_videos %>%
  group_by(channel_id, ideology) %>%
  summarise(
    text = paste(title, collapse = " "),
    n_videos = n(),
    .groups = "drop"
  )
View(channel_docs)


# 02 Text preprocessing and tokenization ----------------------------------
custom_stopwords <- tibble::tibble(
  word = c(
    "amp", "subscribe", "substack", "patreon",
    "youtube", "channel", "video", "videos",
    "trump", "donald", "biden", "channel", "watch",
    "patreon", "join", "www", "http", "https",
    "com", "youtube", "live", "shorts", "amp",
    "free", "access", "link", "membership"
  )
)

# Tokenize the aggregated text and clean it for analysis
channel_words <- channel_docs %>%
  select(channel_id, ideology, text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stopwords, by = "word") %>%
  filter(str_detect(word, "^[a-z]+$")) %>%
  filter(str_length(word) > 2)

View(channel_words)

# 03 Channel Similarity Computation ---------------------------------------
# Count the frequency of each word for each channel to create a term-document matrix
channel_word_counts <- channel_words %>%
  count(channel_id, word, ideology, sort = TRUE)

View(channel_word_counts)

# Compute pairwise cosine similarity between channels based on shared word usage
channel_pairs <- channel_word_counts %>%
  pairwise_similarity(
    item = channel_id,
    feature = word,
    value = n,
    sort = TRUE
  )


# adding ideology column to the channel_pairs dataframe
channel_meta <- channel_word_counts %>%
  select(channel_id, ideology) %>%
  distinct()

channel_pairs <- channel_word_counts %>%
  pairwise_similarity(
    item = channel_id,
    feature = word,
    value = n,
    sort = TRUE
  ) %>%
  left_join(channel_meta, by = c("item1" = "channel_id")) %>%
  rename(ideology1 = ideology) %>%
  left_join(channel_meta, by = c("item2" = "channel_id")) %>%
  rename(ideology2 = ideology)

View(channel_pairs)


# 04 Network Construction -------------------------------------------------

