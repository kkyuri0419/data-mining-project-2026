# 01 Setup and Package Loading ----

install.packages(c("httr", "jsonlite", "dplyr", "purrr", "tibble",
                   "readr", "tidytext", "textdata", "stringr"))

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(readr)
library(tidytext)
library(textdata)
library(stringr)

# Set your YouTube API key
credentials <- fromJSON(file.path(getwd(),"youtube_api_oauth_client_credentials.json"))
client_id <- credentials$installed$client_id
client_secret <- credentials$installed$client_secret
api_key <- credentials$installed$api_key

# Load data
trump_videos <- read_csv("data_preprocessed/trump_videos.csv")
View(trump_data)

# 02 Define a Function to Retrieve One Page of Top-Level Comments ----

# This function retrieves one page of top-level comments for a single video.
# It returns both the comments and the next page token for pagination.
get_video_comments_page <- function(video_id, api_key, page_token = NULL, max_results = 100) {
  
  res <- GET(
    "https://www.googleapis.com/youtube/v3/commentThreads",
    query = list(
      part = "snippet",
      videoId = video_id,
      maxResults = max_results,
      textFormat = "plainText",
      order = "relevance",   # Retrieves top comments rather than newest comments
      pageToken = page_token,
      key = api_key
    )
  )
  
  # If comments are disabled or the API returns an error, return an empty tibble
  if (status_code(res) != 200) {
    return(list(
      comments = tibble(),
      next_page_token = NULL
    ))
  }
  
  txt <- content(res, as = "text", encoding = "UTF-8")
  dat <- fromJSON(txt, flatten = TRUE)
  
  # If no comments are available, return an empty tibble
  if (!"items" %in% names(dat) || length(dat$items) == 0) {
    return(list(
      comments = tibble(),
      next_page_token = NULL
    ))
  }
  
  comments <- tibble(
    video_id = video_id,
    comment_id = dat$items$id,
    comment_text = dat$items$snippet.topLevelComment.snippet.textDisplay,
    like_count = dat$items$snippet.topLevelComment.snippet.likeCount,
    published_at = dat$items$snippet.topLevelComment.snippet.publishedAt
  )
  
  next_token <- if ("nextPageToken" %in% names(dat)) dat$nextPageToken else NULL
  
  list(
    comments = comments,
    next_page_token = next_token
  )
}


# 03 Define a Function to Retrieve Multiple Top Comments per Video ----

# This function repeatedly calls the one-page function above
# until it reaches the requested number of comments or runs out of pages.
get_video_comments <- function(video_id, api_key, max_comments = 50) {
  
  all_comments <- list()
  next_token <- NULL
  total_fetched <- 0
  
  repeat {
    remaining <- max_comments - total_fetched
    if (remaining <= 0) break
    
    batch_size <- min(100, remaining)
    
    result <- get_video_comments_page(
      video_id = video_id,
      api_key = api_key,
      page_token = next_token,
      max_results = batch_size
    )
    
    comments_batch <- result$comments
    
    # Stop if no comments were returned
    if (nrow(comments_batch) == 0) break
    
    all_comments[[length(all_comments) + 1]] <- comments_batch
    total_fetched <- total_fetched + nrow(comments_batch)
    next_token <- result$next_page_token
    
    # Stop if there are no more pages
    if (is.null(next_token)) break
  }
  
  bind_rows(all_comments)
}



# 04 Test Comment Collection on a Small Subset of Videos ----

# Before collecting comments for all videos, test the pipeline on a few videos.
# This step is strongly recommended to avoid wasting API quota.

test_video_ids <- trump_data$video_id[1:5]

video_comments_test <- map_dfr(
  test_video_ids,
  ~ get_video_comments(.x, api_key = api_key, max_comments = 20)
)

View(video_comments_test)



# 05 Collect Top Comments for All Videos ----

video_comments <- map_dfr(
  trump_data$video_id,
  ~ get_video_comments(.x, api_key = api_key, max_comments = 30)
)

# Inspect the collected comments
glimpse(video_comments)
View(video_comments)

write_csv(video_comments, "data_raw/video_comments_raw.csv")




# 06 Merge Comment Data with Video Metadata ----

# Merge comments back with the original video metadata.
video_comments_full <- video_comments %>%
  left_join(
    trump_data %>%
      select(video_id, channel_id, ideology, title),
    by = "video_id"
  )


glimpse(video_comments_full)
View(video_comments_full)



# 07 Build Video-Level Documents from Titles and Comments ----

# Aggregate all comments belonging to the same video and combine them with the video title.
video_docs <- video_comments_full %>%
  group_by(video_id, channel_id, ideology, title) %>%
  summarise(
    comments_text = paste(comment_text, collapse = " "),
    n_comments = n(),
    .groups = "drop"
  ) %>%
  mutate(
    text = paste(title, comments_text, sep = " ")
  )

glimpse(video_docs)
View(video_docs)

# 08 Build Channel-Level Documents ----

# In this step, each channel becomes one document by combining the text
# from all videos published by that channel.

#channel_docs <- video_docs %>%
#  group_by(channel_id, ideology) %>%
#  summarise(
#    text = paste(text, collapse = " "),
#    n_videos = n(),
#    total_comments = sum(n_comments),
#    .groups = "drop"
#  )

#glimpse(channel_docs)
#View(channel_docs)


# 09 Tokenize the Text Data ----

# Tokenize the video-level text into individual words.
tokens_unigram <- video_docs %>%
  unnest_tokens(word, text) # unigram

# Tokenize the video-level text into 2 words.  
tokens <- video_docs %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) # Try bigrams (2 words) instead of unigrams (individual word) to capture more context  

glimpse(tokens)
View(tokens)

# Remove common stop words and keep alphabetic tokens only.
# This reduces noise in the text analysis.

data("stop_words")
custom_stopwords <- tibble::tibble(
  word = c(
    "amp", "subscribe", "substack", "patreon",
    "youtube", "channel", "video", "videos",
    "biden", "channel", "watch",
    "patreon", "join", "www", "http", "https",
    "com", "youtube", "live", "shorts", "amp",
    "free", "access", "link", "membership",
    "kyle", "kulinski|", "biden", "donald", "kulinski", 
    "president", "adam", "dave", "pbd", "afroman", "jon",
    "murphy", "stewart", "josh", "jon stewart", "dave smith",
    "fox news"
  )
)

tokens_clean <- tokens %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  filter(!word1 %in% custom_stopwords$word,
         !word2 %in% custom_stopwords$word) %>%
  filter(!str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  unite(word, word1, word2, sep = " ") %>% 
  filter(!word %in% custom_stopwords$word)

tokens_unigram_clean <- tokens_unigram %>%
  anti_join(stop_words, by = "word") %>% # unigram
  anti_join(custom_stopwords, by = "word") %>% # unigram
  filter(str_detect(word, "[a-z]")) # unigram

# filtering out bigrams that appear less than 5 times across the entire corpus to reduce noise
bigram_keep <- tokens_clean %>%
  count(word, sort = TRUE) %>%
  filter(n >= 5) %>%
  pull(word)
tokens_clean <- tokens_clean %>%
  filter(word %in% bigram_keep)

glimpse(tokens_clean)
View(tokens_clean)




# 10 Compute Word Frequencies by Ideology ----

# Count how often each word appears within each channel.
#channel_word_counts <- tokens_clean %>%
#  count(channel_id, ideology, word, sort = TRUE)

# Count how often each word appears within each ideology
#ideology_word_counts <- tokens_clean %>%
#  count(channel_id, ideology, word, sort = TRUE)

#View(channel_word_counts)
#head(channel_word_counts, 20)

#channel_tfidf <- channel_word_counts %>%
#  bind_tf_idf(term = word, document = channel_id, n = n) %>%
#  arrange(desc(tf_idf))

#View(channel_tfidf)
#head(channel_tfidf, 30)

#channel_tfidf %>% 
#  group_by(ideology) %>%
#  slice_max(tf_idf, n = 10) %>%
#  print (n = 35)

# Count how often each word appears within each ideology
ideology_word_counts <- tokens_clean %>%
  count(ideology, word, sort = TRUE)

# Compute TF-IDF using ideology as document
ideology_tfidf <- ideology_word_counts %>%
  bind_tf_idf(term = word, document = ideology, n = n) %>%
  arrange(desc(tf_idf))

# View top words per ideology
ideology_tfidf %>% 
  group_by(ideology) %>%
  slice_max(tf_idf, n = 10) %>%
  print(n = 35)


# 11 Compute Sentiment by Ideology ----

# Use a lexicon-based approach to compare positive and negative emotional language across ideological groups.
bing <- get_sentiments("bing")

sentiment_scores <- tokens_unigram_clean %>%
  inner_join(bing, by = "word") %>%
  count(ideology, sentiment)

View(sentiment_scores)
write_csv(sentiment_scores, "data_preprocessed/sentiment_scores.csv")
