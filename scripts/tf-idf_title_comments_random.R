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
video_data <- read_csv("data_preprocessed/video_data.csv")
View(video_data)

# 02 Define a Function to Retrieve One Page of Top-Level Comments ----

# This function retrieves one page of top-level comments for a single video.
# It returns both the comments and the next page token for pagination.
get_video_comments_page <- function(video_id, api_key, page_token = NULL, max_results = 30) {
  
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
get_video_comments <- function(video_id, api_key, max_comments = 30) {
  
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



# 05 Collect Top Comments for All Videos ----

video_comments_wo_keyword <- map_dfr(
  video_data$video_id,
  ~ get_video_comments(.x, api_key = api_key, max_comments = 30)
)

# Inspect the collected comments
View(video_comments_wo_keyword)

write_csv(video_comments_wo_keyword, "data_raw/video_comments_raw_wo_keyword.csv")




# 06 Merge Comment Data with Video Metadata ----

# Merge comments back with the original video metadata.
video_comments_full_wo_keyword <- video_comments_wo_keyword %>%
  left_join(
    video_data %>%
      select(video_id, channel_id, ideology, title),
    by = "video_id"
  )


glimpse(video_comments_full_wo_keyword)
View(video_comments_full_wo_keyword)

write_csv(video_comments_full_wo_keyword, "data_raw/full_video_comments_raw_wo_keyword.csv")



# 07 Build Video-Level Documents from Titles and Comments ----

# Aggregate all comments belonging to the same video and combine them with the video title.
video_docs_wo_keyword <- video_comments_full_wo_keyword %>%
  group_by(video_id, channel_id, ideology, title) %>%
  summarise(
    comments_text = paste(comment_text, collapse = " "),
    n_comments = n(),
    .groups = "drop"
  ) %>%
  mutate(
    text = paste(title, comments_text, sep = " ")
  )

glimpse(video_docs_wo_keyword)
View(video_docs_wo_keyword)



# 09 Tokenize the Text Data ----

# Tokenize the video-level text into individual words.
unigram_tokens_wo_keyword <- video_docs_wo_keyword %>%
  unnest_tokens(word, text) # unigram

# Tokenize the video-level text into 2 words.  
bigram_tokens_wo_keyword <- video_docs_wo_keyword %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) # Try bigrams (2 words) instead of unigrams (individual word) to capture more context  


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
    "fox news", "rebel news", "patti smith", "amy goodman",
    "tommy robinson", "reza pahlavi", "ken paxton",
    "flávio bolsonaro", "tiger woods", "amy juan", "lies lies",
    
    # media / format
    "meidastouch podcast", "panel featuring", "quot panel",
    "delivers remarks",
    
    # names
    "michael stipe", "mark carney", "michael knowles",
    "prince reza",
    
    # web
    "www.foxnews.com",
    
    # weak semantics
    "happy anniversary", "pound cake"
  )
)

bigram_tokens_clean_wo_keyword <- bigram_tokens_wo_keyword %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  filter(!word1 %in% custom_stopwords$word,
         !word2 %in% custom_stopwords$word) %>%
  filter(!str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  unite(word, word1, word2, sep = " ") %>% 
  filter(!word %in% custom_stopwords$word)


unigram_tokens_clean_wo_keyword <- unigram_tokens_wo_keyword %>%
  anti_join(stop_words, by = "word") %>% # unigram
  anti_join(custom_stopwords, by = "word") %>% # unigram
  filter(str_detect(word, "[a-z]")) # unigram

# filtering out bigrams that appear less than 5 times across the entire corpus to reduce noise
bigram_keep <- bigram_tokens_clean_wo_keyword %>%
  count(word, sort = TRUE) %>%
  filter(n >= 5) %>%
  pull(word)
bigram_tokens_clean_wo_keyword <- bigram_tokens_clean_wo_keyword %>%
  filter(word %in% bigram_keep)

# filtering out bigrams that appear in only one channel to focus on more widely used bigrams
bigram_tokens_clean_wo_keyword %>%
  group_by(word) %>%
  filter(n_distinct(channel_id) >= 2)


# 10 Compute Word Frequencies by Ideology ----

# Count how often each word appears within each ideology
ideology_word_counts_wo_keyword <- bigram_tokens_clean_wo_keyword %>%
  count(ideology, word, sort = TRUE)

# Compute TF-IDF using ideology as document
ideology_tfidf_wo_keyword <- ideology_word_counts_wo_keyword %>%
  bind_tf_idf(term = word, document = ideology, n = n) %>%
  arrange(desc(tf_idf))

# View top words per ideology
ideology_tfidf_wo_keyword %>% 
  group_by(ideology) %>%
  slice_max(tf_idf, n = 10) %>%
  print(n = 35)

write_csv(ideology_tfidf_wo_keyword, "data_raw/ideology_tfidf_wo_keywordd.csv")


# 11 Compute Sentiment by Ideology ----

# Use a lexicon-based approach to compare positive and negative emotional language across ideological groups.
bing <- get_sentiments("bing")

sentiment_scores_wo_keyword <- unigram_tokens_clean_wo_keyword %>%
  inner_join(bing, by = "word") %>%
  count(ideology, sentiment)

View(sentiment_scores_wo_keyword)
write_csv(sentiment_scores_wo_keyword, "data_preprocessed/sentiment_scores_without_specific_keyword.csv")
