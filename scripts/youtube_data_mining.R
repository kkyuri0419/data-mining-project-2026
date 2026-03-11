install.packages("tuber")
install.packages("jsonlite")
library(tuber)
library(tidyverse)
library(jsonlite)
library(httr)

# Read Credentials from downloaded OAuth Client JSON file
credentials <- fromJSON(file.path(getwd(),"youtube_api_oauth_client_credentials.json"))
client_id <- credentials$installed$client_id
client_secret <- credentials$installed$client_secret
api_key <- credentials$installed$api_key



# 01 Searching for Channels --------------------------------------------------

search_channels <- function(query){
  res <- GET(
    "https://www.googleapis.com/youtube/v3/search",
    query = list(
      part = "snippet",
      q = query,
      type = "channel",
      maxResults = 10,
      regionCode = "US",
      key = api_key))
  
  data <- fromJSON(content(res, "text"), flatten=TRUE)
  print(data$items)
  
  # Extract relevant information and create a data frame
  df <- data.frame(
    channel_id = data$items$snippet.channelId,
    channel_title = data$items$snippet.title,
    channel_description = data$items$snippet.description
  )
  
  return(df)
}


queries <- c(
  #"US politics",
  "political commentary US",
  #"left wing politics",
  #"progressive politics",
  "right wing politics",
  "US news",
  "liberal news",
  "progressive politics US",
  "liberal political commentary",
  "left wing politics US",
  "progressive panel discussion",
  "democrat debate panel"
)

# Loop through each query and collect channels, then combine into a single data frame
all_channels <- data.frame()
for (q in queries){
  temp <- search_channels(q)
  temp$query <- q
  all_channels <- bind_rows(all_channels, temp)
} 

# Remove duplicate channels based on channel_id
all_channels <- all_channels %>%
  distinct(channel_id, .keep_all = TRUE)

View(all_channels)
