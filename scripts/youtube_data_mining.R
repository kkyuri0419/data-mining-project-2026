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



# 02 Channel Filtering ----------------------------------------------------

# Function to get channel statistics (like subscriber count) for a list of channel IDs
get_channel_stats <- function(channel_ids) {
  
  # YouTube API allows a maximum of 50 channel IDs per request, so we need to split the list into chunks
  channel_ids <- unique(channel_ids)
  chunks <- split(channel_ids, ceiling(seq_along(channel_ids) / 50))
  
  all_stats <- lapply(chunks, function(chunk){
    res <- GET(
      url = "https://www.googleapis.com/youtube/v3/channels",
      query = list(
        part = "statistics",
        id = paste(chunk, collapse = ","),
        key = api_key
      )
    )
    
    data <- fromJSON(content(res, "text"), flatten = TRUE)
    
    df <- data.frame(
      channel_id = data$items$id,
      subscribers = as.numeric(data$items$statistics.subscriberCount)
    )
  })
  
  bind_rows(all_stats)
}

# Get channel statistics for all collected channels and merge with the main data frame

stats <- get_channel_stats(all_channels$channel_id)
str(stats)

# Merge the statistics back into the main data frame
all_channels <- all_channels %>%
  left_join(stats, by = "channel_id")

View(all_channels)

# Filter channels with more than 1 million subscribers
filtered_channels <- all_channels %>%
  filter(subscribers >= 1000000)

View(filtered_channels)


# 03 Manual Labeling of Ideological Category ------------------------------

filtered_channels$ideology <- NA

filtered_channels$ideology[filtered_channels$channel_title %in% c(
  "Pod Save America",
  "Democracy Now!",
  "Majority Report",
  "David Pakman Show",
  "The Young Turks",
  "Secular Talk")] <- "left"

filtered_channels$ideology[filtered_channels$channel_title %in% c(
  "Newsmax",
  "Right Side Broadcasting Network",
  "One America News Network",
  "Fox News",
  "Conservative Twins"
)] <- "right"

filtered_channels$ideology[filtered_channels$channel_title %in% c(
  "ABC News",
  "Good Morning America",
  "NBC News"
)] <- "center"

View(filtered_channels)


# 04 Getting Videos from each Channels ------------------------------------

get_videos <- function(channel_id){
  res <- GET(
    "https://www.googleapis.com/youtube/v3/search",
    query = list(
      part = "snippet",
      channelId = channel_id,
      order = "date",
      type = "video",
      maxResults = 20,
      key = api_key))
  
  data <- fromJSON(content(res, "text"), flatten=TRUE)
  print(data$items)
  
  tibble::tibble(
    video_id = data$items$id.videoId,
    title = data$items$snippet.title,
    video_description = data$items$snippet.description,
  )
}

library(purrr)

video_data <- filtered_channels %>%
  filter(ideology == "left" | ideology == "right") %>% # Focus on left and right channels
  mutate(videos = map(channel_id, get_videos)) %>% # Get videos for each channel
  unnest(videos) # Unnest the video data into a flat structure

View(video_data)