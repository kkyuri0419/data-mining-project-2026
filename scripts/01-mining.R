install.packages("tuber")
install.packages("jsonlite")
library(tuber)
library(tidyverse)
library(jsonlite)

# Read Credentials from downloaded OAuth Client JSON file
credentials <- fromJSON(file.path(getwd(),"youtube_api_oauth_client_credentials.json"))
client_id <- credentials$installed$client_id
client_secret <- credentials$installed$client_secret

str(credentials)
str(client_id)
str(client_secret)

# Authorize tuber with the credentials
?yt_oauth
file.exists(".httr-oauth")
file.remove(".httr-oauth")
yt_oauth(app_id = client_id, app_secret = client_secret)

