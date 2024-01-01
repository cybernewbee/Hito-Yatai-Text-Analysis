# Load the packages
library(rvest)
library(tidyverse)
library(data.table)

# Web scrape
# URL object
url = "https://www.yelp.com/biz/hito-yatai-carmichael"

# convert URL to html object
page <- read_html(url)

pageNums <- page %>% 
  html_elements(xpath = "//div[@class=' css-1aq64zd']") %>%
  html_text() %>%
  str_extract('of.*') %>%
  str_remove('of ') %>%
  as.numeric()

pageSequence <- seq(from = 0, to = (pageNums * 10)-10, by = 10)

#store variables in vectors
review_date_all = c()
review_rating_all = c()
review_text_all = c()
review_username_all = c()
review_location_all = c()

for (i in pageSequence) {
  if ( i == 0) {
    page <- read_html(url)
  } else {
    page <- read_html(paste0(url, '?start=', i))
  }
  #usernames
  review_usernames <- page %>%
    html_elements(xpath = "//div[starts-with(@class, 'user-passport')]") %>%
    html_elements(xpath = ".//a[starts-with(@href, '/user_details')]") %>%
    html_text()
  
  #location
  review_locations <- page %>%
    html_elements(xpath = "//div[starts-with(@class, 'user-passport')]") %>%
    html_elements(xpath = ".//span[@class=' css-qgunke']")%>%
    html_text() %>%
    .[.!="Location"]
    
  # review date
  review_date <- page %>%
    html_elements(xpath = "//*[@class=' css-chan6m']") %>%
    html_text() %>%
    .[str_detect(.,"^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) \\d{1,2}, \\d{4}$")]
  
  # review data
  review_ratings <- page %>%
    html_elements(xpath = "//*[@class=' css-10n911v']") %>% 
    html_elements(xpath = ".//div[contains(@aria-label, 'rating')]") %>%
    html_attr('aria-label') %>%
    str_remove_all(' star rating') %>%
    as.numeric()
  
  # review text
  review_text <- page %>%
    html_elements(xpath = "//p[starts-with(@class, 'comment')]") %>%
    html_text() 
  
  review_date_all = append(review_date_all, review_date)
  review_rating_all = append(review_rating_all, review_ratings)
  review_text_all = append(review_text_all, review_text)
  review_username_all = append(review_username_all, review_usernames)
  review_location_all = append(review_location_all, review_locations)
  
}

df <- data.frame('Date' = review_date_all,
                 'Ratings' = review_rating_all,
                 'Text' = review_text_all,
                 "Usernames" = review_username_all,
                 "Locations" = review_location_all)



# Covert date column in to date data type 

df$Date <- as.Date(df$Date, "%b %d, %Y")


# Exploratory analysis
library(ggplot2)


# Plot ratings by month 
# Convert Date to Month
df$Month <- months(df$Date, abbreviate = F)
df$Month<- as.Date(paste("1", df$Month, "2023"), format = "%d %B %Y")

ggplot(data=df,
       mapping = aes(x = Ratings, fill = factor(Ratings))
       ) +
  geom_bar() + 
  scale_fill_brewer(palette = "YlOrRd") +
  facet_wrap(~Month)

ggplot(data=df,
       mapping = aes(y = Locations, x = Ratings, color = factor(Ratings))
       ) +
  geom_count(mapping = aes(size = 3)) 

# Text analysis
library(tidytext)
library(widyr)
library(wordcloud)
library(tidyr)


review_words <- 
  df %>%
  unnest_tokens(output = word, input = Text) %>%
  anti_join(stop_words, by = "word") %>%
  distinct()
  
review_words %>%
  count(word) %>%
  with(wordcloud(word, n, min.freq=3,))

bing <- get_sentiments("bing")

bing_word_counts <- review_words %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)


bing_word_counts %>%
  filter(n > 1) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")




