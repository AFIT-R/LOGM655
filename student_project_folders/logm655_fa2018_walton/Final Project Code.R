
#Code adapted from the following:
# https://www.kaggle.com/rtatman/tutorial-sentiment-analysis-in-r


library(rprojroot)
library(tidytext)
library(tidyr)
library(dplyr)
library(ggplot2)

library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(wordcloud)

root <- find_root(is_rstudio_project)
dest <- file.path(root, 'BeigeBooks')
my_files <- list.files(path = dest, 
                       pattern = "pdf",  
                       full.names = TRUE)

GetSentiment <- function(file){
  
  bookText <- pdftools::pdf_text(pdf = file)
  
  tokens <- data_frame(text = bookText) %>% 
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
  
  sentiment <- tokens %>%
    inner_join(get_sentiments("bing")) %>% 
    count(sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative) %>%
    mutate(file = basename(file)) %>%
    mutate(YYYYMM = as.numeric(str_match(file, "\\d{6}"))) %>%
    mutate(method = "bing")
  
  sentimentsNRC <- tokens %>%
    inner_join(get_sentiments("nrc")) %>%
    filter(sentiment %in% c("positive", "negative")) %>%
    count(sentiment) %>%
    spread(sentiment, n, fill = 0) %>% 
    mutate(sentiment = positive - negative) %>%
    mutate(file = basename(file)) %>%
    mutate(YYYYMM = as.numeric(str_match(file, "\\d{6}"))) %>%
    mutate(method = "nrc")
  
  sentiment <- rbind(sentiment, sentimentsNRC)
  
  sentimentsL <- tokens %>%
    inner_join(get_sentiments("loughran")) %>%
    filter(sentiment %in% c("positive", "negative")) %>%
    count(sentiment) %>%
    spread(sentiment, n, fill = 0) %>% 
    mutate(sentiment = positive - negative) %>%
    mutate(file = basename(file)) %>%
    mutate(YYYYMM = as.numeric(str_match(file, "\\d{6}"))) %>%
    mutate(method = "loughran")
  
  sentiment <- rbind(sentiment, sentimentsL)
  
  sentimentAFINN <- tokens %>%
    inner_join(get_sentiments("afinn")) %>% 
    group_by(index = word ) %>%
    summarise(sentiment = sum(score))
  
  sentiment <- rbind(sentiment, c("N/A", 
                                  "N/A", 
                                  sum(sentimentAFINN$sentiment), 
                                  basename(file), 
                                  as.numeric(str_match(file, "\\d{6}")),
                                  "afinn"))
  
  sentimentsNRC <- tokens %>%
    inner_join(get_sentiments("nrc")) %>%
    filter(sentiment %in% c("positive", "negative")) %>%
    count(sentiment)
  
  return(sentiment)
}

get_sentiments("loughran")


sentiments <- bplot <- data_frame()

# get the sentiments for each file in our datset
for(i in my_files){
  sentiments <- rbind(sentiments, GetSentiment(i))
}

# plot of sentiment over time & automatically choose a method to model the change
ggplot(sentiments, aes(group = method, x = YYYYMM, y = sentiment, color = method)) + 
  geom_line() + 
  geom_vline(xintercept = 9) +
  geom_smooth(method = "auto")

#Word clouds

sentimentsNRC <- tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 150))

sentimentsL <- tokens %>%
  inner_join(get_sentiments("loughran")) %>%
  filter(sentiment %in% c("positive", "negative"))%>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 150))

sentimentAFINN <- tokens %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 150))

sentimentBing <- tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 150))



bookText <- pdftools::pdf_text(pdf = my_files[16])

tokens <- data_frame(text = bookText) %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

sentiment <- tokens %>%
  inner_join(get_sentiments("bing")) %>% 
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(file = basename(my_files[1])) %>%
  mutate(YYYYMM = as.numeric(str_match(my_files[1], "\\d{6}"))) %>%
  mutate(method = "bing")

sentiment <- tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>%
  mutate(file = basename(file)) %>%
  mutate(YYYYMM = as.numeric(str_match(file, "\\d{6}"))) %>%
  mutate(method = "nrc")

sentiment <- rbind(sentiment, sentimentsNRC)

sentiment <- tokens %>%
  inner_join(get_sentiments("loughran")) %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>%
  mutate(file = basename(file)) %>%
  mutate(YYYYMM = as.numeric(str_match(file, "\\d{6}"))) %>%
  mutate(method = "loughran")

sentiment <- rbind(sentiment, sentimentsL)

sentimentAFRINN <- tokens %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = word ) %>%
  summarise(sentiment = sum(score))

sentiment <- rbind(sentiment, c("N/A", 
                                "N/A", 
                                sum(sentimentAFINN$sentiment), 
                                basename(file), 
                                as.numeric(str_match(file, "\\d{6}")),
                                "afinn"))

sentimentsNRC <- tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment)
