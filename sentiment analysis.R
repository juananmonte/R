#aim of this project is to build a sentiment analysis model which will allow us to categorize 
#words based on their sentiments, that is whether they are positive, negative 
#and also the magnitude of it. 


#intall packages

install.packages("tidytext")
install.packages("wordcloud")

#Load packages
library(tidytext)
library(janeaustenr)
library(stringr)
library(tidytext)
library(devtools)


#Check the words
sentiments

get_sentiments("bing")

tidy_data <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(), chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>% ungroup() %>% unnest_tokens(word, text)

tidy_data

positive_senti

#filter a positive sentiment words
positive_senti <- get_sentiments("bing") %>% filter(sentiment == "positive")

tidy_data %>% filter(book  == "Emma") %>% semi_join(positive_senti) %>% count(word, sort= TRUE)

#There are many positive words like "good", "happy", "love" etc.


#we will use spread() function to segregate our data into separate columns of positive and negative sentiments.

library(tidyr)

bing <- get_sentiments("bing")

Emma_sentiments <- tidy_data %>% inner_join(bing) %>% 
  count(book = "Emma", index = linenumber %/% 80, sentiment) %>% 
          spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative)

ggplot(Emma_sentiment, aes(index, sentiment, fill = book))+
  geom_bar(stat = "identity", show.legend = TRUE) + facet_wrap(~book, ncol = 2, scales = "free_x")


counting_words <- tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)
head(counting_words)


counting_words %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment))+
  geom_col() +
  coord_flip() +
  labs(y = "Sentiment Score")


library(reshape2)
library(wordcloud)
tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "dark green"),
                   max.words = 100)




