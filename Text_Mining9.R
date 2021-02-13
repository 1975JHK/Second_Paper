# Sentiment Analysis
# February 12, 2021
# JH Kim

# Loading the required packages
library(dplyr)
library(tidyr)
library(readr)
library(caret)
library(ggplot2)
library(stringr)
library(tidytext)
library(textclean)
library(KoNLP)
library(ggwordcloud)
library(showtext)

# Setting up environments
setwd('D:/Advanced_R/')
font_add_google(name = 'Nanum Gothic', family = 'nanumgothic')
font_add_google(name = 'Black Han Sans', family = 'blackhansans')
theme_set(new = theme_minimal(base_family = 'nanumgothic'))

# Basic functions for sentiment analysis
dic <- read_delim("SentiWord_Dict.txt", delim='\t', col_names=c("word", "polarity"))
dic %>% filter(polarity == 2) %>% arrange(word)
dic %>% filter(polarity == -2) %>% arrange(word)
dic %>% filter(word %in% c('좋은', '나쁜', '착하다'))
dic %>% filter(word %in% c('기쁜', '슬픈', '안타깝다'))
dic %>% filter(!str_detect(string = word, pattern = '[가-힝]')) %>% arrange(word)
dic %>% mutate(sentiment = ifelse(polarity >= 1, 'pos',
                                  ifelse(polarity <= -1, 'neg', 'neu'))) %>% 
  count(sentiment)

df <- tibble(sentence = c('디자인 예쁘고 마감도 좋아서 만족스럽다.',
                          '디자인은 괜찮다. 그런데 마감이 나쁘고 가격이 비싸다.'))
df <- df %>% 
  unnest_tokens(input = sentence,
                output = word,
                token = 'words',
                drop = F)
df %>% print(n = Inf)

df <- df %>% 
  left_join(dic, by = 'word') %>% 
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))
df %>% print(n = Inf)

df %>% group_by(sentence) %>% 
  summarise(score = sum(polarity),
            sentiment = ifelse(score >= 1, 'positive', 
                               ifelse(score <= -1, 'negative', 'neutral')))

raw_news_comment <- read_csv('news_comment_parasite.csv')
raw_news_comment %>% head(5)

news_comment <- raw_news_comment %>% 
  mutate(id = row_number(),
         reply = str_squish(replace_html(reply)))
glimpse(news_comment)

word_comment <- news_comment %>% 
  unnest_tokens(input = reply,
               output = word,
               token = extractNoun,
               drop = F)
word_comment %>% select(reply, word)

word_comment <- word_comment %>% 
  left_join(dic, by = 'word') %>% 
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))
word_comment %>% select(reply, word, polarity)

word_comment <- word_comment %>% 
  mutate(sentiment = ifelse(polarity == 2, 'positive',
                            ifelse(polarity == -2, 'negative', 'neutral')))
word_comment %>% count(sentiment)

top20_comment <- word_comment %>% 
  filter(sentiment != 'neutral' & str_count(word) > 1) %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n = 20)
top10_comment

ggplot(data = top10_comment, aes(x = reorder_within(word, n, sentiment), y = n, fill = sentiment))+
  geom_col(color = 'black', width = 0.8, size = 0.3, alpha = 0.8)+
  geom_text(aes(label = n), size = 4, color = 'blue', fontface = 'italic', hjust = 0.5)+
  ggtitle('Sentiment Analysis\non news articles for Parasite')+
  labs(x = NULL, y = 'Frequency')+
  coord_flip()+
  scale_x_reordered()+
  facet_wrap(~sentiment, scale = 'free')+
  theme(axis.text = element_text(size = 13, face = 'bold'),
        axis.title = element_text(size = 15, face = 'bold'),
        strip.text = element_text(size = 17, face = 'bold'),
        legend.position = 'none',
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5))
