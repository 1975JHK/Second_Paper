# Sentiment Analysis
# February 14, 2021
# JH Kim

# Loading the required packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(showtext)
library(readr)
library(KoNLP)
library(textclean)

# Setting up enviroments
setwd('D:/Advanced_R/')
font_add_google(name = 'Black Han Sans', family = 'blackhansans')
showtext_auto()
theme_set(new = theme_minimal(base_family = 'blackhansans'))
useNIADic()

# Importing the dataset
dic <- read_delim('SentiWord_Dict.txt', delim = '\t1', col_names = F) %>% 
  rename(word = X1, polarity = X2)
glimpse(dic)
dic %>% head(10)

dic %>% filter(polarity == 2) %>% arrange(word)
dic %>% filter(polarity == -2) %>% arrange(word)
dic %>% filter(word %in% c('좋은', '나쁜'))
dic %>% filter(word %in% c('기쁜', '슬픈'))
dic %>% filter(!str_detect(word, pattern = '[가-힣]')) %>% arrange(word)
dic %>% mutate(sentiment = ifelse(polarity >= 1, 'pos',
                                  ifelse(polarity <= -1, 'neg', 'neu'))) %>%
  count(sentiment)

df <- tibble(sentence = c('디자인 예쁘고 마감도 좋아서 만족스럽다.',
                          '디자인은 괜찮다. 그런데 마감이 나쁘고 가격도 비싸다.'))
df

df <- df %>% 
  unnest_tokens(input = sentence,
                output = word,
                token = 'words',
                drop = F)
df %>% print(n = Inf)
df <- df %>% left_join(dic, by = 'word') %>% 
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))
df

df %>% group_by(sentence) %>% summarise(score = sum(polarity))


raw_news_comment <- read_csv('news_comment_parasite.csv')
raw_news_comment %>% head(3)
news_comment <- raw_news_comment %>% 
  mutate(id = row_number(), reply = str_squish(replace_html(reply)))
glimpse(news_comment)

word_comment <- news_comment %>% 
  unnest_tokens(input = reply,
                output = word,
                token = 'words',
                drop = F)
word_comment %>% select(word, reply)

word_comment <- word_comment %>% 
  left_join(dic, by = 'word') %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

word_comment %>% select(word, polarity)
word_comment <- word_comment %>% 
  mutate(sentiment = ifelse(polarity == 2, 'positive',
                            ifelse(polarity == -2, 'negative', 'neutral')))
word_comment %>% count(sentiment)


top10 <- word_comment %>% 
  filter(sentiment != 'neutral') %>% 
  group_by(sentiment) %>% 
  count(word) %>% 
  slice_max(n, n = 20)
top10 <- top10 %>% 
  filter(str_count(word) > 1)
top10 %>% head(10)
top10 %>% tail(10)

ggplot(data = top10, aes(x = reorder_within(word, n, sentiment), 
                         y = n, fill =sentiment))+
  geom_col(color = 'black', size = 0.3, width = 0.8, alpha = 0.8)+
  geom_text(aes(label = n), size = 4, color ='red', fontface = 'italic')+
  ggtitle('SNS Comments on Move Parasite')+
  labs(x = NULL, y = NULL)+
  coord_flip()+
  scale_x_reordered()+
  facet_wrap(~sentiment, scale = 'free')+
  theme(axis.text = element_text(size = 13, face = 'bold'),
        axis.title = element_text(size = 15, face = 'bold'),
        plot.title = element_text(size = 18, face = 'bold', hjust = 0.5),
        strip.text = element_text(size = 15, face = 'bold'),
        legend.position = 'none')


score_comment <- word_comment %>% 
  group_by(id, reply) %>% 
  summarise(score = sum(polarity)) %>% 
  ungroup()
score_comment %>% head(10)

score_comment %>% 
  arrange(-score) %>% 
  select(reply, score) %>% 
  head(5)

score_comment %>% 
  arrange(score) %>% 
  select(reply, score) %>% 
  head(5)

score_distribution <- score_comment %>% 
  count(score) %>% 
  select(score, n)

ggplot(data = score_distribution, aes(x = score, y = n))+
  geom_col(color = 'black', fill = 'steelblue', alpha = 0.8, width = 0.5)

score_comment <- score_comment %>% 
  mutate(sentiment = ifelse(score >= 1, 'positive',
                            ifelse(score <= -1, 'negative', 'neutral')))

frequency_score <- score_comment %>% 
  count(sentiment) %>% 
  mutate(ratio = n/sum(n)*100)
str(frequency_score)

ggplot(data = frequency_score, aes(x = sentiment, y = ratio, fill = sentiment))+
  geom_col(color = 'black', width = 0.8, alpha = 0.8)+
  geom_text(aes(label = round(n, 2)), size = 5, color = 'red', fontface = 'bold.italic')+
  scale_x_discrete(limits = c('positive', 'neutral', 'negative'))+
  theme(axis.text = element_text(size = 13, face = 'bold'),
        axis.title = element_text(size = 15, face = 'bold'))

df <- tibble(country = c('korea', 'korea', 'japan', 'japan'),
             sex = c('m', 'f', 'm', 'f'),
             ratio = c(60, 40, 30, 70))
df
ggplot(data = df, aes(x = country, y = ratio, fill = sex))+
  geom_col(position = 'stack', color = 'black', size = 0.3, alpha = 0.8)+
  geom_text(aes(label = paste0(ratio, '%')), size = 5, color = 'black', 
            position = position_stack(vjust = 0.5), fontface = 'bold.italic')

frequency_score$dummy <- 0
ggplot(data = frequency_score, aes(x = dummy, y = ratio, fill = sentiment))+
  geom_col(color = 'black', alpha = 0.8, position = 'stack', width = 0.3)+
  geom_text(aes(label = paste0(round(ratio, 1), '%')), size = 5, color = 'black',
            position = position_stack(vjust = 0.5), fontface = 'bold.italic')+
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

comment <- score_comment %>% 
  unnest_tokens(input = reply,
                output = word,
                token = 'words',
                drop = F) %>% 
  filter(str_count(word) > 1 & str_detect(word, pattern = '[가-힣]'))
comment %>% head()

frequency_word <- comment %>% 
  count(sentiment, word, sort = T)
frequency_word %>% head(10)

frequency_word %>% 
  filter(sentiment == 'positive')
frequency_word %>% 
  filter(sentiment == 'negative')

comment_wide <- frequency_word %>% 
  filter(sentiment != 'neutral') %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n = 0))
comment_wide %>% head()

comment_wide <- comment_wide %>% 
  mutate(log_odds_ratio = log((positive + 1 / sum(positive + 1))/
                                (negative + 1 / sum(negative + 1))))
comment_wide %>% head()

top10 <- comment_wide %>% 
  group_by(sentiment = ifelse(log_odds_ratio > 0, 'pos', 'neg')) %>% 
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
top10


ggplot(data = top10, aes(x = reorder_within(word, log_odds_ratio, sentiment),
                         y = log_odds_ratio, fill = sentiment))+
  geom_col(color = 'black', size = 0.3, width = 0.8, alpha = 0.8)+
  ggtitle('Sentiment Analysis \n on SNS')+
  labs(y = 'log_odds_ratio', x = 'Words')+
  coord_flip()+
  scale_x_reordered()+
  theme(axis.text = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 15, face = 'bold'),
        plot.title = element_text(size = 18, hjust = 0.5))
  