# Comoparison with odds ratio
# February 12, 2021
# JH Kim

# Loading the required packages
library(ggplot2)
library(caret)
library(dplyr)
library(tidyr)
library(ggwordcloud)
library(stringr)
library(tidytext)
library(showtext)
library(KoNLP)

# Setting up environments
setwd('D:/Advanced_R/')
font_add_google(name = 'Black Han Sans', family = 'blackhansans')
theme_set(new = theme_minimal(base_family = 'blackhansans'))
showtext_auto()

# Importing the dataset
raw_moon <- readLines('speech_moon.txt', encoding = 'UTF-8')
raw_park <- readLines('speech_park.txt', encoding = 'UTF-8')

# Preprocessing
moon <- raw_moon %>% 
  str_replace_all(pattern = '[^가-힣]', replacement = ' ') %>% 
  str_squish() %>% 
  unique() %>% 
  as_tibble()
moon <- moon[-c(1,2), ]
moon %>% head(5)

park <- raw_park %>% 
  str_replace_all(pattern = '[^가-힣]', replacement = ' ') %>% 
  str_squish() %>% 
  unique() %>% 
  as_tibble()
park %>% head(5)

# Morphological Analysis
noun_moon <- moon %>% 
  unnest_tokens(input = value,
                output = noun,
                token = extractNoun) %>% 
  count(noun, sort = T) %>% 
  filter(str_count(noun) > 1)
noun_moon <- noun_moon %>% mutate(president = 'moon')
noun_moon %>% head(10)


noun_park <- park %>% 
  unnest_tokens(input = value,
                output = noun,
                token = extractNoun) %>% 
  count(noun, sort = T) %>% 
  filter(str_count(noun) > 1)
noun_park <- noun_park %>% mutate(president = 'park')
noun_park %>% head(10)


# Odds ratio
bind_speeches <- bind_rows(noun_moon, noun_park)
bind_speeches %>% head
frequency <- bind_speeches %>% 
  group_by(president) %>% 
  slice_max(n, n = 20, with_ties = F)
frequency %>% tail(20)
frequency %>% head(20)

frequency <- frequency %>% 
  filter(!noun %in% c('사회', '중요', '하게', '사람', '하게',
                      '우리나라', '정버', '국민', '박근혜',
                      '대통령', '우리', '나라', '들이'))
frequency

wide_df <- frequency %>% 
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = list(n = 0))
wide_df

comparison <- wide_df %>% 
  mutate(moon_proportion = ((moon + 1)/sum(moon + 1)),
         park_proportion = ((park + 1)/sum(park + 1)),
         odds_ratio = moon_proportion/park_proportion)
comparison

top10 <- comparison %>% 
  filter(rank(odds_ratio) <= 10|
           rank(-odds_ratio) <= 10)
top10

top10 <- top10 %>% 
  mutate(president = ifelse(odds_ratio >1 , 'moon', 'park'),
         n = ifelse(odds_ratio > 1, moon, park))
top10

ggplot(data = top10, aes(x = reorder_within(noun, odds_ratio, president),
                         y = odds_ratio, fill = president))+
  geom_col(color = 'black', width = 0.8, alpha = 0.8,
           size = 0.3)+
  geom_text(aes(label = round(odds_ratio, 3)), vjust = 0.8,
            size = 5, color = 'black')+
  scale_x_reordered()+
  ggtitle('Word Frequency /n by Odds Ratio')+
  labs(x = NULL, y = NULL)+
  coord_flip()+
  theme(legend.position = 'non',
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 15),
        plot.title = element_text(size = 18, hjust = 0.5),
        strip.text = element_text(size = 15, face = 'bold'))+
  facet_wrap(~president, scale = 'free')


# Detecting sentences with words
moon <- moon %>% mutate(president = 'moon')
park <- park %>% mutate(president = 'park')
bind.speeches <- bind_rows(moon, park)

speeches_sentence <- bind.speeches %>% 
  as_tibble() %>% 
  unnest_tokens(input = value,
                output = sentence,
                token = 'sentences')
head(speeches_sentence)

speeches_sentence %>% 
  filter(str_detect(sentence, pattern = '복지'))
speeches_sentence %>% 
  filter(str_detect(sentence, pattern = '행복'))


head(comparison)
comparison %>% arrange(abs(1-odds_ratio)) %>% head(10)
comparison %>% 
  filter(moon >= 5, park >= 5) %>%
  arrange(abs(1-odds_ratio)) %>%
  head(10)


comparison %>% head(20)
comparison <- comparison %>% mutate(log_odds_ratio = log(odds_ratio))
comparison %>% filter(rank(log_odds_ratio) <= 10 | rank(-log_odds_ratio) <= 10)
