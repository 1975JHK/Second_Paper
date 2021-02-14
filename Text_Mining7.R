# Comparison Analysis
# February 11, 2021
# JH Kim

# Loading the required packages
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggwordcloud)
library(KoNLP)
library(showtext)


# Setting up environmemts for text mining
setwd('D:/Advanced_R/')
useNIADic()
font_add_google(name = 'Black Han Sans', family = 'blackhansans')
font_add_google(name = 'Nanum Gothic', family = 'nanumgothic')
showtext_auto()
theme_set(new = theme_minimal(base_family = 'blackhansans'))


# Importing the dataset
raw_park <- readLines('speech_park.txt', encoding = 'UTF-8')
raw_park %>% head(3)
str(raw_park)


# Preprocessing the dataset
park <- raw_park %>% 
  str_replace_all(pattern = '[^가-힣]', replacement = ' ') %>% 
  str_squish() %>% 
  unique() %>% 
  as_tibble()
park %>% head(5)


# Tokenization with word
word_park <- park %>% 
  unnest_tokens(input = value,
                output = word,
                token = 'words') %>% 
  filter(str_count(word) > 1)
word_park %>% head(10)

top20 <- word_park %>% 
  count(word, sort = T) %>% 
  head(20)
top20

# Frequency Analysis with bar chart
ggplot(data = top20, aes(x = reorder(word, n), y = n, fill = word))+
  geom_col(color = 'black', width = 0.8, alpha = 0.8)+
  geom_text(aes(label = n), size = 5, family = 'blackhansans')+
  ggtitle('박근혜 전 대통령 연설문 분석')+
  labs(x = '빈출 단어', y = '사용 빈도(회)')+
  coord_flip()+
  theme(legend.position = 'none',
        axis.text = element_text(size = 13, family = 'blackhansans'),
        axis.title = element_text(size = 15, family = 'blackhansans'))


# Frequency Analysis with Nouns
raw_park <- readLines('speech_park.txt', encoding = 'UTF-8')
raw_park %>% head(3)
park <- raw_park %>% 
  str_replace_all(pattern ='[^가-힣]' , replacement = ' ') %>% 
  str_squish() %>% 
  unique() %>% 
  as_tibble()
park %>% head()

noun_park <- park %>% 
  unnest_tokens(input = value,
                output = noun,
                token = extractNoun)

noun_park <- noun_park %>% 
  count(noun, sort = T) %>% 
  filter(str_count(noun) > 1)
noun_park %>% head(20)

top50 <- noun_park %>% head(50)
top_new <- top50 %>% subset(!noun %in% c('국민', '여러분', '사회', '박근혜', '우리',
                                         '국가', '하게', '만들겠습니', '있습니',
                                         '가겠습니', '그동안', '나라', '개개인'))
top_new

ggplot(data = top_new %>% head(20),
       aes(x = reorder(noun, n), y = n, fill = noun))+
  geom_col(color = 'black', width = 0.5, alpha = 0.8)+
  geom_text(aes(label = n), size = 4, color = 'red', hjust = -0.3)+
  ggtitle('대통령 연설문 단어 빈도 분석')+
  labs(x = '사용된 단어들', y = '사용 빈도')+
  coord_flip()+
  theme(legend.position = 'none', 
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 18, face = 'italic'))


ggplot(data = top_new, aes(label = noun, size = n, color = n))+
  geom_text_wordcloud(seed = 1975)+
  scale_radius(limits = c(1, NA), range = c(5, 35))+
  scale_color_gradient(low = '#66aaf2', high = '#004EA1')

park %>% filter(str_detect(value, pattern = '경제'))
park %>% filter(str_detect(value, pattern = '행복'))


sentence_park <- park %>% 
  unnest_tokens(input = value,
                output = sentence,
                token = 'sentences')
sentence_park %>% head(5)
sentence_park %>% filter(str_detect(sentence, pattern = '경제'))
sentence_park %>% filter(str_detect(sentence, pattern = '일자리'))


# Comparison Analysis
raw_park <- readLines('speech_park.txt', encoding = 'UTF-8')
park <- raw_park %>% 
  str_replace_all(pattern = '[^가-힣]', replacement = ' ') %>% 
  str_squish() %>% 
  unique() %>% 
  as_tibble() %>% 
  mutate(president = 'park')
park %>% head(3)


raw_moon <- readLines('speech_moon.txt', encoding = 'UTF-8')
moon <- raw_moon %>% 
  str_replace_all(pattern = '[^가-힣]', replacement = ' ') %>% 
  str_squish() %>% 
  unique() %>% 
  as_tibble() %>% 
  mutate(president = 'moon')
moon <- moon[-c(1:2), ]


bind_speeches <- bind_rows(moon, park) %>% select(value, president)
bind_speeches <- bind_speeches[-c(108:109), ]
bind_speeches %>% head(3)
bind_speeches %>% tail(3)

speeches <- bind_speeches %>% 
  unnest_tokens(input = value,
                output = noun,
                token = extractNoun)
speeches %>% head(5)

frequency <- speeches %>% 
  count(president, noun, sort = T) %>% 
  filter(str_count(noun) > 1)

frequency %>% head(10)
frequency %>% tail(10)

top10 <- frequency %>% 
  group_by(president) %>% 
  slice_max(n, n = 10, with_ties = F)
top10

ggplot(data = top10, aes(x = reorder_within(noun, n, president), 
                         y = n, fill = president))+
  geom_col(color = 'black', width = 0.8, 
           alpha = 0.8, position = 'dodge')+
  geom_text(aes(label = n), color = 'red', 
            size = 4, fontface = 'italic')+
  ggtitle('Morphological Analysis by Presidents')+
  labs(x = 'Used Words', y = 'Frequency')+
  scale_x_reordered()+
  coord_flip()+
  facet_wrap(~president, scale = 'free')


str(frequency)
top50 <- frequency %>% 
  filter(!noun %in% c('번째', '하겠습니', '그동안', '나라', '동안',
                      '들이', '우리나라', '사람', '하게', '대통령',
                      '나가겠습니', '만들겠습니', '가겠습니', '때문',
                      '박근혜', '대통령의', '국민')) %>% 
  group_by(president) %>% 
  slice_max(n, n = 50, with_ties = F)
  
  
top50 %>% head(20)
top50 %>% tail(20)


ggplot(data = top50, aes(label = noun, size = n, 
                         color = n))+
  geom_text_wordcloud(seed = 1975)+
  scale_radius(limits = c(2, NA), range = c(5, 40))+
  scale_color_gradient(low = '#66aaf2', high = '#004EA1')+
  facet_wrap(~president, scale = 'free')

ggplot(data = top50 %>% group_by(president) %>% 
         slice_max(n, n = 20), aes(x = reorder_within(noun, n, president),
                         y = n, fill = president))+
  geom_col(color = 'black', width = 0.8,
           size = 0.3, alpha = 0.8)+
  geom_text(aes(label = n), color = 'red', size = 5,
            fontface = 'italic')+
  ggtitle('Word Frequency \n by President')+
  labs(x = NULL, y = NULL)+
  coord_flip()+
  scale_x_reordered()+
  facet_wrap(~president, scale = 'free')+
  theme(legend.position = 'none',
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 15),
        plot.title = element_text(size = 17, hjust = 0.5))
  

# Comparision with odds ratio
df_long <- frequency %>%
  group_by(president) %>% 
  slice_max(n, n = 10) %>% 
  filter(noun %in% c('국민', '우리', '정치', '행복', '나라', '경제'))
df_long

df_wide <- df_long %>% 
  pivot_wider(names_from = president,
              values_from = n)
df_wide

df_wide <- df_long %>% 
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = list(n = 0))
df_wide

str(frequency)
frequency_wide <- frequency %>% 
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = list(n = 0))
frequency_wide %>% head(20)

frequency_wide <- frequency_wide %>% 
  mutate(ratio_moon = ((moon + 1) / sum(moon + 1)),
         ratio_park = ((park + 1) / sum(park + 1)))
frequency_wide %>% head(20)

frequency_wide <- frequency_wide %>% 
  mutate(odds_ratio = ratio_moon / ratio_park)
frequency_wide %>% slice_max(odds_ratio, n = 20)
frequency_wide %>% slice_min(odds_ratio, n = 20)

top10 <- frequency_wide %>% 
  filter(rank(-odds_ratio) <= 10 | rank(odds_ratio) <= 10)
top10

top10 %>% arrange(-odds_ratio) %>% 
  print(n = Inf)
