# Latent Dirichlet Allocation(LDA)
# March 3, 2021
# JH Kim

# Loading the required packages
library(easypackages)
libraries(c('tidyr', 'dplyr', 'caret', 'readxl', 'readr', 'ggplot2',
            'ggwordcloud', 'tidytext', 'stringr', 'textclean', 'showtext',
            'KoNLP', 'widyr', 'tm', 'topicmodels', 'scales'))

# Setting Up environments
setwd('D:/Advanced_R/')
font_add_google(name = 'Black Han Sans', family = 'blackhansans')
showtext_auto()
theme_set(new = theme_minimal(base_family = 'blackhansans'))
useNIADic()

# Importing the dataset
raw_news_comment <- read_csv('news_comment_parasite.csv') %>% 
  mutate(id = row_number())
raw_news_comment %>% head()

# Preprocessing
news_comment <- raw_news_comment %>% 
  mutate(reply = str_replace_all(reply, pattern = '[^가-힣]', replacement = ' '),
         reply = str_squish(reply)) %>% 
  distinct(reply, .keep_all = T) %>% 
  filter(str_count(reply, boundary('word')) >= 3)

news_comment %>% head()


# Morphological Analysis
comment <- news_comment %>% 
  unnest_tokens(input = reply,
                output = word,
                token = extractNoun,
                drop = F) %>% 
  filter(str_count(word) > 1) %>% 
  group_by(id) %>% 
  distinct(word, .keep_all = T) %>% 
  ungroup() %>% 
  select(id, word)

comment %>% head()

count_word <- comment %>% 
  add_count(word) %>% 
  filter(n <= 200) %>% 
  select(-n)

count_word %>% 
  count(word, sort = T) %>% 
  print(n = 200)

stopword <- c('들이', '하다', '하게', '하면', '해서', '이번', '하네',
              '해요', '이것', '니들', '하기', '하지', '한거', '해주',
              '그것', '어디', '여기', '까지', '이거', '하신', '만큼')

count_word <- count_word %>% 
  filter(!word %in% stopword) %>% 
  mutate(word = recode(word, 
                       '자랑스럽습니' = '자랑',
                       '자랑스럽' = '자랑',
                       '자한' = '자유한국당',
                       '문재' = '문재인',
                       '한국의' = '한국',
                       '그네' = '박근혜',
                       '추카' = '축하',
                       '정경' = '정경심',
                       '방탄' = '방탄소년단'))

# Document-Term Matrix
count_word_doc <- count_word %>% 
  count(id, word, sort = T)

count_word_doc %>% head()

dtm_comment <- count_word_doc %>% 
  cast_dtm(document = id, term = word, value = n)

dtm_comment

as.matrix(dtm_comment[1:5, 1:10])

# Building LDA Model
lda_model <- LDA(dtm_comment,
                 k = 8,
                 method = 'Gibbs',
                 control = list(seed = 1975))
lda_model

glimpse(lda_model)

term_topic <- tidy(lda_model, matrix = 'beta')
term_topic

term_topic %>% 
  count(topic)

term_topic %>% 
  filter(topic == 1) %>% 
  summarise(sum_beta = sum(beta))

term_topic %>% 
  filter(term == '작품상')

term_topic %>% 
  filter(topic == 6) %>% 
  arrange(-beta)

terms(lda_model, 20) %>% data.frame()

top_term_topic <- term_topic %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10)

ggplot(data = top_term_topic, aes(x = reorder_within(term, beta, topic), 
                                  y = beta, fill = as.factor(topic)))+
  geom_col(color = 'black', size = 0.3, alpha = 0.8, show.legend = F)+
  scale_x_reordered()+
  scale_y_continuous(n.breaks = 4,
                     labels = number_format(accuracy = 0.01))+
  coord_flip()+
  labs(x = NULL, y = 'Beta')+
  facet_wrap(~topic, scale = 'free')+
  theme(text = element_text(size = 13, face = 'bold'))
