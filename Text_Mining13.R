# Topic Modeling (Latent Dirichlet Allocation)
# March 6, 2021
# JH Kim

# Loading the required packages
library(easypackages)
libraries(c('dplyr', 'tidyr', 'reaxl', 'readr', 'caret', 'ggplot2',
            'ggwordcloud', 'textclean', 'KoNLP', 'showtext', 'stringr',
            'tidytext', 'widyr', 'tm', 'topicmodels', 'scales'))

# Setting up environments
setwd('D:/Advanced_R')
font_add_google(name = 'Nanum Gothic', family = 'nanumgothic')
theme_set(new = theme_minimal(base_family = 'nanumgothic'))
showtext_auto()
useNIADic()

# Importing the dataset
raw_news_comment <- read_csv('news_comment_parasite.csv') %>% 
  mutate(id = row_number())

raw_news_comment %>% head(3)


# Preprocessing 
news_comment <- raw_news_comment %>% 
  mutate(reply = str_replace_all(reply, '[^가-힣]', ' '),
         reply = str_squish(reply)) %>% 
  distinct(reply, .keep_all = T) %>% 
  filter(str_count(reply, boundary('word')) >= 3)


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
                       '자한' = '자한당',
                       '문재' = '문재인',
                       '한국의' = '한국',
                       '그네' = '박근혜',
                       '추카' = '축하',
                       '정경' = '정경심',
                       '방탄' = '방탄소년단'))

count_word %>% head()

count_word <- count_word %>% 
  count(id, word, sort = T)

count_word %>% head()

dtm_comment <- count_word %>% 
  cast_dtm(document = id, term = word, value = n)

dtm_comment
as.matrix(dtm_comment[1:8, 1:8])

lda_model <- LDA(dtm_comment,
                 k = 8,
                 method = 'Gibbs',
                 control = list(seed = 1975))
lda_model

glimpse(lda_model)

term_topic <- tidy(lda_model, matrix = 'beta')

term_topic %>% 
  filter(term == '작품상')
term_topic

term_topic %>% filter(topic == 6) %>% 
  arrange(desc(beta))

terms(lda_model, 20) %>% 
  data.frame()

top_term_topic <- term_topic %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10)

top_term_topic %>% head()

ggplot(data = top_term_topic, aes(x = reorder_within(term, beta, topic), y = beta, 
                                  fill = as.factor(topic)))+
  geom_col(color = 'black', size = 0.3, alpha = 0.8, show.legend = F)+
  coord_flip()+
  scale_x_reordered()+
  facet_wrap(~topic, scale = 'free')+
  scale_y_continuous(n.breaks = 4, labels = number_format(accuracy = .01))+
  labs(x = NULL)+
  theme(text = element_text(size = 13, face = 'bold',
                            family = 'nanumgothic'))
  

# Classification on documents
doc_topic <- tidy(lda_model, matrix = 'gamma')
doc_topic %>% head()
doc_topic %>% count(topic)
doc_topic %>% 
  filter(document == 1) %>% 
  summarise(sum_gamma = sum(gamma))

doc_class <- doc_topic %>% 
  group_by(document) %>% 
  slice_max(gamma, n = 1)

doc_class$document <- as.integer(doc_class$document)

news_comment_topic <- raw_news_comment %>% 
  left_join(doc_class, by = c('id' = 'document'))
news_comment_topic %>% select(id, topic, gamma)
news_comment_topic %>% 
  count(topic)

news_comment_topic <- news_comment_topic %>% na.omit()

top_terms <- term_topic %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 6, with_ties = F) %>% 
  summarise(term = paste(term, collapse = ", "))
top_terms

count_topic <- news_comment_topic %>% 
  count(topic)
count_topic %>% head()

count_topic_word <- count_topic %>% 
  left_join(top_terms, by = 'topic') %>% 
  mutate(topic_name = paste('Topic', topic))
count_topic_word

ggplot(data = count_topic_word, aes(x = reorder_within(topic_name, n, topic_name), 
                                    y = n, fill = topic_name))+
  geom_col(color = 'black', size = 0.3, alpha = 0.8, show.legend = F)+
  scale_x_reordered()+
  coord_flip()+
  labs(x = NULL)+
  theme(text = element_text(size = 13, family = 'nanumgothic'))+
  geom_text(aes(label = term), hjust = 1.0, size = 7,
            color = '#FFFFFF', fontface = 'bold.italic')+
  geom_text(aes(label = n), size = 7, hjust = -0.3, fontface = 'bold')

comment_topic <- news_comment_topic %>% 
  mutate(reply = str_squish(replace_html(reply))) %>% 
  arrange(-gamma)

comment_topic %>% 
  select(gamma, reply)

comment_topic %>% 
  filter(topic == 1 & str_detect(reply, '작품')) %>% 
  head(50) %>% 
  pull(reply)

comment_topic %>% 
  filter(topic == 1 & str_detect(reply, '진심')) %>% 
  head(50) %>% 
  pull(reply)

name_topic <- tibble(topic = 1:8,
                     name = c('1. 새 역사 쓴 세계적 영화',
                              '2. 수상 축하, 시상식 감상',
                              '3. 문화계 블랙리스트, 보수 정당 비판',
                              '4. 자랑스럽고 감사한 마음',
                              '5, 한국의 세계적 위상',
                              '6. 작품상 축하, 정치석 댓글 비판',
                              '7. 조국 가족, 정치적 해석',
                              '8. 놀라운 4관왕 수상'))

top_term_topic_name <- top_term_topic %>% 
  left_join(name_topic, by = 'topic')

top_term_topic_name

ggplot(data = top_term_topic_name, aes(x = reorder_within(term, beta, name),
                                       y = beta, fill = factor(topic)))+
  geom_col(color = 'black', size = 0.3, alpha = 0.8, show.legend = F)+
  coord_flip()+
  scale_x_reordered()+
  labs(title = '영화 기생충 아카데미상 수상 기사 댓글 토픽',
       subtitle = '토픽별 주요 단어 Top 10', x = NULL, y = NULL)+
  facet_wrap(~name, scale = 'free', ncol = 2)+
  theme(text = element_text(family = 'nanumgothic'),
        title = element_text(size = 15, face = 'bold.italic'),
        strip.text = element_text(size = 12, face = 'bold'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


