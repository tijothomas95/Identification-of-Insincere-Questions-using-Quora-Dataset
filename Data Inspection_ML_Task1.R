
#All the required libraries imported

library(wordcloud)
library(ggraph)
library(igraph)
library(Rmisc)
library(scales)
library(tidytext)
library(text2vec)
library(stopwords)
library(Matrix)
library(tokenizers)
library(knitr)
library(keras)
library(tensorflow)
library(magrittr)
library(tidyverse)
library(tm)
library(SnowballC)
library(sqldf)
library(tidyverse) 
library(stringr)
library(SnowballC)
library(koRpus)
library(quanteda)


#Reading all the filkes required
train_data <- read_csv("G:/DIT WebCourse/S2/Machine learning/assignment/assignmnt1/datasets/train.csv")
test_data <- read_csv("G:/DIT WebCourse/S2/Machine learning/assignment/assignmnt1/datasets/test.csv")


#Joining test and train datasets
train_test <- train_data %>% 
  bind_rows(test_data) %>% 
  mutate(group = ifelse(is.na(target), "Test", "Train") %>% factor)


#Tokenizing sentences into words
tokens <- train_test %>%
  mutate(question_text = str_replace_all(question_text, "[^[:alpha:][:space:]]+", "")) %>%  
  unnest_tokens(word, question_text)

#Finding top 10 words
tokens %>% 
  count(word, sort = TRUE) %>%
  top_n(10, n) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(., aes(x=word, y=n))+geom_bar(stat='identity')+coord_flip()


#Removing stop words from the dataset
tokens %<>% 
  anti_join(stop_words, by = "word")

#Splitting the tokenized dataset based on whethr sincere or insincere
tokens_Train_0<-subset(tokens, target==0 & group=='Train')
tokens_Train_1<-subset(tokens, target==1 & group=='Train')

#Top 10 words in sincere questions after stopping words removed
tokens_Train_0 %>% 
  count(word, sort = TRUE) %>%
  top_n(10, n) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(., aes(x=word, y=n, label = n))+geom_bar(stat='identity')+coord_flip()

#Word cloud of top 200 words in sincere questions after stopping words removed
w_cloud_train_0<-sqldf("select word, count(word) as Cnt from tokens_Train_0 group by word")
wordcloud(words = w_cloud_train_0$word, freq = w_cloud_train_0$Cnt, min.freq = 1,
          max.words=500, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Top 10 words in insincere questions after stopping words removed
tokens_Train_1 %>% 
  count(word, sort = TRUE) %>%
  top_n(10, n) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(., aes(x=word, y=n, label = n))+geom_bar(stat='identity')+coord_flip()

#Word cloud of top 200 words in insincere questions after stopping words removed
w_cloud_train_1<-sqldf("select word, count(word) as Cnt from tokens_Train_1 group by word")
wordcloud(words = w_cloud_train_1$word, freq = w_cloud_train_1$Cnt, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#AFINN, BING AND NRC LEXICONS USED
tokens_sent <- inner_join(tokens, get_sentiments("bing"))
bing<-sqldf("select target, sentiment, count(sentiment) as Cnt from tokens_sent group by target, sentiment")
tokens_sent_1 <- inner_join(tokens_sent, get_sentiments("afinn"))
tokens_sent_2 <- left_join(tokens_sent_1, get_sentiments("nrc"))
afinn_score<-sqldf("select target, score, count(score) as Cnt from tokens_sent_2 group by target, score")


tokens_sent_0_pos<-subset(tokens_sent_2, target==0 & group=='Train' & sentiment=='positive')
tokens_sent_0_neg<-subset(tokens_sent_2, target==0 & group=='Train' & sentiment=='negative')

#Top 20 sincere question words with positive sentiment
tokens_sent_0_pos %>% 
  count(word, sort = TRUE) %>%
  top_n(20, n) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(., aes(x=word, y=n, label = n))+geom_bar(stat='identity')+coord_flip()

#Top 20 sincere question words with negative sentiment
tokens_sent_0_neg %>% 
  count(word, sort = TRUE) %>%
  top_n(20, n) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(., aes(x=word, y=n, label = n))+geom_bar(stat='identity')+coord_flip()

tokens_sent_1_pos<-subset(tokens_sent_2, target==1 & group=='Train' & sentiment=='positive')
tokens_sent_1_neg<-subset(tokens_sent_2, target==1 & group=='Train' & sentiment=='negative')

#Top 20 insincere question words with positive sentiment
tokens_sent_1_pos %>% 
  count(word, sort = TRUE) %>%
  top_n(10, n) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(., aes(x=word, y=n, label = n))+geom_bar(stat='identity')+coord_flip()

#Top 20 insincere question words with negative sentiment
tokens_sent_1_neg %>% 
  count(word, sort = TRUE) %>%
  top_n(20, n) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot(., aes(x=word, y=n, label = n))+geom_bar(stat='identity')+coord_flip()

#Word cloud of sincere question words with positive sentiment
tokens_sent_0_pos_1<-sqldf("select word, count(word) as Cnt from tokens_sent_0_pos group by word")
wordcloud(words = tokens_sent_0_pos_1$word, freq = tokens_sent_0_pos_1$Cnt, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Word cloud of sincere question words with negative sentiment
tokens_sent_0_neg_1<-sqldf("select word, count(word) as Cnt from tokens_sent_0_neg group by word")
wordcloud(words = tokens_sent_0_neg_1$word, freq = tokens_sent_0_neg_1$Cnt, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Word cloud of insincere question words with positive sentiment
tokens_sent_1_pos_1<-sqldf("select word, count(word) as Cnt from tokens_sent_1_pos group by word")
wordcloud(words = tokens_sent_1_pos_1$word, freq = tokens_sent_1_pos_1$Cnt, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Word cloud of insincere question words with negative sentiment
tokens_sent_1_neg_1<-sqldf("select word, count(word) as Cnt from tokens_sent_1_neg group by word")
wordcloud(words = tokens_sent_1_neg_1$word, freq = tokens_sent_1_neg_1$Cnt, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

sentiments %>% 
  sample_n(10) %>% 
  kable()

sentiments %>% 
  filter(lexicon == "AFINN") %>% 
  sample_n(10) %>% 
  kable()

tokens_sent <- inner_join(tokens, get_sentiments("afinn")) 

#Frequency of AFINN scores in both test and train
tokens_sent %>%
  group_by(qid, target, group) %>% 
  summarise(score = sum(score)) %>% 
  ungroup() %>% 
  ggplot(aes(score, fill = group)) +
  geom_bar(show.legend = FALSE) +
  labs(x = "") + 
  facet_wrap(~ group, ncol = 2, scales = "free") +
  theme_minimal()

tokens_sentperctg<-sqldf("select target,score as Afinn_Score,count(score) as Frequency from tokens_sent where target not NULL group by score,target order by target asc")

                                    #Bigram Analysis
#Now bigrams that is two word pairs

bigrams <- train_test %>%
  unnest_tokens(bigram, question_text, token = "ngrams", n = 2)

#Top 20 bigrams
bigrams %>% 
  count(bigram, sort = TRUE) %>%
  top_n(20, n)%>%
  mutate(word = fct_reorder(bigram, n)) %>%
  ggplot(., aes(x=bigram, y=n, label = n))+geom_bar(stat='identity')+coord_flip()

#Removing stop words
bigrams %<>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !str_detect(word1, "[[:digit:]]"),
         !str_detect(word2, "[[:digit:]]")) %>% 
  unite(bigram, word1, word2, sep = " ")

#Splitting bigrams into sincere and insincere
bigrams_1<-subset(bigrams, target==1 & group=='Train' )
bigrams_0<-subset(bigrams, target==0 & group=='Train' )

#top 20 insincere bigrams
bigrams_1 %>% 
  count(bigram, sort = TRUE) %>%
  top_n(20, n)%>%
  mutate(word = fct_reorder(bigram, n)) %>%
  ggplot(., aes(x=bigram, y=n, label = n))+geom_bar(stat='identity')+coord_flip()

#WordCloud
bigrams_1_1<-sqldf("select bigram, count(bigram) as Cnt from bigrams_1 group by bigram")
wordcloud(words = bigrams_1_1$bigram, freq = bigrams_1_1$Cnt, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#top 20 sincere bigrams
bigrams_0 %>% 
  count(bigram, sort = TRUE) %>%
  top_n(20, n)%>%
  mutate(word = fct_reorder(bigram, n)) %>%
  ggplot(., aes(x=bigram, y=n, label = n))+geom_bar(stat='identity')+coord_flip()

#WordCloud
bigrams_0_1<-sqldf("select bigram, count(bigram) as Cnt from bigrams_0 group by bigram")
wordcloud(words = bigrams_0_1$bigram, freq = bigrams_0_1$Cnt, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#The cleaning, processing and application of classifiers are done in the python notebook attached in the same folder