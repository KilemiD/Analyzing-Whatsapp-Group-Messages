setwd("C:/Users/Made by Host/Desktop/machine learning class")
setwd("C:/Users/Made by Host/OneDrive/sentiment analysis")
setwd("C:/Users/Made by Host/OneDrive/whatsapp study")
setwd("C:/Users/Made by Host/OneDrive/travel list")
setwd("C:/Users/John/OneDrive/whatsapp study")

library(rwhatsapp)
library(dplyr)
library(ggplot2)
library(lubridate)


chat=rwa_read("PUNFLEX.txt")%>%
  filter(!is.na(author))
View(chat)

#how many people are in the group
author=unique(chat$author)
View(author)

#chats per day
chats_par_day=chat%>%
  mutate(day=date(time))%>%
  count(day)%>%
  ggplot(aes(x=day,y=n))+
  geom_bar(stat="identity",fill = "#00FFFF")

#View(chats_par_day)
#chats by date
chat%>%
  mutate(day=date(time))%>%
  count(day)%>%
  ggplot(aes(x=day,y=n))+
  geom_bar(stat="identity",fill = "#00FFFF")+
  ylab("") + xlab("")+
  ggtitle("Messages per day")+
  theme(plot.title = element_text(hjust = 0.5))


#chats by person
chat%>%
  mutate(day=date(time))%>%
  count(author)%>%
  top_n(n=30)%>%
  ggplot(aes(x=reorder(author,n),y=n))+
  geom_bar(stat="identity",fill="#4169E1")+
  ylab("")+xlab("")+
  coord_flip()+
  ggtitle("Number of Messages per Person")+
  theme(plot.title = element_text(hjust=0.5))


#chat by weekday
#first filter chats
miChat <- chat %>% 
  mutate(day = date(time)) %>% 
  filter(!is.na(author))

chat_weekday=miChat %>% 
  mutate( wday.num = wday(day),
          wday.name = weekdays(day)) %>% 
  group_by(wday.num, wday.name) %>% 
  count()

#here is a plot  
chat_weekday%>%
  ggplot(aes(x=wday.name,y=n))+
  geom_bar(stat="identity",fill="#4169E1")+
  ylab("")+xlab("")+
  coord_flip()+
  ggtitle("Number of Messages Per Day")+
  theme(plot.title = element_text(hjust=0.5))


#filtering highest chatters
top_chats=chat%>%
  mutate(day=date(time))%>%
  count(author,sort=T)%>%
  top_n(n=30)
#plotting the top chats
top_chats%>%
  ggplot(aes(x=reorder(author,n),y=n))+
  geom_bar(stat="identity",fill="#4169E1")+
  ylab("")+xlab("")+
  coord_flip()+
  ggtitle("Top Group Contributors")+
  theme(plot.title = element_text(hjust=0.5))


#favorite emojis
library(tidyverse)
library(tidyr)
library(tidytext)

#this emoji one works
# LIBRARY FOR EMOJI PNG IMAGE FETCH FROM https://abs.twimg.com
library(ggimage)# EMOJI RANKING
plotEmojis <- chat %>% 
  unnest(emoji, emoji_name) %>% 
  mutate( emoji = str_sub(emoji, end = 1)) %>% 
  mutate( emoji_name = str_remove(emoji_name, ":.*")) %>% 
  count(emoji, emoji_name) %>% 
  
  # PLOT TOP 30 EMOJIS
  top_n(20, n) %>% 
  arrange(desc(n)) %>% # C
  mutate( emoji_url = map_chr(emoji, 
                              ~paste0("https://abs.twimg.com/emoji/v2/72x72/", as.hexmode(utf8ToInt(.x)),".png")) )
#View(plotEmojis)
# PLOT OF THE RANKING OF MOST USED EMOJIS
plotEmojis %>% 
  ggplot(aes(x=reorder(emoji_name, n), y=n)) +
  geom_col(aes(fill=n), show.legend = FALSE, width = .2) +
  geom_point(aes(color=n), show.legend = FALSE, size = 3) +
  geom_image(aes(image=emoji_url), size=.030) +
  scale_fill_gradient(low="#2b83ba",high="#d7191c") +
                      scale_color_gradient(low="#2b83ba",high="#d7191c") +
                                           ylab("Number of Times Emoji has been used") +
                                             xlab("Emoji Name") +
                                             ggtitle("Emojis Used") +
                                             coord_flip() +
                                             theme_minimal() +
                                             theme()

#3mojis by user
# EMOJI RANK PER USER
plotEmojis_author <- chat %>%
  unnest(emoji, emoji_name) %>%
  mutate( emoji = str_sub(emoji, end = 1)) %>% # 
  count(author, emoji, emoji_name, sort = TRUE) %>%
  # PLOT TOP 8 EMOJIS PER USER
  group_by(author) %>%
  top_n(n = 8, n) %>%
  slice(1:8) %>% 
  
  # CREATE AN IMAGE URL WITH THE EMOJI UNICODE
  mutate( emoji_url = map_chr(emoji, 
                              ~paste0("https://abs.twimg.com/emoji/v2/72x72/",as.hexmode(utf8ToInt(.x)),".png")) )

#View(plotEmojis)
emoji_4=subset(plotEmojis_author,
               author=="+254 707 500366" | author=="Koros Anaklet"|
                 author=="Miss Moraa"|
                 author=="+254 724 614109"|author=="+254 769 266385"|
                 author=="+254 711 237523")  
#View(emoji_4)
# PLOT DATA
emoji_4 %>% 
  ggplot(aes(x = reorder(emoji, -n), y = n)) +
  geom_col(aes(fill = author, group=author), show.legend = FALSE, width = .20) +
  # USE TO FETCH AN EMOJI PNG IMAGE https://abs.twimg.com
  geom_image(aes(image=emoji_url), size=.08) +
  ylab("Emojis by User") +
  xlab("Emoji") +
  facet_wrap(~author, ncol = 3, scales = "free") +
  ggtitle('Emojis By User') +
  theme_minimal() +
  theme(axis.text.x = element_blank())


#total words used in chat
library(tidytext)
total_word <- chat %>%
  select(text) %>%
  unnest_tokens(word, text)

View(total_word)
dim(total_word)

View(chat)

#unique words of authors (Miss Moraa as an example)
o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "Koros Anaklet") %>% 
  count(word, sort = TRUE) 


chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == "Koros Anaklet") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n =3, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE,fill="#4B0082") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of Miss Moraa")+
  theme(plot.title = element_text(hjust=0.5))

#most commonly used words after stopwords
library("stopwords")
other=c("omitted",'media','na','ni')
to_remove <- c(stopwords(language = "en"),other)
#
most_words=chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 25, n)
View(most_words)
#gettin words greater than 10
most_words_10=most_words%>%
  filter(n>=8)
View(most_words_10)
#now subsetting to desired authors
most_words_authors=subset(most_words_10,
                          author=="+254 707 500366" | author=="Koros Anaklet"|
                            author=="Dan"|
                            author=="+254 707 419042")  

View(most_words)
most_words_10%>%
  ggplot(aes(x = word, y = n, fill =  n)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip()




#now plotting
most_words_authors%>%
ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Most often used words")+
  theme(plot.title = element_text(hjust=0.5))


#now plotting
author_idf_authors%>%
ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Important words by author")+
  theme(plot.title = element_text(hjust=0.5))

#lexical diversity
author_lex=chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity))
View(author_lex)
#filtering the usual authors
author_lex_authors=subset(author_lex,
                          author=="Former Leader" | author=="Dan"|
                            author=="Victor Mandela"|
                            author=="Baker Abu"|author=="Erick Biostats"|
                            author=="Man Of God")
#now plotting
author_lex%>%
ggplot(aes(x = reorder(author, lex_diversity),
           y = lex_diversity,
           fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("unique words") +
  xlab("") +
  ggtitle("Lexical Diversity") +
  coord_flip()+
  theme(plot.title = element_text(hjust=0.5))

#now doing sentiment analysis
chat_clean <- chat %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

chat_clean <- chat_clean %>%
  na.omit(chat_clean)

chat_clean <- chat_clean %>%
  filter(!word %in% to_remove)
View(chat_clean)
library(sentiment)
library(textdata)
#using afinn
bullring_sentiment_afinn <- chat_clean %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(author) %>% 
  mutate(method = "Group Chat Sentiment")

View(bullring_sentiment_afinn)
bullring_sentiment_afinn%>%
  ggplot(aes(author, value, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 3, scales = "free_y")

#contribution to sentiment
bing_word_counts <- chat_clean %>%
  inner_join(get_sentiments("bing")) %>%
  ungroup()


#View(bing_word_counts)


