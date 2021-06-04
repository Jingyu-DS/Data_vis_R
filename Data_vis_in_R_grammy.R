#### Load Packages

library(rvest)
library(tidyverse)
library(genius)
library(qdap)
library(tidytext)
library(dplyr)
library(gridExtra)

data('stop_words')
data("sentiments")
additional_stop_words = read.table('AdditionalStopWords.txt')

## Get 1980 data from website and do the data cleaning 
#### Scrape and create table of records from 1980s
# read webpage for Grammy Awards
webpage <- read_html("https://en.wikipedia.org/wiki/Grammy_Award_for_Record_of_the_Year")

# copy xpath for table of 1980s
XPATH80 <- '/html/body/div[3]/div[3]/div[5]/div[1]/table[5]'

# run the following to create table of songs from 1980s
table_1980 <- 
  webpage %>%
  html_nodes(xpath = XPATH80) %>%
  html_table(fill = TRUE)

d1980 <- table_1980[[1]]
# head(d1980)

#rename the columns and get rid of the brackets in the data
ROY80s <- d1980 %>% 
  na.omit() %>%
  rename(year = `Year[I]` ,track = Record, artist = `Artist(s)`) %>%
  select(year, track, artist) %>%
  mutate(year = str_replace_all(year, "\\[..\\]", ""))

# head(ROY80s)

# get lyrics for songs 1980s
lyrics80s <- ROY80s %>%
  add_genius(artist, track, type = "lyrics")

# head(lyrics80s)

## Get 1990 data from website and do the data cleaning 
XPATH90 <- '//*[@id="mw-content-text"]/div[1]/table[6]'

# run the following to create table of songs from 1990s
table_1990 <- 
  webpage %>%
  html_nodes(xpath = XPATH90) %>%
  html_table(fill = TRUE)

d1990 <- table_1990[[1]]


# rename the columns and remove the brackets from the data 
ROY90s <- d1990 %>% 
  na.omit() %>%
  rename(year = `Year[I]` ,track = Record, artist = `Artist(s)`) %>%
  select(year, track, artist) %>%
  mutate(year = str_replace_all(year, "\\[..\\]", ""))

#get the lyrics for 1990 nominated songs
lyrics90s <- ROY90s %>%
  add_genius(artist, track, type = "lyrics")

# copy xpath for table of 2000s
XPATH00 <- '//*[@id="mw-content-text"]/div[1]/table[7]'

# run the following to create table of songs from 2000s
table_2000 <- 
  webpage %>%
  html_nodes(xpath = XPATH00) %>%
  html_table(fill = TRUE)

d2000 <- table_2000[[1]]

ROY00s <- d2000 %>% 
  na.omit() %>%
  rename(year = `Year[I]` ,track = Record, artist = `Artist(s)`) %>%
  select(year, track, artist) %>%
  mutate(year = str_replace_all(year, "\\[..\\]", ""))

lyrics00s <- ROY00s %>%
  add_genius(artist, track, type = "lyrics")

# copy xpath for table of 2010s
XPATH10 <- '//*[@id="mw-content-text"]/div[1]/table[8]'

# run the following to create table of songs from 2000s
table_2010 <- 
  webpage %>%
  html_nodes(xpath = XPATH10) %>%
  html_table(fill = TRUE)

d2010 <- table_2010[[1]]

ROY10s <- d2010 %>% 
  na.omit() %>%
  rename(year = `Year[I]` ,track = Record, artist = `Artist(s)`) %>%
  select(year, track, artist) %>%
  mutate(year = str_replace_all(year, "\\[..\\]", ""))

lyrics10s <- ROY10s %>%
  add_genius(artist, track, type = "lyrics")

## Graph 1: Boxplots of Words per Grammy Nominated Songs by Decade

count_80s <- lyrics80s %>%
  mutate(count = wc(lyric)) %>%
  group_by(track) %>%
  summarise(total = sum(count, na.rm = TRUE)) %>% #using na.rm = True
  mutate(year = '1980s')

count_90s <- lyrics90s %>%
  mutate(count = wc(lyric))%>%
  group_by(track) %>%
  summarise(total = sum(count, na.rm = TRUE))%>%
  mutate(year = '1990s')

count_00s <- lyrics00s %>%
  mutate(count = wc(lyric))%>%
  group_by(track) %>%
  summarise(total = sum(count, na.rm = TRUE))%>%
  mutate(year = '2000s')

count_10s <- lyrics10s %>%
  mutate(count = wc(lyric))%>%
  group_by(track) %>%
  summarise(total = sum(count, na.rm = TRUE))%>%
  mutate(year = '2010s')

count_table <- rbind(count_80s, count_90s, count_00s, count_10s)
ggplot(count_table) +
  aes(x = year, y = total, fill = year) +
  geom_boxplot() +
  scale_fill_hue() +
  labs(x = "Decade", y = "Words per Song", title = "Boxplots of Words per Grammy Nominated Songs by Decade") +
  theme_minimal() +
  theme(legend.position = "none")

## Graph 2: Ten Most Popular Words of Grammy Nominated Songs from 1980 - 2019
additional_stop_word <- additional_stop_words %>% rename(word = V1)

##get the complete stop words list
total_stop_word <- stop_words %>%
  select(word) %>% 
  rbind(additional_stop_word)

total_lyrics <- rbind(lyrics80s, lyrics90s, lyrics00s, lyrics10s)
total_top10 <- freq_terms(total_lyrics$lyric, 10, at.least=3,stopwords=total_stop_word$word)


total_top10%>%
  add_count(WORD, wt = FREQ) %>%
  mutate(WORD = forcats::fct_reorder(WORD, -FREQ)) %>% #in order to plot in a correct order
  ggplot() +
  aes(x = WORD, weight = FREQ) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Word", y = "Count", title = "Ten Most Popular Words of Grammy Nominated Songs from 1980 - 2019") +
  theme_minimal()

## Graph 3: Top 10 by Decade

top10_80s <- freq_terms(lyrics80s$lyric, 10, at.least=3, 
                        stopwords=total_stop_word$word)
plot80 <- 
  top10_80s %>%
  add_count(WORD, wt = FREQ) %>%
  mutate(WORD = forcats::fct_reorder(WORD, -FREQ)) %>%
  ggplot() +
  aes(x = WORD, weight = FREQ) +
  geom_bar(fill = "#482878") +
  labs(x = "Word", y = "Count", title = "1980s") +
  theme_minimal()

top10_90s <- freq_terms(lyrics90s$lyric, 10, at.least=3, 
                        stopwords=total_stop_word$word)
plot90 <-
  top10_90s%>%
  add_count(WORD, wt = FREQ) %>%
  mutate(WORD = forcats::fct_reorder(WORD, -FREQ)) %>%
  ggplot() +
  aes(x = WORD, weight = FREQ) +
  geom_bar(fill = "#bd3786") +
  labs(x = "Word", y = "Count", title = "1990s") +
  theme_minimal()

top10_00s <- freq_terms(lyrics00s$lyric, 10, at.least=3, 
                        stopwords=total_stop_word$word)
plot00 <-
  top10_00s%>%
  add_count(WORD, wt = FREQ) %>%
  mutate(WORD = forcats::fct_reorder(WORD, -FREQ)) %>%
  ggplot() +
  aes(x = WORD, weight = FREQ) +
  geom_bar(fill = "#26828e") +
  labs(x = "Word", y = "Count", title = "2000s") +
  theme_minimal()

top10_10s <- freq_terms(lyrics10s$lyric, 10, at.least=3, 
                        stopwords=total_stop_word$word)
plot10 <-
  top10_10s %>%
  add_count(WORD, wt = FREQ) %>%
  mutate(WORD = forcats::fct_reorder(WORD, -FREQ)) %>%
  ggplot() +
  aes(x = WORD, weight = FREQ) +
  geom_bar(fill = "#ed7953") +
  labs(x = "Word", y = "Count", title = "2010s") +
  theme_minimal()

grid.arrange(plot80, plot90, plot00, plot10, nrow = 2,top = "Top 10 by Decade")

## Graph 4: Net Sentiment Score by Year

verse_words <- total_lyrics %>%
  unnest_tokens(word, lyric)

ft <- verse_words %>%
  anti_join(total_stop_word)

sentiment_score <- sentiments %>%
  mutate(score = ifelse(sentiment == "positive", 1, 0)) #ifelse: used to set the score for the word

ft_sentiment <- ft %>%
  inner_join(sentiment_score)

sentiment_year <- ft_sentiment %>%
  group_by(year) %>%
  summarise(sentiment_score = sum(score)) %>%
  mutate(decade = ifelse(year >= 2010, "2010s",
                         ifelse(year >= 2000, "2000s",
                                ifelse(year >= 1990, "1990s","1980s")))) %>% #The decade >>> color the plot
  mutate(year = as.numeric(year))

ggplot(sentiment_year) +
  aes(x = year, fill = decade, colour = decade, weight = sentiment_score) +
  geom_bar() +
  scale_fill_hue() +
  scale_color_hue() +
  labs(x = "Year", y = "Net Sentiment", title = "Net Sentiment Score by Year") +
  theme_minimal()

## Graph 5: Mean Sentiment Score by Decade

sentiment_mean <- sentiment_year %>%
  group_by(decade) %>%
  summarise(meanscore = mean(sentiment_score))


ggplot(sentiment_mean) +
  aes(x = decade, fill = decade, colour = decade, weight= meanscore) +
  geom_bar() +
  labs(x = "Year", y = "Mean Sentiment Score", title = "Mean Sentiment Score by Decade") +
  theme_minimal()

## Graph 6: Net Sentiment Score by Year of Grammy Nominated Records from 1980 - 2019 with Linear Model Fit
ggplot(sentiment_year,warning=FALSE,message=FALSE) +
  aes(x = year, y = sentiment_score, colour = decade) +
  geom_smooth(method = 'loess', formula = y ~ x, na.rm = TRUE, fullrange= TRUE,
              aes(group=1),colour="blue", se = FALSE) +
  geom_point() +
  scale_color_hue() +
  labs(x = "Year", y = "Net Sentiment ", title = " Net Sentiment Score by Year of Grammy Nominated Records from 1980 - 2019 with Linear Model Fit") +
  theme_minimal()