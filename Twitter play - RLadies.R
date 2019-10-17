# Taryn Morris
# R-Ladies
# 17 October 2019


# HOW TO GET BASIC~ TWITTER DATA

#Helper article: https://cfss.uchicago.edu/notes/twitter-api-practice/


#install.packages("rtweet")
library(rtweet)
library(tidyverse)
library(readxl)
library(lubridate)
library(tidytext)






#SEARCH TWEETS BY TOPIC or HASHTAG

#This returns a data frame. One of the column names is is_retweet, which makes filtering for retweets easy. Or just use include_rts = FALSE in search_tweets().

tweets <- search_tweets(
  q = "#capetown",
  n = 3000,
  include_rts = FALSE
)

View(tweets)
class(tweets)


# saveRDS(tweets, "capetown.rds")


#SEARCH TWEETS BY USER
cyril_tweets <- get_timeline(user = "CyrilRamaphosa", n = 1000)
cyril_tweets

#Built in plots
ts_plot(cyril_tweets, by = "3 hours")


ts_plot(cyril_tweets, by = "3 hours") +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    x = NULL, y = NULL,
    title = "Frequency of #CapeTown Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using one-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )



#### LETS USE SOME TIDY TEXT INSTEAD


## CLEANING TWEETS


#Let's use the I normally use the tidytext package for text analysis. We can split tweet text into words, filter for words that we don't want and remove common "stop words":

tidy_tweets <- tweets %>% 
filter(is_retweet == FALSE) %>%
select(text) %>%
unnest_tokens(word, text) %>% 
select(word) %>% 
filter(!word %in% c("https", "t.co", "amp"),   # and whatever else to ignore
!word %in% tolower(tweets$screen_name), # remove user names
!grepl("^\\d+$", word)) %>%             # remove numbers      
anti_join(stop_words)


frequency <- tidy_tweets %>% 
  count(word, sort = TRUE) 

frequency_top10 <- frequency[1:10,]


ggplot(frequency_top10, aes(word, n, fill = word)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab("Word count") +
  ggtitle(paste("Most common words in tweets containing Cape Town")) +
  theme(legend.position="none") +
  coord_flip()



#### LETS DO SOME SENTIMENT ANALYSIS
# Emotions for each tweet using NRC dictionary

library(RCurl)
library(httr)
library(tm)
library(wordcloud)
library(syuzhet)

emotions <- get_nrc_sentiment(tweets$text)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

# Visualize the emotions from NRC sentiments {different to the tidy text way}
library(plotly)
p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion Type for hashtag: #CapeTown")



# Create comparison word cloud data {also not tidytext but thats okay}

wordcloud_tweet = c(
  paste(tweets$text[emotions$anger > 0], collapse=" "),
  paste(tweets$text[emotions$anticipation > 0], collapse=" "),
  paste(tweets$text[emotions$disgust > 0], collapse=" "),
  paste(tweets$text[emotions$fear > 0], collapse=" "),
  paste(tweets$text[emotions$joy > 0], collapse=" "),
  paste(tweets$text[emotions$sadness > 0], collapse=" "),
  paste(tweets$text[emotions$surprise > 0], collapse=" "),
  paste(tweets$text[emotions$trust > 0], collapse=" ")
)

# create corpus
corpus = Corpus(VectorSource(wordcloud_tweet))

# create document term matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)
tdmnew <- tdm[nchar(rownames(tdm)) < 11,]

# column name binding
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdmnew) <- colnames(tdm)
comparison.cloud(tdmnew, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)


wordcloud(corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)


