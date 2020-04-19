history <- system.file("extdata", "sample.txt", package = "rwhatsapp")
history <- system.file("extdata", "ldmechat.txt", package = "rwhatsapp")
history <- system.file("extdata", "alwaysworthy.txt", package = "rwhatsapp")
history

library("rwhatsapp")

chat <- rwa_read(history, tz = "Canada/Eastern", format = "yyyy-MM-dd, hh:mm a")
chat

chat <- chat[rowSums(is.na(chat)) == 0,, drop = FALSE]

chat <- chat[!grepl("This message was deleted", chat$text),]
chat <- chat[!grepl("You deleted this message", chat$text),]
chat <- chat[!grepl("Changed the subject", chat$text),]
chat <- chat[!grepl("Changed the subject", chat$author),]
chat <- chat[!grepl(":", chat$text),]


#---------------- Messages Per Day

library("dplyr")
library("tidyr")
library("ggplot2"); theme_set(theme_minimal())
library("lubridate")
chat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  ggtitle("Messages per day")

#------------------ Number of Messages

chat %>%
  mutate(day = date(time)) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Number of messages")


#---------------------Most often used emojis

library("tidyr")
chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y")  +
  ggtitle("Most often used emojis")

#------------------Most often used emojis - ggimage
library("ggimage")
emoji_data <- rwhatsapp::emojis %>% # data built into package
  mutate(hex_runes1 = gsub("\\s[[:alnum:]]+", "", hex_runes)) %>% # ignore combined emojis
  mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", 
                            tolower(hex_runes1), ".png"))

chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  left_join(emoji_data, by = "emoji") %>% 
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  geom_image(aes(y = n + 20, image = emoji_url)) +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("c") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

library("stopwords")
to_remove <- c(stopwords(language = "de"),
               "media",
               "omitted",
               "ref",
               "dass",
               "schon",
               "mal",
               "android.s.wt")

library("tidytext")
chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Most often used words")


library("stopwords")
to_remove <- c(stopwords(language = "de"),
               "media",
               "omitted",
               "ref",
               "dass",
               "schon",
               "mal",
               "android.s.wt")

#--------------------Most often used words
library("tidytext")
chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Most often used words")


#-----------------------Important words using tf-idf by author
chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  select(word, author) %>%
  filter(!word %in% to_remove) %>%
  mutate(word = gsub(".com", "", word)) %>%
  mutate(word = gsub("^gag", "9gag", word)) %>%
  count(author, word, sort = TRUE) %>%
  bind_tf_idf(term = word, document = author, n = n) %>%
  filter(n > 10) %>%
  group_by(author) %>%
  top_n(n = 6, tf_idf) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +    
  ggtitle("Important words using tf-idf by author")

#-----------------------------"Lexical Diversity
chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(x = reorder(author, lex_diversity),
             y = lex_diversity,
             fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("unique words") +
  xlab("") +
  ggtitle("Lexical Diversity") +
  coord_flip()

# --------- Unique Word by person


o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "Akshay Shah") %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == "Akshay Shah") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of Akshay Shah")




o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "Sanket") %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == "Sanket") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of Sanket")





o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "+91 97377 29007") %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == "+91 97377 29007") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of +91 97377 29007")



#-----------------------------------------------------------------------
o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "Akshay Shah") %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == "Akshay Shah") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of Akshay Shah")

o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "LD Aditya") %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == "LD Aditya") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of LD Aditya")

o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "LD Rohan") %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == "LD Rohan") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of LD Rohan")

o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "LD Deep") %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == "LD Deep") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of LD Deep")

o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "LD Daksh") %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == "LD Daksh") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of LD Daksh")


o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "+91 99258 11344") %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == "+91 99258 11344") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of +91 99258 11344")



#--------------------------------------------------------------
o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "Akshay Shah") %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == "Akshay Shah") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of Akshay Shah")




o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "Ravi") %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == "Ravi") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of Ravi")





o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "Smit") %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == "Smit") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of Smit")



o_words <- chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author != "Bhargav") %>% 
  count(word, sort = TRUE) 

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(author == "Bhargav") %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% o_words$word) %>% # only select words nobody else uses
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(show.legend = FALSE) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Unique words of Bhargav")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

library("tm")
#library("shiny")
text <- readLines("D:/Program Files/R/R-3.6.3/library/rwhatsapp/extdata/sample.txt")
docs <- Corpus(VectorSource(text))
length(docs)

library("ggplot2")
dtm <- TermDocumentMatrix(docs, control = list(wordLengths=c(2, Inf)))
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
ggplot(d[1:30,])+
  geom_col(aes(reorder(d$word[1:30],freq), d$freq[1:30]), fill="darkblue")+
  xlab("Words")+
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+coord_flip()+theme(axis.text.x = element_text(angle = 0))


library("wordcloud")
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.2, 
          colors=brewer.pal(8, "Dark2"))
#fetch sentiment words from texts
Sentiment <- get_nrc_sentiment(text)
head(Sentiment)
text <- cbind(text,Sentiment)


get_nrc
#count the sentiment words by category
TotalSentiment <- data.frame(colSums(text[,c(2:11)]))
names(TotalSentiment) <- "count"
TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
rownames(TotalSentiment) <- NULL


#total sentiment score of all texts
ggplot(data = TotalSentiment, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score")



#-----------------------------------------------------
#------------------------------new

chat$Day <- format(as.Date(chat$time),"%d")
chat$Month <- format(as.Date(chat$time),"%m")
chat$Hour <- format(as.POSIXct(chat$time), "%H")
chat$weekday<-weekdays(as.POSIXct(chat$time), abbreviate = T)


#-------------Chats per hour
chat%>%
  ggplot(aes(x=Hour)) +
  geom_histogram(stat = "count", fill="gold") +
  labs(title = "Chats Per Hour", x= "Time") +
  theme_classic()

#----------------- animated
#install packages
library(gapminder)
library(gganimate)
chat$Hour<-as.integer(chat$Hour)
s_plot<-chat%>%
  ggplot(aes(x=Hour)) +
  geom_histogram(stat = "count", fill="gold") +
  labs(title = "Chats Per Hour", x= "Time") +
  theme_classic()

s_plot + transition_time(chat$Hour) +
  labs(title = "Hour: {frame_time}") +
  shadow_wake(wake_length = 0.1, alpha = FALSE)


#-------------------- sentiment analysis
#if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
#pacman::p_load(plyr, readr,stringr, syuzhet, RColorBrewer, wordcloud, NLP, tm, SnowballC, RWeka, knitr, tidytext, RSentiment, DT, sqldf, tidyverse, gifski, png)
#theme_set(theme_bw())


library("syuzhet")
library("tm")
library("RSentiment")
library("RColorBrewer")
library("wordcloud")
my_text<-chat$text
set.seed(100)
sample <- sample(my_text, (length(my_text)))
corpus <- Corpus(VectorSource(list(sample)))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)
dt_matrix <- DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))
final_words <- colSums(as.matrix(dt_matrix))
cal_sentiments <- calculate_sentiment(names(final_words))
cal_sentiments <- cbind(cal_sentiments, as.data.frame(final_words))

count<-table(cal_sentiments$sentiment)
barplot(count, main="Sentiment distribution",
        xlab="Number of sentiments", col = c("red", "blue", "green"))

#--------------------------positive- negative- neutral



pos_sent<-cal_sentiments[cal_sentiments$sentiment == 'Positive',]
neg_sent<-cal_sentiments[cal_sentiments$sentiment == 'Negative',]
neut_sent<-cal_sentiments[cal_sentiments$sentiment == 'Neutral',]



#Possitive 
DT::datatable(pos_sent)

#possitive word bag
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
set.seed(100)
wordcloud(pos_sent$text,final_words,min.final_words=10,colors=brewer.pal(6,"Dark2"))

#Negative
DT::datatable(neg_sent)

#Negative Word bag
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
set.seed(100)
wordcloud(neg_sent$text,final_words, min.final_words=10,colors=brewer.pal(6,"Dark2"))

#Let us compare the Proportions of positive, negative and neutral sentiments Total sentiments
total_sent<-length(cal_sentiments$sentiment)

#positive 
pos_count<-sqldf("select count(sentiment) from cal_sentiments where sentiment='Positive'")
print(pos_count)

pos_prop<-pos_count/total_sent * 100
print(paste("The proportion of positive sentiments is ", round(pos_prop, digits = 1), "%"))

#Negative
neg_count<-sqldf("select count(sentiment) from cal_sentiments where sentiment='Negative'")
print(neg_count)

neg_prop<-neg_count/total_sent * 100
print(paste("The proportion of Negative sentiments is ", round(neg_prop, digits = 1), "%"))

#Neutral
neut_count<-sqldf("select count(sentiment) from cal_sentiments where sentiment='Neutral'")
print(neut_count)

neut_prop<-neut_count/total_sent * 100
print(paste("The proportion of Neutral sentiments is ", round(neut_prop, digits = 1), "%"))


#---------------------------------------------------------------------------------------
res <- prop.test(x = c(309, 267), n = c(4929, 4929))
res


new_text<-gsub("http[^[:blank:]]+","",my_text)
new_text<-gsub("@\\w+","",new_text)
new_text<-gsub("[[:punct:]]"," ",new_text)
new_text<-gsub("[^[:alnum:]]"," ",new_text)

new_sentiment<-get_nrc_sentiment((new_text))

new_sentiment.positive =sum(new_sentiment$positive)
new_sentiment.anger =sum(new_sentiment$anger)
new_sentiment.anticipation =sum(new_sentiment$anticipation)
new_sentiment.disgust =sum(new_sentiment$disgust)
new_sentiment.fear =sum(new_sentiment$fear)
new_sentiment.joy =sum(new_sentiment$joy)
new_sentiment.sadness =sum(new_sentiment$sadness)
new_sentiment.surprise =sum(new_sentiment$surprise)
new_sentiment.trust =sum(new_sentiment$trust)
new_sentiment.negative =sum(new_sentiment$negative)


yAxis <- c(new_sentiment.positive,
           + new_sentiment.anger,
           + new_sentiment.anticipation,
           + new_sentiment.disgust,
           + new_sentiment.fear,
           + new_sentiment.joy,
           + new_sentiment.sadness,
           + new_sentiment.surprise,
           + new_sentiment.trust,
           + new_sentiment.negative)
xAxis <- c("Positive","Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust","Negative")
colors <- c("green","red","blue","orange","red","green","orange","blue","green","red")
yRange <- range(0,yAxis)+10
barplot(yAxis, names.arg = xAxis,
        xlab = "Emotional valence", ylab = "Score", main = "
        Data Science Class Emotional Valence", sub = "Dec 2019", col = colors, border = "black", ylim = yRange, xpd = F, axisnames = T, cex.axis = 0.8, cex.sub = 0.8, col.sub = "blue")


colSums(new_sentiment)

