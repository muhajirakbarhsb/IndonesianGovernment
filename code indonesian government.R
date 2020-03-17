library(tm)
library(wordcloud2)
library(twitteR)
library(rtweet)
library(qdap)
library(twitteR)
library(RCurl)
library(httr)
library(wordcloud)
library(syuzhet)
library(xlsx)
library(tidyr)
library(dplyr)

# Ganti Sesuai dengan Key Milik Kita
consumer_key <- "MqzFk9dsAI3ggptKo3LoEPiXH"
consumer_secret <- "jDWxBmncET5JSjTczjKTXoSlc6kZgrnLKx1FCZSSUgEViDlZUl"
access_token <- "108243066-TSTbOb1sZtp3PCv2pvPjgWSlgdxKJh9OKilifwEd"
access_secret <- "odMTbb9eJgPcTVf9DaubO17nOF351mVPNrcwGp5ZPolPp"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
some_tweets2 = searchTwitter('indonesian government -filter:retweets', n = 12000, lang = "en",retryOnRateLimit = 10e3)


##save dulu datanya
saveRDS(some_tweets,file = 'tweetfovfilter2.rds')
##load datanya
tw <- readRDS('tweetfovfilter2.rds')
d2 = twListToDF(tw)
write.xlsx(d2, "twittergov2.xlsx")
remove(tw)
ts_plot(d2, "1 hour") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of 'Indonesian Government' Twitter statuses from past 1 Week",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#### hapus data 

komen2 <- d2$text
textclean <- Corpus(VectorSource(komen2))

#### cleaning data
##Cleaning data
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
twitclean <- tm_map(textclean, removeURL)

removeNL <- function(y) gsub("\n", " ", y)
twitclean <- tm_map(twitclean, removeNL)

replacecomma <- function(y) gsub(",", "", y)
twitclean <- tm_map(twitclean, replacecomma)

removeRT <- function(y) gsub("RT ", "", y)
twitclean <- tm_map(twitclean, removeRT)

removetitik2 <- function(y) gsub(":", "", y)
twitclean <- tm_map(twitclean, removetitik2)

removetitikkoma <- function(y) gsub(";", " ", y)
twitclean <- tm_map(twitclean, removetitikkoma)

removetitik3 <- function(y) gsub("p.", "", y)
twitclean <- tm_map(twitclean, removetitik3)

removeamp <- function(y) gsub("&amp;", "", y)
twitclean <- tm_map(twitclean, removeamp)

removeUN <- function(z) gsub("@\\w+", "", z)
twitclean <- tm_map(twitclean, removeUN)

remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
twitclean <- tm_map(twitclean,remove.all)


#Menghapus  titik koma, menjadi non kapital
twitclean <- tm_map(twitclean, removePunctuation)
twitclean <- tm_map(twitclean, tolower)
twitclean <- tm_map(twitclean , removeWords, 
                    c('corona','virus','jokowi','joko',
                      'widodo','covid19','covid-19','coronavirus' , 'viruscorona' , 'indonesian' , 'government' , 'outbreak' , 'cant' , 'taking' , 'bcs' , 'covid', 'indian','embas','governments' , 'will' , 'care' , 'adds' , 'rsup' , 'new', 'rsal','indonesia','like','dont','jakarta','international','just' ,'the', 'and', 'this', 'for', 'this', 'that', 'has', 'about', 'with','are','not','from','its','about'))

stop = stopwords(kind = "en")
twitclean <- tm_map(twitclean , removeWords, stop)
twitclean <- tm_map(twitclean, content_transformer(tolower))
#
#Build a term-document matrix

{
  dtm <- TermDocumentMatrix(tweets.df$text)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  b <- data.frame(word = names(v),freq=v)
}
head(b,n=12)

## mencari asosiasi
asosiasi1<-as.list(findAssocs(dtm,
                              terms= c('handling', 'case'),
                              corlimit= c(0.50,0.15,0.15,0.15,0.15,0.15,0.15)))
asosiasi1


## save data
dataframe<-data.frame(text=unlist(sapply(textclean, `[`)), stringsAsFactors=F)
View(dataframe)

write.csv(dataframe,file = 'twitclean-10kv8.csv')
dataframe[110,]

d2$retweetCount

###### cleaninf untuk ncr sentiment
tweets.df = d2
tweets.df$text=gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
tweets.df$text = gsub("@\\w+", "", tweets.df$text)
tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
tweets.df$text = gsub("[[:digit:]]", "", tweets.df$text)
tweets.df$text = gsub("http\\w+", "", tweets.df$text)
tweets.df$text = gsub("[ \t]{2,}", "", tweets.df$text)
tweets.df$text = gsub("^\\s+|\\s+$", "", tweets.df$text)
tweets.df$text <- iconv(tweets.df$text, "UTF-8", "ASCII", sub="")

############# ncr sentiment

emotions <- get_nrc_sentiment(tweets.df$text)
emotions$negative = NULL
emotions$positive = NULL
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])
library(plotly)
p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion Type for 'Indonesian Government' ")
p

waktu = d2$created
tsemo = cbind(emotions, waktu)
library("tidyr")
library("dplyr")
library("wesanderson")

data <- tsemo %>%
  select(fear , trust, sadness, anticipation, anger, joy, disgust, surprise, waktu) %>% gather(key = "variable", value = "value", -waktu)
head(data, 3)

data2 <- tsemo %>%
  select(fear , trust, waktu2) %>% gather(key = "variable", value = "value", -waktu2)
head(data, 3)


waktu = d2$created
tsemo = cbind(emotions, waktu2)
time = strftime(waktu, format="%Y-%m-%d")
tsemo$waktu = NULL
tsemo = cbind(emotions, time)
head(tsemo)


trust = ag2$trust
sadness = ag3$sadness
anticipation = ag4$anticipation
joy = ag5$joy
disgust = ag6$disgust
surprise = ag7$surprise
fear = ag8$fear
tsemotion = cbind(ag1, trust, sadness, anticipation, joy, disgust, surprise, fear)
tsemotion

tsemotion$time = as.Date(tsemotion$time)
df <- tsemotion %>%
  select(time, anger, trust, sadness, anticipation, joy, disgust, surprise, fear) %>%
  gather(key = "variable", value = "value", -time)
ggplot(df, aes(x = time, y = value)) + 
  geom_line(aes(color = variable), size = 1) + theme_minimal()




