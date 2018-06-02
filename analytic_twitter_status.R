# Package
install.packages(twitterR)
install.packages("slam")
install.packages("pacman")
pacman::p_load(tm)
install.packages("syuzhet")
install.packages("ggplot2")
library(twitteR)
library(tm)
library(syuzhet)
library(ggplot2)

# Use API twitter
consumer_key <- '7Cr7X5JILogMJGpiwzSC83WSu'
consumer_secret <-
  'GaKjZ04E6GMu6FBZrbAoBTm4kGNrPoRGXPBMlTw6wCIJINuaes'
token <- '998986642114269184-azIp4kprJQFf6iOGSXPXkjQTYgT7J8k'
token_secret <- 'LmTZxTooyTr7MlleIVy0wumO0AyQsAhCsHuNWokWQvJQu'

setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    token,
                    token_secret)
# Get user friend and follow
users <-
  c(
    'BarackObama',
    'billgate',
    'Cristiano',
    'EyeOfJackieChan',
    'GarethBale11',
    'GATDIGIOTNUOCM1',
    'SonTungMTP_SKY',
    'tim_cook'
  )

for(i in 1:8){
  start <- getUser(users[i])
  friend <- start$getFriends(n = 10000)
  a <- twListToDF(friend)
  follow <- start$getFollowers(n = 10000)
  b <- twListToDF(follow)
  name <- user[i]
  friend_name <- paste(name, "-friend.csv",sep="")
  follow_name <- paste(name, "-follow.csv",sep="")
  write.csv(a, friend_name)
  write.csv(b, follow_name)
}

#Get status user

BarackObama <- userTimeline('BarackObama', n = 3200)
bo.df <- twListToDF(BarackObama)
write.csv(bo.df, "BarackObama-stt.csv")
billgate <- userTimeline('billgate', n = 3200)
bg.df <- twListToDF(billgate)
write.csv(bg.df, "billgate-stt.csv")
Cristiano <- userTimeline('Cristiano', n = 3200)
c.df <- twListToDF(Cristiano)
write.csv(c.df, "Cristiano-stt.csv")
EyeOfJackieChan <- userTimeline('EyeOfJackieChan', n = 3200)
jc.df <- twListToDF(EyeOfJackieChan)
write.csv(jc.df, "EyeOfJackieChan-stt.csv")
GarethBale11 <- userTimeline('GarethBale11', n = 3200)
gb.df <- twListToDF(GarethBale11)
write.csv(gb.df, "GarethBale11-stt.csv")
GATDIGIOTNUOCM1 <- userTimeline('GATDIGIOTNUOCM1', n = 3200)
pt.df <- twListToDF(GATDIGIOTNUOCM1)
write.csv(pt.df, "GATDIGIOTNUOCM1-stt.csv")
SonTungMTP_SKY <- userTimeline('SonTungMTP_SKY', n = 3200)
st.df <- twListToDF(SonTungMTP_SKY)
write.csv(st.df, "SonTungMTP_SKY-stt.csv")
tim_cook <- userTimeline('tim_cook', n = 3200)
tc.df <- twListToDF(tim_cook)
write.csv(tc.df, "tim_cook-stt.csv")


list <-
  list(BarackObama,
       billgate,
       Cristiano,
       EyeOfJackieChan,
       GarethBale11,
       GATDIGIOTNUOCM1,
       SonTungMTP_SKY,
       tim_cook
       )
namecsv <- c('BarackObama',
             'billgate',
             'Cristiano',
             'EyeOfJackieChan',
             'GarethBale11',
             'GATDIGIOTNUOCM1',
             'SonTungMTP_SKY',
             'tim_cook'
)

#Remove

for(i in 1:8){
  some_txt <- sapply(list[[i]], function(x)
    x$getText())
  some_txt1 <- gsub("(RT|via)((?:\\b\\W=@\\w+)+)", "", some_txt)
  some_txt2 <- gsub("http[^[:blank:]]+", "", some_txt1)
  some_txt3 <- gsub("@\\w+", "", some_txt2)
  some_txt4 <- gsub("[[:punct:]]", " ", some_txt3)
  some_txt5 <- gsub("[^[:alnum:]]", " ", some_txt4)
  name <- namecsv[i]
  se <- paste(name, "_text.csv",sep="")
  write.csv(some_txt5, se)
}
# Read file
data <-
  read.csv(
    file.choose(),
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE
  )
x <- as.character(data$x)

# Use sentiment library
mysentiment <- get_nrc_sentiment(x)
SentimentScores <- data.frame(colSums(mysentiment[, ]))
names(SentimentScores) <- "Score"
SentimentScores <-
  cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) + geom_bar(aes(fill = sentiment), stat = "identity") + theme(legend.position = "none") + xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")
