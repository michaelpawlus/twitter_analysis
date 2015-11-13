setwd("C:/Users/pawlusm/Desktop/decTree/twitter/twitter_analysis")

#connect all libraries
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(curl)
library(httr)
library(base64enc)


#connect to API
download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')
reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'
consumerKey <- 'ZAV9io0PypRDEn7NIDQxadG34' #put the Consumer Key from Twitter Application
consumerSecret <- 'jiLtxPf0iJSR13J09L0f3jEHAWfJwZwEWcygaQEZe2DtR5U8xe'  #put the Consumer Secret from Twitter Application
access_token <- '16344033-dNBbXoBROVH6bQfGqDTd6S2eqdnqdt0NuUBApV28w'
access_secret <- 'I2ZQfNjqk2F3BkEhy6JHh7G4fXXo4QJLJIhauG7lISY3k'
Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=reqURL,
                         accessURL=accessURL,
                         authURL=authURL)
Cred$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')) #There is URL in Console. You need to go to it, get code and enter it on Console
save(Cred, file='twitter authentication.Rdata')
load('twitter authentication.Rdata') #Once you launch the code first time, you can start from this line in the future (libraries should be connected)
registerTwitterOAuth(Cred)

setup_twitter_oauth(consumerKey, consumerSecret, access_token, access_secret)

pos.words <- scan('positive-words.txt', what='character', comment.char=';')
neg.words <- scan('negative-words.txt', what='character', comment.char=';')

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

gvsu.tweets <- searchTwitter('#gvsu', n=500, lang="en")
gvsu.text = laply(gvsu.tweets, function(t) t$getText())
gvsu.text = gsub("[^[:alnum:]|^[:space:]]", "", gvsu.text)
gvsu.scores <- score.sentiment(gvsu.text, pos.words, neg.words, .progress='text')

summary(gvsu.scores)

pos_tweet <- as.character(gvsu.scores$text[ which(gvsu.scores$score>2)])

laker.tweets <- searchTwitter('#Laker4aLifetime', n=500, lang="en")
laker.text = laply(laker.tweets, function(t) t$getText())
laker.text = gsub("[^[:alnum:]|^[:space:]]", "", laker.text)
laker.scores <- score.sentiment(laker.text, pos.words, neg.words, .progress='text')

summary(laker.scores)

laker_tweet <- as.character(laker.scores$text[ which(laker.scores$score>1)])

