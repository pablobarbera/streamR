\name{countTweets}
\alias{countTweets}
\title{Connect to Mongo database and return count of tweets that match
certain conditions.}
\usage{
  countTweets(ns, string = NULL, retweets = NULL,
    hashtags = NULL, verbose = TRUE)
}
\arguments{
  \item{ns}{string, namespace of the MongoDB collection
  where tweets were stored. Generally, it will be of the
  form "database.collection".}

  \item{string}{string, set to NULL by default (will return
  count of all tweets). If it is a string, it will return
  the number of tweets that contain that string in the text
  of the tweet.}

  \item{retweets}{logical, set to NULL by default (will
  return count of all tweets). If \code{TRUE}, will count
  only tweets that are retweets (i.e. contain an embededed
  retweeted status - manual retweets are not included). If
  \code{FALSE}, will count only tweets that are not
  retweets (manual retweets are now included).}

  \item{hashtags}{logical, set to NULL by default (will
  return count of all tweets). If \code{TRUE}, will count
  only tweets that use a hashtag. If \code{FALSE}, will
  count only tweets that do not contain a hashtag.}

  \item{verbose}{logical, default is \code{TRUE}, which
  generates some output to the R console with information
  about the count of tweets. If code{FALSE}, function will
  not return any object.}
}
\description{
  \code{countTweets} opens a connection to a Mongo database
  and returns the number of tweets that match a series of
  conditions: whether it contains a certain keyword,
  whether it is or not a retweet, or whether or not it
  contains a hashtag. It depends on the \code{rmongodb}
  package, and a mongo object needs to be loaded in the
  workspace.
}
\examples{
\dontrun{
## capture 100 tweets that mention "twitter" and store them in local MongoDB,
## in collection 'twitter' of database 'tweets'
## (Note: since the 'track' parameter searches also in URLs, this search
## is equivalent to capturing only tweets that contain pictures sent
## using Twitter's built-in service)
 load(my_oauth)
 tweets <- filterStream( ns="tweets.twitter",
      track="twitter", tweets=100, oauth=my_oauth )

## connect to the Mongo database using rmongodb package
 library(rmongodb)
 mongo <- mongo.create("localhost", db="tweets")
## if required, specify username and password
## (MongoDB defaults are empty username and password)
 mongo.authenticate(mongo, username="", password="",
   db="tweets")

## count all tweets in the database
 countTweets( ns="tweets.twitter" )

## count tweets that mention 'twitter' in their text
 countTweets( ns="tweets.twitter", string="twitter")

## count all retweets in the database
 countTweets( ns="tweets.twitter", retweets=TRUE)

## count all tweets that mention 'twitter' and are not retweets
 countTweets( ns="tweets.twitter", string="twitter", retweets=FALSE)

## count all tweets that use a hashtag
 countTweets( ns="tweets.twitter", hashtags=TRUE)
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}

