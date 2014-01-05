\name{topRetweets}
\alias{topRetweets}
\title{Connect to Mongo database and extract top retweets that match
conditions specified in the arguments.}
\usage{
  topRetweets(ns, string = NULL, min = 10, verbose = TRUE)
}
\arguments{
  \item{ns}{string, namespace of the MongoDB collection
  where tweets were stored. Generally, it will be of the
  form "database.collection".}

  \item{string}{string, set to NULL by default (will return
  count of all tweets). If it is a string, it will return
  the number of tweets that contain that string in the text
  of the tweet.}

  \item{min}{numeric, set to 10 by default (will return all
  retweets whose retweet count is at least 10). In large
  datasets, choose a high number to increase speed of
  query.}

  \item{verbose}{logical, default is \code{TRUE}, which
  generates some output to the R console with information
  about the count of tweets.}
}
\description{
  \code{topRetweets} opens a connection to a Mongo database
  and returns all retweets (or only retweets that mention a
  specific keyword) ordered by total number of retweets
  received.
}
\details{
  Note that this function will only return retweets that
  are made using the built-in retweeting system - this is,
  'manual' retweets using copy&paste are not included. Also
  note that total retweet counts are based on Twitter's
  internal tally, and do not reflect the number of retweets
  in the database. In other words, it could happen that the
  most popular retweet in a given moment is a tweet that
  was originally sent days ago, but was retweeted during
  the time of that tweets were captured.
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

## extract all retweets that were retweeted at least 100 times
 rts <- topRetweets( ns="tweets.twitter", min=100)

## show top 10 retweets from previous query
 head(rts, n=10)
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}

