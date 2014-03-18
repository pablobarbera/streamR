\name{tweetsToMongo}
\alias{tweetsToMongo}
\title{Parses and exports tweets to a Mongo DB collection, adding fields useful
for their analysis}
\usage{
  tweetsToMongo(file.name = NULL, ns = NULL,
    host = "localhost", username = "", password = "",
    verbose = TRUE)
}
\arguments{
  \item{file.name}{string, name of the file where tweets
  were written.}

  \item{ns}{string, namespace of the collection to which
  tweets will be added. Generally, it will be of the form
  "database.collection". If the database or the collection
  do not exist, they will be automatically created; if they
  exist, tweets will be appended.}

  \item{host}{string host/port where mongo database is
  hosted. Default is localhost (127.0.0.1).}

  \item{username}{string, username to be used for
  authentication purposes with MongoDB.}

  \item{password}{string, password corresponding to the
  given username.}

  \item{verbose}{logical, default is \code{TRUE}, which
  generates some output to the R console with information
  about the capturing process.}
}
\description{
  \code{tweetsToMongo} read tweets downloaded with
  \code{filterStream}, adds an index and other fields that
  can facilitate their analysis, and stores them in a
  MongoDB collection
}
\examples{
\dontrun{

## An example of an authenticated request using the ROAuth package,
## where consumerkey and consumer secret are fictitious.
## You can obtain your own at dev.twitter.com
  library(ROAuth)
  requestURL <- "https://api.twitter.com/oauth/request_token"
  accessURL <- "http://api.twitter.com/oauth/access_token"
  authURL <- "http://api.twitter.com/oauth/authorize"
  consumerKey <- "xxxxxyyyyyzzzzzz"
  consumerSecret <- "xxxxxxyyyyyzzzzzzz111111222222"
  my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
    consumerSecret=consumerSecret, requestURL=requestURL,
    accessURL=accessURL, authURL=authURL)
  my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

## capture 10 tweets mentioning the "Rstats" hashtag
  filterStream( file.name="tweets_rstats.json",
     track="rstats", tweets=10, oauth=my_oauth )

## exporting those 10 tweets to MongoDB
  tweetsToMongo( file.name="tweets_rstats.json", ns="tweets.rstats")

}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}
\seealso{
  \code{\link{sampleStream}}, \code{\link{userStream}},
  \code{\link{parseTweets}}
}

