\name{filterStream}
\alias{filterStream}
\title{Connect to Twitter Streaming API and return public statuses that
match one or more filter predicates.}
\usage{
  filterStream(file.name = NULL, track = NULL,
    follow = NULL, locations = NULL, language = NULL,
    timeout = 0, tweets = NULL, oauth, ns = NULL,
    host = "localhost", username = "", password = "",
    verbose = TRUE)
}
\arguments{
  \item{file.name}{string, name of the file where tweets
  will be written. "" indicates output to the console,
  which can be redirected to an R object (see examples). If
  the file already exists, tweets will be appended (not
  overwritten).}

  \item{track}{string or string vector containing keywords
  to track. See the \code{track} parameter information in
  the Streaming API documentation for details:
  \url{http://dev.twitter.com/docs/streaming-apis/parameters#track}.}

  \item{follow}{string or numeric, vector of Twitter user
  IDs, indicating the users whose public statuses should be
  delivered on the stream. See the \code{follow} parameter
  information in the Streaming API documentation for
  details:
  \url{http://dev.twitter.com/docs/streaming-apis/parameters#follow}.}

  \item{locations}{numeric, a vector of longitude, latitude
  pairs (with the southwest corner coming first) specifying
  sets of bounding boxes to filter public statuses by. See
  the \code{locations} parameter information in the
  Streaming API documentation for details:
  \url{http://dev.twitter.com/docs/streaming-apis/parameters#locations}}

  \item{language}{string or string vector containing a list
  of BCP 47 language identifiers. If not \code{NULL}
  (default), function will only return tweets that have
  been detected as being written in the specified
  languages. Note that this parameter can only be used in
  combination with any of the other filter parameters. See
  documentation for details:
  \url{https://dev.twitter.com/docs/streaming-apis/parameters#language}}

  \item{timeout}{numeric, maximum length of time (in
  seconds) of connection to stream. The connection will be
  automatically closed after this period. For example,
  setting \code{timeout} to 10800 will keep the connection
  open for 3 hours. The default is 0, which will keep the
  connection open permanently.}

  \item{tweets}{numeric, maximum number of tweets to be
  collected when function is called. After that number of
  tweets have been captured, function will stop. If set to
  \code{NULL} (default), the connection will be open for
  the number of seconds specified in \code{timeout}
  parameter.}

  \item{oauth}{an object of class \code{oauth} that
  contains the access tokens to the user's twitter session.
  This is currently the only method for authentication. See
  examples for more details.}

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
  \code{filterStream} opens a connection to Twitter's
  Streaming API that will return public statuses that match
  one or more filter predicates. Tweets can be filtered by
  keywords, users, and location. The output can be saved as
  an object in memory, written to a text file or stored in
  MongoDB
}
\details{
  \code{filterStream} provides access to the
  statuses/filter Twitter stream.

  It will return public statuses that match the keywords
  given in the \code{track} argument, published by the
  users specified in the \code{follow} argument, and sent
  within the location bounding boxes declared in the
  \code{locations} argument.

  Note that location bounding boxes do not act as filters
  for other filter parameters. In the fourth example below,
  we capture all tweets containing the term rstats (even
  non-geolocated tweets) OR coming from the New York City
  area. For more information on how the Streaming API
  request parameters work, check the documentation at:
  \url{http://dev.twitter.com/docs/streaming-apis/parameters}.

  If any of these arguments is left empty (e.g. no user
  filter is specified), the function will return all public
  statuses that match the other filters. At least one
  predicate parameter must be specified.

  Note that when no file name is provided, tweets are
  written to a temporary file, which is loaded in memory as
  a string vector when the connection to the stream is
  closed.

  The total number of actual tweets that are captured might
  be lower than the number of tweets requested because
  blank lines, deletion notices, and incomplete tweets are
  included in the count of tweets downloaded.

  To store tweets in MongoDB, it is necessary to install
  the MongoDB server in a local or remote machine. See here
  for instructions:
  \url{http://docs.mongodb.org/manual/installation/}
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

## capture tweets published by Twitter's official account
  filterStream( file.name="tweets_twitter.json",
     follow="783214", timeout=600, oauth=my_oauth )

## capture tweets sent from New York City in Spanish only, and saving as an object in memory
  tweets <- filterStream( file.name="", language="es",
      locations=c(-74,40,-73,41), timeout=600, oauth=my_oauth )

## capture tweets mentioning the "rstats" hashtag or sent from New York City
  filterStream( file="tweets_rstats.json", track="rstats",
      locations=c(-74,40,-73,41), timeout=600, oauth=my_oauth )

## capture 100 tweets sent from New York City and storing in MongoDB, in collection
## 'nyc' of database 'tweets'
  tweets <- filterStream( ns="tweets.nyc",
      locations=c(-74,40,-73,41), tweets=100, oauth=my_oauth )

## same as above, but also storing tweets in disk
  tweets <- filterStream( file.name="tweets_nyc.json", ns="tweets.nyc",
      locations=c(-74,40,-73,41), tweets=100, oauth=my_oauth )
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}
\seealso{
  \code{\link{sampleStream}}, \code{\link{userStream}},
  \code{\link{parseTweets}}
}

