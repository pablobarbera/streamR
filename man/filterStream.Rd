\name{filterStream}
\alias{filterStream}
\title{Connect to Twitter Streaming API and return public statuses that
match one or more filter predicates.}
\usage{
  filterStream(file.name, track = NULL, follow = NULL,
    locations = NULL, timeout = 10800, user = NULL,
    password = NULL, oauth = NULL, verbose = TRUE)
}
\arguments{
  \item{file.name}{string, name of the file where tweets
  will be written. "" indicates output to the console,
  which can be redirected to an R object (see examples).}

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

  \item{timeout}{numeric, maximum length of time (in
  seconds) of connection to stream. The connection will be
  automatically closed after this period. Default is 10800
  (3 hours).}

  \item{user}{string containing the screen name of the
  Twitter account that will be used for authentication.}

  \item{password}{string containing the password of the
  Twitter account that will be used for authentication.
  Note that this password will be visible to anyone with
  access to the console. Authentication using OAuth is
  recommended, and will be the only authentication method
  allowed once version 1 of the Twitter API is deprecated.}

  \item{oauth}{an object of class \code{oauth} that
  contains the access tokens to the user's twitter session.
  This is the recommended method for authentication. See
  examples for more details.}

  \item{verbose}{logical, default is \code{TRUE}, which
  generates some output to the R console with information
  about the capturing process.}
}
\description{
  \code{filterStream} opens a connection to Twitter's
  Streaming API that will return public statuses that match
  one or more filter predicates. Tweets can be filtered by
  keywords, users, and location. The output can be saved as
  an object in memory or written to a text file.
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
}
\examples{
\dontrun{
## capture tweets mentioning the "Rstats" hashtag
  filterStream( file="tweets_rstats.json",
     track="rstats", timeout=3600, user=FOO, password=BAR )

## capture tweets published by Twitter's official account
  filterStream( file="tweets_twitter.json",
     follow="783214", timeout=600, user=FOO, password=BAR )

## capture tweets sent from New York City and saving as an object in memory
  tweets <- filterStream( file="",
      locations=c(-74,40,-73,41), timeout=600, user=FOO, password=BAR )

## capture tweets mentioning the "rstats" hashtag or sent from New York City
  filterStream( file="tweets_rstats.json", track="rstats",
      locations=c(-74,40,-73,41), timeout=600, user=FOO, password=BAR )

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
  filterStream( file="tweets_rstats.json",
	   track="rstats", timeout=3600, oauth=my_oauth )
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}
\seealso{
  \code{\link{sampleStream}}, \code{\link{userStream}},
  \code{\link{parseTweets}}
}

