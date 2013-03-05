\name{userStream}
\alias{userStream}
\title{Connect to Twitter Streaming API and return messages for a single user.}
\usage{
  userStream(file.name, with = "user", replies = NULL,
    track = NULL, locations = NULL, timeout = 10800,
    oauth = NULL, verbose = TRUE)
}
\arguments{
  \item{file.name}{string, name of the file where tweets
  will be written. "" indicates output to the console,
  which can be redirected to an R object.}

  \item{with}{string, detault is "user", which will only
  stream messages from the authenticated user. If set to
  "followings", stream will add messages from accounts the
  user follows. The output would be equivalent to the
  user's home timeline.

  See the \code{with} parameter information in the
  Streaming API documentation for details:
  \url{https://dev.twitter.com/docs/streaming-apis/parameters#with}}

  \item{replies}{string, default is \code{NULL}, which will
  only stream replies sent by a different user if the
  authenticated user follows the receiver of the reply. All
  replies will be included if this argument is set to
  "all".

  See the \code{replies} parameter information in the
  Streaming API documentation for details:
  \url{https://dev.twitter.com/docs/streaming-apis/parameters#replies}}

  \item{track}{string or string vector containing keywords
  to track. See the track parameter information in the
  Streaming API documentation for details:
  \url{http://dev.twitter.com/docs/streaming-apis/parameters#track}.}

  \item{locations}{numeric, a vector of longitude, latitude
  pairs (with the southwest corner coming first) specifying
  sets of bounding boxes to filter statuses by. See the
  locations parameter information in the Streaming API
  documentation for details:
  \url{http://dev.twitter.com/docs/streaming-apis/parameters#locations}}

  \item{timeout}{numeric, maximum length of time (in
  seconds) of connection to stream. The connection will be
  automatically closed after this period. Default is 10800
  (3 hours).}

  \item{oauth}{an object of class \code{oauth} that
  contains the access tokens to the user's twitter session.
  This is the only method for authentication available for
  user streams. See examples for more details.}

  \item{verbose}{logical, default is \code{TRUE}, which
  generates some output to the R console with information
  about the capturing process.}
}
\description{
  \code{userStream} opens a connection to Twitter's
  Streaming API that will return statuses specific to the
  authenticated user.
}
\details{
  This function provides access to messages for a single
  user.

  The set of messages to be returned can include the user's
  tweets and/or replies, and public statuses published by
  the accounts the user follows. Tweets can also be
  filtered by keywords and location, using the \code{track}
  and \code{locations} arguments.

  Note that when no file name is provided, tweets are
  written to a temporary file, which is loaded in memory as
  a string vector when the connection to the stream is
  closed.
}
\examples{
\dontrun{
## The following example shows how to capture a user's home timeline
## with the Streaming API and using authentication via the ROAuth
## package, with fictitious consumerkey and consumer secret.
## You can obtain your own at dev.twitter.com
 library(ROAuth)
 reqURL <- "https://api.twitter.com/oauth/request_token"
 accessURL <- "http://api.twitter.com/oauth/access_token"
 authURL <- "http://api.twitter.com/oauth/authorize"
 consumerKey <- "xxxxxyyyyyzzzzzz"
 consumerSecret <- "xxxxxxyyyyyzzzzzzz111111222222"
 my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
    consumerSecret=consumerSecret, requestURL=requestURL,
    accessURL=accessURL, authURL=authURL)
 my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
 userStream( file="my_timeline.json", with="followings",
     timeout=600, oauth=my_oauth )
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}
\seealso{
  \code{\link{filterStream}}, \code{\link{sampleStream}},
  \code{\link{parseTweets}}
}

