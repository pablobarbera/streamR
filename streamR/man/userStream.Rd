\name{userStream}
\alias{userStream}
\title{Connect to Twitter Streaming API and return messages for a single user.}
\usage{
  userStream(file.name = NULL, with = "followings",
    replies = NULL, track = NULL, locations = NULL,
    timeout = 0, tweets = NULL, oauth, verbose = TRUE)
}
\arguments{
  \item{file.name}{string, name of the file where tweets
  will be written. "" indicates output to the console,
  which can be redirected to an R object. If the file
  already exists, tweets will be appended (not
  overwritten).}

  \item{with}{string, detault is "followings", which will
  stream messages from accounts the authenticated user
  follow. If set to "user", will only stream messages from
  authenticated user.

  See the \code{with} parameter information in the
  Streaming API documentation for details:
  \url{https://dev.twitter.com/docs/streaming-apis/parameters#with}}

  \item{replies}{string, default is \code{NULL}, which will
  only stream replies sent by a different user if the
  authenticated user follows the receiver of the reply. All
  replies to users that the authenticated user follows will
  be included if this argument is set to "all".

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
  This is the only method for authentication available for
  user streams. See examples for more details.}

  \item{verbose}{logical, default is \code{TRUE}, which
  generates some output to the R console with information
  about the capturing process.}
}
\description{
  \code{userStream} opens a connection to Twitter's
  Streaming API that will return statuses specific to the
  authenticated user. The output can be saved as an object
  in memory or written to a text file.
}
\details{
  This function provides access to messages for a single
  user.

  The set of messages to be returned can include the user's
  tweets and/or replies, and public statuses published by
  the accounts the user follows, as well to replies to
  those accounts.

  Tweets can also be filtered by keywords and location,
  using the \code{track} and \code{locations} arguments.

  The total number of actual tweets that are captured might
  be lower than the number of tweets requested because
  blank lines, deletion notices, and incomplete tweets are
  included in the count of tweets downloaded.

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
 requestURL <- "https://api.twitter.com/oauth/request_token"
 accessURL <- "http://api.twitter.com/oauth/access_token"
 authURL <- "http://api.twitter.com/oauth/authorize"
 consumerKey <- "xxxxxyyyyyzzzzzz"
 consumerSecret <- "xxxxxxyyyyyzzzzzzz111111222222"
 my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
    consumerSecret=consumerSecret, requestURL=requestURL,
    accessURL=accessURL, authURL=authURL)
 my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
 save(my_oauth, file="my_oauth")
## Capturing 10 tweets from a user's timeline
 userStream( file.name="my_timeline.json", with="followings",
     tweets=10, oauth=my_oauth )
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}
\seealso{
  \code{\link{filterStream}}, \code{\link{sampleStream}},
  \code{\link{parseTweets}}
}

