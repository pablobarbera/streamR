\name{sampleStream}
\alias{sampleStream}
\title{Connect to Twitter Streaming API and return a small random sample of
all public statuses.}
\usage{
  sampleStream(file.name, timeout = 0, tweets = NULL,
    oauth = NULL, verbose = TRUE)
}
\arguments{
  \item{file.name}{string, name of the file where tweets
  will be written. "" indicates output to the console,
  which can be redirected to an R object. If the file
  already exists, tweets will be appended (not
  overwritten).}

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

  \item{verbose}{logical, default is \code{TRUE}, which
  generates some output to the R console with information
  about the capturing process.}
}
\description{
  \code{sampleStream} opens a connection to Twitter's
  Streaming API that will return a small random sample of
  public statuses, around 1\% at any given time.
}
\details{
  For more information, check the documentation at:
  \url{https://dev.twitter.com/docs/api/1.1/get/statuses/sample}

  Note that when no file name is provided, tweets are
  written to a temporary file, which is loaded in memory as
  a string vector when the connection to the stream is
  closed.

  The total number of actual tweets that are captured might
  be lower than the number of tweets requested because
  blank lines, deletion notices, and incomplete tweets are
  included in the count of tweets downloaded.
}
\examples{
\dontrun{
## capture a random sample of tweets
sampleStream( file.name="tweets_sample.json", user=FOO, password=BAR )

## An example of an authenticated request using the ROAuth package,
## where consumerkey and consumer secret are fictitious.
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
 sampleStream( file.name="tweets_sample.json", oauth=my_oauth )

}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}
\seealso{
  \code{\link{filterStream}}, \code{\link{userStream}},
  \code{\link{parseTweets}}
}

