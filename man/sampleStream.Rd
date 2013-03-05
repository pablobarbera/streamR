\name{sampleStream}
\alias{sampleStream}
\title{Connect to Twitter Streaming API and return a small random sample of
all public statuses.}
\usage{
  sampleStream(file.name, timeout = 10800, user = NULL,
    password = NULL, oauth = NULL, verbose = TRUE)
}
\arguments{
  \item{file.name}{string, name of the file where tweets
  will be written. "" indicates output to the console,
  which can be redirected to an R object.}

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
}
\examples{
\dontrun{
## capture a random sample of tweets
sampleStream( file="tweets_sample.json", user=FOO, password=BAR )

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
 sampleStream( file="tweets_sample.json", oauth=my_oauth )

}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}
\seealso{
  \code{\link{filterStream}}, \code{\link{userStream}},
  \code{\link{parseTweets}}
}

