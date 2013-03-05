\name{parseTweets}
\alias{parseTweets}
\title{Converts tweets in JSON format to data frame.}
\usage{
  parseTweets(tweets, simplify = FALSE, verbose = TRUE)
}
\arguments{
  \item{tweets}{A character string naming the file where
  tweets are stored or the name of the object in memory
  where the tweets were saved as strings.}

  \item{simplify}{If \code{TRUE} it will return a data
  frame with only tweet and user fields (i.e., no
  geographic information or url entities).}

  \item{verbose}{logical, default is \code{TRUE}, which
  will print in the console the number of tweets that have
  been parsed.}
}
\description{
  This function parses tweets downloaded using the
  \code{filterStream} function and returns a data frame.
}
\details{
  \code{parseTweets} parses tweets downloaded using the
  \code{\link{filterStream}}, \code{\link{sampleStream}} or
  \code{\link{userStream}} functions and returns a data
  frame where each row corresponds to one tweet and each
  column represents a different field for each tweet (id,
  text, created_at, etc.).

  The total number of tweets that are parsed might be lower
  than the number of lines in the file or object that
  contains the tweets because blank lines, deletion
  notices, and incomplete tweets are ignored.
}
\examples{
## The dataset example_tweets contains 10 public statuses published
## by @twitterapi in plain text format. The code below converts the object
## into a data frame that can be manipulated by other functions.

data(example_tweets)
tweets.df <- parseTweets(example_tweets, simplify=TRUE)

\dontrun{
## A more complete example, that shows how to capture a user's home timeline
## for one hour using authentication via OAuth, and then parsing the tweets
## into a data frame.

 library(ROAuth)
 reqURL <- "https://api.twitter.com/oauth/request_token"
 accessURL <- "http://api.twitter.com/oauth/access_token"
 authURL <- "http://api.twitter.com/oauth/authorize"
 consumerKey <- "xxxxxyyyyyzzzzzz"
 consumerSecret <- "xxxxxxyyyyyzzzzzzz111111222222"
 my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
                              consumerSecret=consumerSecret,
                              requestURL=reqURL,
                              accessURL=accessURL,
                              authURL=authURL)
 my_oauth$handshake()
 userStream( file="my_timeline.json", with="followings",
         timeout=3600, oauth=my_oauth )
 tweets.df <- parseTweets("my_timeline.json")
}
}
\author{
  Pablo Barbera \email{pablo.barbera@nyu.edu}
}
\seealso{
  \code{\link{filterStream}}, \code{\link{sampleStream}},
  \code{\link{userStream}}
}

