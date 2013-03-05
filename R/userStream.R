#' @rdname userStream
#' @export
#'
#' @title 
#' Connect to Twitter Streaming API and return messages for a single user.
#'
#' @description
#' \code{userStream} opens a connection to Twitter's Streaming API
#' that will return statuses specific to the authenticated user.
#'
#' @details
#' This function provides access to messages for a single user.
#'
#' The set of messages to be returned can include the user's tweets and/or replies, 
#' and public statuses published by the accounts the user follows. 
#' Tweets can also be filtered by keywords and location, using the \code{track}
#' and \code{locations} arguments.
#'
#' Note that when no file name is provided, tweets are written to a temporary file, 
#' which is loaded in memory as a string vector when the connection to the stream
#' is closed.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{filterStream}}, \code{\link{sampleStream}}, \code{\link{parseTweets}}
#'
#' @param file.name string, name of the file where tweets will be written. 
#' "" indicates output to the console, which can be redirected to an R object.
#'
#' @param with string, detault is "user", which will only stream messages from the authenticated 
#' user. If set to "followings", stream will add messages from accounts the user follows. The output
#' would be equivalent to the user's home timeline. 
#'
#' See the \code{with} parameter information in the Streaming API documentation for details:
#' \url{https://dev.twitter.com/docs/streaming-apis/parameters#with}
#'
#' @param replies string, default is \code{NULL}, which will only stream replies sent by
#' a different user if the authenticated user follows the receiver of the reply. All replies
#' will be included if this argument is set to "all".
#'
#' See the \code{replies} parameter information in the Streaming API documentation for details:
#' \url{https://dev.twitter.com/docs/streaming-apis/parameters#replies}
#'
#' @param track string or string vector containing keywords to track.
#' See the track parameter information in the Streaming API documentation for details:
#' \url{http://dev.twitter.com/docs/streaming-apis/parameters#track}.
#'
#' @param locations numeric, a vector of longitude, latitude pairs (with the southwest corner
#' coming first) specifying sets of bounding boxes to filter statuses by. 
#' See the locations parameter information in the Streaming API documentation for details:
#' \url{http://dev.twitter.com/docs/streaming-apis/parameters#locations}
#' 
#' @param timeout numeric, maximum length of time (in seconds) of connection to stream.
#' The connection will be automatically closed after this period. Default is 10800 (3 hours).
#'
#' @param oauth an object of class \code{oauth} that contains the access tokens
#' to the user's twitter session. This is the only method for authentication available
#' for user streams. See examples for more details.
#'
#' @param verbose logical, default is \code{TRUE}, which generates some output to the
#' R console with information about the capturing process.
#'
#' @examples \dontrun{
#' ## The following example shows how to capture a user's home timeline
#' ## with the Streaming API and using authentication via the ROAuth
#' ## package, with fictitious consumerkey and consumer secret.
#' ## You can obtain your own at dev.twitter.com
#'  library(ROAuth)
#'  reqURL <- "https://api.twitter.com/oauth/request_token"
#'  accessURL <- "http://api.twitter.com/oauth/access_token"
#'  authURL <- "http://api.twitter.com/oauth/authorize"
#'  consumerKey <- "xxxxxyyyyyzzzzzz"
#'  consumerSecret <- "xxxxxxyyyyyzzzzzzz111111222222"
#'  my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
#'     consumerSecret=consumerSecret, requestURL=requestURL,
#'     accessURL=accessURL, authURL=authURL)
#'  my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
#'  userStream( file="my_timeline.json", with="followings",
#'      timeout=600, oauth=my_oauth )
#' }
#'

userStream <- function(file.name, with="user", replies=NULL, track=NULL, locations=NULL, 
	timeout=10800, oauth=NULL, verbose=TRUE)
{
	require("RCurl");require("ROAuth")
   
	# checking user input is correct
	if (with!="user" && with!="followings"){
		stop("'with' argument was not properly specified. 
			See ?userStream for details.")
	}
	if (!is.null(replies)){
		if (replies!="all") {
			stop("'replies' argument was not properly specified. 
				See ?userStream for details.")
		}
	}
	if (missing(file.name)||is.character(file.name)==FALSE){
		stop("The file where the tweets will be stored was not named properly.")
	}
	if (timeout<0||is.numeric(timeout)==FALSE||length(timeout)>1){
		stop("The specified time out was not properly formatted.")
	}

   # authentication
   	if (is.null(oauth)) {
   		stop("No authentication method was provided.")   
   	}
   	if (!inherits(oauth, "OAuth")) {
   		stop("oauth argument must be of class OAuth")
   	}
    if (!oauth$handshakeComplete) {
    	stop("Oauth needs to complete its handshake. See ?userStream.")
    }
 	# building parameter lists
 	params <- buildArgList(with, replies, track, locations, oauth=oauth)

	# write the JSON tweets from the Twitter Streaming API to a text file
	# opening connection to file (temporary file if not specified)
	if (verbose==TRUE) message("Capturing tweets...")
	open.in.memory <- FALSE
	if (nchar(file.name)==0) {
		open.in.memory <- TRUE
		file.name <- tempfile()
	}
	conn <- file(description=file.name, open="a")
	write.tweets <- function(x){
	# writes output of stream to a file
		if (nchar(x)>0) {
			writeLines(x, conn, sep="")
		}
	}   	

	url <- "https://userstream.twitter.com/1.1/user.json"
	output <- tryCatch(oauth$OAuthRequest(URL=url, params=list(), method="GET",
		cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"), 
		writefunction = write.tweets, timeout = timeout), error=function(e) e)
	close(conn)

	# information messages
	seconds <- gsub(".*after (.*) milliseconds.*", "\\1", output$message)
	seconds <- round(as.numeric(seconds)/1000, 0)
	kb.received <- gsub(".*with (.*) bytes.*", "\\1", output$message)
	kb.received <- round(as.numeric(kb.received)/1024, 0) 

	# if tweets were saved in temporary file, it now opens it in memory
	if (open.in.memory==TRUE){
		raw.tweets <- readLines(file.name, warn=FALSE, encoding="UTF-8")
		if (verbose==TRUE) {message("Connection to Twitter stream was closed after ", seconds,
			" seconds with ", length(raw.tweets), " tweets downloaded.")}	
		unlink(file.name)			
		return(raw.tweets)
	}
	if (open.in.memory==FALSE) {
		if (verbose==TRUE){message("Connection to Twitter stream was closed after ", seconds,
			" seconds with ", kb.received, " kB received.")}	
	}
}

