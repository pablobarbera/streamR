#' @rdname sampleStream
#' @export
#'
#' @title 
#' Connect to Twitter Streaming API and return a small random sample of
#' all public statuses.
#'
#' @description
#' \code{sampleStream} opens a connection to Twitter's Streaming API
#' that will return a small random sample of public statuses, around 1\%
#' at any given time.
#'
#' @details
#' For more information, check the documentation at:
#' \url{https://dev.twitter.com/docs/api/1.1/get/statuses/sample}
#'
#' Note that when no file name is provided, tweets are written to a temporary file, 
#' which is loaded in memory as a string vector when the connection to the stream
#' is closed.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{filterStream}}, \code{\link{userStream}}, \code{\link{parseTweets}}
#'
#' @param file.name string, name of the file where tweets will be written. 
#' "" indicates output to the console, which can be redirected to an R object.
#'
#' @param timeout numeric, maximum length of time (in seconds) of connection to stream.
#' The connection will be automatically closed after this period. Default is 10800 (3 hours).
#'
#' @param user string containing the screen name of the Twitter 
#' account that will be used for authentication.
#'
#' @param password string containing the password of the Twitter
#' account that will be used for authentication. Note that this password will be visible
#' to anyone with access to the console. Authentication using OAuth is recommended, and
#' will be the only authentication method allowed once version 1 of the Twitter API is
#' deprecated.
#'
#' @param oauth an object of class \code{oauth} that contains the access tokens
#' to the user's twitter session. This is the recommended method for authentication. 
#' See examples for more details.
#'
#' @param verbose logical, default is \code{TRUE}, which generates some output to the
#' R console with information about the capturing process.
#'
#' @examples \dontrun{
#' ## capture a random sample of tweets
#' sampleStream( file="tweets_sample.json", user=FOO, password=BAR )
#'
#' ## An example of an authenticated request using the ROAuth package, 
#' ## where consumerkey and consumer secret are fictitious. 
#' ## You can obtain your own at dev.twitter.com
#'  library(ROAuth)
#'  reqURL <- "https://api.twitter.com/oauth/request_token"
#'  accessURL <- "http://api.twitter.com/oauth/access_token"
#'  authURL <- "http://api.twitter.com/oauth/authorize"
#'  consumerKey <- "xxxxxyyyyyzzzzzz"
#'  consumerSecret <- "xxxxxxyyyyyzzzzzzz111111222222"
#'   my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
#'     consumerSecret=consumerSecret, requestURL=requestURL,
#'     accessURL=accessURL, authURL=authURL)
#'  my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
#'  sampleStream( file="tweets_sample.json", oauth=my_oauth )
#'
#' }
#'

sampleStream <- function(file.name, timeout=10800, user=NULL, password=NULL, oauth=NULL, verbose=TRUE)
{
	require(RCurl)
    
	# authentication
   	if (is.null(oauth)) {
   		if (is.null(user)|is.null(password)) { stop("No authentication method was provided. 
   			Please enter your user name and password or use OAuth.") }
   		userpwd <- paste(c(user, password), collapse=":")     
   	}
   	if (!is.null(oauth)){
   		require("ROAuth")
   		if (!inherits(oauth, "OAuth"))
    		stop("oauth argument must be of class OAuth")
  		if (!oauth$handshakeComplete)
    		stop("Oauth needs to complete its handshake. See ?sampleStream.")
   	}

   	# write the JSON tweets from the Twitter Streaming API to a text file
	# opening connection to file (temporary file if not specified)
	if (verbose==TRUE) {message("Capturing tweets...")}
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

	if (is.null(oauth)){
		output <- tryCatch(getURL("https://stream.twitter.com/1/statuses/sample.json",
		   userpwd = userpwd,  write = write.tweets,
		   cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"),
		 	.opts = list(verbose = FALSE, timeout=timeout)),
		     	error=function(e) e)
		close(conn)  
	}
	if (!is.null(oauth)){
		url <- "https://stream.twitter.com/1.1/statuses/sample.json"
		output <- tryCatch(oauth$OAuthRequest(URL=url, params=list(), method="GET",
			cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"),
			writefunction = write.tweets, timeout = timeout), error=function(e) e)
		close(conn)
	}
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
		if (verbose==TRUE) {message("Connection to Twitter stream was closed after ", seconds,
			" seconds with ", kb.received, " kB received.")}		
	}
}

