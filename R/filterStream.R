#' @rdname filterStream
#' @export
#'
#' @title 
#' Connect to Twitter Streaming API and return public statuses that 
#' match one or more filter predicates.
#'
#' @description
#' \code{filterStream} opens a connection to Twitter's Streaming API
#' that will return public statuses that match one or more filter predicates.
#' Tweets can be filtered by keywords, users, and location. The output can be
#' saved as an object in memory or written to a text file.
#'
#' @details
#' \code{filterStream} provides access to the statuses/filter Twitter stream.
#'
#' It will return public statuses that
#' match the keywords given in the \code{track} argument, published by the users
#' specified in the \code{follow} argument, and sent within the location bounding
#' boxes declared in the \code{locations} argument.
#'
#' Note that location bounding boxes do not act as filters for other filter
#' parameters. In the fourth example below, we capture all tweets containing the term
#' rstats (even non-geolocated tweets) OR coming from the New York City area. For more
#' information on how the Streaming API request parameters work, check the
#' documentation at: \url{http://dev.twitter.com/docs/streaming-apis/parameters}.
#'
#' If any of these arguments is left empty (e.g. no user filter is specified),
#' the function will return all public statuses that match the other filters.
#' At least one predicate parameter must be specified.
#'
#' Note that when no file name is provided, tweets are written to a temporary file, 
#' which is loaded in memory as a string vector when the connection to the stream
#' is closed.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{sampleStream}}, \code{\link{userStream}}, \code{\link{parseTweets}}
#'
#' @param file.name string, name of the file where tweets will be written. 
#' "" indicates output to the console, which can be redirected to an R object (see examples).
#'
#' @param track string or string vector containing keywords to track.
#' See the \code{track} parameter information in the Streaming API documentation for details:
#' \url{http://dev.twitter.com/docs/streaming-apis/parameters#track}.
#'
#' @param follow string or numeric, vector of Twitter user IDs, indicating the users whose public
#' statuses should be delivered on the stream. See the \code{follow} parameter information
#' in the Streaming API documentation for details:
#' \url{http://dev.twitter.com/docs/streaming-apis/parameters#follow}.
#'
#' @param locations numeric, a vector of longitude, latitude pairs (with the southwest corner
#' coming first) specifying sets of bounding boxes to filter public statuses by. 
#' See the \code{locations} parameter information in the Streaming API documentation for details:
#' \url{http://dev.twitter.com/docs/streaming-apis/parameters#locations}
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
#' ## capture tweets mentioning the "Rstats" hashtag
#'   filterStream( file="tweets_rstats.json", 
#'      track="rstats", timeout=3600, user=FOO, password=BAR )
#'
#' ## capture tweets published by Twitter's official account      
#'   filterStream( file="tweets_twitter.json", 
#'      follow="783214", timeout=600, user=FOO, password=BAR )
#'
#' ## capture tweets sent from New York City and saving as an object in memory
#'   tweets <- filterStream( file="", 
#'       locations=c(-74,40,-73,41), timeout=600, user=FOO, password=BAR )  
#'
#' ## capture tweets mentioning the "rstats" hashtag or sent from New York City
#'   filterStream( file="tweets_rstats.json", track="rstats",
#'       locations=c(-74,40,-73,41), timeout=600, user=FOO, password=BAR )
#'  
#' ## An example of an authenticated request using the ROAuth package, 
#' ## where consumerkey and consumer secret are fictitious. 
#' ## You can obtain your own at dev.twitter.com
#'   library(ROAuth)
#'   requestURL <- "https://api.twitter.com/oauth/request_token"
#'   accessURL <- "http://api.twitter.com/oauth/access_token"
#'   authURL <- "http://api.twitter.com/oauth/authorize"
#'   consumerKey <- "xxxxxyyyyyzzzzzz"
#'   consumerSecret <- "xxxxxxyyyyyzzzzzzz111111222222"
#'   my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
#'     consumerSecret=consumerSecret, requestURL=requestURL,
#'     accessURL=accessURL, authURL=authURL)
#'   my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
#'   filterStream( file="tweets_rstats.json",
#'	   track="rstats", timeout=3600, oauth=my_oauth )
#' }
#'

filterStream <- function(file.name, track=NULL, follow=NULL, locations=NULL, 
	timeout=10800, user=NULL, password=NULL, oauth=NULL, verbose=TRUE)
{
	require(RCurl)
   
   # checking user input is correct
   if (all(is.null(c(track,follow,locations)))) {
    	stop("No filter parameter was specified. At least one is necessary. 
    		See ?filterStream for more information about this error.")
   } 
   if (missing(file.name)||is.character(file.name)==FALSE){
   	stop("The file where the tweets will be stored was not named properly.")
   }
   if (timeout<0||is.numeric(timeout)==FALSE||length(timeout)>1){
   	stop("The specified time out was not properly formatted.")
   }

   # authentication
   if (is.null(oauth)) {
   	if (is.null(user)||is.null(password)) { stop("No authentication method was provided. 
   		Please enter your user name and password or use OAuth.") }
   	userpwd <- paste(c(user, password), collapse=":")     
   }
   if (!is.null(oauth)){
   	require("ROAuth")
   	if (!inherits(oauth, "OAuth"))
    		stop("oauth argument must be of class OAuth")
  		if (!oauth$handshakeComplete)
    		stop("Oauth needs to complete its handshake. See ?filterStream.")
   }

 	# building parameter lists
 	params <- buildArgList(track, follow, locations, oauth=oauth)

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

	if (is.null(oauth)){
		output <- tryCatch(getURL("https://stream.twitter.com/1/statuses/filter.json",
		   userpwd=userpwd,  write = write.tweets,  postfields = params,   
		   cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"),
		 	.opts = list(verbose = FALSE, timeout=timeout)),
		     	error=function(e) e)
		close(conn)  
	}
	if (!is.null(oauth)){
		url <- "https://stream.twitter.com/1.1/statuses/filter.json"
		output <- tryCatch(oauth$OAuthRequest(URL=url, params=params, method="POST", 
			customHeader=NULL, timeout = timeout, writefunction = write.tweets, 
			cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl")), 
				error=function(e) e)
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
		if (verbose==TRUE){ message("Connection to Twitter stream was closed after ", seconds,
			" seconds with ", length(raw.tweets), " tweets downloaded.") }
		unlink(file.name)			
		return(raw.tweets)
	}
	if (open.in.memory==FALSE) {
		if (verbose==TRUE) {message("Connection to Twitter stream was closed after ", seconds,
			" seconds with ", kb.received, " kB received.")}	
	}
}

format.param <- function(param.name, param){
	param <- as.character(param)
	if (length(param)>1) param <- paste(param, collapse=",")
	param.field <- paste(param.name, "=", param, sep="")
}

buildArgList <- function(track=NULL, follow=NULL, locations=NULL, with=NULL,
	replies=NULL, oauth=NULL){
	if (is.null(oauth)){
		params <- c()
		if (!is.null(track)){
			params <- c(params, format.param("track", sapply(track, URLencode)))
		}
		if (!is.null(follow)){
			params <- c(params, format.param("follow", follow))
		}
		if (!is.null(locations)){
			params <- c(params, format.param("locations", locations))
		}
		if (!is.null(with)){
			params <- c(params, format.param("with", with))
		}
		if (!is.null(replies)){
			params <- c(params, format.param("replies", replies))
		}

		# putting it all together
		if (length(params)>1) params <- paste(params, collapse="&")
	}	
	if (!is.null(oauth)){
		params <- list()
		if (!is.null(track)) params[["track"]] <- paste(track, collapse=",")
		if (!is.null(follow)) params[["follow"]] <- paste(as.character(follow), collapse=",")
		if (!is.null(locations)) params[["locations"]] <- paste(as.character(locations), collapse=",")
		if (!is.null(with)) params[["with"]] <- paste(as.character(with), collapse=",")
		if (!is.null(replies)) params[["replies"]] <- paste(as.character(replies), collapse=",")
	}
	return(params)
}



