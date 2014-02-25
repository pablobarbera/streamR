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
#' Tweets can be filtered by keywords, users, language, and location. The output
#' can be saved as an object in memory or written to a text file.
#'
#' @details
#' \code{filterStream} provides access to the statuses/filter Twitter stream.
#'
#' It will return public statuses that
#' match the keywords given in the \code{track} argument, published by the users
#' specified in the \code{follow} argument, written in the language specified
#' in the \code{language} argument, and sent within the location bounding
#' boxes declared in the \code{locations} argument.
#'
#' Note that location bounding boxes do not act as filters for other filter
#' parameters. In the fourth example below, we capture all tweets containing the term
#' rstats (even non-geolocated tweets) OR coming from the New York City area. For more
#' information on how the Streaming API request parameters work, check the
#' documentation at: \url{http://dev.twitter.com/docs/streaming-apis/parameters}.
#'
#' Also note that the \code{language} parameter needs to be used in combination
#' with another filter option (either keywords or location).
#'
#' If any of these arguments is left empty (e.g. no user filter is specified),
#' the function will return all public statuses that match the other filters.
#' At least one predicate parameter must be specified.
#'
#' Note that when no file name is provided, tweets are written to a temporary file, 
#' which is loaded in memory as a string vector when the connection to the stream
#' is closed.
#'
#' The total number of actual tweets that are captured might be lower than the number 
#' of tweets requested because blank lines, deletion notices, and incomplete
#' tweets are included in the count of tweets downloaded.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{sampleStream}}, \code{\link{userStream}}, \code{\link{parseTweets}}
#'
#' @param file.name string, name of the file where tweets will be written. 
#' "" indicates output to the console, which can be redirected to an R object (see examples).
#' If the file already exists, tweets will be appended (not overwritten).
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
#' @param language string or string vector containing a list of BCP 47 language identifiers.
#' If not \code{NULL} (default), function will only return tweets that have been detected
#' as being written in the specified languages. Note that this parameter can only be used
#' in combination with any of the other filter parameters. See documentation for details:
#' \url{https://dev.twitter.com/docs/streaming-apis/parameters#language}
#' 
#' @param timeout numeric, maximum length of time (in seconds) of connection to stream.
#' The connection will be automatically closed after this period. For example, setting
#' \code{timeout} to 10800 will keep the connection open for 3 hours. The default is 0,
#' which will keep the connection open permanently.
#'
#' @param tweets numeric, maximum number of tweets to be collected when function is called.
#' After that number of tweets have been captured, function will stop. If set to \code{NULL}
#' (default), the connection will be open for the number of seconds specified in \code{timeout}
#' parameter.
#'
#' @param oauth an object of class \code{oauth} that contains the access tokens
#' to the user's twitter session. This is currently the only method for authentication. 
#' See examples for more details.
#'
#'
#' @param verbose logical, default is \code{TRUE}, which generates some output to the
#' R console with information about the capturing process.
#'
#' @examples \dontrun{
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
#'
#' ## capture 10 tweets mentioning the "Rstats" hashtag
#'   filterStream( file.name="tweets_rstats.json", 
#'      track="rstats", tweets=10, oauth=my_oauth )
#'
#' ## capture tweets published by Twitter's official account      
#'   filterStream( file.name="tweets_twitter.json", 
#'      follow="783214", timeout=600, oauth=my_oauth )
#'
#' ## capture tweets sent from New York City in Spanish only, and saving as an object in memory
#'   tweets <- filterStream( file.name="", language="es",
#'       locations=c(-74,40,-73,41), timeout=600, oauth=my_oauth )  
#'
#' ## capture tweets mentioning the "rstats" hashtag or sent from New York City
#'   filterStream( file="tweets_rstats.json", track="rstats",
#'       locations=c(-74,40,-73,41), timeout=600, oauth=my_oauth )
#'
#' }
#'

filterStream <- function(file.name=NULL, track=NULL, follow=NULL, locations=NULL, language=NULL, 
	timeout=0, tweets=NULL, oauth=NULL, verbose=TRUE)
{
    if(!is.null(oauth)){library(ROAuth)}
	open.in.memory <- FALSE
   
  	# checking user input is correct
   if (all(is.null(c(track,follow,language,locations)))) {
    	stop("No filter parameter was specified. At least one is necessary. 
    		See ?filterStream for more information about this error.")
   } 
   if (all(is.null(c(track,follow,locations))) & !is.null(language)){
   		stop("Language parameter can only be used in combination with other filter parameters.")
   }
   if ((missing(file.name)||is.character(file.name)==FALSE)){
   	stop("The file where the tweets will be stored was not named properly.")
   }
   if (timeout<0||is.numeric(timeout)==FALSE||length(timeout)>1){
   	stop("The specified time out was not properly formatted.")
   }

   # authentication
   if (is.null(oauth)) {
    stop("No authentication method was provided. 
   		Please use an OAuth token.") }
   if (!is.null(oauth)){
   	if (!inherits(oauth, "OAuth")) {
   			stop("oauth argument must be of class OAuth") }
  		if (!oauth$handshakeComplete) {
    		stop("Oauth needs to complete its handshake. See ?filterStream.") }
   }

 	# building parameter lists
 	params <- buildArgList(track, follow, language, locations, oauth=oauth)

 	# WRITING FUNCTION
 	## tweet counter
 	i <- 0

 	## write the JSON tweets from Streaming API to a text file
 	if (!is.null(file.name)){
 		if (verbose==TRUE) message("Capturing tweets...")
		if (nchar(file.name)==0) {
			open.in.memory <- TRUE
			file.name <- tempfile()
		}
		conn <- file(description=file.name, open="a")
		write.tweets <- function(x){
			# writes output of stream to a file
			if (nchar(x)>0) {
				i <<- i + 1
				writeLines(x, conn, sep="")
			}
		} 
		if (!is.null(tweets) && is.numeric(tweets) && tweets>0){	
			write.tweets <- function(x){	
				if (i>=tweets){break}	
				# writes output of stream to a file	
				if (nchar(x)>0) {	
					i <<- i + 1	
					writeLines(x, conn, sep="")	
				}	
			}
		}  
 	}


 	init <- Sys.time()
 	# connecting to Streaming API
	url <- "https://stream.twitter.com/1.1/statuses/filter.json"
    if (!is.null(oauth)){
    	output <- tryCatch(oauth$OAuthRequest(URL=url, params=params, method="POST", 
    		customHeader=NULL, timeout = timeout, writefunction = write.tweets, 
    		cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl")), 
    			error=function(e) e)
    }
	# housekeeping...
	if (!is.null(file.name)){ close(conn) }

	# information messages
	seconds <- round(as.numeric(difftime(Sys.time(), init, units="secs")),0)

	# if tweets were saved in temporary file, it now opens it in memory
	if (open.in.memory==TRUE){
		raw.tweets <- readLines(file.name, warn=FALSE, encoding="UTF-8")
		if (verbose==TRUE){ message("Connection to Twitter stream was closed after ", seconds,
			" seconds with up to ", length(raw.tweets), " tweets downloaded.") }
		unlink(file.name)			
		return(raw.tweets)
	}
	if (open.in.memory==FALSE) {
		if (verbose==TRUE) {message("Connection to Twitter stream was closed after ", seconds,
			" seconds with up to ", i, " tweets downloaded.")}	
	}
}

format.param <- function(param.name, param){
	param <- as.character(param)
	if (length(param)>1) param <- paste(param, collapse=",")
	param.field <- paste(param.name, "=", param, sep="")
}

buildArgList <- function(track=NULL, follow=NULL, language=NULL, locations=NULL, 
	with=NULL,replies=NULL, oauth=NULL)
{
	params <- list()
	if (!is.null(track)) params[["track"]] <- paste(track, collapse=",")
	if (!is.null(follow)) params[["follow"]] <- paste(as.character(follow), collapse=",")
	if (!is.null(locations)) params[["locations"]] <- paste(as.character(locations), collapse=",")
	if (!is.null(language)) params[["language"]] <- paste(as.character(language), collapse=",")
	if (!is.null(with)) params[["with"]] <- paste(as.character(with), collapse=",")
	if (!is.null(replies)) params[["replies"]] <- paste(as.character(replies), collapse=",")
	return(params)
}
