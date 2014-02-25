#' @rdname userStream
#' @export
#'
#' @title 
#' Connect to Twitter Streaming API and return messages for a single user.
#'
#' @description
#' \code{userStream} opens a connection to Twitter's Streaming API
#' that will return statuses specific to the authenticated user. The output
#' can be saved as an object in memory, written to a text file or stored in MongoDB.
#'
#' @details
#' This function provides access to messages for a single user.
#'
#' The set of messages to be returned can include the user's tweets and/or replies, 
#' and public statuses published by the accounts the user follows, as well to replies
#' to those accounts. 
#' 
#' Tweets can also be filtered by keywords and location, using the \code{track}
#' and \code{locations} arguments.
#'
#' The total number of actual tweets that are captured might be lower than the number 
#' of tweets requested because blank lines, deletion notices, and incomplete
#' tweets are included in the count of tweets downloaded.
#'
#' Note that when no file name is provided, tweets are written to a temporary file, 
#' which is loaded in memory as a string vector when the connection to the stream
#' is closed.
#'
#' To store tweets in MongoDB, it is necessary to install the MongoDB server in a local
#' or remote machine. See here for instructions: \url{http://docs.mongodb.org/manual/installation/}
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{filterStream}}, \code{\link{sampleStream}}, \code{\link{parseTweets}}
#'
#' @param file.name string, name of the file where tweets will be written. 
#' "" indicates output to the console, which can be redirected to an R object.
#' If the file already exists, tweets will be appended (not overwritten).
#'
#' @param with string, detault is "followings", which will stream messages from accounts the
#' authenticated user follow. If set to "user", will only stream messages from authenticated
#' user.
#'
#' See the \code{with} parameter information in the Streaming API documentation for details:
#' \url{https://dev.twitter.com/docs/streaming-apis/parameters#with}
#'
#' @param replies string, default is \code{NULL}, which will only stream replies sent by
#' a different user if the authenticated user follows the receiver of the reply. All replies
#' to users that the authenticated user follows will be included if this argument is set to "all".
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
#' to the user's twitter session. This is the only method for authentication available
#' for user streams. See examples for more details.
#'
#' @param ns string, namespace of the collection to which tweets will be added. Generally,
#' it will be of the form "database.collection". If the database or the collection do not exist,
#' they will be automatically created; if they exist, tweets will be appended.
#'
#' @param host string host/port where mongo database is hosted. Default is localhost (127.0.0.1).
#'
#' @param username string, username to be used for authentication purposes with MongoDB.
#' 
#' @param password string, password corresponding to the given username.
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
#'  requestURL <- "https://api.twitter.com/oauth/request_token"
#'  accessURL <- "http://api.twitter.com/oauth/access_token"
#'  authURL <- "http://api.twitter.com/oauth/authorize"
#'  consumerKey <- "xxxxxyyyyyzzzzzz"
#'  consumerSecret <- "xxxxxxyyyyyzzzzzzz111111222222"
#'  my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
#'     consumerSecret=consumerSecret, requestURL=requestURL,
#'     accessURL=accessURL, authURL=authURL)
#'  my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
#'  save(my_oauth, file="my_oauth")
#'  userStream( file.name="my_timeline.json", with="followings",
#'      timeout=600, oauth=my_oauth )
#' ## Capturing 10 tweets from user's timeline and storing in MongoDB
#'	load("my_oauth")
#'  userStream( ns="tweets.mytimeline", with="followings", 
#'      tweets=10, oauth=my_oauth )
#' }
#'

userStream <- function(file.name=NULL, with="followings", replies=NULL, track=NULL, 
	locations=NULL, timeout=0, tweets=NULL, oauth, 
	ns=NULL, host='localhost', username="", password="", verbose=TRUE)
{
	if (!is.null(ns)){require(rmongodb, quietly=TRUE)}
	require(ROAuth)
	open.in.memory <- FALSE
   
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
	if ((missing(file.name)||is.character(file.name)==FALSE) & is.null(ns)){
		stop("The file where the tweets will be stored was not named properly.")
	}
	if (timeout<0||is.numeric(timeout)==FALSE||length(timeout)>1){
		stop("The specified time out was not properly formatted.")
	}

   # authentication
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
 	params <- buildArgList(with=with, replies=replies, track=track, locations=locations, oauth=oauth)

 	# WRITING FUNCTIONS:

 	if (is.null(file.name) & is.null(ns)){
 		stop("Error: file.name and ns parameters are empty")
 	}

 	## tweet counter
 	i <- -1 ## starting at -1 because stream returns list of friends first

 	## write the JSON tweets from Streaming API to a mongoDB collection
 	if (is.null(file.name) & !is.null(ns)){
 		db <- strsplit(ns, "\\.")[[1]][1]
 		coll <- strsplit(ns, "\\.")[[1]][2]
 		if (verbose==TRUE) { message("Storing tweets in collection '", 
 			coll, "' of database '", db, "' in MongoDB") }
		mongo <- mongo.create(host=host, username=username, password=password, db=db)
		if (mongo.get.err(mongo)!=0){ stop("Error in connection to MongoDB") }
		# function that will insert tweets into db
		write.tweets <- function(x){
			if (nchar(x)>0){
				i <<- i + 1
				json.list <- fromJSON(x)
				fields <- names(json.list)
				if ('text' %in% fields){
					json.list[['_id']] <- json.list[['id_str']]
    				json.list[['timestamp']] <- format.twitter.date(json.list[['created_at']])
    				json.list[['random_number']] <- runif(1, 0, 1)
					mongo.insert(mongo=mongo, ns=ns, json.list)
				}	
			}	
		} 
		if (!is.null(tweets) && is.numeric(tweets) && tweets>0){
			write.tweets <- function(x){
				if (i>=tweets){break}
				if (nchar(x)>0){
					i <<- i + 1
					json.list <- fromJSON(x)
					fields <- names(json.list)
					if ('text' %in% fields){
						json.list[['_id']] <- json.list[['id_str']]
    					json.list[['timestamp']] <- format.twitter.date(json.list[['created_at']])
    					json.list[['random_number']] <- runif(1, 0, 1)
						mongo.insert(mongo=mongo, ns=ns, json.list)
					}	
				}	
			}
		}
 	}

 	## write the JSON tweets from Streaming API to a text file
 	if (!is.null(file.name) & is.null(ns)){
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



 	## write the JSON tweets from Streaming API to a text file AND a mongo db
 	if (!is.null(file.name) & !is.null(ns)){
 		if (nchar(file.name)==0){ 
 			stop("The file where the tweets will be stored was not named properly.") }
 		db <- strsplit(ns, "\\.")[[1]][1]
 		coll <- strsplit(ns, "\\.")[[1]][2]
 		if (verbose==TRUE) { message("Storing tweets in collection '", 
 			coll, "' of database '", db, "' in MongoDB and in file '", file.name, "'") }
		mongo <- mongo.create(host=host, username=username, password=password, db=db)
		if (mongo.get.err(mongo)!=0){ stop("Error in connection to MongoDB") }
		conn <- file(description=file.name, open="a")
		# function that will insert tweets into db
		write.tweets <- function(x){
			if (nchar(x)>0){
				i <<- i + 1
				writeLines(x, conn, sep="")
				json.list <- fromJSON(x)
				fields <- names(json.list)
				if ('text' %in% fields){
					json.list[['_id']] <- json.list[['id_str']]
    				json.list[['timestamp']] <- format.twitter.date(json.list[['created_at']])
    				json.list[['random_number']] <- runif(1, 0, 1)
					mongo.insert(mongo=mongo, ns=ns, json.list)
				}	
			}	
		} 
		
		if (!is.null(tweets) && is.numeric(tweets) && tweets>0){	
			write.tweets <- function(x){
				if (i>=tweets){break}
				if (nchar(x)>0){
					i <<- i + 1
					writeLines(x, conn, sep="")
					json.list <- fromJSON(x)
					fields <- names(json.list)
					if ('text' %in% fields){
						json.list[['_id']] <- json.list[['id_str']]
    					json.list[['timestamp']] <- format.twitter.date(json.list[['created_at']])
    					json.list[['random_number']] <- runif(1, 0, 1)
						mongo.insert(mongo=mongo, ns=ns, json.list)
					}	
				}	
			}
		}
 	}

 	init <- Sys.time()	

	url <- "https://userstream.twitter.com/1.1/user.json"
	output <- tryCatch(oauth$OAuthRequest(URL=url, params=params, method="GET",
		customHeader=NULL, cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"), 
		writefunction = write.tweets, timeout = timeout), error=function(e) e)

	# housekeeping...
	if (!is.null(ns)){mongo.disconnect(mongo)}	
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

