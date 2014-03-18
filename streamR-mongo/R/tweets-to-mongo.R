#' @rdname tweetsToMongo
#' @export
#'
#' @title 
#' Parses and exports tweets to a Mongo DB collection, adding fields useful
#' for their analysis
#'
#' @description
#' \code{tweetsToMongo} read tweets downloaded with \code{filterStream}, adds
#' an index and other fields that can facilitate their analysis, and stores
#' them in a MongoDB collection
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{sampleStream}}, \code{\link{userStream}}, \code{\link{parseTweets}}
#'
#' @param file.name string, name of the file where tweets were written. 
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
#' ## exporting those 10 tweets to MongoDB
#'   tweetsToMongo( file.name="tweets_rstats.json", ns="tweets.rstats")
#'
#' }
#'

tweetsToMongo <- function(file.name=NULL, ns=NULL, host='localhost', username="", 
	password="", verbose=TRUE)
{
	if (!is.null(ns)){require(rmongodb)}
	
	## from json to list
    results.list <- readTweets(file.name, verbose=FALSE)

    ## connecting to MongoDB
 	db <- strsplit(ns, "\\.")[[1]][1]
 	coll <- strsplit(ns, "\\.")[[1]][2]
	if (verbose==TRUE) { 
		message("Storing tweets in collection '", coll, "' of database '", 
			db, "' in MongoDB\n") }
	mongo <- mongo.create(host=host, username=username, password=password, db=db)
	if (mongo.get.err(mongo)!=0){ stop("Error in connection to MongoDB") }

    ## loop over tweets
    pb <- txtProgressBar(min=1,max=length(results.list), style=3) 
    i <- 1
    for (tweet in results.list){
    	fields <- names(tweet)
    	if ('text' %in% fields){
    		tweet[['_id']] <- tweet[['id_str']]
    		tweet[['timestamp']] <- format.twitter.date(tweet[['created_at']])
    		tweet[['random_number']] <- runif(1, 0, 1)
    		mongo.insert(mongo=mongo, ns=ns, tweet)
    	}
    	i <- i + 1
    	setTxtProgressBar(pb, i)
    }
}



format.twitter.date <- function(datestring, format="datetime"){
    if (format=="datetime"){
        date <- as.POSIXct(datestring, format="%a %b %d %H:%M:%S %z %Y")
    }
    if (format=="date"){
        date <- as.Date(datestring, format="%a %b %d %H:%M:%S %z %Y")
    }   
    return(date)
}
