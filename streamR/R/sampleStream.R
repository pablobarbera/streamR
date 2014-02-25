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
#' The total number of actual tweets that are captured might be lower than the number 
#' of tweets requested because blank lines, deletion notices, and incomplete
#' tweets are included in the count of tweets downloaded.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#' @seealso \code{\link{filterStream}}, \code{\link{userStream}}, \code{\link{parseTweets}}
#'
#' @param file.name string, name of the file where tweets will be written. 
#' "" indicates output to the console, which can be redirected to an R object.
#' If the file already exists, tweets will be appended (not overwritten).
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
#' @param verbose logical, default is \code{TRUE}, which generates some output to the
#' R console with information about the capturing process.
#'
#' @examples \dontrun{
#' ## capture a random sample of tweets
#' sampleStream( file.name="tweets_sample.json", user=FOO, password=BAR )
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
#'  sampleStream( file.name="tweets_sample.json", oauth=my_oauth )
#'
#' }
#'

sampleStream <- function(file.name, timeout=0, tweets=NULL, oauth=NULL, verbose=TRUE)
{
    if(!is.null(oauth)){library(ROAuth)}
    open.in.memory <- FALSE
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

    ## tweet counter
    i <- 0

   	# write the JSON tweets from the Twitter Streaming API to a text file
	# opening connection to file (temporary file if not specified)
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

    init <- Sys.time()
    # connecting to Streaming API
	url <- "https://stream.twitter.com/1.1/statuses/sample.json"
    if (!is.null(oauth)){
    	output <- tryCatch(oauth$OAuthRequest(URL=url, params=list(), method="POST",
    		customHeader=NULL, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"),
    		writefunction = write.tweets, timeout = timeout), error=function(e) e)
    }
    # housekeeping...
    close(conn)

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

