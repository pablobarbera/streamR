#' @rdname topRetweets
#' @export
#'
#' @title 
#' Connect to Mongo database and extract top retweets that match
#' conditions specified in the arguments.
#'
#' @description
#' \code{topRetweets} opens a connection to a Mongo database and returns
#' all retweets (or only retweets that mention a specific keyword) ordered
#' by total number of retweets received.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param ns string, namespace of the MongoDB collection where tweets were stored. 
#' Generally, it will be of the form "database.collection". 
#'
#' @param string string, set to NULL by default (will return count of all tweets). If
#' it is a string, it will return the number of tweets that contain that string
#' in the text of the tweet.
#'
#' @param min numeric, set to 10 by default (will return all retweets whose
#' retweet count is at least 10). In large datasets, choose a high number to
#' increase speed of query.
#'
#' @param verbose logical, default is \code{TRUE}, which generates some output to the
#' R console with information about the count of tweets.
#'
#' @details
#' Note that this function will only return retweets that are made using the built-in
#' retweeting system - this is, 'manual' retweets using copy&paste are not included.
#' Also note that total retweet counts are based on Twitter's internal tally, and do
#' not reflect the number of retweets in the database. In other words, it could happen
#' that the most popular retweet in a given moment is a tweet that was originally sent
#' days ago, but was retweeted during the time of that tweets were captured.
#'
#' @examples \dontrun{
#' ## capture 100 tweets that mention "twitter" and store them in local MongoDB, 
#' ## in collection 'twitter' of database 'tweets'
#' ## (Note: since the 'track' parameter searches also in URLs, this search
#' ## is equivalent to capturing only tweets that contain pictures sent 
#' ## using Twitter's built-in service)
#'  load(my_oauth)
#'  tweets <- filterStream( ns="tweets.twitter", 
#'       track="twitter", tweets=100, oauth=my_oauth ) 
#'
#' ## connect to the Mongo database using rmongodb package
#'  library(rmongodb)
#'  mongo <- mongo.create("localhost", db="tweets")
#' ## if required, specify username and password
#' ## (MongoDB defaults are empty username and password)
#'  mongo.authenticate(mongo, username="", password="", 
#'    db="tweets")
#'
#' ## extract all retweets that were retweeted at least 100 times
#'  rts <- topRetweets( ns="tweets.twitter", min=100)
#'
#' ## show top 10 retweets from previous query
#'  head(rts, n=10)
#' }
#'
#'


topRetweets <- function(ns, string=NULL, min=10, verbose=TRUE)
{
    require(rmongodb)
    if (("mongo" %in% ls()) == FALSE){
        message("R couldn't find mongo object that connects to MongoDB in workspace. ",
            "Trying to connect to MongoDB using default setings...")
        db <- strsplit(ns, "\\.")[[1]][1]
        mongo <- mongo.create("localhost", db=db)
    }
    query <- list(retweeted_status=list('$exists'=TRUE), retweeted_status.retweet_count=list('$gte'=min))
    ## querying by string using regex
    if (!is.null(string)){
        query <- c(query, list(text=list('$regex'=string, '$options'='i')))
    }
    n <- mongo.count(mongo, ns=ns, query=query)
    if (n==0){
        stop("Zero retweets match the specified conditions.")
    }
    out <- rep(NA, n)
    # making query
    res <- mongo.find(mongo=mongo, ns=ns, query=query, fields=list(retweeted_status=1L))
    i <- 1
    if (verbose==TRUE) {pb <- txtProgressBar(min=1,max=n, style=3)}
    while (mongo.cursor.next(res)) {
        out[i] <- list(mongo.bson.to.list(mongo.cursor.value(res)))
        i <- i + 1
        if (verbose==TRUE) {setTxtProgressBar(pb, i)}
    }
    retweets <- lapply(out, "[[", "retweeted_status")
    retweets.ids <- unlist(lapply(retweets, '[', "id_str"))
    dups <- duplicated(retweets.ids, fromLast=TRUE)
    retweets <- retweets[!dups]
    sorted <- order(unlist(lapply(retweets, '[[', "retweet_count")), decreasing=TRUE)
    retweets <- retweets[sorted]
    if (verbose==TRUE) {
        cat("\n", length(retweets), "unique retweets with more than", min, "hits, after removing duplicates")
    }
    retweets <- data.frame(
            id_str = unlist(lapply(retweets, '[[', 'id_str')),
            screen_name = unlist(lapply(retweets, function(x) x[['user']][['screen_name']])),
            text = unlist(lapply(retweets, '[[', 'text')),
            retweet_count = unlist(lapply(retweets, '[[', 'retweet_count')),
            stringsAsFactors=F)
    return(retweets)
}





