#' @rdname getTweets
#' @export
#'
#' @title 
#' Connect to Mongo database and extract tweets that match 
#' conditions specified in the arguments.
#'
#' @description
#' \code{getTweets} opens a connection to a Mongo database and returns
#' tweets that match a series of conditions: whether it contains a certain 
#' keyword, whether it is or not a retweet, or whether or not it contains a 
#' hashtag. It allows to specify the fields of the tweet to be extracted. The 
#' function depends on the rmongodb package, and a mongo object needs to be 
#' loaded in user's workspace.
#'
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
#' @param df logical, default is \code{TRUE}, which will return tweets in a data
#' frame. Otherwise, tweets will be return in list format.
#'
#' @param fields vector of strings, indicates fields from tweets that will be 
#' returned. Default is the date and time of the tweet, its text, and the screen
#' name of the user that published it. See details for full list of possible fields.
#'
#' @param retweets logical, set to NULL by default (will return count of all tweets).
#' If \code{TRUE}, will count only tweets that are retweets (i.e. contain an embededed
#' retweeted status - manual retweets are not included). If \code{FALSE}, will count 
#' only tweets that are not retweets (manual retweets are now included).
#'
#' @param hashtags logical, set to NULL by default (will return count of all tweets).
#' If \code{TRUE}, will count only tweets that use a hashtag. If \code{FALSE}, will
#' count only tweets that do not contain a hashtag. 
#'
#' @param verbose logical, default is \code{TRUE}, which generates some output to the
#' R console with information about the count of tweets.
#'
#' @details
#' The following is a non-exhaustive of relevant fields that can be specified on the 
#' \code{fields} argument (for a complete list, check the documentation at:
#' \url{https://dev.twitter.com/docs/platform-objects}
#' Tweet: text, created_at, id_str, favorite_count, source, retweeted,
#' retweet_count, lang, in_reply_to_status_id, in_reply_to_screen_name
#' Entities: entities.hashtags, entities.user_mentions, entities.hashtags, entities.urls
#' Retweeted_status: retweeted_status.text, retweeted_status.created_at... (and all 
#' other tweet, user, and entities fields)
#' User: user.screen_name, user.id_str, user.geo_enabled, user.location, 
#' user.followers_count, user.statuses_count, user.friends_count, 
#' user.description, user.lang, user.name, user.url, user.created_at, user.time_zone 
#' Geo: geo.coordinates
#'
#' @examples \dontrun{
#' ## capture 100 tweets sent from New York City and stored in local MongoDB, 
#' ## in collection 'nyc' of database 'tweets'
#'  load(my_oauth)
#'  tweets <- filterStream( ns="tweets.nyc", 
#'       locations=c(-74,40,-73,41), tweets=100, oauth=my_oauth ) 
#'
#' ## connect to the Mongo database using rmongodb package
#'  library(rmongodb)
#'  mongo <- mongo.create("localhost", db="tweets")
#' ## if required, specify username and password
#' ## (MongoDB defaults are empty username and password)
#'  mongo.authenticate(mongo, username="", password="", 
#'    db="tweets")
#'
#' ## extract text from all tweets in the database
#'  tweets <- getTweets( ns="tweets.nyc", fields="text")
#' 
#' ## extract tweets, with text and screen name
#'  tweets <- getTweets( ns="tweets.nyc", fields=c("user.screen_name", "text"))
#'
#' ## extract all tweets that mention times square
#'  tweets <- getTweets( ns="tweets.nyc", string="times square")
#'
#' ## extract date of all tweets that mention 'times square'
#'  tweets <- getTweets( ns="tweets.nyc", string="times square", fields="created_at")
#' }
#'

## count number of tweets
getTweets <- function(ns, string=NULL, df=TRUE,
    fields=c('created_at', 'user.screen_name', 'text'),
    retweets=NULL, hashtags=NULL, verbose=TRUE)
{

    require(rmongodb)
    if (("mongo" %in% ls()) == FALSE){
        message("R couldn't find mongo object that connects to MongoDB in workspace. ",
            "Trying to connect to MongoDB using default setings...")
        db <- strsplit(ns, "\\.")[[1]][1]
        mongo <- mongo.create("localhost", db=db)
    }
    query <- list()
    fields.arg <- fields
    
    ## querying by string using regex
    if (!is.null(string)){
        query <- c(query, list(text=list('$regex'=string, '$options'='i')))
    }
    
    ## adding retweets and hashtag condition
    if (!is.null(retweets)){
        if (retweets==TRUE){
            query <- c(query, list(retweeted_status=list('$exists'=TRUE)))
        }
        if (retweets==FALSE){
            query <- c(query, list(retweeted_status=list('$exists'=FALSE)))
        }
    }
    if (!is.null(hashtags)){
        if (hashtags==TRUE){
            query <- c(query, list(entities.hashtags.1=list('$exists'=TRUE)))
        }
        if (hashtags==FALSE){
            query <- c(query, list(entities.hashtags.1=list('$exists'=FALSE)))
        }
    }

    ## all tweets if no condition is specified
    if (length(query)==0) query <- mongo.bson.empty()

    ## preparing size of output vector
    n <- mongo.count(mongo, ns=ns, query=query)
    if (n==0){
        stop("Zero tweets match the specified conditions.")
    }
    out <- rep(NA, n)

    ## adding fields
    if (is.null(fields)) {fields <- mongo.bson.empty()}
    if (!is.null(fields)) {
        new.fields <- list()
        for (f in fields){
            new.fields[f] <- 1L
        }
        fields <- new.fields
    }

    # making query
    res <- mongo.find(mongo=mongo, ns=ns, query=query, fields=fields)
    i <- 1
    if (verbose==TRUE & n>1) {pb <- txtProgressBar(min=1,max=n, style=3)}

    while (mongo.cursor.next(res)) {
        out[i] <- list(mongo.bson.to.list(mongo.cursor.value(res)))
        i <- i + 1

        if (verbose==TRUE & n>1) {setTxtProgressBar(pb, i)}
    }

    if (df==TRUE){
        out <- parseMongo(out, fields=fields.arg)
    }
    return(out)

}

parseMongo <- function(tweets, fields){
    fields.list <- strsplit(fields, "\\.")
    tweets.out <- matrix(NA, 
        nrow=length(tweets), ncol=length(fields))
    for (i in 1:length(fields)){
        tweets.out[,i] <- unlistWithNA(tweets, fields.list[[i]])
    }
    tweets.out <- data.frame(tweets.out, stringsAsFactors=F)
    names(tweets.out) <- fields
    return(tweets.out)
}







