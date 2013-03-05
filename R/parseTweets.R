#' @rdname parseTweets
#' @export
#'
#' @title 
#' Converts tweets in JSON format to data frame.
#'
#' @description
#' This function parses tweets downloaded using the \code{filterStream} function
#' and returns a data frame.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param tweets A character string naming the file where tweets are stored or the
#' name of the object in memory where the tweets were saved as strings.
#' @param simplify If \code{TRUE} it will return a data frame with only tweet and user
#' fields (i.e., no geographic information or url entities).
#'
#' @param verbose logical, default is \code{TRUE}, which will print in the console
#' the number of tweets that have been parsed.
#'
#' @details
#' \code{parseTweets} parses tweets downloaded using the \code{\link{filterStream}},
#' \code{\link{sampleStream}} or \code{\link{userStream}} functions
#' and returns a data frame where each row corresponds to one tweet and each column
#' represents a different field for each tweet (id, text, created_at, etc.).
#'
#' The total number of tweets that are parsed might be lower than the number of lines
#' in the file or object that contains the tweets because blank lines, deletion notices,
#' and incomplete tweets are ignored.
#'
#' @seealso \code{\link{filterStream}}, \code{\link{sampleStream}}, \code{\link{userStream}}
#'
#' @examples 
#' ## The dataset example_tweets contains 10 public statuses published
#' ## by @@twitterapi in plain text format. The code below converts the object
#' ## into a data frame that can be manipulated by other functions.
#'
#' data(example_tweets)
#' tweets.df <- parseTweets(example_tweets, simplify=TRUE)
#' 
#' \dontrun{
#' ## A more complete example, that shows how to capture a user's home timeline
#' ## for one hour using authentication via OAuth, and then parsing the tweets
#' ## into a data frame.
#'
#'  library(ROAuth)
#'  reqURL <- "https://api.twitter.com/oauth/request_token"
#'  accessURL <- "http://api.twitter.com/oauth/access_token"
#'  authURL <- "http://api.twitter.com/oauth/authorize"
#'  consumerKey <- "xxxxxyyyyyzzzzzz"
#'  consumerSecret <- "xxxxxxyyyyyzzzzzzz111111222222"
#'  my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
#'                               consumerSecret=consumerSecret,
#'                               requestURL=reqURL,
#'                               accessURL=accessURL,
#'                               authURL=authURL)
#'  my_oauth$handshake()
#'  userStream( file="my_timeline.json", with="followings",
#'          timeout=3600, oauth=my_oauth )
#'  tweets.df <- parseTweets("my_timeline.json")
#' } 
#' 
#'

parseTweets <- function(tweets, simplify=FALSE, verbose=TRUE){
    require(rjson)
    ## checking input is correct
    if (is.null(tweets)){
        stop("Error: you need to specify file or object where tweets text was stored.")
    }

    ## Read the text file and save it in memory as a list           
    if (length(tweets)==1 && file.exists(tweets)){
        lines <- readLines(tweets, encoding="UTF-8")
    }       
    else {
        lines <- tweets
    }

    results.list <- lapply(lines[nchar(lines)>0], function(x) tryCatch(fromJSON(x), error=function(e) e))

    ## removing lines that do not contain tweets or were not properly parsed
    errors <- which(unlist(lapply(results.list, length))<18)
    if (length(errors)>0){
        results.list <- results.list[-errors]
    }
              
    # Variables of interest, for each tweet and user
    tweet.vars <- c("text", "retweet_count", "favorited", "truncated", "id_str", 
        "in_reply_to_screen_name", "source", "retweeted", "created_at", 
        "in_reply_to_status_id_str", "in_reply_to_user_id_str")
    user.vars <- c("listed_count", "verified", "location", "id_str", "description", "geo_enabled", 
        "created_at", "statuses_count", "followers_count", "favourites_count", "protected", "url", 
        "name", "time_zone", "id", "lang", "utc_offset", "friends_count", "screen_name")
    place.vars <- c("country_code", "country", "place_type", "full_name", "name", "id") 
    

    # Saves tweet and user information into memory
    df.tweet <- as.data.frame(sapply(tweet.vars, parse.tweet, results.list), stringsAsFactors=FALSE)
    df.user <- as.data.frame(sapply(user.vars, parse.user, results.list), stringsAsFactors=FALSE)
    if (simplify==FALSE){
        df.place <- as.data.frame(sapply(place.vars, parse.place, results.list), stringsAsFactors=FALSE)
        df.coord <- as.data.frame(parse.coordinates(results.list), stringsAsFactors=FALSE); names(df.coord) <- c("lon", "lat")
        df.entities <- as.data.frame(parse.entities(results.list), stringsAsFactors=FALSE); names(df.entities) <- c("expanded_url", "url")          
    }
    # fixing duplicated names
    names(df.user)[c(4,7,12)] <- c("user_id_str", "user_created_at", "user_url")
    if (simplify==FALSE){
        names(df.place)[c(5,6)] <- c("place_name", "place_id")
    }

    if (simplify==FALSE){
        df <- cbind(df.tweet, df.user, df.place, df.coord, df.entities) 
    }
    if (simplify==TRUE){
        df <- cbind(df.tweet, df.user)    
    }

    # information message
    if (verbose==TRUE) cat(length(df$text), "tweets have been parsed.", "\n")
    return(df)
}

# Function to parse tweet information
parse.tweet <- function(var, list=list){
        values <- rep(NA, length(list))
        missing <- sapply((sapply(list, '[[', var)), is.null)
        values[missing==FALSE] <- unlist(sapply(list, '[[', var))
        return(values)
}

# Function to parse user information
parse.user <- function(user.var, list=list){
        values <- rep(NA, length(list))
        user <- sapply(list, '[', "user")
        missing <- sapply(sapply(user, '[', user.var), is.null)
        values[missing==FALSE] <- unlist(sapply(user, '[', user.var))
        return(values)
}

# Function to parse location
parse.place <- function(place.var, list=list){
        values <- rep(NA, length(list))
        place <- if (!is.null(sapply(list, '[', "place"))) sapply(list, '[', "place") else vector("list", length(list))
        missing <- sapply(sapply(place, '[[', place.var), is.null)
        values[missing==FALSE] <- unlist(sapply(place, '[[', place.var))
        return(values)
}

# Function to parse coordinates
parse.coordinates <- function(list=list){
        values <- matrix(NA, ncol=2, nrow=length(list))
        coord <- sapply(sapply(list, '[', "coordinates"), '[', "coordinates")
        missing <- as.character(sapply(sapply(coord, '[', "coordinates"), is.null))
        values[missing=="FALSE"] <- matrix(as.character(unlist(coord)[unlist(coord)!="Point"]), ncol=2, byrow=TRUE)
        return(values)
}

# Function to parse entities
parse.entities <- function(list=list){
        values <- matrix(NA, ncol=5, nrow=length(list))
        entities <- sapply(sapply(list, '[', "entities"), '[[', "entities")
        urls <- sapply(sapply(sapply(entities, '[[', "urls"), '[', 1), '[[', 1)
        missing <- as.character(sapply(urls, is.null))
        values[missing=="FALSE"] <- matrix(as.character(unlist(urls)), ncol=5, byrow=TRUE)
        values <- values[,c(4,5)]
        return(values)
}

