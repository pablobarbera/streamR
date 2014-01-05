#' @rdname parseTweets
#' @export
#'
#' @title 
#' Converts tweets in JSON format to data frame.
#'
#' @description
#' This function parses tweets downloaded using \code{filterStream}, 
#' \code{sampleStream} or \code{userStream} and returns a data frame.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param tweets A character string naming the file where tweets are stored or the
#' name of the object in memory where the tweets were saved as strings.
#'
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
#' To parse json to a twitter list, see \code{\link{readTweets}}. That function can be significantly
#' faster for large files, when only a few fields are required.
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
    
    ## from json to list
    results.list <- readTweets(tweets, verbose=FALSE)

    # if no text in list, change it to NULL
    if (length(results.list)==0){
        stop(deparse(substitute(tweets)), " did not contain any tweets. ",
            "See ?parseTweets for more details.")
    }
    
    # constructing data frame with tweet and user variable
    df <- data.frame(
        text = unlistWithNA(results.list, 'text'),
        retweet_count = unlistWithNA(results.list, c('retweeted_status', 'retweet_count')),
        favorited = unlistWithNA(results.list, 'favorited'),
        truncated = unlistWithNA(results.list, 'truncated'),
        id_str = unlistWithNA(results.list, 'id_str'),
        in_reply_to_screen_name = unlistWithNA(results.list, 'in_reply_to_screen_name'),
        source = unlistWithNA(results.list, 'source'),
        retweeted = unlistWithNA(results.list, 'retweeted'),
        created_at = unlistWithNA(results.list, 'created_at'),
        in_reply_to_status_id_str = unlistWithNA(results.list, 'in_reply_to_status_id_str'),
        in_reply_to_user_id_str = unlistWithNA(results.list, 'in_reply_to_user_id_str'),
        lang = unlistWithNA(results.list, 'lang'),
        listed_count = unlistWithNA(results.list, c('user', 'listed_count')),
        verified = unlistWithNA(results.list, c('user', 'verified')),
        location = unlistWithNA(results.list, c('user', 'location')),
        user_id_str = unlistWithNA(results.list, c('user', 'id_str')),
        description = unlistWithNA(results.list, c('user', 'description')),
        geo_enabled = unlistWithNA(results.list, c('user', 'geo_enabled')),
        user_created_at = unlistWithNA(results.list, c('user', 'created_at')),
        statuses_count = unlistWithNA(results.list, c('user', 'statuses_count')),
        followers_count = unlistWithNA(results.list, c('user', 'followers_count')),
        favourites_count = unlistWithNA(results.list, c('user', 'favourites_count')),
        protected = unlistWithNA(results.list, c('user', 'protected')),
        user_url = unlistWithNA(results.list, c('user', 'url')),
        name = unlistWithNA(results.list, c('user', 'name')),
        time_zone = unlistWithNA(results.list, c('user', 'time_zone')),
        user_lang = unlistWithNA(results.list, c('user', 'lang')),
        utc_offset = unlistWithNA(results.list, c('user', 'utc_offset')),
        friends_count = unlistWithNA(results.list, c('user', 'friends_count')),
        screen_name = unlistWithNA(results.list, c('user', 'screen_name')),
        stringsAsFactors=F)

    # retweet_count is extracted from retweeted_status. If this is not a RT, set to zero
    df$retweet_count[is.na(df$retweet_count)] <- 0

    # adding geographic variables and url entities
    if (simplify==FALSE){
        df$country_code <- unlistWithNA(results.list, c('place', 'country_code'))
        df$country <- unlistWithNA(results.list, c('place', 'country'))
        df$place_type <- unlistWithNA(results.list, c('place', 'place_type'))
        df$full_name <- unlistWithNA(results.list, c('place', 'full_name'))
        df$place_name <- unlistWithNA(results.list, c('place', 'place_name'))
        df$place_id <- unlistWithNA(results.list, c('place', 'place_id'))
        place_lat_1 <- unlistWithNA(results.list, c('place', 'bounding_box', 'coordinates', 1, 1, 2))
        place_lat_2 <- unlistWithNA(results.list, c('place', 'bounding_box', 'coordinates', 1, 2, 2))
        df$place_lat <- sapply(1:length(results.list), function(x) 
            mean(c(place_lat_1[x], place_lat_2[x]), na.rm=TRUE))
        place_lon_1 <- unlistWithNA(results.list, c('place', 'bounding_box', 'coordinates', 1, 1, 1))
        place_lon_2 <- unlistWithNA(results.list, c('place', 'bounding_box', 'coordinates', 1, 3, 1))
        df$place_lon <- sapply(1:length(results.list), function(x) 
            mean(c(place_lon_1[x], place_lon_2[x]), na.rm=TRUE))
        df$lat <- unlistWithNA(results.list, c('geo', 'coordinates', 1))
        df$lon <- unlistWithNA(results.list, c('geo', 'coordinates', 2))
        df$expanded_url <- unlistWithNA(results.list, c('entities', 'urls', 1, 'expanded_url'))
        df$url <- unlistWithNA(results.list, c('entities', 'urls', 1, 'url'))

    }

    # information message
    if (verbose==TRUE) cat(length(df$text), "tweets have been parsed.", "\n")
    return(df)
}


unlistWithNA <- function(lst, field){
    if (length(field)==1){
        notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field]])))
        vect <- rep(NA, length(lst))
        vect[notnulls] <- unlist(lapply(lst[notnulls], '[[', field))
    }
    if (length(field)==2){
        notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
        vect <- rep(NA, length(lst))
        vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]]))
    }
    if (length(field)==3 & field[1]!="geo"){
        notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]][[field[3]]])))
        vect <- rep(NA, length(lst))
        vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[field[3]]]))
    }
    if (field[1]=="geo"){
        notnulls <- unlist(lapply(lst, function(x) !is.null(x[[field[1]]][[field[2]]])))
        vect <- rep(NA, length(lst))
        vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[as.numeric(field[3])]]))
    }

    if (length(field)==4 && field[2]!="urls"){
        notnulls <- unlist(lapply(lst, function(x) length(x[[field[1]]][[field[2]]][[field[3]]][[field[4]]])>0))
        vect <- rep(NA, length(lst))
        vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[field[3]]][[field[4]]]))
    }
    if (length(field)==4 && field[2]=="urls"){
        notnulls <- unlist(lapply(lst, function(x) length(x[[field[1]]][[field[2]]])>0))
        vect <- rep(NA, length(lst))
        vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) x[[field[1]]][[field[2]]][[as.numeric(field[3])]][[field[4]]]))
    }
    if (length(field)==6 && field[2]=="bounding_box"){
        notnulls <- unlist(lapply(lst, function(x) length(x[[field[1]]][[field[2]]])>0))
        vect <- rep(NA, length(lst))
        vect[notnulls] <- unlist(lapply(lst[notnulls], function(x) 
            x[[field[1]]][[field[2]]][[field[3]]][[as.numeric(field[4])]][[as.numeric(field[5])]][[as.numeric(field[6])]]))
    }
    return(vect)
}
