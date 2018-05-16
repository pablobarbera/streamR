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
#' @param legacy logical, default is \code{FALSE}. Read tweets using old method (reading lines into memory and parsing
#' line by line). Try using \code{legacy=TRUE} if getting errors with default options.
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
#' Note also that the \code{retweet_count} field contains the number of times a given tweet
#' was retweeted at the time it was captured from the API, or for automatic retweets the number
#' of times the original tweet was retweeted.
#'
#' @seealso \code{\link{filterStream}}, \code{\link{sampleStream}}, \code{\link{userStream}}
#'
#' @examples 
#' ## The dataset example_tweets contains 10 public statuses published
#' ## by @@twitterapi in plain text format. The code below converts the object
#' ## into a data frame that can be manipulated by other functions.
#'
#' data(example_tweets)
#' tweets.df <- parseTweets(example_tweets, simplify=TRUE, legacy=TRUE)
#' 
#' \dontrun{
#' ## A more complete example, that shows how to capture a user's home timeline
#' ## for one hour using authentication via OAuth, and then parsing the tweets
#' ## into a data frame.
#'
#'  library(ROAuth)
#'  reqURL <- "https://api.twitter.com/oauth/request_token"
#'  accessURL <- "https://api.twitter.com/oauth/access_token"
#'  authURL <- "https://api.twitter.com/oauth/authorize"
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

parseTweets <- function(tweets, simplify=FALSE, verbose=TRUE, legacy=FALSE){
  
  # if tweets is not a file or an object, try legacy mode
  if (!file.exists(tweets[1]) & !exists(tweets[1])){
    legacy <- TRUE
  }
    
  if (!legacy){
    results <- stream_in(tweets)
    
    retweet_count <- rep(NA, length(results$text))
    if (!is.null(results$retweeted_status.retweet_count)){
      retweet_count <- ifelse(!is.na(results$retweeted_status.retweet_count),
                              results$retweeted_status.retweet_count, results$retweet_count)
    }                         
    
    favorite_count <- rep(NA, length(results$text))
    if (!is.null(results$retweeted_status.favorite_count)){
      favorite_count <- ifelse(!is.na(results$retweeted_status.favorite_count),
              results$retweeted_status.favorite_count, results$favorite_count)
    }      
    
    df <- data.frame(
      text = results$text,
      retweet_count = retweet_count,
      favorite_count = favorite_count,
      favorited = results$favorited,
      truncated = results$truncated,
      id_str = results$id_str,
      in_reply_to_screen_name = results$in_reply_to_screen_name,
      source = results$source,
      retweeted = results[[grep("retweeted", names(results))[1]]],
      created_at = results$created_at,
      in_reply_to_status_id_str = results$in_reply_to_status_id_str,
      in_reply_to_user_id_str = results$in_reply_to_user_id_str,
      lang = results$lang,
      listed_count = results$user.listed_count,
      verified = results$user.verified,
      location = results$user.location,
      user_id_str = results$user.id_str,
      description = results$user.description,
      geo_enabled = results$user.geo_enabled,
      user_created_at = results$user.created_at,
      statuses_count = results$user.statuses_count,
      followers_count = results$user.followers_count,
      favourites_count = results$user.favourites_count,
      protected = results$user.protected,
      user_url = results$user.url,
      name = results$user.name,
      time_zone = results$user.time_zone,
      user_lang = results$user.lang,
      utc_offset = results$user.utc_offset,
      friends_count = results$user.friends_count,
      screen_name = results$user.screen_name,
      stringsAsFactors=F)
    
    # adding geographic variables and url entities
    if (simplify==FALSE){
      df$country_code <- NA
      if (!is.null(results$place.country_code)) df$country_code <- results$place.country_code 
      df$country <- NA
      if (!is.null(results$place.country)) df$country <- results$place.country
      df$place_type <- NA
      if (!is.null(results$place.type)) df$place_type <- results$place.type
      df$full_name <- NA
      if (!is.null(results$place.full_name)) df$full_name <- results$place.full_name
      df$place_name <- NA
      if (!is.null(results$place.name)) df$place_name <- results$place.name
      df$place_id <- NA
      if (!is.null(results$place.id)) df$place_id <- results$place.id
      place_lat_1 <- rep(NA, nrow(df))
      if (!is.null(results$place.bounding_box.coordinates.0.0.1)){
        place_lat_1 <- results$place.bounding_box.coordinates.0.0.1
      }
      place_lat_2 <- rep(NA, nrow(df))
      if (!is.null(results$place.bounding_box.coordinates.0.1.1)){
        place_lat_2 <- results$place.bounding_box.coordinates.0.1.1
      }
      df$place_lat <- sapply(1:nrow(df), function(x) 
        mean(c(place_lat_1[x], place_lat_2[x]), na.rm=TRUE))
      place_lon_1 <- rep(NA, nrow(df))
      if (!is.null(results$place.bounding_box.coordinates.0.0.0)){
        place_lon_1 <- results$place.bounding_box.coordinates.0.0.0
      }
      place_lon_2 <- rep(NA, nrow(df))
      if (!is.null(results$place.bounding_box.coordinates.0.2.0)){
        place_lon_2 <- results$place.bounding_box.coordinates.0.2.0
      }
      df$place_lon <- sapply(1:nrow(df), function(x) 
        mean(c(place_lon_1[x], place_lon_2[x]), na.rm=TRUE))      
      df$lat <- NA
      if (!is.null(results$geo.coordinates.0)) df$lat <- results$geo.coordinates.0
      df$lon <- NA
      if (!is.null(results$geo.coordinates.1)) df$lon <- results$geo.coordinates.1
      df$expanded_url <- NA
      if (!is.null(results$entities.urls.0.expanded_url)){
        df$expanded_url <- results$entities.urls.0.expanded_url
      } 
      df$url <- NA
      if (!is.null(results$entities.urls.0.url)){
        df$url <- results$entities.urls.0.url
      } 
    }

  }
  
  if (legacy){
  
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
        retweet_count = unlistWithNA(results.list, 'retweet_count'),
        favorite_count = unlistWithNA(results.list, 'favorite_count'),
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

    # adding geographic variables and url entities
    if (simplify==FALSE){
        df$country_code <- unlistWithNA(results.list, c('place', 'country_code'))
        df$country <- unlistWithNA(results.list, c('place', 'country'))
        df$place_type <- unlistWithNA(results.list, c('place', 'place_type'))
        df$full_name <- unlistWithNA(results.list, c('place', 'full_name'))
        df$place_name <- unlistWithNA(results.list, c('place', 'name'))
        df$place_id <- unlistWithNA(results.list, c('place', 'id'))
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

  }
  
    # information message
    if (verbose==TRUE) message(length(df$text), " tweets have been parsed.")
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
