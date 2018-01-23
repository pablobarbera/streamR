#' @rdname readTweets
#' @export
#'
#' @title 
#' Converts tweets in JSON format to R list.
#'
#' @description
#' This function parses tweets downloaded using \code{filterStream},
#' \code{sampleStream} or \code{userStream} and returns a list.
#'
#' @author
#' Pablo Barbera \email{pablo.barbera@@nyu.edu}
#'
#' @param tweets A character string naming the file where tweets are stored or the
#' name of the object in memory where the tweets were saved as strings.
#'
#' @param verbose logical, default is \code{TRUE}, which will print in the console
#' the number of tweets that have been parsed.
#' 
#' @details
#' This function is the first step in the \code{\link{parseTweets}} function and
#' is provided now as an independent function for convenience purposes. In cases
#' where only one field is needed, it can be faster to extract it directly from
#' the JSON data read in R as a list. It can also be useful to extract fields
#' that are not parsed by \code{\link{parseTweets}}, such as hashtags or mentions.
#'
#' The total number of tweets that are parsed might be lower than the number of lines
#' in the file or object that contains the tweets because blank lines, deletion notices,
#' and incomplete tweets are ignored.
#'
#' @seealso \code{\link{parseTweets}}.
#'
#' @examples 
#' ## The dataset example_tweets contains 10 public statuses published
#' ## by @@twitterapi in plain text format. The code below converts the object
#' ## into a list and extracts only the text.
#'
#' data(example_tweets)
#' tweets.list <- readTweets(example_tweets)
#' only.text <- unlist(lapply(tweets.list, '[[', 'text'))
#' ## it can be done with an explicit loop:
#' only.text <- c()
#' for (i in 1:length(tweets.list)){
#'    only.text[i] <- tweets.list[[i]]['text']
#' }
#' print(unlist(only.text))
#' 
#' 
#'

readTweets <- function(tweets, verbose=TRUE){
    ## checking input is correct
    if (is.null(tweets)){
        stop("Error: you need to specify file or object where tweets text was stored.")
    }

    ## Read the text file and save it in memory as a list           
    if (length(tweets)==1 && file.exists(tweets)){
        lines <- readLines(tweets)
    }       
    else {
        lines <- tweets
    }
    ## Converting to UTF-8
    lines <- iconv(lines, "ASCII", "UTF-8", sub="")

    results.list <- lapply(lines[nchar(lines)>0], function(x) tryCatch(fromJSON(x), error=function(e) e))

    ## check if JSON file is coming from search endpoint instead of API
    search <- 'search_metadata' %in% names(results.list[[1]])
    if (search) results.list <- results.list[[1]]$statuses

    ## removing lines that do not contain tweets or were not properly parsed
    #errors <- which(unlist(lapply(results.list, length))<18)
    if (!search){
        errors <- which(unlist(lapply(results.list, function(x) 'id' %in% names(x) == FALSE)))
        if (length(errors)>0){
            results.list <- results.list[-errors]
        }        
    }
              
    # information message
    if (verbose==TRUE) message(length(results.list), " tweets have been parsed.")
    return(results.list)
}



