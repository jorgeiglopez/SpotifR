#' @title
#' Get basic info of an User
#'@description
#' Get basic info of an User
#'
#'
##'function to get basic info of an user
#'@param user_id user id
#'@param token An OAuth token created with \code{spotifyOAuth}.
#'
#'
#'@export
#'
#function to retrieve information about a user
#information about an user
getUser<-function(user_id,token){
  req <- httr::GET(paste0("https://api.spotify.com/v1/users/",user_id), httr::config(token = token))
  ctnt<-httr::content(req)
  if(req$status_code != 200) {
    print(paste0("ERROR #",req$status_code," - ERROR MESSAGE: ",ctnt$error$message))
    stop()
  }
  return(data.frame(display_name=ctnt$display_name,
                   id=ctnt$id,
                   followers=ctnt$followers$total,
                   stringsAsFactors = F))
}


