#' @title
#'Get a list of the playlists owned or followed by a Spotify user
#'@description
#'Get a list of the playlists owned or followed by a Spotify user
#'
#'
#'function to get Info about the playlist of a specific user using a username
#'@param username The username
#'@param token An OAuth token created with \code{spotifyOAuth}.
#'@param limit The number of results per request. Max 50
#'@return get playlist from a specific user using a username
#'@export

#function to get playlists from a user
getUserPlaylists<-function(username, limit = 50, token){
  resp = data.frame()
  hasMore = T
  URI = paste0("https://api.spotify.com/v1/users/",username,"/playlists?offset=0&limit=",limit)
  while(hasMore){
    req<-httr::GET(URI,httr::config(token = token))
    ctnt<-httr::content(req)
    if(req$status_code != 200) {
      print(paste0("ERROR #",req$status_code," - ERROR MESSAGE: ",ctnt$error$message))
      break()
    }
    if(is.null(ctnt[["next"]])){
      hasMore = FALSE
    } else {
      URI = ctnt[["next"]]
    }
    id<-sapply(ctnt$items, function(x) x$id)
    name<-sapply(ctnt$items, function(x) x$name)
    ownerid<-sapply(ctnt$items, function(x) x$owner$id)
    tracks<-sapply(ctnt$items, function(x) x$tracks$total)
    public<- sapply(ctnt$items, function(x) x$public)
    collaborative<- sapply(ctnt$items, function(x) x$collaborative)
    resp<-rbind(resp, data.frame(id,name,ownerid,tracks,public, collaborative, stringsAsFactors = F))
  }
  return(resp)
}
