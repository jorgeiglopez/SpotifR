#' @title
#'Get the songs of an specific playlist
#' @description
#'Get the songs of an specific playlist
#'
#'
#'function to get songs about a specifc playlist
#'@param user_id Owner of the playlist ID (username)
#'@param playlist_id Playlist ID
#'@param limit The limit of result that brings each request
#'@param offset The index of the first songs to return. Default: 0 (the first object). Maximum offset: 100.000.
#'@param token An OAuth token created with \code{spotifyOAuth}.
#'@export

#function to get playlists' songs
getPlaylistsTracks<-function(user_id, playlist_id, offset=0, limit = 100, token){
  resp <- data.frame()
  hasMore = T
  URI = paste0("https://api.spotify.com/v1/users/",user_id,"/playlists/",playlist_id,"/tracks?offset=",offset,"&limit=", limit)
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
      json<-jsonlite::fromJSON(jsonlite::toJSON(ctnt))$items
      tracks<-unlist(json$track$name)
      popularity<-unlist(json$track$popularity)
      id<-unlist(json$track$id)
      artist_name<- sapply(seq(1:length(tracks)), function (x){
                    return(paste(data.frame(json$track$artists[x])$name, collapse = " || "))})
      artist_id<- sapply(seq(1:length(tracks)), function (x){
                  return(paste(data.frame(json$track$artists[x])$id, collapse = "; "))})
      #artistId<-unlist(lapply(seq(1:length(tracks)), function (x){return(data.frame(json$track$artists[x])$id[1])}))
      album<-unlist(json$track$album$name)
      albumId<-unlist(json$track$album$id)
      resp <- rbind(resp, data.frame(id, tracks,artist_name,artist_id,album,albumId,popularity, playlist_id,stringsAsFactors = F))
  }
  return(resp)
}
