#' @title
#' Get Spotify catalog information for a single artist
#'
#'@description
#' Get Spotify catalog information for a single artist
#'
#'Function to get Spotify catalog information for a single artist identified by their unique Spotify ID.
#'@param artist_id The Spotify ID for the artist.
#'@param token An OAuth token created with \code{spotifyOAuth}.
#'@return Get Spotify catalog information for a single track identified by its unique Spotify ID.
#'@export
#'
getArtist<-function(artist_id, token){
  req <- httr::GET(paste0("https://api.spotify.com/v1/artists/",artist_id), httr::config(token = token))
  ctnt<-httr::content(req)
  if(req$status_code != 200) {
    print(paste0("ERROR #",req$status_code," - ERROR MESSAGE: ",ctnt$error$message))
    stop()
  }
  resp<-data.frame("id"=ctnt$id,
                   "name"=ctnt$name,
                   "popularity"=ctnt$popularity,
                   "followers"=ctnt$followers$total,
                   "genres"=paste(ctnt$genres,collapse ="; "))
  return(resp)
}

