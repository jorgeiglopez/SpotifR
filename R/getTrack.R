#' @title
#'Get a Track information
#'@description
#'Get a Track information
#'
#'
#'Get Spotify catalog information for a single track identified by its unique Spotify ID.
#'@param id The Spotify ID for the track.
#'@return 'Get Spotify catalog information for a single track identified by its unique Spotify ID.
#'@param token An OAuth token created with \code{spotifyOAuth}.
#'@export

getTrack<-function(id, token){
  req<-httr::GET(paste0("https://api.spotify.com/v1/tracks/",id),httr::config(token = token))
  ctnt<-httr::content(req)
  if(req$status_code != 200) {
    print(paste0("ERROR #",req$status_code," - ERROR MESSAGE: ",ctnt$error$message))
    stop()
  }
  return(data.frame(
    id = ctnt$id,
    name = ctnt$name,
    popularity = ctnt$popularity,
    album = ctnt$album$name,
    album_id = ctnt$album$id,
    preview= ctnt$preview_url,
    track_number = ctnt$track_number,
    duration = ctnt$duration_ms,
    artists = paste(lapply(ctnt$artists, function(x) x$name), collapse = " || "),
    artists_ids= paste(lapply(ctnt$artists, function(x) x$id), collapse = ";")
  ))
}

