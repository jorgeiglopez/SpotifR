#' @title
#'Get Related artists
#'@description
#'Get Related artists
#'
#'
#'function to get the related artists of an Artist
#'@param artist_id Id of the artist
#'@param token An OAuth token created with \code{spotifyOAuth}.
#'@export


getRelatedArtist <-function(artist_id, token){
  req <- httr::GET(paste0("https://api.spotify.com/v1/artists/",artist_id,"/related-artists"),httr::config(token = token))
  ctnt<-httr::content(req)
  artists <- ctnt$artists
  if(req$status_code != 200) {
    print(paste0("ERROR #",req$status_code," - ERROR MESSAGE: ",ctnt$error$message))
    stop()
  }
  return (data.frame(
      id = sapply(artists, function(x) x$id),
      name = sapply(artists, function(x) x$name),
      popularity = sapply(artists, function(x) x$popularity),
      followers = sapply(artists, function(x) x$followers$total),
      gnres = sapply(artists, function(x) paste(x$genres, collapse = "; "))
  ))
}
