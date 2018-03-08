#' @title
#'Get additional album info
#'@description
#'Get additional album info
#'
#'function to get additional album info from an artist using their ID
#'@param album_id The Spotify ID for the artist.
#'@param market Optional. An ISO 3166-1 alpha-2 country code. Supply this parameter to limit the response to one particular geographical market. For example, for albums available in Sweden: market=SE.
#'@return 'Get an Artist’s Albums
#'@param token An OAuth token created with \code{spotifyOAuth}.
#'@export
getAlbum<-function(album_id, market= "US", token){
  req<-httr::GET(paste0("https://api.spotify.com/v1/albums/",album_id, "?market=",market),httr::config(token = token))
  ctnt <- httr::content(req)
  if(req$status_code != 200) {
    print(paste0("ERROR #",req$status_code," - ERROR MESSAGE: ",ctnt$error$message))
    stop()
  }
  json<-jsonlite::fromJSON(jsonlite::toJSON(ctnt))
  result <- data.frame("albumId" = json$id,
                       "albumName" = json$name,
                       "artistName" = as.character(json$artists$name),
                       "artistId" = as.character(json$artists$id),
                       "popularity" = json$popularity,
                       "release_date" = json$release_date,
                       "release_date_precision" = json$release_date_precision,
                       "album_type" = json$album_type,
                       "track_total" = json$tracks$total)
  return(result)
}
