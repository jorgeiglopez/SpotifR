#' @title
#'Get top tracks of an Artist
#' @description
#'Get top tracks of an Artist
#'
#'
#'function to get top tracks of an Artist by artist ID and country
#'@param artist_id Artist ID
#'@param country Required. The country: an ISO 3166-1 alpha-2 country code.
#'@param token An OAuth token created with \code{spotifyOAuth}.
#'@export

# Roger Waters: 40DqL6Tv84cKT2pH2NMs9r
getArtistTopTracks<-function(artist_id, country = "US", token){
  require(dplyr)
  req<-httr::GET(paste0("https://api.spotify.com/v1/artists/",artist_id,"/top-tracks?country=",country),
                       httr::config(token = token))
  ctnt<- httr::content(req)
  if(req$status_code != 200) {
    print(paste0("ERROR #",req$status_code," - ERROR MESSAGE: ",ctnt$error$message))
    stop()
  }
  return(data.frame(id = sapply(ctnt$tracks, function(x) x$id),
                    track = sapply(ctnt$tracks, function(x) x$name),
                    popularity = sapply(ctnt$tracks, function(x) x$popularity),
                    album = sapply(ctnt$tracks, function(x) x$album$name),
                    album_id = sapply(ctnt$tracks, function(x) x$album$id),
                    preview= sapply(ctnt$tracks, function(x) x$preview_url),
                    track_number = sapply(ctnt$tracks, function(x) x$track_number),
                    duration = sapply(ctnt$tracks, function(x) x$duration_ms)))
                    ##artists = sapply(X=seq(1:10), FUN= function(x) ctnt$tracks[[x]]$artist[[1]]$name),
                    ##artists_ids= sapply(X=seq(1:10), FUN= function(x) ctnt$tracks[[x]]$artist[[1]]$id))
}

