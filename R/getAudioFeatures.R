#' @title
#'Get audio feature information for a single track identified by its unique Spotify ID.
#' @description
#'Get audio feature information for a single track identified by its unique Spotify ID.
#'
#'
#'function to get audio feature information for a single track identified by its unique Spotify ID.
#'@param track_id The Spotify ID for the track.
#'@param token An OAuth token created with \code{spotifyOAuth}.
#'@export
#'

getAudioFeatures<-function(track_id,token){
  req <- httr::GET(paste0("https://api.spotify.com/v1/audio-features/",track_id), httr::config(token = token))
  ctnt<-httr::content(req)
  if(req$status_code != 200) {
    print(paste0("ERROR #",req$status_code," - ERROR MESSAGE: ",ctnt$error$message))
    stop()
  }
  resp=data.frame( "id"=ctnt$id,
                   "danceability"=ctnt$danceability,
                   "energy"=ctnt$energy,
                   "key"=ctnt$key,
                   "loudness"=ctnt$loudness,
                   "mode"=ctnt$mode,
                   "speechiness"=ctnt$speechiness,
                   "acousticness"=ctnt$acousticness,
                   "instrumentalness"=ctnt$instrumentalness,
                   "liveness"=ctnt$liveness,
                   "valence"=ctnt$valence,
                   "tempo"=ctnt$tempo,
                   "duration_ms"=ctnt$duration_ms,
                   "time_signature"=ctnt$time_signature,
                   "uri"=ctnt$uri,
                   "analysis_url"=ctnt$analysis_url,
                   stringsAsFactors = F)
  return(resp)
}





